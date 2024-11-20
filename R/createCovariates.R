#' Extract covariate values from spatial rasters and prepare rasters for spatial predictions
#'
#' @description
#'  This function extracts covariate values from spatial raster data 
#'  (e.g. for use in modelling) and prepares these covariates for use in 
#'  spatial predictions. 
#'  
#'  It accepts a camera trap table containing spatial information, 
#'  along with either a directory containing covariate raster files or a character 
#'  vector specifying the file paths of these covariate rasters. 
#'  
#'  Additionally, users can provide parameters to control how covariates are
#'  extracted, and how they are aggregated to prediction rasters. 
#'  
#'  The function generates prediction rasters based on a provided template or 
#'  creates one automatically if no template is provided.
#'  
#'  The function returns a list containing the camera trap dataset with 
#'  extracted covariate values (e.g for use in occupancy modelling), and 
#'  prediction rasters ready for spatial modeling.
#'  
#' @param CTtable sf object as defined by the \code{sf} package. Essentially a camera trap data frame with spatial information.
#' @param directory character. The directory containing the covariate rasters.
#' @param filenames character (optionally named). A vector of file paths of covariate rasters. If it is named the covariates will be named according to the names. If unnamed the file names will be used as covariate names.
#' @param buffer_ct numeric. A value (in meters) by which to buffer the point locations in \code{CTtable} for extraction of covariate values. 
#' @param bilinear logical. If \code{TRUE}, extract covariate values with bilinear interpolation (nearest 4 raster cells). If \code{FALSE}, extract value at the cell the point falls in. Only relevant if \code{buffer_ct} is 0.
#' @param buffer_aoi numeric. A value (in meters) by which to buffer the overall camera trapping grid to ensure that prediction rasters are larger than the camera trapping grid.
#' @param raster_template SpatRaster. A SpatRaster (as defined in the \code{terra} package) to use as template for the creation of prediction rasters.
#' @param resolution numeric. Spatial resolution of prediction rasters. Ignored if \code{raster_template} is provided.
#' @param append logical. If \code{TRUE}, add the extracted covariates to the existing \code{CTtable}. If \code{FALSE}, return only the extracted covariate values without the existing \code{CTtable}.
#' @param formats character. Possible file formats for raster data. Defaults to \code{.tif} files.
#' @param recursive logical. If \code{TRUE}, search for raster files recursively in subdirectories when a directory is provided. Defaults to \code{FALSE}.
#'
#' @details
#' 
#' The input camera trap table must be an \code{\link[sf]{sf}} object (a data 
#' frame with a geometry column specifying the spatial information). For details 
#' on how to convert an exising camera trap table to sf, see the examples below.
#' The input rasters can be in different coordinate systems. During covariate 
#' extraction the CTtable is projected to each raster's coordinate system 
#' individually. For the prediction raster all input rasters are either 
#' resampled or reprojected to a consistent coordinate system.
#' 
#' When \code{recursive = TRUE} and a directory is provided, the function will search
#' for raster files in all subdirectories. In this case, the subdirectory names are
#' used as covariate names, and only one raster file per subdirectory is allowed.
#' 
#' @return
#' \describe{
#' A list containing three elements:
#'   \item{CTtable}{An \code{sf} object representing the camera trap data frame with extracted covariate values.}
#'   \item{predictionRaster}{A \code{SpatRaster} object containing covariate raster layers}
#'   \item{originalRaster}{A list of the original input rasters}
#' }
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # load camera trap table
#' data(camtraps)
#' 
#' # create sf object
#' camtraps_sf <- st_as_sf(camtraps, 
#'                         coords = c("utm_x", "utm_y"), 
#'                         crs = 32650)
#' 
#' # extract covariates (with 100m buffer around cameras)
#' covariates <- createCovariates(camtraps_sf, 
#' "path/to/covariate_rasters", 
#' buffer_ct = 100, 
#' buffer_aoi = 1000,
#' resolution = 100)
#' 
#' # camera trap table with extracted covariate values
#' camtraps_sf_cov <- covariates$CTtable
#' 
#' # covariate raster layer
#' r_cov <- covariates$predictionRaster
#' 
#' }
#' 
createCovariates <- function(CTtable,
                             directory,
                             filenames, 
                             buffer_ct = 0,
                             bilinear = FALSE,
                             buffer_aoi = 1000,
                             raster_template = NULL,
                             resolution = NULL,
                             append = TRUE, 
                             formats = ".tif",
                             recursive = FALSE)
{
  if(!inherits(CTtable, "sf")){ 
    stop("CTtable is not an sf object.")
  }
  if(is.na(sf::st_crs(CTtable))) stop("Coordinate reference system of CTtable is not defined. See: sf::st_crs(CTtable).")
  
  if (nrow(CTtable) == 0) {
    stop("The 'CTtable' is empty.")
  }
  
  if (!hasArg(directory) && !hasArg(filenames)) {
    stop("Please provide either 'directory' or 'filenames'.")
  }
  
  if (hasArg(directory) && hasArg(filenames)) {
    stop("Provide either 'directory' or 'filenames', not both.")
  }
  
  if (is.null(raster_template) && is.null(resolution)) {
    warning("Neither 'raster_template' or 'resolution' are defined. No prediction rasters will be created.", call. = F, immediate. = T)
    create_raster <- FALSE
  } else {
    create_raster <- TRUE
  }
  
  # Check buffer sizes
  if (!is.numeric(buffer_ct) || buffer_ct < 0) {
    stop("Buffer size for 'buffer_ct' must be numeric and positive.")
  }
  if (!is.numeric(buffer_aoi) || buffer_aoi < 0) {
    stop("Buffer size for 'buffer_aoi' must be numeric and positive.")
  }
  
  # Throw warning if buffer sizes exceed thresholds
  if (buffer_ct > 2000) {
    warning("Buffer size for 'buffer_ct' exceeds 2km. Ensure this is intended.", call. = F, immediate. = T)
  }
  if (buffer_aoi > 50000) {
    warning("Buffer size for 'buffer_aoi' exceeds 50km. Ensure this is intended.", call. = F, immediate. = T)
  }
  
  if(is.null(raster_template) && !is.null(resolution)) {
    if (resolution <= 0) {
      stop("Invalid spatial resolution. Please provide a positive value.")
    }
    
    if(buffer_aoi > 0) {
      extent_aoi <- terra::ext(terra::buffer(vect(CTtable), width = buffer_aoi))
    } else {
      extent_aoi <- terra::ext(CTtable)
    }
    
    
    
    
    # # Create a raster template based on extent_aoi, resolution, in coordinate system of camera trap table
    # raster_template <- terra::rast(extent_aoi, 
    #                                resolution = resolution, 
    #                                crs = sf::st_crs(CTtable)$wkt)
    
    
    # Get the CRS of the input data
    input_crs <- sf::st_crs(CTtable)
    
    # Check if the CRS is geographic (like WGS84)
    if(input_crs$proj4string %>% stringr::str_detect("\\+proj=longlat")) {
      # If geographic, create a temporary UTM-based CRS for the center of the study area
      center_point <- sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(CTtable))))
      utm_zone <- (floor((center_point[1] + 180)/6) %% 60) + 1
      temp_crs <- sprintf("+proj=utm +zone=%d +datum=WGS84", utm_zone)
      
      # Transform extent to the temporary UTM CRS
        extent_aoi_utm <- sf::st_transform(CTtable, temp_crs) %>% 
          vect() %>% 
          terra::buffer(width = buffer_aoi)  %>%
          # sf::st_buffer(dist = buffer_aoi) %>%
          # sf::st_bbox() %>% 
          terra::ext()

      
      # Create raster template in UTM
      raster_template_utm <- terra::rast(extent_aoi_utm, 
                                         resolution = resolution, 
                                         crs = temp_crs)
      
      # Project back to original CRS
      raster_template <- terra::project(raster_template_utm, input_crs$proj4string)
    } else {
      # If not geographic, create raster template directly
      raster_template <- terra::rast(extent_aoi, 
                                     resolution = resolution, 
                                     crs = input_crs$wkt)
    }
    
    # After creating raster_template
    n_cells <- terra::ncell(raster_template)
    if(n_cells < 100) {
      warning("The resulting raster has very few cells (", n_cells, "). Consider using a smaller resolution value.")
    } else if(n_cells > 1e8) {
      warning("The resulting raster has a very large number of cells (", n_cells, "). This may cause performance issues. Consider using a larger resolution value.")
    }
    
  }
  
  if (hasArg(directory)) {
    if(length(directory) != 1) stop("The 'directory' parameter should be a single directory path.")
    
    # Get file paths from directory
    lf_covariates <- list.files(directory, recursive = recursive, pattern = paste0(formats, "$"), full.names = TRUE)
    if(length(lf_covariates) == 0) stop(paste("No files with suffix", paste(formats, collapse = "/"), "found in", directory))
    
    if(recursive) {
      # Use directory names as covariate names when recursive is TRUE
      covariate_names <- basename(dirname(lf_covariates))
      
      
      # Check for subdirectories with depth > 1
      dir_depth <- sapply(lf_covariates, function(x) length(strsplit(x, .Platform$file.sep)[[1]]))
      if(any(dir_depth > length(strsplit(directory, .Platform$file.sep)[[1]]) + 2)) {
        warning("Subdirectories with depth > 1 found. Only using rasters in the first level of subdirectories.", call. = F, immediate. = T)
        valid_depth <- dir_depth <= length(strsplit(directory, .Platform$file.sep)[[1]]) + 2
        lf_covariates <- lf_covariates[valid_depth]
        covariate_names <- covariate_names[valid_depth]
      }
      
      # Check for multiple files per covariate
      files_per_cov <- table(covariate_names)
      if(any(files_per_cov > 1)) {
        multi_file_covs <- names(files_per_cov[files_per_cov > 1])
        stop(paste("Multiple raster files found for covariate(s):", 
                   paste(multi_file_covs, collapse = ", "), 
                   ". Ensure only one raster file per covariate directory."))
      }
    } else {
      # Use file names as covariate names when recursive is FALSE (original behavior)
      covariate_names <- tools::file_path_sans_ext(basename(lf_covariates))
    }
    
    list_r_cov <- lapply(lf_covariates, terra::rast)
  } else {
    if (!is.vector(filenames) && !is.character(filenames)) {
      stop("Invalid 'filenames'. Please provide a (named) vector.")
    }
    
    # ensure files exist
    file_exists <- file.exists(filenames)
    if(!all(file_exists)) stop(paste("file in filenames don't exist:\n", 
                                     paste(filenames[!file_exists], collapse = "\n")))
    
    # Extract covariate names and file paths from parameter 'filenames'
    if(!is.null(names(filenames))) {
      covariate_names <- names(filenames)
      lf_covariates <- unname(filenames)
    } else {
      covariate_names <- tools::file_path_sans_ext(basename(filenames))
    }
    list_r_cov <- lapply(lf_covariates, function(file) terra::rast(file))
  }
  names(list_r_cov) <- covariate_names
  
  
  
  if(length(list_r_cov) == 0) stop("No covariate raster loaded. This may be a bug.")
  
  # switch for bilinear interpolation
  if(bilinear) {
    extract_method <- "bilinear"
  } else {
    extract_method <- "simple"
  }
  
  
  
  cov_value_list <- lapply(1:length(list_r_cov), FUN = function(i) {
    current_crs <- sf::st_crs(list_r_cov[[i]])$wkt
    CTtable_i <- sf::st_transform(CTtable, current_crs)
    CTtable_i_v <- terra::vect(CTtable_i)
    
    # extract at exact camera trap location or with buffer
    if(buffer_ct == 0) {
      extracted <- terra::extract(list_r_cov[[i]], 
                                  CTtable_i_v, 
                                  method = extract_method)
    } else {
      # CTtable_i_buff <- sf::st_buffer(CTtable_i, 
      #                                 dist = buffer_ct)    # slow!!
      CTtable_i_buff <- terra::buffer(CTtable_i_v, 
                                      width = buffer_ct)     # fast
      extracted <- terra::extract(list_r_cov[[i]], 
                                  CTtable_i_buff, 
                                  fun = mean, 
                                  na.rm = T)
    }
    
    # Handle multi-band rasters
    if(nlyr(list_r_cov[[i]]) > 1) {
      layer_names <- names(list_r_cov[[i]])
      colnames(extracted)[-1] <- paste(covariate_names[i], layer_names, sep = "_")
    } else {
      colnames(extracted)[-1] <- covariate_names[i]
    }
    
    return(extracted[, -1, drop = FALSE])
  })
  
  df_covariates <- do.call(cbind, cov_value_list)
  
  
  # cbind to input camera trap table, if requested
  if(append) {
    df_covariates_out <- cbind(CTtable, df_covariates)
  } else {
    df_covariates_out <- df_covariates
  }
  
  # clip rasters to desired extent (in their original CRS)
  list_r_cov_c <- lapply(1:length(list_r_cov), 
                         FUN = function(i){
                           # get raster CRS
                           current_crs <- sf::st_crs(list_r_cov[[i]])
                           # project camera trap table to that CRS
                           CTtable_i <- sf::st_transform(CTtable, crs = current_crs)
                           CTtable_i_v <- terra::vect(CTtable_i)
                           
                           # get extent of AOI (buffered, if requested)
                           if(buffer_aoi > 0) {
                             # extent_aoi  <- terra::ext(sf::st_buffer(CTtable_i, buffer_aoi))
                             extent_aoi  <- terra::ext(terra::buffer(CTtable_i_v, buffer_aoi))
                             
                           } else {
                             extent_aoi  <- terra::ext(CTtable_i)
                           }
                           
                           # clip raster
                           out <- try(terra::crop(list_r_cov[[i]], 
                                                  y = extent_aoi)) 
                           
                           if(inherits(out, "try-error")) {
                             warning(paste("Failed to crop raster '", names(list_r_cov)[i], "'. Skipping this covariate."), call. = F, immediate. = T)
                             return(NULL)
                           } else {
                             return(out)
                           }
                         })
  names(list_r_cov_c) <- names(list_r_cov)
  
  if(create_raster){
    # resample rasters according to template
    r_cov_list <- lapply(1:length(list_r_cov_c),
                         FUN = function(i){
                           if(is.null(list_r_cov_c[[i]])) return(NULL)
                           current_crs <- sf::st_crs(list_r_cov_c[[i]])
                           
                           crs_template <- sf::st_crs(raster_template)
                           
                           if(current_crs == crs_template) out <- terra::resample(list_r_cov_c[[i]], raster_template)
                           if(current_crs != crs_template) out <- terra::project(list_r_cov_c[[i]], raster_template)
                           
                           return(out)
                         })
    names(r_cov_list) <- names(list_r_cov)
    
    r_cov <- terra::rast(r_cov_list)
  } else {
    r_cov <- NULL
  }
  return(list(CTtable = df_covariates_out,
              predictionRaster = r_cov,
              originalRaster = list_r_cov))
}
