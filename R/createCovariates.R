
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
#' @param raster_template SparRaster. A SparRaster (as defined in the \code{terra} package) to use as template for the creation of prediction rasters.
#' @param resolution numeric. Spatial resolution of prediction rasters. Ignored if \code{raster_template} is provided.
#' @param append logical. If \code{TRUE}, add the extracted covariates to the existing \code{CTtable}. If \code{FALSE}, return only the extracted covariate values without the existing \code{CTtable}.
#' @param formats character. Possible file formats for raster data. Defaults to \code{.tif} files.
#'
#' @details
#' 
#' The input camera trap table must be an \code{\link[sf]{sf}} object (a data 
#' frame with a geometry column specifying the spatial information). For details 
#' on how to convert an exising camera trap table to sf, see the Examples below.
#' The input rasters can be in different coordinate systems. During covariate 
#' extraction the CTtable is projected to each raster's coordinate system 
#' individually. For the prediction raster all input rasters are either 
#' resampled or reprojected to a consistent coordinate system.
#' 
#' 
#' @return
#' \describe{
#'   \item{CTtable}{An \code{sf} object representing the camera trap data frame with extracted covariate values.}
#'   \item{prediction_raster}{A \code{SpatRaster} object containing prediction rasters.}
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
#' camtraps_sf_cov <- createCovariates(camtraps_sf, 
#' "path/to/covariate_rasters", 
#' buffer_ct = 100, 
#' buffer_aoi = 1000, 
#' raster_template = my_template)
#' 
#' }
#' 
createCovariates <- function(CTtable,
                             directory,
                             filenames, 
                             buffer_ct = 0,
                             bilinear = FALSE,
                             buffer_aoi = 1000,
                             raster_template,
                             resolution,
                             append = TRUE, 
                             formats = ".tif")
{
  
  # Future improvements: performance of st_buffer() when raster is in WGS84
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
  
  if (!hasArg(raster_template) && !hasArg(resolution)) {
    # stop("Please provide either 'raster_template' or 'resolution'.")
    message("Neither 'raster_template' or 'resolution' are defined. No prediction rasters will be created.")
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
    warning("Buffer size for 'buffer_ct' exceeds 2km. Ensure this is intended.")
  }
  if (buffer_aoi > 100000) {
    warning("Buffer size for 'buffer_aoi' exceeds 100km. Ensure this is intended.")
  }
  
  
  if(!hasArg(raster_template) && hasArg(resolution)) {
    
    if (resolution <= 0) {
      stop("Invalid spatial resolution. Please provide a positive value.")
    }
    
    CTtable_crs <- sf::st_crs(CTtable)
    extent_aoi <- terra::ext(sf::st_buffer(CTtable, buffer_aoi))
    
    # Create a raster template based on extent_aoi, resolution, and CTtable's CRS
    raster_template <- terra::rast(extent_aoi, res = resolution, crs = CTtable_crs)
  }
  
  
  
  if (hasArg(directory)) {
    if(length(directory) != 1) stop("The 'directory' parameter should be a single directory path.")
    
    # Get file paths from directory, and derive covariate names from file names
    lf_covariates <- list.files(directory, recursive = FALSE, pattern = paste0(formats, "$"))
    if(length(lf_covariates) == 0) stop(paste("No files with suffix", paste(formats, collapse = "/"), "found in", directory))
    covariate_names <- strsplit(lf_covariates, split = ".", fixed = TRUE)
    covariate_names <- sapply(covariate_names, FUN = function(x) x[1])
    list_r_cov <- lapply(file.path(directory, lf_covariates), terra::rast)
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
      covariate_names <- strsplit(filenames, split = ".", fixed = TRUE)
      covariate_names <- sapply(covariate_names, FUN = function(x) x[1])
    }
    list_r_cov <- lapply(lf_covariates, function(file) terra::rast(file))
  }
  names(list_r_cov) <- covariate_names
  
  if(length(list_r_cov) == 0) stop("No covariate raster loaded. This may be a bug.")
  

  #  Check buffer sizes
  if (buffer_ct < 0 || buffer_aoi < 0) {
    stop("Buffer sizes must be non-negative.")
  }
  

  if(!is.null(buffer_ct))    CTtable_buff <- sf::st_buffer(CTtable, buffer_ct)
  
  # switch for bilinear interpolation
  if(bilinear) {
    extract_method <- "bilinear"
  } else {
    extract_method <- "simple"
  }
  
  # extract covariate values from rasters
  cov_value_list <- lapply(1:length(list_r_cov), FUN = function(i) {
    current_crs <- sf::st_crs(list_r_cov[[i]])
    CTtable_i <- sf::st_transform(CTtable, current_crs)
    
    # extract at exact camera trap location
    if(buffer_ct == 0) {
      return(terra::extract(list_r_cov[[i]], 
                            terra::vect(CTtable_i), 
                            method = extract_method) [, -1, drop = F])
    }
    # extract at camera trap location + buffer
    if(!is.null(buffer_ct)){
      CTtable_i_buff <- st_buffer(sf::st_transform(CTtable_buff, 
                                                   current_crs), 
                                  dist = buffer_ct)
      return(terra::extract(list_r_cov[[i]], 
                            terra::vect(CTtable_i_buff), 
                            fun = mean, 
                            na.rm = T) [, -1, drop = F])
    }
  })
  
  df_covariates <- data.frame(cov_value_list)
  names(df_covariates) <- names(list_r_cov)
  
  # cbind to input camera trap table, if requested
  if(append) {
    df_covariates_out <- cbind(CTtable, df_covariates)
  } else {
    df_covariates_out <- df_covariates
  }

  # clip rasters to desired extent
  list_r_cov_c <- lapply(1:length(list_r_cov), 
                         FUN = function(i){
                           current_crs <- sf::st_crs(list_r_cov[[i]])
                           CTtable_i <- sf::st_transform(CTtable, crs = current_crs)
                           extent_aoi  <- terra::ext(sf::st_buffer(CTtable_i, buffer_aoi))
                           
                           out <- try(terra::crop(list_r_cov[[i]], y = extent_aoi)) 
                           
                           if(is(out, "try-error")) {
                             warning(paste("Failed to crop raster '", names(list_r_cov)[i], "'. Skipping this covariate."), call. = FALSE)
                             return(NULL)
                           } else {
                             return(out)
                           }
                         })
  names(list_r_cov_c) <- names(list_r_cov)
  
  
  # resample rasters according to template
  r_cov_list <- lapply(1:length(list_r_cov_c),
                       FUN = function(i){
                         if(is.null(list_r_cov_c[[i]])) return(NULL)
                         current_crs <- sf::st_crs(list_r_cov[[i]])

                         if(current_crs == sf::st_crs(raster_template)) out <- terra::resample(list_r_cov[[i]], raster_template)
                         if(current_crs != sf::st_crs(raster_template)) out <- terra::project(list_r_cov[[i]], raster_template)

                         return(out)
                       })
  names(r_cov_list) <- names(list_r_cov)
  
  r_cov <- terra::rast(r_cov_list)
  return(list(CTtable = df_covariates_out,
              prediction_raster = r_cov))
}
