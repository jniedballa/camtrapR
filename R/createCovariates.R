#' Extract covariate values from spatial rasters and prepare rasters for spatial predictions
#'
#' @description
#'  This function extracts covariate values from spatial raster data 
#'  (e.g. for use in modelling) and prepares these covariates for use in 
#'  spatial predictions. 
#'  
#'  It accepts a camera trap table containing spatial information, 
#'  along with either a directory containing covariate raster files, a character 
#'  vector specifying the file paths of these covariate rasters, or direct
#'  SpatRaster objects from the terra package.
#'  
#'  Additionally, users can provide parameters to control how covariates are
#'  extracted, and how they are aggregated to prediction rasters.
#'  
#'  The function can also download elevation data and calculate terrain metrics
#'  if requested.
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
#' @param rasters SpatRaster object or list of SpatRaster objects. Direct input of rasters from the terra package instead of reading from disk.
#' @param buffer_ct numeric. A value (in meters) by which to buffer the point locations in \code{CTtable} for extraction of covariate values. 
#' @param bilinear logical. If \code{TRUE}, extract covariate values with bilinear interpolation (nearest 4 raster cells). If \code{FALSE}, extract value at the cell the point falls in. Only relevant if \code{buffer_ct} is 0.
#' @param buffer_aoi numeric. A value (in meters) by which to buffer the overall camera trapping grid to ensure that prediction rasters are larger than the camera trapping grid.
#' @param raster_template SpatRaster. A SpatRaster (as defined in the \code{terra} package) to use as template for the creation of prediction rasters.
#' @param resolution numeric. Spatial resolution of prediction rasters in the units of the coordinate reference system. Ignored if \code{raster_template} is provided.
#' @param append logical. If \code{TRUE}, add the extracted covariates to the existing \code{CTtable}. If \code{FALSE}, return only the extracted covariate values without the existing \code{CTtable}.
#' @param formats character. Possible file formats for raster data (must include the dot). Defaults to \code{.tif} files.
#' @param recursive logical. If \code{TRUE}, search for raster files recursively in subdirectories when a directory is provided. Defaults to \code{FALSE}.
#' @param download_elevation logical. If \code{TRUE}, download elevation data from AWS. Defaults to \code{FALSE}.
#' @param elevation_zoom numeric. Zoom level for elevation data download (6-12). Higher values provide more detail but longer download times. Zoom 12 corresponds to ~20m pixel resolution, 11 to ~40m, 10 to ~80m, and so on (resolution halves with each decrease in zoom level). Defaults to 9.
#' @param terrain_measures character. Vector of terrain metrics to calculate from elevation data. Options include "slope" (slope in degrees), "aspect" (compass direction in degrees), "TRI" (Terrain Ruggedness Index, measuring elevation difference between adjacent cells), "TPI" (Topographic Position Index, comparing cell elevation to mean of surrounding cells), and "roughness" (difference between max and min of surrounding cells). Defaults to NULL (no terrain metrics).
#' @param standardize_na logical. Logical. If \code{TRUE}, ensures all layers in the prediction raster have identical NA patterns by setting a cell to NA in all bands if it's NA in any band. This creates consistency for spatial predictions across covariates but may lose data in covariates.
#' @param scale_covariates logical. If \code{TRUE}, scale numeric covariates and return both original and scaled versions of data and prediction rasters. Scaling is performed using R's \code{scale} function. Defaults to \code{FALSE}.
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
#' When \code{download_elevation = TRUE}, the function will download elevation data
#' from AWS using the \code{elevatr} package. The \code{elevation_zoom}
#' parameter controls the level of detail, with values between 6 and 12.
#' Higher zoom levels provide finer resolution but require longer download times and
#' may consume significant memory. Approximate resolutions: zoom 12 = ~20m, 
#' 11 = ~40m, 10 = ~80m, etc.
#' 
#' If \code{terrain_measures} is specified, the function calculates the requested 
#' terrain metrics from the elevation data using \code{terra::terrain()} with the 
#' default 3x3 neighborhood. Available terrain metrics include "slope", "aspect", 
#' "TRI" (Terrain Ruggedness Index), "TPI" (Topographic Position Index), and 
#' "roughness".
#' 
#' When using \code{scale_covariates = TRUE}, the function returns both original and
#' scaled versions of the data and prediction rasters. #' The function uses R's 
#' \code{scale()} function to perform centering and scaling, and
#' includes the scaling parameters in the returned metadata.
#' 
#' \subsection{Warning about Categorical Covariates}{
#' This function does not explicitly handle categorical rasters. All raster values are
#' treated as numeric, which can be problematic when scaling is applied. The function
#' attempts to identify "likely categorical" variables (numeric variables with few unique
#' integer values) and will provide warnings, but it cannot automatically handle them
#' correctly for scaling.
#' 
#' When using scaled covariates with categorical variables in models:
#' \itemize{
#'   \item Use \code{CTtable_scaled} for numeric predictors
#'   \item Use \code{CTtable} (original) for categorical predictors
#'   \item Similarly, use \code{predictionRaster_scaled} for numeric predictors in spatial predictions
#'   \item Use \code{predictionRaster} for categorical predictors in spatial predictions
#' }
#' 
#' Future versions may implement proper categorical raster handling with RAT (Raster
#' Attribute Table) support.
#' }
#' 
#' @return
#' \describe{
#' When \code{scale_covariates = FALSE}, a list containing three elements:
#'   \item{CTtable}{An \code{sf} object representing the camera trap data frame with extracted covariate values.}
#'   \item{predictionRaster}{A \code{SpatRaster} object containing covariate raster layers}
#'   \item{originalRaster}{A list of the original input rasters}
#' 
#' When \code{scale_covariates = TRUE}, a list containing six elements:
#'   \item{CTtable}{The original \code{sf} object with unscaled covariate values}
#'   \item{CTtable_scaled}{The \code{sf} object with scaled numeric covariate values}
#'   \item{predictionRaster}{The original unscaled prediction raster}
#'   \item{predictionRaster_scaled}{The prediction raster with scaled numeric layers}
#'   \item{originalRaster}{A list of the original input rasters}
#'   \item{scaling_params}{A list containing center and scale information of numeric covariates}
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
#' # doesn't run because 'directory' is only a placeholder
#' 
#' covariates <- createCovariates(camtraps_sf, 
#' "path/to/covariate_rasters", 
#' buffer_ct = 100, 
#' buffer_aoi = 1000,
#' resolution = 100)
#' 
#' 
#' # extract covariates with elevation data (this code runs)
#' 
#' covariates_elev <- createCovariates(camtraps_sf,
#' buffer_ct = 100,
#' buffer_aoi = 1000,
#' resolution = 100,
#' download_elevation = TRUE,
#' elevation_zoom = 11,
#' terrain_measures = c("slope", "aspect", "TRI"))
#' 
#' # Note that if local rasters are available they can be extracted alongside
#' # elevation data in a single function call
#' 
#' # camera trap table with extracted covariate values
#' camtraps_sf_cov <- covariates_elev$CTtable
#' 
#' # covariate raster layer
#' r_cov <- covariates_elev$predictionRaster
#' plot(r_cov)
#' 
#' # Use SpatRaster objects directly as input
#' r1 <- rast("elevation.tif")
#' r2 <- rast("landcover.tif")
#' raster_list <- list(elevation = r1, landcover = r2)
#' 
#' covariates_direct <- createCovariates(camtraps_sf,
#'                                       rasters = raster_list,
#'                                       buffer_ct = 100,
#'                                       resolution = 100)
#'
#' # Scale numeric covariates for modeling
#' covariates_scaled <- createCovariates(camtraps_sf,
#'                                       rasters = raster_list,
#'                                       buffer_ct = 100,
#'                                       resolution = 100,
#'                                       scale_covariates = TRUE)
#'
#' # Use scaled data with categorical variables
#' # Mix and match from original and scaled outputs for tabular data
#' model_data <- covariates_scaled$CTtable_scaled  # Use scaled numeric covariates
#' model_data$landcover <- covariates_direct$CTtable$landcover  # Use original categorical covariate
#' 
#' # Mix and match for prediction rasters
#' # Create a combined prediction raster with scaled numeric variables and original categorical variables
#' # Extract scaled elevation layer
#' elev_scaled <- covariates_scaled$predictionRaster_scaled$elevation
#' 
#' # Extract original landcover layer (categorical)
#' landcover_orig <- covariates_direct$predictionRaster$landcover
#' 
#' # Combine into a new SpatRaster for predictions
#' prediction_raster <- c(elev_scaled, landcover_orig)
#' names(prediction_raster) <- c("elevation", "landcover")
#' 
#' # Use this combined raster for spatial predictions
#' plot(prediction_raster)
#' }
#' 

createCovariates <- function(CTtable,
                             directory,
                             filenames,
                             rasters,
                             buffer_ct = 0,
                             bilinear = FALSE,
                             buffer_aoi = 1000,
                             raster_template = NULL,
                             resolution = NULL,
                             append = TRUE, 
                             formats = ".tif",
                             recursive = FALSE,
                             download_elevation = FALSE,
                             elevation_zoom = 10,
                             terrain_measures = NULL,
                             standardize_na = FALSE,
                             scale_covariates = FALSE)
{
  if(!inherits(CTtable, "sf")){ 
    stop("CTtable is not an sf object.")
  }
  if(is.na(sf::st_crs(CTtable))) stop("Coordinate reference system of CTtable is not defined. See: sf::st_crs(CTtable).")
  
  if (nrow(CTtable) == 0) {
    stop("The 'CTtable' is empty.")
  }
  
  # Count how many input sources are provided
  input_sources <- sum(
    hasArg(directory),
    hasArg(filenames),
    hasArg(rasters),
    download_elevation
  )
  
  # Check if any input source is provided
  if (input_sources == 0) {
    stop("Please provide one of 'directory', 'filenames', 'rasters', or set 'download_elevation = TRUE'.")
  }
  
  # Check that only one of directory, filenames, or rasters is provided
  if (sum(hasArg(directory), hasArg(filenames), hasArg(rasters)) > 1) {
    stop("Provide only one of 'directory', 'filenames', or 'rasters'.")
  }
  
  # Validate formats
  if(!all(grepl("^\\.", formats))) {
    stop("All values in 'formats' must start with a dot (e.g., '.tif')")
  }
  
  # Validate terrain_measures if provided
  if (!is.null(terrain_measures)) {
    valid_measures <- c("slope", "aspect", "TRI", "TPI", "roughness")
    terrain_measures <- match.arg(terrain_measures, valid_measures, several.ok = TRUE)
  }
  
  # Validate elevation_zoom
  if (download_elevation) {
    if (!is.numeric(elevation_zoom) || elevation_zoom < 6 || elevation_zoom > 12) {
      stop("'elevation_zoom' must be a numeric value between 6 and 12.")
    }
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
  
  # Initialize list for all rasters
  list_r_cov <- list()
  
  # Create raster template if needed
  if(is.null(raster_template) && !is.null(resolution)) {
    if (resolution <= 0) {
      stop("Invalid spatial resolution. Please provide a positive value.")
    }
    
    if(buffer_aoi > 0) {
      extent_aoi <- terra::ext(terra::buffer(terra::vect(CTtable), width = buffer_aoi))
    } else {
      extent_aoi <- terra::ext(CTtable)
    }
    
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
        terra::vect() %>% 
        terra::buffer(width = buffer_aoi) %>%
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
  
  # Process local raster files if directory or filenames are provided
  if (hasArg(directory) || hasArg(filenames)) {
    
    if (hasArg(directory)) {
      if(length(directory) != 1) stop("The 'directory' parameter should be a single directory path.")
      
      # Get file paths from directory
      lf_covariates <- list.files(directory, recursive = recursive, pattern = paste0(formats, "$"), full.names = TRUE)
      if(length(lf_covariates) == 0) stop(paste("No files with suffix", paste(formats, collapse = "/"), "found in", directory))
      
      # Always use the file names (not directory names) as the base name
      covariate_names <- tools::file_path_sans_ext(basename(lf_covariates))
      
      if(recursive) {
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
      }
      
      local_r_cov <- lapply(lf_covariates, terra::rast)
      names(local_r_cov) <- covariate_names
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
        covariate_names <- names(filenames)  # use names if available
      } else {
        covariate_names <- tools::file_path_sans_ext(basename(filenames))
      }
      local_r_cov <- lapply(filenames, function(file) terra::rast(file))
      names(local_r_cov) <- covariate_names
    }
    
    # Add local rasters to the master list
    list_r_cov <- c(list_r_cov, local_r_cov)
  }
  
  # Process provided SpatRaster objects
  if (hasArg(rasters)) {
    if (inherits(rasters, "SpatRaster")) {
      # Single SpatRaster object
      if (terra::nlyr(rasters) == 1) {
        # Single layer raster
        if (is.null(names(rasters)) || names(rasters) == "") {
          # No name provided, assign default name
          warning("No name provided for input raster. Using 'layer1' as name.", call. = F, immediate. = T)
          names(rasters) <- "layer1"
        }
        spat_r_cov <- list(rasters)
        names(spat_r_cov) <- names(rasters)
      } else {
        # Multi-layer raster - handle as a single multi-band raster
        spat_r_cov <- list(rasters)
        names(spat_r_cov) <- "raster"
      }
    } else if (is.list(rasters)) {
      # List of SpatRaster objects
      if (!all(sapply(rasters, inherits, "SpatRaster"))) {
        stop("All elements in 'rasters' must be SpatRaster objects.")
      }
      
      # Check for and assign names to rasters
      if (is.null(names(rasters))) {
        # No names provided, assign default names
        warning("No names provided for input rasters. Using 'layer1', 'layer2', ... as names.", call. = F, immediate. = T)
        names(rasters) <- paste0("layer", 1:length(rasters))
      }
      
      spat_r_cov <- rasters
    } else {
      stop("'rasters' must be a SpatRaster object or a list of SpatRaster objects.")
    }
    
    # Add spatial rasters to the master list
    list_r_cov <- c(list_r_cov, spat_r_cov)
  }
  
  # Download elevation data and calculate terrain metrics if requested
  if (download_elevation) {
    # Create buffered polygon from points for elevation download
    buffered_sf <- sf::st_buffer(CTtable, dist = buffer_aoi)
    
    # Transform to EPSG:4326 if needed for elevatr
    if (sf::st_crs(buffered_sf) != sf::st_crs(4326)) {
      buffered_sf_4326 <- sf::st_transform(buffered_sf, 4326)
    } else {
      buffered_sf_4326 <- buffered_sf
    }
    
    # Download elevation data from AWS
    elevation_rast <- elevatr::get_elev_raster(
      locations = buffered_sf_4326,
      z = elevation_zoom,
      source = "aws",
      clip = "bbox"
    )
    
    # Convert to terra raster if necessary
    if(inherits(elevation_rast, "RasterLayer")) {
      elevation_rast <- terra::rast(elevation_rast)
    }
    
    # Initialize terrain rasters list with elevation
    terrain_rasts <- list(elevation = elevation_rast)
    
    # Calculate requested terrain metrics using terra::terrain with default 3x3 neighborhood
    if (!is.null(terrain_measures)) {
      if ("slope" %in% terrain_measures) {
        terrain_rasts$slope <- terra::terrain(elevation_rast, "slope", unit = "degrees")
      }
      if ("aspect" %in% terrain_measures) {
        terrain_rasts$aspect <- terra::terrain(elevation_rast, "aspect", unit = "degrees")
      }
      if ("TRI" %in% terrain_measures) {
        terrain_rasts$TRI <- terra::terrain(elevation_rast, "TRI")
      }
      if ("TPI" %in% terrain_measures) {
        terrain_rasts$TPI <- terra::terrain(elevation_rast, "TPI")
      }
      if ("roughness" %in% terrain_measures) {
        terrain_rasts$roughness <- terra::terrain(elevation_rast, "roughness")
      }
    }
    
    # Add terrain rasters to the master list
    list_r_cov <- c(list_r_cov, terrain_rasts)
  }
  
  if(length(list_r_cov) == 0) {
    stop("No covariate raster loaded. Please check your inputs.")
  }
  
  # Set extraction method based on bilinear parameter
  extract_method <- if(bilinear) "bilinear" else "simple"
  
  
  # Extract values from all rasters
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
      CTtable_i_buff <- terra::buffer(CTtable_i_v, 
                                      width = buffer_ct)
      extracted <- terra::extract(list_r_cov[[i]], 
                                  CTtable_i_buff, 
                                  fun = mean, 
                                  na.rm = TRUE)
    }
    
    # Handle multi-band rasters
    if(terra::nlyr(list_r_cov[[i]]) > 1) {
      layer_names <- names(list_r_cov[[i]])
      base_name <- names(list_r_cov)[i]
      
      # Apply make.names to ensure column names match R's sanitization rules
      colnames(extracted)[-1] <- make.names(paste(base_name, layer_names, sep = "_"), unique = FALSE)
    } else {
      colnames(extracted)[-1] <- make.names(names(list_r_cov)[i], unique = FALSE)
    }
    
    return(extracted[, -1, drop = FALSE])
  })
  
  df_covariates <- do.call(cbind, cov_value_list)
  
  # Check for NAs in extracted covariates
  na_counts <- sapply(df_covariates, function(x) sum(is.na(x)))
  if(any(na_counts > 0)) {
    na_cols <- names(na_counts[na_counts > 0])
    na_percent <- na_counts[na_counts > 0] / nrow(df_covariates) * 100
    
    # Format the warning message
    warning_msg <- "NAs found in extracted covariates:\n"
    for(i in seq_along(na_cols)) {
      warning_msg <- paste0(warning_msg, "  ", na_cols[i], ": ", na_counts[na_cols[i]],
                            " NAs (", round(na_percent[i], 1), "%)\n")
    }
    warning(warning_msg, call. = FALSE, immediate. = TRUE)
  }
  
  # cbind to input camera trap table, if requested
  if(append) {
    df_covariates_out <- cbind(CTtable, df_covariates)
  } else {
    df_covariates_out <- df_covariates
  }
  
  # Identify which columns are covariates (needed for scaling later)
  if(append) {
    # Get the names of the columns that were added as covariates
    if(inherits(df_covariates_out, "sf")) {
      geom_col <- attr(df_covariates_out, "sf_column")
      original_cols <- setdiff(names(CTtable), geom_col)
    } else {
      original_cols <- names(CTtable)
    }
    covariate_cols <- setdiff(names(df_covariates_out), original_cols)
  } else {
    # All columns are covariates if append=FALSE
    covariate_cols <- names(df_covariates_out)
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
                             extent_aoi <- terra::ext(terra::buffer(CTtable_i_v, buffer_aoi))
                           } else {
                             extent_aoi <- terra::ext(CTtable_i)
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
                           
                           # Process the raster
                           if(current_crs == crs_template) {
                             out <- terra::resample(list_r_cov_c[[i]], raster_template)
                           } else {
                             out <- terra::project(list_r_cov_c[[i]], raster_template)
                           }
                           
                           # Ensure consistent naming for multi-band rasters
                           if(terra::nlyr(list_r_cov_c[[i]]) > 1) {
                             base_name <- names(list_r_cov)[i]
                             layer_names <- names(list_r_cov_c[[i]])
                             # Make the raster names match the sanitized column names
                             names(out) <- make.names(paste(base_name, layer_names, sep = "_"), unique = FALSE)
                             
                           } else {
                             names(out) <- make.names(names(list_r_cov)[i], unique = FALSE)
                           }
                           
                           return(out)
                         })
    names(r_cov_list) <- names(list_r_cov)
    
    r_cov <- terra::rast(r_cov_list)
  } else {
    r_cov <- NULL
  }
  
  # ensure the output has correct layer names (rast doesn't preserve names of multi-band raster bands)
  if (!is.null(r_cov)) {
    names(r_cov) <- unlist(sapply(r_cov_list, names))
  }
  
  # Standardize NA patterns across all layers if requested
  if(standardize_na && !is.null(r_cov) && terra::nlyr(r_cov) > 1) {
    r_cov <- terra::mask(x = r_cov,                  # update this layer
                         mask = any(is.na(r_cov)),   # where any band is NA
                         maskvalues = 1,             # (coded as "1" in any(is.na()))
                         updatevalue = NA)           # replace cells where any band is 1 in the mask with NA
  }
  
  # Scale covariates if requested
  if(scale_covariates && length(covariate_cols) > 0) {
    # Check which columns are numeric and can be scaled
    is_numeric <- sapply(df_covariates_out[covariate_cols], is.numeric)
    numeric_covariate_cols <- covariate_cols[is_numeric]
    
    if(length(numeric_covariate_cols) > 0) {
      # Handle sf objects for scaling
      is_sf <- inherits(df_covariates_out, "sf")
      if(is_sf) {
        df_temp <- sf::st_drop_geometry(df_covariates_out)
      } else {
        df_temp <- df_covariates_out
      }
      
      # Scale numeric covariates
      scaled_data <- scale(df_temp[numeric_covariate_cols])
      center_values <- attr(scaled_data, "scaled:center")
      scale_values <- attr(scaled_data, "scaled:scale")
      names(center_values) <- numeric_covariate_cols
      names(scale_values) <- numeric_covariate_cols
      
      # Create scaled dataframe
      df_scaled <- df_temp
      df_scaled[numeric_covariate_cols] <- scaled_data
      
      # Restore geometry if needed
      if(is_sf) {
        geom_col <- attr(df_covariates_out, "sf_column")
        geometry <- sf::st_geometry(df_covariates_out)
        df_scaled <- sf::st_sf(df_scaled, geometry = geometry)
      }
      
      # Create scaled raster if prediction raster exists
      r_cov_scaled <- NULL
      if(!is.null(r_cov) && terra::nlyr(r_cov) > 0) {
        r_cov_scaled <- r_cov
        
        # Scale applicable layers
        for(col in numeric_covariate_cols) {
          if(col %in% names(r_cov_scaled)) {
            r_cov_scaled[[col]] <- (r_cov_scaled[[col]] - center_values[col]) / scale_values[col]
          } else {
            # warning if covariate in df_scaled but not in prediction stack
            warning(paste(col, "is not in prediction raster"), call. = F, immediate. = T)
          }
        }
      }
      
      # Print summary message
      message(paste0("Scaled ", length(numeric_covariate_cols), " numeric covariates."))
      
      # Prepare full output with both original and scaled data
      result <- list(
        CTtable = df_covariates_out,
        CTtable_scaled = df_scaled,
        predictionRaster = r_cov,
        predictionRaster_scaled = r_cov_scaled,
        originalRaster = list_r_cov,
        scaling_params = list(center = center_values, scale = scale_values)
      )
    } else {
      warning("No numeric covariates found to scale.", call. = F, immediate. = T)
      # Return unscaled data if no numeric covariates
      result <- list(
        CTtable = df_covariates_out,
        predictionRaster = r_cov,
        originalRaster = list_r_cov
      )
    }
  } else {
    # Return standard output without scaling
    result <- list(
      CTtable = df_covariates_out,
      predictionRaster = r_cov,
      originalRaster = list_r_cov
    )
  }
  
  return(result)
}
