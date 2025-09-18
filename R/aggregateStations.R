#' @title Aggregate Camera Trap Table to Station Level
#'
#' @description
#' Aggregates a camera trap table from a station-camera (location-deployment) level to a station (location) level.
#' This function is useful when modeling or analysis is conducted at the station level,
#' when multiple cameras were deployed at a single station or a single camera had multiple deployments.
#'
#'
#' @param CTtable A data frame or `sf` object representing the camera trap table.
#'           Each row typically represents a unique camera deployment.
#' @param stationCol A character string specifying the name of the column in \code{CTtable}
#'                   that contains the unique station identifiers.
#' @param cameraCol A character string specifying the name of the column in \code{CTtable}
#'                   that contains the unique camera (deployment) identifiers.
#' @param setupCol character. name of the column containing camera setup dates
#'                  in \code{CTtable}
#' @param retrievalCol character. name of the column containing camera
#'                  retrieval dates in \code{CTtable}
#' @param dateFormat character. The format of columns \code{setupCol} and
#'                  \code{retrievalCol} (and potential problem columns) in \code{CTtable}. Must
#'                  be interpretable by either \code{as.Date} or the "orders" argument
#'                  \code{\link[lubridate]{parse_date_time}} in \pkg{lubridate}. Can be a date
#'                  or a date-time.
#'
#' @details
#' The aggregation logic handles different column types as follows:
#' \itemize{
#'   \item \strong{Station ID (specified by `stationCol`):} Remains unique for each row in the output.
#'   \item \strong{Camera ID (if present):} A character vector (comma-separated)
#'         of unique camera IDs deployed at that station.
#'   \item \strong{Setup/Retrieval Dates:} The earliest setup date and the latest retrieval date
#'         for all deployments within a station will be retained.
#'   \item \strong{Numeric Columns:} The `mean` of all values for that station will be calculated.
#'   \item \strong{Logical Columns:} The `mean` (which effectively calculates the proportion of `TRUE`s)
#'         of all values for that station will be calculated.
#'   \item \strong{Character Columns (other than Camera ID):} A character vector of unique values
#'         (comma-separated) for that station will be created.
#'   \item \strong{Factor Columns:} Similar to character columns, unique levels will be combined.
#'   \item \strong{Geometry Column (`sf` objects):} Centroid (midpoint) of all unique points at a 
#'          station is calculated.
#' }
#' 
#' It is recommended to inspect the aggregated output carefully, especially for columns
#' with mixed data types or specific aggregation requirements not covered by the defaults.
#'
#' @note The function is mainly intended for aggregating covariates to station level. 
#'          It does currently not handle camera malfunction (via columns `ProblemX_from` / `ProblemX_to`)
#'          and does not provide proper handling of problem columns.
#'          Use \code{\link{cameraOperation}} to aggregate camera trap tables to station level for 
#'          analyses while accounting for camera malfunction / problem periods.
#' 
#' @return A data frame with one row per unique station, containing aggregated information.
#'          Spatial information (for `sf` objects) is preserved.
#'         
#' 
#' @export
#' 
#' @importFrom dplyr summarise
#' @importFrom lubridate parse_date_time as_date
#'
# #' @examples
#' 
#' 
aggregateStations <- function(CTtable, 
                              stationCol,
                              cameraCol = NULL,
                              setupCol = NULL,
                              retrievalCol = NULL,
                              dateFormat = NULL) {
  
  
  # --- Handle sf objects ---
  
  is_sf_input <- inherits(CTtable, "sf")
  
  if (is_sf_input) {
    # message("Input 'CTtable' is an sf object. Geometry column will be dropped during aggregation.")
    df <- sf::st_drop_geometry(CTtable)
    df_geometry <- CTtable[, stationCol]
  } else {
    df <- as.data.frame(CTtable)  
  }
  
  
  
  # --- Input Validation ---
  required_cols <- c(stationCol)
  if (!is.null(cameraCol)) required_cols <- c(required_cols, cameraCol)
  if (!is.null(setupCol)) required_cols <- c(required_cols, setupCol)
  if (!is.null(retrievalCol)) required_cols <- c(required_cols, retrievalCol)
  
  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop(paste("Error in aggregateStations: The following required columns were not found in 'CTtable':",
               paste(missing_cols, collapse = ", "),
               ". Available columns are:", paste(names(df), collapse = ", ")))
  }
  
  # Validate dateFormat if date columns are provided and are characters
  if ((!is.null(setupCol) && is.character(df[[setupCol]])) ||
      (!is.null(retrievalCol) && is.character(df[[retrievalCol]]))) {
    if (is.null(dateFormat)) {
      stop("Error in aggregateStations: 'dateFormat' must be provided if 'setupCol' or 'retrievalCol' are character strings.")
    }
  }
  
  
  # --- Early exit if no aggregation needed ---
  if (all(table(df[[stationCol]]) == 1)) {
    message("No aggregation needed: All 'stationCol' values are already unique.")
    return(df)
  }
  
  
  
  # --- Coerce date/datetime columns to appropriate types using lubridate ---
  # Determine if date format is likely date-only or datetime
  is_datetime_format <- !is.null(dateFormat) && grepl("[HhMmsS]", dateFormat)
  
  if (!is.null(setupCol) && is.character(df[[setupCol]])) {
    if (is_datetime_format) {
      df[[setupCol]] <- lubridate::parse_date_time(df[[setupCol]], orders = dateFormat)
    } else {
      df[[setupCol]] <- lubridate::as_date(df[[setupCol]], format = dateFormat)
    }
    if (anyNA(df[[setupCol]]) && any(is.na(df[[setupCol]][!is.na(CTtable[[setupCol]])]))) {
      stop(paste0("Error in aggregateStations: Failed to convert '", setupCol, "' to date/datetime using format '", dateFormat, "'. Check 'dateFormat' or data consistency."))
    }
  }
  if (!is.null(retrievalCol) && is.character(df[[retrievalCol]])) {
    if (is_datetime_format) {
      df[[retrievalCol]] <- lubridate::parse_date_time(df[[retrievalCol]], orders = dateFormat)
    } else {
      df[[retrievalCol]] <- lubridate::as_date(df[[retrievalCol]], format = dateFormat)
    }
    if (anyNA(df[[retrievalCol]]) && any(is.na(df[[retrievalCol]][!is.na(CTtable[[retrievalCol]])]))) {
      stop(paste0("Error in aggregateStations: Failed to convert '", retrievalCol, "' to date/datetime using format '", dateFormat, "'. Check 'dateFormat' or data consistency."))
    }
  }
  
  
  
  # --- Build the aggregation list for dplyr::summarise ---
  agg_list <- list()
  
  for (col_name in names(df)) {
    if (col_name == stationCol) {
      next # This is the grouping variable
    } else if (!is.null(cameraCol) && col_name == cameraCol) {
      # Use expr() and .data[[col_name]]
      agg_list[[col_name]] <- rlang::expr(paste(sort(unique(.data[[!!sym(col_name)]])), collapse = ", "))
    } else if (!is.null(setupCol) && col_name == setupCol) {
      agg_list[[col_name]] <- rlang::expr(min(.data[[!!sym(col_name)]], na.rm = TRUE))
    } else if (!is.null(retrievalCol) && col_name == retrievalCol) {
      agg_list[[col_name]] <- rlang::expr(max(.data[[!!sym(col_name)]], na.rm = TRUE))
    } else if (is.numeric(df[[col_name]])) {
      agg_list[[col_name]] <- rlang::expr(mean(.data[[!!sym(col_name)]], na.rm = TRUE))
    } else if (is.logical(df[[col_name]])) {
      agg_list[[col_name]] <- rlang::expr(mean(as.integer(.data[[!!sym(col_name)]]), na.rm = TRUE))
    } else if (is.character(df[[col_name]]) || is.factor(df[[col_name]])) {
      agg_list[[col_name]] <- rlang::expr(paste(sort(unique(.data[[!!sym(col_name)]])), collapse = ", "))
    } else if (lubridate::is.Date(df[[col_name]]) || lubridate::is.POSIXct(df[[col_name]]) || lubridate::is.POSIXlt(df[[col_name]])) {
      warning(paste0("Column '", col_name, "' is a date/datetime type not specified as 'setupCol' or 'retrievalCol'. Aggregating by pasting unique values. Consider handling this column manually if this is not desired."))
      agg_list[[col_name]] <- rlang::expr(paste(sort(unique(.data[[!!sym(col_name)]])), collapse = ", "))
    } else {
      warning(paste0("Column '", col_name, "' has an unhandled data type and will be aggregated by pasting unique values. Consider converting it to a supported type if this is not desired."))
      agg_list[[col_name]] <- rlang::expr(paste(sort(unique(.data[[!!sym(col_name)]])), collapse = ", "))
    }
  }

  
  # Perform aggregation using dplyr
  df_agg <- df |>
    dplyr::group_by(!!dplyr::sym(stationCol)) |>
    dplyr::summarise(!!!agg_list, .groups = "drop")
  
  
  if(is_sf_input){
    df_agg_geometry <- df_geometry |> 
      group_by(!!dplyr::sym(stationCol)) |>
      dplyr::summarise(geometry = sf::st_centroid(sf::st_union(.data$geometry)),
                       .groups = "drop")
    
    df_agg <- dplyr::left_join(df_agg, df_agg_geometry, by = stationCol) |>
      sf::st_as_sf()   # Convert back to sf
  }
  
  return(df_agg)
}
