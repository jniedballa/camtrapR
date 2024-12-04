#' Species Accumulation Curves for Camera Trap Data
#'
#' @description Generates species accumulation, rarefaction and extrapolation curves
#'    from camera trap data using the \pkg{iNEXT} package (Chao et al. 2014). The function
#'    creates sampling effort-based accumulation curves with sampling units being either
#'    camera trap stations or days.
#'    
#'    Note that these curves are based on observed detections only and do not account 
#'    for imperfect detection. Species may be present but not detected, leading to 
#'    underestimation of true species richness. For analyses that explicitly account 
#'    for imperfect detection, consider using occupancy-based approaches (see 
#'    \code{\link{communityModel}}).
#'
#' @details
#' The function provides three types of curves:
#' \itemize{
#'   \item Sample-size-based rarefaction/extrapolation curve
#'   \item Sample completeness curve
#'   \item Coverage-based rarefaction/extrapolation curve
#' }
#' 
#' While these curves provide useful insights into sampling completeness and species 
#' richness patterns, they should be interpreted with caution in camera trap studies. 
#' Unlike occupancy models, they do not account for:
#' \itemize{
#'   \item Imperfect detection (species present but not detected)
#'   \item Variation in detection probability among species
#'   \item Spatial variation in species occurrence
#'   \item Temporal variation in species activity
#' }
#'
#'
#' @param CTtable data.frame containing the camera trap deployment information.
#' @param recordTable data.frame containing the camera trap records.
#' @param speciesCol character. Name of the column specifying species names in recordTable
#' @param recordDateTimeCol character. Name of the column containing date and time information in recordTable
#' @param setupCol character. Name of the column containing camera setup dates in CTtable
#' @param stationCol character. Name of the column containing station IDs in both tables
#' @param assemblageCol character. Optional. Name of column in recordTable for grouping data into separate assemblages
#' @param q numeric. The order of diversity measure. Default is 0 (species richness)
#' @param x_unit character. Whether to use "station" or "day" as sampling unit. Default is "station"
#' @param knots numeric. number of values along x axis for which values are computed
#' @param conf numeric. confidence interval
#' @param nboot numeric. number of replications
#' 
#'
#' @return An object of class "iNEXT" containing:
#' \itemize{
#'   \item DataInfo - data information
#'   \item iNextEst - diversity estimates for rarefied and extrapolated samples
#'   \item AsyEst - asymptotic diversity estimates
#' }
#'
#' @note Requires package \pkg{iNEXT}.
#'
#' @references
#' Chao, A., Gotelli, N. J., Hsieh, T. C., Sander, E. L., Ma, K. H., Colwell, R. K., 
#' & Ellison, A. M. (2014). Rarefaction and extrapolation with Hill numbers: 
#' A framework for sampling and estimation in species diversity studies. 
#' Ecological Monographs, 84(1), 45-67.
#'
#' @examples
#' \dontrun{
#' # Basic usage with stations as sampling units
#' result <- run_camera_trap_inext(
#'   CTtable = cams,
#'   recordTable = recs,
#'   speciesCol = "Species",
#'   recordDateTimeCol = "DateTime", 
#'   setupCol = "Setup_date",
#'   stationCol = "Station",
#'   q = 0,
#'   x_unit = "station"
#' )
#'
#' # Plot results
#' ggiNEXT(result, type = 1)  # Sample-size-based R/E curve
#' ggiNEXT(result, type = 2)  # Sample completeness curve
#' ggiNEXT(result, type = 3)  # Coverage-based R/E curve
#'
#' # With assemblage grouping and days as sampling units
#' result_by_assemblage <- run_camera_trap_inext(
#'   CTtable = cams,
#'   recordTable = recs,
#'   speciesCol = "Species",
#'   recordDateTimeCol = "DateTime",
#'   setupCol = "Setup_date",
#'   stationCol = "Station",
#'   assemblageCol = "Season",
#'   q = 0,
#'   x_unit = "day"
#' )
#' }
#'
#' @seealso \code{\link{surveyDashboard}} for interactive species accumulation analysis
#'
#' @author Juergen Niedballa
#'
#' @importFrom dplyr mutate arrange distinct count
#' @importFrom stats setNames
#' @export
#' 
speciesAccum <- function(CTtable, 
                         recordTable,
                         speciesCol,
                         recordDateTimeCol,
                         setupCol, 
                         stationCol,
                         assemblageCol = NULL,
                         q = 0,
                         x_unit = c("station", "day"),
                         knots = 40,
                         conf = 0.95, 
                         nboot = 50) {
  
  # Input validation
  x_unit <- match.arg(x_unit)
  
  # Check if required columns exist
  required_cols <- c(speciesCol, recordDateTimeCol, stationCol)
  if(!all(required_cols %in% names(recordTable))) {
    stop("Missing required columns in recordTable")
  }
  if(!all(c(setupCol, stationCol) %in% names(CTtable))) {
    stop("Missing required columns in CTtable")
  }
  
  # Load required packages
  # requireNamespace("dplyr")
  
  # Function to create incidence matrix
  create_incidence_matrix <- function(records_subset, 
                                      stations_subset, 
                                      species_list, 
                                      temporal = FALSE) {
    if(temporal) {
      # avoid CRAN check notes
      survey_date <- NULL
      
      # For temporal analysis (days)
      records_subset <- records_subset %>%
        mutate(
          survey_date = as.Date(.data[[recordDateTimeCol]]),
          survey_day = as.numeric(survey_date - min(as.Date(stations_subset[[setupCol]]))) + 1
        )
      
      units <- 1:max(records_subset$survey_day)
      unit_col <- "survey_day"
    } else {
      # For spatial analysis (stations)
      units <- unique(stations_subset[[stationCol]])
      unit_col <- stationCol
    }
    
    # Create empty matrix
    incidence_matrix <- matrix(0, 
                               nrow = length(species_list), 
                               ncol = length(units),
                               dimnames = list(species_list, as.character(units)))
    
    # Fill matrix
    for(sp in species_list) {
      for(unit in units) {
        if(temporal) {
          detected <- any(records_subset[[speciesCol]] == sp & 
                            records_subset$survey_day == unit)
        } else {
          detected <- any(records_subset[[speciesCol]] == sp & 
                            records_subset[[unit_col]] == unit)
        }
        if(detected) {
          incidence_matrix[sp, as.character(unit)] <- 1
        }
      }
    }
    
    return(incidence_matrix)
  }
  
  # If assemblageCol is provided, create separate matrices for each assemblage
  if(!is.null(assemblageCol)) {
    if(!assemblageCol %in% names(recordTable)) {
      stop("assemblageCol not found in recordTable")
    }
    
    assemblages <- unique(recordTable[[assemblageCol]])
    incidence_matrices <- list()
    
    for(assem in assemblages) {
      records_subset <- recordTable[recordTable[[assemblageCol]] == assem,]
      stations_subset <- CTtable[CTtable[[stationCol]] %in% 
                                   unique(records_subset[[stationCol]]),]
      species_list <- unique(records_subset[[speciesCol]])
      
      incidence_matrices[[assem]] <- create_incidence_matrix(
        records_subset, 
        stations_subset,
        species_list,
        temporal = x_unit == "day"
      )
    }
  } else {
    # Create single matrix for all data
    species_list <- unique(recordTable[[speciesCol]])
    incidence_matrices <- create_incidence_matrix(
      recordTable,
      CTtable,
      species_list,
      temporal = x_unit == "day"
    )
    incidence_matrices <- list(incidence_matrices)
  }
  
  
  # Run iNEXT
  out <- iNEXT::iNEXT(incidence_matrices, 
                      datatype = "incidence_raw", 
                      q = q,
                      knots = knots,
                      conf = conf,
                      nboot = nboot)
  
  return(out)
}


# Helper function to convert records to station-days
convert_to_station_days <- function(recordTable, 
                                    CTtable, 
                                    speciesCol, 
                                    stationCol,
                                    recordDateTimeCol, 
                                    setupCol,
                                    return = "accumulation") {
  
  return <- match.arg(return, choices = c("recordTable", "accumulation"))
  
  # Convert all times to POSIXct
  record_times <- as.POSIXct(recordTable[[recordDateTimeCol]])
  setup_times <- as.POSIXct(CTtable[[setupCol]])
  
  # Create lookup for station setup times
  station_setups <- setNames(setup_times, CTtable[[stationCol]])
  
  # prevent CRAMN check notes
  datetime <- setup_time <- station_day <- species <- new_species <- station <- NULL
  
  # Calculate day number for each record
  records_df <- data.frame(
    station = recordTable[[stationCol]],
    species = recordTable[[speciesCol]],
    datetime = record_times
  ) %>%
    # Add setup time for each record's station
    mutate(
      setup_time = station_setups[station],
      # Calculate days since setup (rounded down to integer)
      station_day = as.integer(difftime(datetime, setup_time, units="days")) + 1
    )
  
  # For each station-day, get number of new species
  accumulation_df <- records_df %>%
    arrange(station_day) %>%
    # Keep first detection of each species
    distinct(species, .keep_all = TRUE) %>%
    # Count species per day
    count(station_day, name = "new_species") %>%
    # Calculate cumulative species
    mutate(cumulative_species = cumsum(new_species))
  
  if(return == "recordTable")   return(records_df)
  if(return == "accumulation") return (accumulation_df)
}


