#' Convert Camtrap DP format data to camtrapR format
#'
#' @description
#' This function converts camera trap data from the Camtrap DP standard format to the format
#' used by camtrapR. Camtrap DP is an open standard for the FAIR exchange and archiving of 
#' camera trap data.
#'
#' @param file character. Path to the `datapackage.json` file. Replaces individual CSV arguments.
#' @param deployments_file Deprecated. Ignored in favor of `file`.
#' @param media_file Deprecated. Ignored in favor of `file`.
#' @param observations_file Deprecated. Ignored in favor of `file`.
#' @param datapackage_file Deprecated. Alias for `file`.
#' @param min_gap_hours numeric. Minimum gap in hours to consider as a camera interruption (default: 24)
#' @param removeNA logical. Whether to remove columns with only NA values
#' @param removeEmpty logical. Whether to remove columns with only empty values
#' @param remove_bbox logical. Whether to remove bounding box columns in the observation table
#' @param add_file_path logical. Whether to add file path from media table to the observation table
#' @param filter_observations Controls which observation types to include. \code{NULL} or \code{FALSE} keeps all (default), \code{TRUE} keeps only animal, or provide a character vector of types.
#' 
#' 
#' @details
#' While the 'camtrapdp' package provides general functionality for reading, 
#' manipulating, and transforming Camtrap DP data, this function specifically converts Camtrap DP 
#' data directly into the format required by camtrapR, producing the camera trap table and recordTable objects 
#' that camtrapR functions expect.
#' 
#' This function acts as a wrapper around \code{\link[camtrapdp]{read_camtrapdp}}, utilizing it to 
#' robustly read the datapackage, upgrade older versions, and validate the schema, before applying 
#' camtrapR's specific formatting logic and gap analysis.
#'
#' The function provides a fallback mechanism for location names and species names. 
#' 
#' @section Handling Optional Data and Fallbacks:
#' The Camtrap DP standard is designed to be highly flexible, meaning several
#' columns that are traditionally required by \code{camtrapR} (such as specific
#' station names or common species names) are technically optional.
#' To ensure your data always loads smoothly into \code{camtrapR} formats, the 
#' \code{readCamtrapDP()} function employs a robust fallback hierarchy.
#' 
#' \strong{Location / Station Fallbacks}
#' 
#' In \code{camtrapR}, deployments are grouped by a \code{Station} identifier.
#' Under the Camtrap DP standard, both \code{locationID} and \code{locationName}
#' are optional. Real-world datasets vary wildly: the official standard example
#' provides \code{locationName}, while other datasets might only provide
#' \code{locationID} (leaving \code{locationName} as \code{NA}), or neither!
#' 
#' To guarantee a \code{Station} ID is created, the function looks for
#' identifiers in this exact order:
#' \enumerate{
#'   \item \code{locationID}: The primary identifier.
#'   \item \code{locationName}: Used if \code{locationID} is completely missing or all \code{NA}.
#'   \item \code{deploymentID}: The ultimate fallback. If no location data is
#'   provided, the function treats each individual deployment as its own unique station.
#' }
#' 
#' \emph{(Note: If the chosen column has some missing values, those specific
#' blanks are automatically filled with the \code{deploymentID}).}
#' 
#' \strong{Taxonomy and Species Fallbacks}
#' 
#' Similarly, \code{vernacularNames} (common names) are entirely optional in
#' Camtrap DP. Furthermore, non-animal records (like camera misfires or human
#' activity) will naturally lack a \code{scientificName}. In \code{camtrapR},
#' users typically expect a single classification column that contains both
#' animals and non-animals.
#' 
#' To handle this, the function applies this taxonomic fallback hierarchy:
#' \enumerate{
#'   \item \code{vernacularNames}: If provided in the dataset, these are joined
#'   to the observations (e.g., as \code{vernacularName_en}). Non-animal records
#'   (which have no name) are filled using the core \code{observationType}
#'   (e.g., \code{"blank"}, \code{"human"}, \code{"vehicle"}).
#'   \item \code{scientificName}: If no vernacular names exist, the function
#'   falls back to using \code{scientificName} as the primary classification column.
#'   It will issue a warning to inform you of this, and populate any empty rows
#'   with the \code{observationType} so that non-animal events are not lost as
#'   \code{NA}s in downstream \code{camtrapR} analyses.
#' }
#'
#'
#' 
#' 
#' @return List (of class `cams_dp`) containing three elements:
#' \itemize{
#'   \item \strong{CTtable}: Data frame with camera trap deployment information in camtrapR format
#'   \item \strong{recordTable}: Data frame with species records in camtrapR format
#'   \item \strong{metadata}: List containing project metadata extracted from datapackage.json
#' }
#' 
#' @note
#' The Camtrap DP standard structures data in a way that supports both media-based observations
#' (using a single media file as source) and event-based observations (considering an event with
#' a specified duration as source). For the purpose of this function, both are treated to be equivalent. 
#' Event-based observations are converted to a single timestamp in the recordTable (using the event start time).
#'
#'
#' @references
#' Bubnicki, J.W., Norton, B., Baskauf, S.J., et al. (2023). Camtrap DP: an open standard for the 
#' FAIR exchange and archiving of camera trap data. Remote Sensing in Ecology and Conservation, 10(3), 283-295.
#' 
#' Desmet, P., Govaert, S., Huybrechts, P., Oldoni, D. (2024). camtrapdp: Read and Manipulate Camera 
#' Trap Data Packages. R package  https://CRAN.R-project.org/package=camtrapdp
#' 
#' @examples
#' \dontrun{
#' # load the sample camtrap DP dataset included in camtrapR.
#'  path_camtrapdp <- system.file("sample_data/tdwg_camtrap-dp_1.0.2_example", 
#'                                package = "camtrapR")
#' camtrapdp_data <- readCamtrapDP(file = file.path(path_camtrapdp, "datapackage.json")) 
#' camtrapdp_data
#' 
#' # Extract components
#' ct_table     <- camtrapdp_data$CTtable
#' record_table <- camtrapdp_data$recordTable
#' metadata     <- camtrapdp_data$metadata
#'
#'# See Vignette 6 for more thorough examples for working with camtrap DP data. 
#' }
#' 
#' 
#' @export
#' @importFrom dplyr bind_rows left_join
#' 
#' 
#' 
readCamtrapDP <- function(
    file = "datapackage.json", 
    deployments_file = NULL, 
    media_file = NULL, 
    observations_file = NULL,
    datapackage_file = NULL,
    min_gap_hours = 24,
    removeNA = FALSE,
    removeEmpty = FALSE,
    remove_bbox = TRUE,
    add_file_path = FALSE,
    filter_observations = NULL 
) {
  
  # Ensure the underlying parser package is available
  if (!requireNamespace("camtrapdp", quietly = TRUE)) {
    stop("The 'camtrapdp' package is required. Please install it using install.packages('camtrapdp').")
  }
  
  # --------------------------------------------------------- #
  # ARGUMENT HANDLING & DATA INGESTION   ----
  # --------------------------------------------------------- #
  
  # Handle legacy arguments to ensure backwards compatibility with older camtrapR scripts
  if (!is.null(datapackage_file)) {
    file <- datapackage_file
  }
  
  # Warn the user if they try to pass individual CSVs, as camtrapdp strictly uses the JSON metadata
  if (!is.null(deployments_file) || !is.null(media_file) || !is.null(observations_file)) {
    warning("Arguments deployments_file, media_file, and observations_file are ignored. Data is read strictly via the datapackage.json file using camtrapdp.")
  }
  
  # Read full datapackage using the camtrapdp package.
  # This automatically handles schema validation, version upgrades (e.g., 1.0 to 1.0.2), 
  # event/media assignment, and joining taxonomic metadata into observations.
  dp <- camtrapdp::read_camtrapdp(file)
  
  # Extract the three core data frames from the camtrapdp object
  deployments <- as.data.frame(camtrapdp::deployments(dp))
  observations <- as.data.frame(camtrapdp::observations(dp))
  
  if (add_file_path) {
    media <- as.data.frame(camtrapdp::media(dp))
  }
  
  # Extract metadata (strip the 'data' payload to mimic the old camtrapR output structure)
  metadata <- unclass(dp)
  metadata$data <- NULL
  
  # --------------------------------------------------------- #
  #  COLUMN MAPPING   ----
  # --------------------------------------------------------- #
  # The Camtrap DP standard specifies that certain columns are optional. 
  # camtrapR expects specific identifiers to group deployments (stations) and cameras.
  
  # 1. Station ID (locationID)
  station_id_col <- "locationID"
  
  # If locationID is entirely missing or completely NA, we need a fallback.
  if (!station_id_col %in% colnames(deployments) || all(is.na(deployments[[station_id_col]]))) {
    # Prefer locationName if it exists and has data
    if ("locationName" %in% colnames(deployments) && !all(is.na(deployments$locationName))) {
      station_id_col <- "locationName"
    } else {
      # Ultimate fallback: treat each deployment as its own station
      station_id_col <- "deploymentID"
    }
  }
  
  # Ensure there are absolutely no NAs/blanks in the chosen station ID column for individual rows
  na_stations <- is.na(deployments[[station_id_col]]) | deployments[[station_id_col]] == ""
  if (any(na_stations)) {
    warning(paste(sum(na_stations), "deployments have no station ID."))
    deployments[[station_id_col]][na_stations] <- deployments$deploymentID[na_stations]
  }
  
  # 2. Camera ID (cameraID)
  # If cameras are not explicitly identified, provide a safe dummy value to prevent downstream errors.
  camera_id_col <- "cameraID"
  if (!camera_id_col %in% colnames(deployments)) {
    deployments[[camera_id_col]] <- "unknown_camera"
  } else {
    # Fill in any missing camera IDs in an existing column
    is_missing_cam <- is.na(deployments[[camera_id_col]]) | deployments[[camera_id_col]] == ""
    deployments[[camera_id_col]][is_missing_cam] <- "unknown_camera"
  }
  
  # 3. Date formatting
  # Ensure event boundaries are POSIXct (camtrapdp usually handles this, but we force it to be safe)
  if (!inherits(deployments$deploymentStart, "POSIXct")) {
    deployments$deploymentStart <- as.POSIXct(deployments$deploymentStart, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
  }
  if (!inherits(deployments$deploymentEnd, "POSIXct")) {
    deployments$deploymentEnd <- as.POSIXct(deployments$deploymentEnd, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
  }
  
  # --------------------------------------------------------- #
  # TAG PARSING ----
  # --------------------------------------------------------- #
  # Camtrap DP allows pipe-separated tags (e.g., "bait:meat | setup:tree"). 
  # We expand these into wide format boolean/value columns.
  
  if ("deploymentTags" %in% colnames(deployments)) {
    tag_columns <- parse_tags(deployments, "deploymentTags")
    if (ncol(tag_columns) > 0) deployments <- cbind(deployments, tag_columns)
  }
  
  if ("deploymentGroups" %in% colnames(deployments)) {
    group_columns <- parse_tags(deployments, "deploymentGroups")
    if (ncol(group_columns) > 0) deployments <- cbind(deployments, group_columns)
  }
  
  if ("observationTags" %in% colnames(observations)) {
    obs_tag_columns <- parse_tags(observations, "observationTags")
    if (ncol(obs_tag_columns) > 0) observations <- cbind(observations, obs_tag_columns)
  }
  
  # --------------------------------------------------------- #
  # PHASE 1: GENERATE CTtable (Deployment summary)   ----
  # ---------------------------------------------------------  #
  
  stations <- unique(deployments[[station_id_col]])
  ctTable <- data.frame(stringsAsFactors = FALSE)
  
  # Iterate over each unique station to build a single row per station
  for (station in stations) {
    # Isolate all deployments belonging to this specific station, ordered chronologically
    station_deployments <- deployments[deployments[[station_id_col]] == station, ]
    station_deployments <- station_deployments[order(station_deployments$deploymentStart), ]
    
    # Initialize the base row with the absolute start and end times across all deployments
    row <- data.frame(
      Station = as.character(station),   # <- this is the persistent station identifier (based on locationID / locationName / deploymentID)
      Setup_date = as.character(format(min(station_deployments$deploymentStart), "%Y-%m-%d %H:%M:%S")),
      Retrieval_date = as.character(format(max(station_deployments$deploymentEnd), "%Y-%m-%d %H:%M:%S")),
      stringsAsFactors = FALSE
    )
    
    # GAP ANALYSIS: If a station was deployed multiple times, check for gaps between them.
    # If the gap between deployment N end and deployment N+1 start exceeds min_gap_hours,
    # log it as a "Problem" period (camera wasn't active).
    if (nrow(station_deployments) > 1) {
      problem_count <- 0
      for (i in 2:nrow(station_deployments)) {
        current_start <- station_deployments$deploymentStart[i]
        previous_end <- station_deployments$deploymentEnd[i-1]
        
        gap_hours <- as.numeric(difftime(current_start, previous_end, units = "hours"))
        if (!is.na(gap_hours) && gap_hours >= min_gap_hours) {
          problem_count <- problem_count + 1
          row[[paste0("Problem", problem_count, "_from")]] <- as.character(format(previous_end, "%Y-%m-%d %H:%M:%S"))
          row[[paste0("Problem", problem_count, "_to")]] <- as.character(format(current_start, "%Y-%m-%d %H:%M:%S"))
        }
      }
    }
    
    # Aggregate all other deployment columns. 
    # If a station had multiple deployments with differing metadata (e.g. moved from lat X to lat Y),
    # combine them with semicolons.
    for (col in colnames(station_deployments)) {
      if (col %in% c("deploymentID", "deploymentStart", "deploymentEnd")) next
      
      values <- unique(station_deployments[[col]])
      values <- values[!is.na(values)]
      
      if (length(values) > 0) {
        row[[col]] <- paste(as.character(values), collapse = "; ")
      } else {
        row[[col]] <- NA_character_ 
      }
    }
    
    # First iteration: Initialize empty dataframe with the exact structure of 'row'
    if (nrow(ctTable) == 0) {
      empty_df <- as.data.frame(lapply(row, function(x) character(0)))
      ctTable <- empty_df
    }
    
    ctTable <- dplyr::bind_rows(ctTable, row)
  }
  
  # Type restoration: Because of the semicolon concatenation above, all cols became characters.
  # We look for columns that never received a semicolon (meaning they were consistent across deployments)
  # and convert them back to their proper numeric or logical types.
  numeric_cols <- c("latitude", "longitude", "coordinateUncertainty", 
                    "cameraDelay", "cameraHeight", "cameraTilt", 
                    "cameraHeading", "detectionDistance")
  for (col in numeric_cols) {
    if (col %in% colnames(ctTable) && !any(grepl(";", ctTable[[col]]), na.rm = TRUE)) {
      ctTable[[col]] <- as.numeric(ctTable[[col]])
    }
  }
  
  logical_cols <- c("timestampIssues", "baitUse")
  for (col in logical_cols) {
    if (col %in% colnames(ctTable) && !any(grepl(";", ctTable[[col]]), na.rm = TRUE)) {
      ctTable[[col]] <- as.logical(ctTable[[col]])
    }
  }
  
  # --------------------------------------------------------- #
  # PHASE 2: GENERATE recordTable (Observations summary) ----
  # --------------------------------------------------------- #
  
  recordTable <- observations
  
  # Pull station and camera context from deployments and join it into the observations
  selected_deployment_cols <- c("deploymentID", station_id_col, camera_id_col, "locationName")
  selected_deployment_cols <- intersect(selected_deployment_cols, colnames(deployments))
  
  recordTable <- dplyr::left_join(recordTable, deployments[, selected_deployment_cols], by = "deploymentID")
  
  # Ensure the recordTable gets the exact same 'Station' column as the CTtable
  # name is always "Station", based on locationID, locationName, or deploymentID
  recordTable$Station <- recordTable[[station_id_col]]
  
  # Append media file paths if requested (requires 'mediaID' to map observations back to specific images/video)
  if (add_file_path && "mediaID" %in% colnames(recordTable) && "mediaID" %in% colnames(media)) {
    recordTable <- dplyr::left_join(recordTable, media[, c("mediaID", "filePath")], by = "mediaID")
  }
  
  # camtrapR expects 'DateTimeOriginal'. We derive this from eventStart.
  if ("eventStart" %in% colnames(recordTable)) {
    recordTable$DateTimeOriginal <- as.POSIXct(recordTable$eventStart, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
  }
  
  # Optionally strip out machine-learning bounding box coordinates
  if (remove_bbox) {
    bbox_cols <- c("bboxX", "bboxY", "bboxWidth", "bboxHeight")
    recordTable <- recordTable[, !colnames(recordTable) %in% bbox_cols, drop = FALSE]
  }
  
  # Filter to purely animal observations, specific categories, or leave as-is
  if (!is.null(filter_observations)) {
    if (isTRUE(filter_observations)) {
      recordTable <- recordTable[recordTable$observationType == "animal", ]
    } else if (is.character(filter_observations)) {
      recordTable <- recordTable[recordTable$observationType %in% filter_observations, ]
    }
  }
  
  # --------------------------------------------------------- #
  # PHASE 3: TAXONOMY PROCESSING   ----
  # --------------------------------------------------------- #
  # camtrapdp already joined taxonomic metadata, prefixing them with 'taxon.'
  # rename these columns for camtrapR compatibility (e.g. taxon.vernacularNames.en -> vernacularName_en)
  colnames(recordTable) <- gsub("^taxon\\.vernacularNames\\.", "vernacularName_", colnames(recordTable))
  colnames(recordTable) <- gsub("^taxon\\.", "", colnames(recordTable))
  
  vern_cols <- grep("^vernacularName", colnames(recordTable), value = TRUE)
  
  if (length(vern_cols) == 0) {
    # If no vernacular names exist, warn the user and fall back to scientificName
    warning("No vernacular names found in metadata. Using 'scientificName' as the primary species column and filling non-animal records (e.g., blanks) with 'observationType'.")
    
    if ("scientificName" %in% colnames(recordTable)) {
      speciesCol <- "scientificName"
      
      empty_names <- is.na(recordTable$scientificName) | recordTable$scientificName == ""
      has_obs_type <- !is.na(recordTable$observationType) & recordTable$observationType != ""
      
      # Fill NAs in scientificName with observationType (e.g. 'blank') to satisfy camtrapR formats
      recordTable$scientificName[empty_names & has_obs_type] <- as.character(recordTable$observationType[empty_names & has_obs_type])
    } else {
      stop("No species column found in observations table (no 'scientificName' or 'vernacularName' columns).")
    }
  } else {
    # If vernacular names DO exist, apply the observationType fallback to them instead
    for (vc in vern_cols) {
      empty_names <- is.na(recordTable[[vc]]) | recordTable[[vc]] == ""
      has_obs_type <- !is.na(recordTable$observationType) & recordTable$observationType != ""
      
      recordTable[[vc]][empty_names & has_obs_type] <- as.character(recordTable$observationType[empty_names & has_obs_type])
    }
    # guess species column (prefer english, otherwise take first one)
    if("vernacularName_eng" %in% vern_cols) {
      speciesCol <- "vernacularName_eng"
    } else {
      speciesCol <- vern_cols[1]
    }
  }
  
  # --------------------------------------------------------- #
  # PHASE 4: FINAL CLEANUP & REORDERING  ----
  # --------------------------------------------------------- #
  
  # Drop completely uninformative columns if requested
  if (removeNA) {
    ctTable <- remove_na_columns(ctTable)
    recordTable <- remove_na_columns(recordTable)
  }
  
  if (removeEmpty) {
    ctTable <- remove_empty_columns(ctTable)
    recordTable <- remove_empty_columns(recordTable)
  }
  
  # Apply consistent column ordering for readability
  ctTable <- reorder_deployment_columns(ctTable)
  recordTable <- reorder_observation_columns(recordTable)
  
  # Assemble the list
  out <- list(
    CTtable = ctTable,
    recordTable = recordTable,
    metadata = metadata
  )
  
  # declare specific classes and store attributes
  out$CTtable <- as_cams(out$CTtable, 
                         stationCol = "Station")

  out$recordTable <- as_records(out$recordTable, 
                                stationCol = "Station",
                                speciesCol = speciesCol) # depends on presence of vernacularName* columns
  
  out <- as_cams_dp(out)
  
  out
}

# --------------------------------------------------------- #
# HELPER FUNCTIONS  ----
# --------------------------------------------------------- #

# Reorders the deployment table columns logically (locations first, then dates, problems, coordinates, tags, etc.)
reorder_deployment_columns <- function(df) {
  all_cols <- colnames(df)
  
  location_cols <- c("locationName", "Station", "locationID")
  temporal_cols <- c("Setup_date", "Retrieval_date")
  problem_cols <- grep("^Problem[0-9]+_(from|to)$", all_cols, value = TRUE)
  spatial_cols <- c("latitude", "longitude", "coordinateUncertainty")
  tag_cols <- grep("^deploymentTags_", all_cols, value = TRUE)
  group_cols <- grep("^deploymentGroups_", all_cols, value = TRUE)
  original_tag_cols <- c("deploymentTags", "deploymentGroups")
  env_cols <- c("habitat", "featureType")
  camera_cols <- c("cameraID", "cameraModel", "cameraDelay", "cameraHeight", 
                   "cameraTilt", "cameraHeading", "detectionDistance", 
                   "timestampIssues", "baitUse", "setupBy")
  comment_cols <- c("deploymentComments")
  
  priority_cols <- c(
    intersect(location_cols, all_cols),
    intersect(temporal_cols, all_cols),
    intersect(problem_cols, all_cols),
    intersect(spatial_cols, all_cols),
    intersect(tag_cols, all_cols),
    intersect(group_cols, all_cols),
    intersect(original_tag_cols, all_cols),
    intersect(env_cols, all_cols),
    intersect(camera_cols, all_cols),
    intersect(comment_cols, all_cols)
  )
  
  # Append any columns that aren't specifically prioritized to the end
  remaining_cols <- setdiff(all_cols, priority_cols)
  new_order <- c(priority_cols, remaining_cols)
  existing_cols <- intersect(new_order, all_cols)
  
  return(df[, existing_cols, drop = FALSE])
}

# Reorders the observation table logically (location, species/taxonomy, timestamps, observation data, IDs)
reorder_observation_columns <- function(df) {
  all_cols <- colnames(df)
  
  location_cols <- c("locationName", "Station", "locationID", "cameraID")
  species_cols <- c("Species", "scientificName")
  vernacular_cols <- grep("^vernacularName", all_cols, value = TRUE)
  taxon_cols <- c("taxonRank")
  temporal_cols <- c("DateTimeOriginal", "eventStart", "eventEnd")
  individual_cols <- c("count", "sex", "lifeStage", "individualID", "behavior")
  tag_cols <- grep("^observationTags_", all_cols, value = TRUE)
  original_tag_cols <- c("observationTags")
  obs_meta_cols <- c("observationType", "observationLevel", "cameraSetupType")
  classification_cols <- c("classificationMethod", "classifiedBy", 
                           "classificationTimestamp", "classificationProbability")
  bbox_cols <- c("bboxX", "bboxY", "bboxWidth", "bboxHeight")
  file_cols <- c("filePath", "mediaID")
  comment_cols <- c("observationComments")
  id_cols <- c("observationID", "eventID", "deploymentID")
  
  priority_cols <- c(
    intersect(location_cols, all_cols),
    intersect(species_cols, all_cols),
    intersect(vernacular_cols, all_cols),
    intersect(taxon_cols, all_cols),
    intersect(temporal_cols, all_cols),
    intersect(individual_cols, all_cols),
    intersect(tag_cols, all_cols),
    intersect(original_tag_cols, all_cols),
    intersect(obs_meta_cols, all_cols),
    intersect(classification_cols, all_cols),
    intersect(bbox_cols, all_cols),
    intersect(file_cols, all_cols),
    intersect(comment_cols, all_cols),
    intersect(id_cols, all_cols)
  )
  
  remaining_cols <- setdiff(all_cols, priority_cols)
  new_order <- c(priority_cols, remaining_cols)
  existing_cols <- intersect(new_order, all_cols)
  
  return(df[, existing_cols, drop = FALSE])
}

# Takes a pipe-separated string column and expands it into multiple wide columns.
# Supports simple flags ("flagA | flagB") and key-value pairs ("key:value1 | key2:value2")
parse_tags <- function(df, tag_column) {
  if (!tag_column %in% colnames(df) || all(is.na(df[[tag_column]]))) return(data.frame())
  
  tag_df <- data.frame(row.names = rownames(df))
  
  for (i in 1:nrow(df)) {
    if (is.na(df[[tag_column]][i]) || df[[tag_column]][i] == "") next
    tags <- strsplit(df[[tag_column]][i], "\\|")[[1]]
    
    for (tag in tags) {
      tag <- trimws(tag)
      if (tag == "") next
      
      if (grepl(":", tag)) {
        # Key-Value tag
        key_value <- strsplit(tag, ":")[[1]]
        key <- trimws(key_value[1])
        value <- trimws(key_value[2])
        column_name <- paste0(tag_column, "_", key)
        
        # Cast value if numeric or logical
        if (tolower(value) %in% c("true", "false")) {
          value <- as.logical(value)
        } else if (grepl("^[0-9]+$", value)) {
          value <- as.integer(value)
        } else if (grepl("^[0-9]*\\.[0-9]+$", value)) {
          value <- as.numeric(value)
        }
        
        if (!column_name %in% colnames(tag_df)) tag_df[[column_name]] <- NA
        tag_df[i, column_name] <- value
      } else {
        # Boolean Flag tag
        column_name <- paste0(tag_column, "_", tag)
        if (!column_name %in% colnames(tag_df)) tag_df[[column_name]] <- FALSE
        tag_df[i, column_name] <- TRUE
      }
    }
  }
  return(tag_df)
}

# Filters out any columns that consist entirely of NA
remove_na_columns <- function(df) {
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  keep_cols <- names(na_counts[na_counts < nrow(df)])
  if (length(keep_cols) > 0) return(df[, keep_cols, drop = FALSE])
  return(df)
}

# Filters out character columns that consist entirely of empty strings ""
remove_empty_columns <- function(df) {
  keep_cols <- sapply(df, function(x) {
    if (!is.character(x)) return(TRUE)
    empty_count <- sum(x == "", na.rm = TRUE)
    non_na_count <- sum(!is.na(x))
    return(empty_count < non_na_count)
  })
  if (any(keep_cols)) return(df[, keep_cols, drop = FALSE])
  return(df)
}


