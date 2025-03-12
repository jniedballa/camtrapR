#' Convert Camtrap DP format data to camtrapR format
#'
#' @description
#' This function converts camera trap data from the Camtrap DP standard format to the format
#' used by camtrapR. Camtrap DP is an open standard for the FAIR exchange and archiving of 
#' camera trap data, structured in a simple yet flexible data model consisting of three tables: 
#' Deployments, Media, and Observations.
#'
#' @details
#' Camtrap DP is a standardized format developed under the umbrella of the Biodiversity Information
#' Standards (TDWG) that facilitates data exchange between different camera trap platforms and systems.
#' It supports a wide range of camera deployment designs, classification techniques, and analytical use cases.
#' 
#' While the 'camtrapdp' package (available on CRAN) provides general functionality for reading, 
#' manipulating, and transforming Camtrap DP data, this function specifically converts Camtrap DP 
#' data directly into the format required by camtrapR, producing the CTtable and recordTable objects 
#' that camtrapR functions expect.
#' 
#' This function reads the three primary tables from a Camtrap DP dataset:
#' \itemize{
#'   \item Deployments: Information about camera trap placements
#'   \item Observations: Classifications derived from the media files
#'   \item Media: Information about the media files (images/videos) recorded
#' }
#'
#' The Media table is only read if the \code{add_file_path} parameter is set to \code{TRUE}. 
#'
#' The function converts these into two primary camtrapR data structures:
#' \itemize{
#'   \item CTtable: Contains deployment information including station ID, setup/retrieval dates, 
#'         camera operation problems, camera setup, and more
#'   \item recordTable: Contains observation records with taxonomic and temporal information
#' }
#'
#' Additional features include:
#' \itemize{
#'   \item Parsing of deploymentTags and deploymentGroups (in deployments) and observationTags (in observations)
#'   \item Extraction of taxonomic information from metadata 
#'   \item Handling of deployment intervals and gaps
#' }
#'
#' @param deployments_file character. Path to deployments.csv file
#' @param media_file character. Path to media.csv file
#' @param observations_file character. Path to observations.csv file
#' @param datapackage_file character. Path to datapackage.json file
#' @param min_gap_hours numeric. Minimum gap in hours to consider as a camera interruption (default: 24)
#' @param removeNA logical. Whether to remove columns with only NA values
#' @param removeEmpty logical. Whether to remove columns with only empty values
#' @param remove_bbox logical. Whether to remove bounding box columns in the observation table
#' @param add_file_path logical. Whether to add file path from media table to the observation table
#' @param filter_observations Controls which observation types to include. \code{NULL} or \code{FALSE} keeps all observations (default), \code{TRUE} keeps only animal observations, or provide a character vector of specific observation types to include.
#' 
#' @return List containing three elements:
#' \itemize{
#'   \item CTtable: Data frame with camera trap deployment information in camtrapR format
#'   \item recordTable: Data frame with species records in camtrapR format
#'   \item metadata: List containing project metadata extracted from datapackage.json
#' }
#' 
#' @note
#' Camtrap DP was developed as a consensus of a long, in-depth consultation process with 
#' camera trap data management platforms and major players in the field of camera trapping.
#' It builds upon the earlier Camera Trap Metadata Standard (CTMS) but addresses its limitations.
#' 
#' The Camtrap DP standard structures data in a way that supports both media-based observations
#' (using a single media file as source) and event-based observations (considering an event with
#' a specified duration as source). For the purpose of this function, both are treated to be equivalent. 
#' Event-based observations are converted to a single timestamp in the recordTable (using the event start time).
#'
#' Consider using the 'camtrapdp' package (Desmet et al., 2024) for more general operations on 
#' Camtrap DP data before converting to camtrapR format with this function.
#'
#' @references
#' Bubnicki, J.W., Norton, B., Baskauf, S.J., et al. (2023). Camtrap DP: an open standard for the 
#' FAIR exchange and archiving of camera trap data. Remote Sensing in Ecology and Conservation, 10(3), 283-295.
#' 
#' Desmet, P., Govaert, S., Huybrechts, P., Oldoni, D. (2024). camtrapdp: Read and Manipulate Camera 
#' Trap Data Packages. R package version 0.3.1. https://CRAN.R-project.org/package=camtrapdp
#' 
#' @examples
#' \dontrun{
#' # Read a Camtrap DP dataset
#' camtrap_data <- readcamtrapDP(
#'   deployments_file = "path/to/deployments.csv",
#'   media_file = "path/to/media.csv",
#'   observations_file = "path/to/observations.csv",
#'   datapackage_file = "path/to/datapackage.json"
#' )
#' 
#' # alternatively, set the working directory only
#' setwd("path/to/camtrapdp_data")
#' camtrap_data <- readcamtrapDP()   # uses default file names
#' 
#' 
#' # Extract components
#' ct_table <- camtrap_data$CTtable
#' record_table <- camtrap_data$recordTable
#' metadata <- camtrap_data$metadata
#' 
#' }
#' 
#' @seealso 
#' \code{\link[camtrapdp]{read_camtrapdp}} in the 'camtrapdp' package for reading Camtrap DP data
#' \code{\link[camtrapdp]{check_camtrapdp}} in the 'camtrapdp' package for validating Camtrap DP data
#' 
#' @export
#' @importFrom dplyr bind_rows
#' 
readcamtrapDP <- function(
    deployments_file = "deployments.csv", 
    media_file = "media.csv", 
    observations_file = "observations.csv",
    datapackage_file = "datapackage.json",
    min_gap_hours = 24,
    removeNA = FALSE,
    removeEmpty = FALSE,
    remove_bbox = TRUE,
    add_file_path = FALSE,
    filter_observations = NULL 
) {
  
  # Read input files
  deployments <- read.csv(deployments_file, stringsAsFactors = FALSE)
  observations <- read.csv(observations_file, stringsAsFactors = FALSE)
  if(add_file_path) media <- read.csv(media_file, stringsAsFactors = FALSE)
  
  # Load metadata from datapackage.json
  metadata <- process_datapackage_json(datapackage_file)
  
  
  # Define column names
  station_id_col = "locationID"
  camera_id_col = "cameraID"
  coord_x = "longitude"
  coord_y = "latitude"
  
  # Ensure date fields are properly formatted 
  deployments$deploymentStart <- ymd_hms(deployments$deploymentStart)
  deployments$deploymentEnd   <- ymd_hms(deployments$deploymentEnd)
  if(add_file_path) media$timestamp  <- ymd_hms(media$timestamp)
  
  # Convert logical columns to proper logical type
  deployments  <- convert_logical_columns(deployments)
  observations <- convert_logical_columns(observations)
  if(add_file_path) media <- convert_logical_columns(media)
  
  # Parse deploymentTags and deploymentGroups
  if ("deploymentTags" %in% colnames(deployments)) {
    tag_columns <- parse_tags(deployments, "deploymentTags")
    if (ncol(tag_columns) > 0) {
      deployments <- cbind(deployments, tag_columns)
    }
  }
  
  if ("deploymentGroups" %in% colnames(deployments)) {
    group_columns <- parse_tags(deployments, "deploymentGroups")
    if (ncol(group_columns) > 0) {
      deployments <- cbind(deployments, group_columns)
    }
  }
  
  # Parse observationTags in observations table
  if ("observationTags" %in% colnames(observations)) {
    obs_tag_columns <- parse_tags(observations, "observationTags")
    if (ncol(obs_tag_columns) > 0) {
      observations <- cbind(observations, obs_tag_columns)
    }
  }
  
  # Get unique stations
  stations <- unique(deployments[[station_id_col]])
  
  # Create data frame to hold all rows
  ctTable <- data.frame(stringsAsFactors = FALSE)
  
  for (station in stations) {
    # Get all deployments for this station
    station_deployments <- deployments[deployments[[station_id_col]] == station, ]
    station_deployments <- station_deployments[order(station_deployments$deploymentStart), ]
    
    # Create basic row with overall deployment period (as character)
    row <- data.frame(
      Station = as.character(station),
      Setup_date = as.character(format(min(station_deployments$deploymentStart), "%Y-%m-%d %H:%M:%S")),
      Retrieval_date = as.character(format(max(station_deployments$deploymentEnd), "%Y-%m-%d %H:%M:%S")),
      stringsAsFactors = FALSE
    )
    
    # Handle problems (gaps between deployments)
    if (nrow(station_deployments) > 1) {
      problem_count <- 0
      
      for (i in 2:nrow(station_deployments)) {
        current_start <- station_deployments$deploymentStart[i]
        previous_end <- station_deployments$deploymentEnd[i-1]
        
        # Calculate gap
        gap_hours <- as.numeric(difftime(current_start, previous_end, units = "hours"))
        
        # If gap exceeds threshold, add as a problem
        if (gap_hours >= min_gap_hours) {
          problem_count <- problem_count + 1
          row[[paste0("Problem", problem_count, "_from")]] <- as.character(format(previous_end, "%Y-%m-%d %H:%M:%S"))
          row[[paste0("Problem", problem_count, "_to")]] <- as.character(format(current_start, "%Y-%m-%d %H:%M:%S"))
        }
      }
    }
    
    # Add all columns from deployments table as character
    for (col in colnames(station_deployments)) {
      # Skip deploymentID, deploymentStart, deploymentEnd which we've already used
      if (col %in% c("deploymentID", "deploymentStart", "deploymentEnd")) {
        next
      }
      
      # Get unique values for this column across all deployments for this station
      values <- unique(station_deployments[[col]])
      values <- values[!is.na(values)] # Remove NA values
      
      if (length(values) > 0) {
        # Convert all values to character before concatenating
        values_char <- as.character(values)
        row[[col]] <- paste(values_char, collapse = "; ")
      } else {
        row[[col]] <- NA_character_  # Use NA_character_ for consistency
      }
    }
    
    # If this is the first row, initialize the ctTable with the same columns
    if (nrow(ctTable) == 0) {
      empty_df <- as.data.frame(lapply(row, function(x) character(0)))
      ctTable <- empty_df
    }
    
    # Add to CTtable
    ctTable <- bind_rows(ctTable, row)
  }
  
  # Convert setup and retrieval dates to character with appropriate format
  # ctTable$Setup_date     <- format(ctTable$Setup_date, "%Y-%m-%d %H:%M:%S")
  # ctTable$Retrieval_date <- format(ctTable$Retrieval_date, "%Y-%m-%d %H:%M:%S")
  
  
  # Convert numeric columns to appropriate types
  numeric_cols <- c("latitude", "longitude", "coordinateUncertainty", 
                    "cameraDelay", "cameraHeight", "cameraTilt", 
                    "cameraHeading", "detectionDistance")
  
  for (col in numeric_cols) {
    if (col %in% colnames(ctTable)) {
      # Only convert if the column doesn't contain semicolons (which would indicate multiple values)
      has_multiple <- grepl(";", ctTable[[col]])
      if (!any(has_multiple, na.rm = TRUE)) {
        ctTable[[col]] <- as.numeric(ctTable[[col]])
      }
    }
  }
  
  # Convert logical columns properly
  logical_cols <- c("timestampIssues", "baitUse")
  for (col in logical_cols) {
    if (col %in% colnames(ctTable)) {
      has_multiple <- grepl(";", ctTable[[col]])
      if (!any(has_multiple, na.rm = TRUE)) {
        ctTable[[col]] <- as.logical(ctTable[[col]])
      }
    }
  }
  
  
  # Process recordTable
  recordTable <- observations
  
  # Add station and camera information from deployments
  selected_deployment_cols <- c("deploymentID", station_id_col, camera_id_col, "locationName")
  selected_deployment_cols <- selected_deployment_cols[selected_deployment_cols %in% colnames(deployments)]
  
  recordTable <- merge(
    recordTable,
    deployments[, selected_deployment_cols],
    by = "deploymentID",
    all.x = TRUE
  )
  
  
  # Add file path if requested
  if (add_file_path && "mediaID" %in% colnames(recordTable)) {
    recordTable <- merge(
      recordTable,
      media[, c("mediaID", "filePath")],
      by = "mediaID",
      all.x = TRUE
    )
  }
  
  # Format DateTimeOriginal from eventStart
  if ("eventStart" %in% colnames(recordTable)) {
    recordTable$DateTimeOriginal <- ymd_hms(recordTable$eventStart)
  }
  
  # Remove bounding box columns if not wanted
  if (remove_bbox) {
    bbox_cols <- c("bboxX", "bboxY", "bboxWidth", "bboxHeight")
    bbox_cols <- bbox_cols[bbox_cols %in% colnames(recordTable)]
    if (length(bbox_cols) > 0) {
      recordTable <- recordTable[, !colnames(recordTable) %in% bbox_cols]
    }
  }
  
  # Remove columns with all NA if requested
  if (removeNA) {
    ctTable <- remove_na_columns(ctTable)
    recordTable <- remove_na_columns(recordTable)
  }
  
  # Remove columns with all empty values if requested
  if (removeEmpty) {
    ctTable <- remove_empty_columns(ctTable)
    recordTable <- remove_empty_columns(recordTable)
  }
  
  
  # Filter observations by type if requested
  if (!is.null(filter_observations)) {
    if (isTRUE(filter_observations)) {
      # Keep only animal observations if TRUE
      recordTable <- recordTable[recordTable$observationType == "animal", ]
    } else if (is.character(filter_observations)) {
      # Keep specified observation types if character vector
      recordTable <- recordTable[recordTable$observationType %in% filter_observations, ]
    }
  }
  
  
  # Add taxonomic information from datapackage.json if available
  if (!is.null(metadata) && !is.null(metadata$taxonomic)) {
    # Create taxonomic lookup
    tax_data <- metadata$taxonomic
    
    # Map scientific names to vernacular names and rank
    if ("scientificName" %in% colnames(recordTable)) {
      # Add taxon rank if available
      if ("taxonRank" %in% names(tax_data)) {
        taxon_rank_map <- setNames(
          as.character(tax_data$taxonRank),
          tax_data$scientificName
        )
        recordTable$taxonRank <- taxon_rank_map[recordTable$scientificName]
      }
      
      # Add vernacular names in all available languages
      if ("vernacularNames" %in% names(tax_data)) {
        vern_names <- tax_data$vernacularNames
        if (is.data.frame(vern_names)) {
          for (lang in colnames(vern_names)) {
            lang_col <- paste0("vernacularName_", lang)
            name_map <- setNames(
              as.character(vern_names[[lang]]),
              tax_data$scientificName
            )
            
            # Map scientific names to vernacular names
            recordTable[[lang_col]] <- name_map[recordTable$scientificName]
            
            # Use observationType as fallback for empty vernacular names
            empty_names <- is.na(recordTable[[lang_col]]) | recordTable[[lang_col]] == ""
            has_obs_type <- !is.na(recordTable$observationType) & recordTable$observationType != ""
            
            # Replace NA/empty vernacular names with observationType where possible
            recordTable[[lang_col]][empty_names & has_obs_type] <- 
              recordTable$observationType[empty_names & has_obs_type]
          }
        }
      } else {
        # # If no vernacular names in metadata, create at least one column with observationType fallback
        # recordTable$vernacularName <- recordTable$scientificName
        # empty_names <- is.na(recordTable$vernacularName) | recordTable$vernacularName == ""
        # has_obs_type <- !is.na(recordTable$observationType) & recordTable$observationType != ""
        # recordTable$vernacularName[empty_names & has_obs_type] <- 
        #   recordTable$observationType[empty_names & has_obs_type]
        
        stop("No vernacularName in metadata")
      }
    }
  } else {
    # # Even without metadata, create a vernacular name column with observationType fallback
    # recordTable$vernacularName <- recordTable$scientificName
    # empty_names <- is.na(recordTable$vernacularName) | recordTable$vernacularName == ""
    # has_obs_type <- !is.na(recordTable$observationType) & recordTable$observationType != ""
    # recordTable$vernacularName[empty_names & has_obs_type] <- 
    #   recordTable$observationType[empty_names & has_obs_type]
    
    stop("No metadata or not taxonomic information in metadata")
  }
  
  
  # Reorder columns of both tables
  ctTable <- reorder_deployment_columns(ctTable)
  recordTable <- reorder_observation_columns(recordTable)
  
  
  return(list(
    CTtable = ctTable,
    recordTable = recordTable,
    metadata = metadata
  ))
}




# #' Reorder columns in the deployment table for better readability
# #'
# #' @param df Deployment data frame
# #' @return Data frame with reordered columns
# #'
reorder_deployment_columns <- function(df) {
  # Get all column names
  all_cols <- colnames(df)
  
  # Define column groups in order of priority
  location_cols <- c("locationName", "Station", "locationID")
  temporal_cols <- c("Setup_date", "Retrieval_date")
  problem_cols <- grep("^Problem[0-9]+_(from|to)$", all_cols, value = TRUE)
  spatial_cols <- c("latitude", "longitude", "coordinateUncertainty")
  
  # Extract tag and group columns (from parsing)
  tag_cols <- grep("^deploymentTags_", all_cols, value = TRUE)
  group_cols <- grep("^deploymentGroups_", all_cols, value = TRUE)
  
  # Original tag/group columns
  original_tag_cols <- c("deploymentTags", "deploymentGroups")
  
  # Environment and habitat info
  env_cols <- c("habitat", "featureType")
  
  # Camera setup info
  camera_cols <- c("cameraID", "cameraModel", "cameraDelay", "cameraHeight", 
                   "cameraTilt", "cameraHeading", "detectionDistance", 
                   "timestampIssues", "baitUse", "setupBy")
  
  # Comment columns
  comment_cols <- c("deploymentComments")
  
  # Combine the priority columns
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
  
  # Any remaining columns go at the end
  remaining_cols <- setdiff(all_cols, priority_cols)
  
  # Final column order
  new_order <- c(priority_cols, remaining_cols)
  
  # Check for missing columns and use only those that exist
  existing_cols <- intersect(new_order, all_cols)
  
  # Return reordered data frame
  return(df[, existing_cols, drop = FALSE])
}

# #' Reorder columns in the observation table for better readability
# #'
# #' @param df Observation data frame
# #' @return Data frame with reordered columns


# Update column reordering to handle taxonomic columns
reorder_observation_columns <- function(df) {
  # Get all column names
  all_cols <- colnames(df)
  
  # Define column groups in order of priority
  location_cols <- c("locationName", "Station", "locationID", "cameraID")
  
  # Expanded taxonomic columns
  species_cols <- c("Species", "scientificName")
  vernacular_cols <- grep("^vernacularName_", all_cols, value = TRUE)
  taxon_cols <- c("taxonRank")
  
  temporal_cols <- c("DateTimeOriginal", "eventStart", "eventEnd")
  
  # Individual info
  individual_cols <- c("count", "sex", "lifeStage", "individualID", "behavior")
  
  # Extract tag columns (from parsing)
  tag_cols <- grep("^observationTags_", all_cols, value = TRUE)
  
  # Original tags column
  original_tag_cols <- c("observationTags")
  
  # Observation metadata
  obs_meta_cols <- c("observationType", "observationLevel", "cameraSetupType")
  
  # Classification info
  classification_cols <- c("classificationMethod", "classifiedBy", 
                           "classificationTimestamp", "classificationProbability")
  
  # Bounding box info
  bbox_cols <- c("bboxX", "bboxY", "bboxWidth", "bboxHeight")
  
  # File and media info
  file_cols <- c("filePath", "mediaID")
  
  # Comment columns
  comment_cols <- c("observationComments")
  
  # ID columns placed at the end
  id_cols <- c("observationID", "eventID", "deploymentID")
  
  # Combine the priority columns
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
  
  # Any remaining columns go at the end
  remaining_cols <- setdiff(all_cols, priority_cols)
  
  # Final column order
  new_order <- c(priority_cols, remaining_cols)
  
  # Check for missing columns and use only those that exist
  existing_cols <- intersect(new_order, all_cols)
  
  # Return reordered data frame
  return(df[, existing_cols, drop = FALSE])
}

# #' Parse pipe-separated tag strings into separate columns
# #'
# #' @param df Data frame containing the tag column
# #' @param tag_column Name of the column containing tags
# #' @return Data frame with new columns for each tag key
# #'
parse_tags <- function(df, tag_column) {
  # Return empty data frame if tag column doesn't exist or all entries are NA
  if (!tag_column %in% colnames(df) || all(is.na(df[[tag_column]]))) {
    return(data.frame())
  }
  
  # Initialize empty data frame for tag columns
  tag_df <- data.frame(row.names = rownames(df))
  
  # Process each row
  for (i in 1:nrow(df)) {
    if (is.na(df[[tag_column]][i]) || df[[tag_column]][i] == "") {
      next
    }
    
    # Split tags by pipe
    tags <- strsplit(df[[tag_column]][i], "\\|")[[1]]
    
    # Process each tag
    for (tag in tags) {
      tag <- trimws(tag)
      
      # Skip empty tags
      if (tag == "") {
        next
      }
      
      # Check if tag has key:value format
      if (grepl(":", tag)) {
        # Split into key and value
        key_value <- strsplit(tag, ":")[[1]]
        key <- trimws(key_value[1])
        value <- trimws(key_value[2])
        
        # Use tag_column as prefix to avoid column name conflicts
        column_name <- paste0(tag_column, "_", key)
        
        # Convert value to appropriate type if possible
        if (tolower(value) %in% c("true", "false")) {
          value <- as.logical(value)
        } else if (grepl("^[0-9]+$", value)) {
          value <- as.integer(value)
        } else if (grepl("^[0-9]*\\.[0-9]+$", value)) {
          value <- as.numeric(value)
        }
        
        # Add or update column
        if (!column_name %in% colnames(tag_df)) {
          tag_df[[column_name]] <- NA
        }
        tag_df[i, column_name] <- value
      } else {
        # Tag is a flag (no value)
        column_name <- paste0(tag_column, "_", tag)
        
        # Add or update column
        if (!column_name %in% colnames(tag_df)) {
          tag_df[[column_name]] <- FALSE
        }
        tag_df[i, column_name] <- TRUE
      }
    }
  }
  
  return(tag_df)
}

# #' Convert columns with "true"/"false" values to logical type
# #'
# #' @param df Data frame to process
# #' @return Data frame with logical columns converted
# #'
convert_logical_columns <- function(df) {
  for (col in colnames(df)) {
    # Check if the column is character type
    if (is.character(df[[col]])) {
      # Remove NA values for checking
      values <- df[[col]][!is.na(df[[col]])]
      
      # Skip empty columns
      if (length(values) == 0) {
        next
      }
      
      # Trim whitespace and convert to lowercase for comparison
      trimmed_lower_values <- tolower(trimws(values))
      
      # Check if all values are "true" or "false" (case insensitive)
      if (all(trimmed_lower_values %in% c("true", "false"))) {
        # Convert to logical, handling case insensitivity and whitespace
        df[[col]] <- tolower(trimws(df[[col]])) == "true"
      }
    }
  }
  return(df)
}

# #' Remove columns containing only NA values
# #'
# #' @param df Data frame to process
# #' @return Data frame with NA-only columns removed
# #'
remove_na_columns <- function(df) {
  # Calculate the number of NA values in each column
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  
  # Keep columns where there's at least one non-NA value
  keep_cols <- names(na_counts[na_counts < nrow(df)])
  
  # Return the filtered data frame
  if (length(keep_cols) > 0) {
    return(df[, keep_cols, drop = FALSE])
  } else {
    return(df)  # Return original if all columns would be removed
  }
}

# #' Remove columns containing only empty strings
# #'
# #' @param df Data frame to process
# #' @return Data frame with empty-string-only columns removed
# #'
remove_empty_columns <- function(df) {
  # Find columns to keep (those with at least one non-empty value)
  keep_cols <- sapply(df, function(x) {
    # Skip non-character columns
    if (!is.character(x)) {
      return(TRUE)
    }
    
    # Count number of empty strings (ignoring NAs)
    empty_count <- sum(x == "", na.rm = TRUE)
    non_na_count <- sum(!is.na(x))
    
    # Keep if at least one non-empty value exists
    return(empty_count < non_na_count)
  })
  
  # Return filtered data frame
  if (any(keep_cols)) {
    return(df[, keep_cols, drop = FALSE])
  } else {
    return(df)  # Return original if all columns would be removed
  }
}

# Function to load and process datapackage.json
process_datapackage_json <- function(datapackage_file) {
  if (!file.exists(datapackage_file)) {
    warning("datapackage.json file not found")
    return(NULL)
  }
  
  # Try built-in R JSON parser first
  tryCatch({
    metadata <- jsonlite::fromJSON(datapackage_file, simplifyVector = TRUE)
    return(metadata)
  }, error = function(e) {
    # Fallback to jsonify if available
    if (requireNamespace("jsonify", quietly = TRUE)) {
      tryCatch({
        metadata <- jsonify::from_json(datapackage_file)
        return(metadata)
      }, error = function(e2) {
        warning("Failed to parse datapackage.json: ", e2$message)
        return(NULL)
      })
    } else {
      warning("Failed to parse datapackage.json: ", e$message)
      return(NULL)
    }
  })
}
