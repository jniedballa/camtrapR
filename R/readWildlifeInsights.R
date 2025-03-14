#' Import Wildlife Insights data to camtrapR
#'
#' @description 
#' This function imports camera trap data from Wildlife Insights into a format compatible with camtrapR.
#' It can read data from a directory containing CSV files, from a ZIP file, or from individual CSV files.
#' 
#' @param directory character. Path to folder containing CSV files exported from Wildlife Insights.
#' @param zipfile character. Path to a ZIP file exported from Wildlife Insights.
#' @param deployment_file character. Path to the deployments CSV file.
#' @param image_file character. Path to the images CSV file.
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{CTtable}{The full camera trap table, based on deployments.csv}
#'   \item{CTtable_aggregated}{An aggregated version of the camera trap table, with one row per station}
#'   \item{recordTable}{The record table, based on images.csv with additional columns from deployments.csv}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Reading from a directory
#' wi_data <- readWildlifeInsights(directory = "path/to/csv/files")
#'
#' # Reading from a ZIP file
#' wi_data <- readWildlifeInsights(zipfile = "path/to/wildlife_insights_export.zip")
#'
#' # Reading from separate CSV files
#' wi_data <- readWildlifeInsights(deployment_file = "path/to/deployments.csv",
#'                                 image_file = "path/to/images.csv")
#' }
readWildlifeInsights <- function(directory = NULL, 
                                 zipfile = NULL, 
                                 deployment_file = NULL, 
                                 image_file = NULL)
{
  # Check consistency of input parameters
  if (sum(!is.null(directory), !is.null(zipfile), 
          (!is.null(deployment_file) & !is.null(image_file))) != 1) {
    stop("Please provide either 'directory', 'zipfile', or both 'deployment_file' and 'image_file'.")
  }
  
  # Define required columns for validation
  required_deployment_cols <- c("deployment_id", "placename", "camera_id", "start_date", "end_date")
  required_image_cols <- c("deployment_id", "timestamp")
  
  # Load data from ZIP file
  if (!is.null(zipfile)) {
    if (!endsWith(zipfile, ".zip")) {
      stop("The provided zipfile does not have a .zip extension.")
    }
    
    if (!file.exists(zipfile)) {
      stop(paste("The specified ZIP file does not exist:", zipfile))
    }
    
    unzip_dir <- file.path(tempdir(), gsub(":", "_", Sys.time()))
    dir.create(unzip_dir)
    
    # Add error handling for unzip operation
    tryCatch({
      unzip(zipfile, exdir = unzip_dir)
    }, error = function(e) {
      stop(paste("Failed to unzip the file:", zipfile, "\nError:", e$message))
    })
    
    lf <- list.files(unzip_dir, recursive = TRUE)
    
    # Check for existence and uniqueness of required files
    image_files <- lf[grep("^images", basename(lf))]
    deployment_files <- lf[grep("^deployments", basename(lf))]
    
    if (length(image_files) == 0) {
      stop("No 'images' file found in the ZIP archive.")
    } else if (length(image_files) > 1) {
      stop("Multiple 'images' files found in the ZIP archive. Expected only one.")
    }
    
    if (length(deployment_files) == 0) {
      stop("No 'deployments' file found in the ZIP archive.")
    } else if (length(deployment_files) > 1) {
      stop("Multiple 'deployments' files found in the ZIP archive. Expected only one.")
    }
    
    # Read files with error handling
    tryCatch({
      images <- read.csv(file.path(unzip_dir, image_files))
    }, error = function(e) {
      stop(paste("Failed to read images file from ZIP archive:", e$message))
    })
    
    tryCatch({
      deployments <- read.csv(file.path(unzip_dir, deployment_files))
    }, error = function(e) {
      stop(paste("Failed to read deployments file from ZIP archive:", e$message))
    })
    
  } else if (!is.null(directory)) {
    if (!dir.exists(directory)) {
      stop("The specified directory does not exist.")
    }
    
    lf <- list.files(directory, recursive = TRUE)
    
    # Check for required files
    image_files <- lf[grep("^images", basename(lf))]
    deployment_files <- lf[grep("^deployments", basename(lf))]
    
    if (length(image_files) == 0) {
      stop("No 'images' file found in the directory.")
    } else if (length(image_files) > 1) {
      stop("Multiple 'images' files found in the directory. Expected only one.")
    }
    
    if (length(deployment_files) == 0) {
      stop("No 'deployments' file found in the directory.")
    } else if (length(deployment_files) > 1) {
      stop("Multiple 'deployments' files found in the directory. Expected only one.")
    }
    
    # Read files with error handling
    tryCatch({
      images <- read.csv(file.path(directory, image_files))
    }, error = function(e) {
      stop(paste("Failed to read images file from directory:", e$message))
    })
    
    tryCatch({
      deployments <- read.csv(file.path(directory, deployment_files))
    }, error = function(e) {
      stop(paste("Failed to read deployments file from directory:", e$message))
    })
    
  } else if (!is.null(deployment_file) && !is.null(image_file)) {
    if (!file.exists(deployment_file)) {
      stop(paste("The specified deployment file does not exist:", deployment_file))
    }
    
    if (!file.exists(image_file)) {
      stop(paste("The specified image file does not exist:", image_file))
    }
    
    # Read files with error handling
    tryCatch({
      deployments <- read.csv(deployment_file)
    }, error = function(e) {
      stop(paste("Failed to read deployments file:", e$message))
    })
    
    tryCatch({
      images <- read.csv(image_file)
    }, error = function(e) {
      stop(paste("Failed to read images file:", e$message))
    })
  }
  
  # Check if data frames are empty
  if (nrow(deployments) == 0) {
    stop("The deployments data is empty. Please check your input files.")
  }
  
  if (nrow(images) == 0) {
    stop("The images data is empty. Please check your input files.")
  }
  
  # Check for required columns in deployments
  missing_deployment_cols <- required_deployment_cols[!required_deployment_cols %in% colnames(deployments)]
  if (length(missing_deployment_cols) > 0) {
    stop(paste("Missing required columns in deployments data:", 
               paste(missing_deployment_cols, collapse = ", "),
               "\nAvailable columns are:", paste(colnames(deployments), collapse = ", ")))
  }
  
  # Check for required columns in images
  missing_image_cols <- required_image_cols[!required_image_cols %in% colnames(images)]
  if (length(missing_image_cols) > 0) {
    stop(paste("Missing required columns in images data:", 
               paste(missing_image_cols, collapse = ", "),
               "\nAvailable columns are:", paste(colnames(images), collapse = ", ")))
  }
  
  # Column references
  deploymentCol <- "deployment_id"
  stationCol <- "placename"
  cameraCol  <- "camera_id"
  date_formats <- c("ymd!*HMS!z", 
                    "ymd HMS", 
                    "mdy HMS",  
                    "dmy HMS"            
  )
  
  # Check for duplicate deployment IDs
  if(any(duplicated(deployments[, deploymentCol]))) {
    dup_deployments <- deployments[, deploymentCol][duplicated(deployments[, deploymentCol])]
    stop(paste("Duplicate deployment_id found:", paste0(dup_deployments, collapse = ", "),
               "\nEach deployment must have a unique identifier."))
  }
  
  # Check for missing values in critical columns
  for (col in c(deploymentCol, stationCol, cameraCol)) {
    if (any(is.na(deployments[, col])) || any(deployments[, col] == "")) {
      warning(paste("Found", sum(is.na(deployments[, col]) | deployments[, col] == ""), 
                    "missing or empty values in column", col, "of deployments data."))
    }
  }
  
  if (any(is.na(images[, deploymentCol])) || any(images[, deploymentCol] == "")) {
    warning(paste("Found", sum(is.na(images[, deploymentCol]) | images[, deploymentCol] == ""), 
                  "missing or empty values in deployment_id column of images data."))
  }
  
  if (any(is.na(images$timestamp)) || any(images$timestamp == "")) {
    warning(paste("Found", sum(is.na(images$timestamp) | images$timestamp == ""), 
                  "missing or empty values in timestamp column of images data."))
  }
  
  # Handle deployments with identical start and end dates
  remove_these <- deployments$start_date == deployments$end_date
  
  if(any(remove_these)) {
    remove_these_deployments_id <- deployments[remove_these, deploymentCol]
    warning(paste(sum(remove_these), "deployments with identical start and end date removed:\n", 
                  paste(remove_these_deployments_id, collapse = "\n")),
            call. = FALSE)
    deployments <- deployments[!remove_these,]
    
    if(any(remove_these_deployments_id %in% images[, deploymentCol])) {
      warning(sum(images[, deploymentCol] %in% remove_these_deployments_id), 
              " images removed because they were from deployments with identical start and end dates.")
      images <- images[!images[, deploymentCol] %in% remove_these_deployments_id,]
    }
  }
  
  # Save original date strings for error reporting
  deployments_start_date_original <- deployments$start_date
  deployments_end_date_original <- deployments$end_date
  
  # Validate that date columns contain string data before parsing
  if (!is.character(deployments$start_date)) {
    stop("The 'start_date' column must contain character (text) data. Check if your CSV file was imported correctly.")
  }
  
  if (!is.character(deployments$end_date)) {
    stop("The 'end_date' column must contain character (text) data. Check if your CSV file was imported correctly.")
  }
  
  # Parse dates
  deployments$start_date <- lubridate::parse_date_time(deployments$start_date, 
                                                       orders = date_formats,
                                                       truncated = 1)
  deployments$end_date   <- lubridate::parse_date_time(deployments$end_date, 
                                                       orders = date_formats,
                                                       truncated = 1)
  
  # Check for NA in start_date
  if(any(is.na(deployments$start_date))) {
    problem_indices <- which(is.na(deployments$start_date))
    example_strings <- head(as.character(deployments_start_date_original[problem_indices]), 3)
    deployment_ids <- head(deployments[problem_indices, deploymentCol], 3)
    
    stop(paste0(
      "Failed to parse ", sum(is.na(deployments$start_date)), " out of ", nrow(deployments), " start date(s) in Wildlife Insights data.\n",
      "Examples of unparseable dates from deployments: ", 
      paste(paste0(deployment_ids, ": '", example_strings, "'"), collapse = ", "), "\n",
      "Current supported formats: ", paste(date_formats, collapse = ", "), ".\n",
      "Check if date format in your data matches one of the supported formats.\n",
      "Date parsing may fail if data was edited in Excel or exported with non-default settings."
    ))
  }
  
  # Check for NA in end_date
  if(any(is.na(deployments$end_date))) {
    problem_indices <- which(is.na(deployments$end_date))
    example_strings <- head(as.character(deployments_end_date_original[problem_indices]), 3)
    deployment_ids <- head(deployments[problem_indices, deploymentCol], 3)
    
    stop(paste0(
      "Failed to parse ", sum(is.na(deployments$end_date)), " out of ", nrow(deployments), " end date(s) in Wildlife Insights data.\n",
      "Examples of unparseable dates from deployments: ", 
      paste(paste0(deployment_ids, ": '", example_strings, "'"), collapse = ", "), "\n",
      "Current supported formats: ", paste(date_formats, collapse = ", "), ".\n",
      "Check if date format in your data matches one of the supported formats.\n",
      "Date parsing may fail if data was edited in Excel or exported with non-default settings."
    ))
  }
  
  # Check that end dates are after start dates
  invalid_deployments <- which(deployments$end_date <= deployments$start_date)
  
  if(length(invalid_deployments) > 0) {
    problem_deployments <- deployments[invalid_deployments, ]
    examples <- head(problem_deployments, 3)
    
    # Format dates for cleaner display
    formatted_examples <- paste0(
      examples[, deploymentCol], ": ",
      "start_date = ", as.character(examples$start_date), ", ",
      "end_date = ", as.character(examples$end_date)
    )
    
    stop(paste0(
      "Found ", length(invalid_deployments), " deployment(s) where end date is not after start date.\n\n",
      "Examples of invalid deployments:\n",
      paste(formatted_examples, collapse = "\n"), 
      if(length(invalid_deployments) > 3) paste0("\n(and ", length(invalid_deployments) - 3, " more)") else "",
      "\n\nPlease check your Wildlife Insights data for date inconsistencies (especially in date/time format)."
    ))
  }
  
  # Aggregate table (by placename)
  col_types <- sapply(deployments, typeof)
  cols_char <- which(col_types %in% "character")
  cols_not_char <- which(!col_types %in% "character")
  
  # Handle character columns
  if (length(cols_char) > 0) {
    deployments1 <- aggregate(deployments[, cols_char, drop = FALSE], 
                              by = list(deployments[, stationCol]),
                              FUN = function(x) paste0(unique(x), collapse = " / "))
  } else {
    deployments1 <- data.frame(Group.1 = unique(deployments[, stationCol]))
  }
  
  # Handle non-character columns
  if (length(cols_not_char) > 0) {
    deployments2 <- aggregate(deployments[, cols_not_char, drop = FALSE], 
                              by = list(deployments[, stationCol]),
                              FUN = mean)
  } else {
    deployments2 <- data.frame(Group.1 = unique(deployments[, stationCol]))
  }
  
  # Combine aggregated data with error handling
  if (ncol(deployments1) > 1 && ncol(deployments2) > 1) {
    deployments_agg <- cbind(deployments1[, -1, drop = FALSE], deployments2[, -1, drop = FALSE])
    deployments_agg <- cbind(deployments1[, 1, drop = FALSE], deployments_agg)
    colnames(deployments_agg)[1] <- stationCol
  } else if (ncol(deployments1) > 1) {
    deployments_agg <- deployments1
    colnames(deployments_agg)[1] <- stationCol
  } else if (ncol(deployments2) > 1) {
    deployments_agg <- deployments2
    colnames(deployments_agg)[1] <- stationCol
  } else {
    # Fallback for aggregation
    stop("Aggregation failed")
    # deployments_agg <- data.frame(deployments[, c(stationCol, deploymentCol, cameraCol)])
    # deployments_agg <- deployments_agg[!duplicated(deployments_agg[, stationCol]), ]
  }
  
  # Remove bounding box column if present
  if("bounding_boxes" %in% colnames(images)) {
    images <- images[, colnames(images) != "bounding_boxes"]
  }
  
  # Check if all deployment_ids in images exist in deployments
  unknown_deployments <- unique(images[, deploymentCol][!images[, deploymentCol] %in% deployments[, deploymentCol]])
  if (length(unknown_deployments) > 0) {
    warning(paste0(
      "Found ", length(unknown_deployments), " deployment_id(s) in images that don't exist in deployments table.\n",
      "Example unknown deployment_ids: ", paste(head(unknown_deployments, 3), collapse = ", "),
      if(length(unknown_deployments) > 3) paste0(" (and ", length(unknown_deployments) - 3, " more)"),
      "\nThese images will be excluded from the merged output."
    ))
  }
  
  # Merge images with deployment data
  images_merged <- merge(images, 
                         deployments[, c(deploymentCol, stationCol, cameraCol)])
  
  # Check if merge reduced the number of records
  if (nrow(images_merged) < nrow(images)) {
    warning(paste0(
      "Merging reduced the number of image records from ", nrow(images), " to ", nrow(images_merged), " (",
      round((1 - nrow(images_merged) / nrow(images)) * 100, 1), "% reduction).\n",
      "This happens because some deployment_ids in the images table don't match any in the deployments table."
    ))
  }
  
  # Ensure record date/time column is formatted correctly
  record_datetime_format <- c("ymd HMS",    # standard format
                              "mdy HM")     # if user modified content of zip in Excel
  
  # Validate timestamp column
  if (!is.character(images_merged$timestamp)) {
    stop("The 'timestamp' column in images must contain character (text) data. Check if your CSV file is formatted correctly.")
  }
  
  # Parse timestamps with error handling
  timestamps_parsed <- lubridate::parse_date_time(images_merged$timestamp, orders = record_datetime_format)
  
  if(any(is.na(timestamps_parsed))) {
    problem_indices <- which(is.na(timestamps_parsed))
    example_strings <- head(images_merged$timestamp[problem_indices], 3)
    
    warning(paste0(
      "Failed to parse ", sum(is.na(timestamps_parsed)), " out of ", nrow(images_merged), " timestamps in images data.\n",
      "Examples of unparseable timestamps: ", paste(paste0("'", example_strings, "'"), collapse = ", "), ".\n",
      "Supported formats are: ", paste(record_datetime_format, collapse = ", "), ".\n",
      "These records will have NA timestamps in the output."
    ))
  }
  
  images_merged$timestamp <- as.character(timestamps_parsed)
  
  # Final checks for empty output tables
  if (nrow(deployments) == 0) {
    warning("The final deployments table is empty after processing.")
  }
  
  if (nrow(deployments_agg) == 0) {
    warning("The aggregated deployments table is empty after processing.")
  }
  
  if (nrow(images_merged) == 0) {
    warning("The merged images table is empty after processing. Check for data compatibility issues.")
  }
  
  return(list(CTtable = deployments, 
              CTtable_aggregated = deployments_agg,
              recordTable = images_merged))
}
