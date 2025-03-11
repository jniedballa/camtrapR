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
  
  if (!is.null(zipfile)) {
    if (!endsWith(zipfile, ".zip")) {
      stop("The provided zipfile does not have a .zip extension.")
    }
    

    
    unzip_dir <- file.path(tempdir(), gsub(":", "_", Sys.time()))
    dir.create(unzip_dir)
    unzip(zipfile, exdir = unzip_dir)
    
    lf <- list.files(unzip_dir, recursive = T)
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
    
    
    images      <- read.csv(file.path(unzip_dir, image_files))
    deployments <- read.csv(file.path(unzip_dir, deployment_files))
    
  } else if (!is.null(directory)) {
    if (!dir.exists(directory)) {
      stop("The specified directory does not exist.")
    }
    
    lf <- list.files(directory, recursive = T)
    
    images      <- read.csv(file.path(directory, lf[grep("^images", basename(lf))]))
    deployments <- read.csv(file.path(directory, lf[grep("^deployments", basename(lf))]))
    
  } else if (!is.null(deployment_file) && !is.null(image_file)) {
    if (!file.exists(deployment_file) || !file.exists(image_file)) {
      stop("One or both of the specified CSV files do not exist.")
    }
    
    deployments <- read.csv(deployment_file)
    images      <- read.csv(image_file)
  }
  
  
  deploymentCol <- "deployment_id"
  stationCol <- "placename"
  cameraCol  <- "camera_id"
  date_formats <- c("ymd!*HMS!z", 
                    "ymd HMS", 
                   "mdy HMS",  
                   "dmy HMS"            
  )
  
  if(any(duplicated(deployments[, deploymentCol]))) stop(paste("Duplicate deployment_id:", paste0(deployments[, deploymentCol][duplicated(deployments[, deploymentCol])], collapse = ", ")))
  
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
  
  
  deployments_start_date_original <- deployments$start_date
  deployments_end_date_original <- deployments$end_date
  
  
  deployments$start_date <- lubridate::parse_date_time(deployments$start_date, 
                                                       orders = date_formats,
                                                       truncated = 1)
  deployments$end_date   <- lubridate::parse_date_time(deployments$end_date, 
                                                       orders = date_formats,
                                                       truncated = 1)
  
  
  
  # NA checking
  if(any(is.na(deployments$start_date))) {
    # Get examples of problematic date strings to show in the error
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
  
  if(any(is.na(deployments$end_date))) {
    # Get examples of problematic date strings to show in the error
    problem_indices <- which(is.na(deployments$end_date))
    example_strings <- head(as.character(deployments$end_date[problem_indices]), 3)
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
  
  
  # Check that all end dates are after start dates
  invalid_deployments <- which(deployments$end_date <= deployments$start_date)
  
  if(length(invalid_deployments) > 0) {
    # Get details of problematic deployments for the error message
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
      # "Camera deployments must have end dates after start dates to calculate operation time correctly.\n\n",
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
  
  deployments1 <- aggregate(deployments[, cols_char], 
                            by = list(deployments[, stationCol]),
                            FUN = function(x) paste0(unique(x), collapse = " / "))
  
  deployments2 <- aggregate(deployments[, cols_not_char], 
                            by = list(deployments[, stationCol]),
                            FUN = mean)
  
  deployments_agg <- cbind(deployments1, deployments2)
  deployments_agg <- deployments_agg[, colnames(deployments)]
  
  # Remove bounding box column
  if("bounding_boxes" %in% colnames(images)) {
    images <- images[, colnames(images) != "bounding_boxes"]
  }
  
  # Add station / camera columns
  images_merged <- merge(images, 
                         deployments[, c(deploymentCol, stationCol, cameraCol)])
  
  # ensure record date/time column is formatted correctly
  record_datetime_format <- c("ymd HMS",    # standard format
                              "mdy HM")     # if user modified content of zip in Excel
  images_merged$timestamp <- as.character(lubridate::parse_date_time(images_merged$timestamp, orders = record_datetime_format))
  
  
  return(list(CTtable = deployments, 
              CTtable_aggregated = deployments_agg,
              recordTable = images_merged))
}