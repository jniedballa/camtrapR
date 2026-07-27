#' Simulate Camera Trapping Data
#'
#' @description 
#' Generates simulated camera trap data and a corresponding species record table. 
#' The output mimics the structure required by \code{camtrapR}, making it useful
#' for creating reproducible examples, testing pipelines, and demonstrating 
#' package functionality.
#'
#' @details 
#' The function simulates realistic deployment metadata and species detections. 
#' Species abundances are generated using a skewed, Zipf-like (harmonic) distribution, 
#' ensuring a realistic mix of common and rare species. Detection times are drawn uniformly 
#' across the active deployment periods. If \code{probProblem > 0}, a subset of 
#' cameras will simulate a single malfunction period (\code{Problem1_from} and \code{Problem1_to}).
#' Cameras are set up and retrieved in the daytime.
#' 
#' \emph{Note:} The initial draft of this function was generated with the assistance 
#' of an AI language model and subsequently modified.
#'
#' @param nStations Integer. The number of camera trap stations to simulate. Default is 10.
#' @param camerasPerStation Integer (1 or 2). Number of cameras per station. If 2, 
#'   a \code{Camera} column is generated to distinguish between devices at the same station. Default is 1.
#' @param duration_days Numeric. The average deployment duration in days. Actual durations 
#'   will have slight random jitter. Default is 40.
#' @param nSeasons Integer. The number of seasons to simulate. If > 1, deployments 
#'   are staggered by 180 days. Default is 1.
#' @param nSpecies Integer. The total number of distinct species to simulate. Default is 8.
#' @param nRecords Integer. The total number of species records (images/events) 
#'   to distribute across the active deployments. Default is 200.
#' @param startDate Character. The base start date for the first season's deployments, 
#'   in "YYYY-MM-DD" format. Default is "2023-01-01".
#' @param probProblem Numeric (0 to 1). The probability that a camera experiences 
#'   a malfunction, triggering \code{Problem1_from} and \code{Problem1_to} dates. Default is 0 (no problems).
#' @param bbox Numeric vector of length 4. Bounding box \code{c(xmin, xmax, ymin, ymax)} 
#'   used to generate random deployment coordinates in decimal degrees.
#' @param covariates List. Specifies the covariates to simulate. Must contain \code{continuous} 
#'   (a named list of \code{c(mean, sd)}) and/or \code{categorical} (a named list specifying 
#'   the number of factor levels). 
#' @param dateFormat Character. A \code{lubridate}-style format string specifying how 
#'   \code{Setup_date} and \code{Retrieval_date} should be formatted (e.g., \code{"ymd"}, 
#'   \code{"dmy HMS"}, \code{"mdy"}).
#' @param seed Numeric. A seed for the random number generator to ensure reproducible output.


#'
#' @return A list containing two \code{data.frame}s:
#' \describe{
#'   \item{\code{camtraps}}{Deployment metadata including Station, Camera (optional), Season (optional), 
#'   longitude, latitude, Setup/Retrieval dates, Problem dates, and covariates.}
#'   \item{\code{recordTable}}{Simulated species record table including Station, Camera (optional), 
#'   Season (optional), Species, and strictly formatted DateTimeOriginal, Date, and Time columns.}
#' }
#' 
#' @author Juergen Niedballa
#' 
#' @importFrom lubridate as_datetime as_date days ddays dseconds hour interval stamp
#' @importFrom stats runif
#' 
#' @export
#'
#' @examples
#' # Simulate basic data (10 stations, 1 camera each, 1 season)
#' 
#' camera_data_simple <- simulateCamtrapData()
#' 
#' camop_simple <- cameraOperation(camera_data_simple$camtraps,
#'                                 setupCol = "Setup_date",
#'                                 retrievalCol = "Retrieval_date")
#' 
#' plot(camop_simple)
#' 
#' 
#' 
#' head(camera_data_simple$camtraps)
#' head(camera_data_simple$recordTable)
#' 
#' 
#' 
#' # Simulate multi-season, multi-camera data with custom covariates
#' # and camera malfuncion (problem columns)
#' camera_data_complex <- simulateCamtrapData(
#'   nStations = 10,
#'   camerasPerStation = 2,
#'   nSeasons = 2,
#'   dateFormat = "ymd HMS",
#'   covariates = list(continuous = list(elevation = c(500, 50)), 
#'                     categorical = list(treatment = 2)),
#'   probProblem = 0.5
#' )
#' 
#' 
#' 
#' camop_complex <- cameraOperation(camera_data_complex$camtraps,
#'                                  setupCol = "Setup_date",
#'                                  retrievalCol = "Retrieval_date",
#'                                  sessionCol = "Season",
#'                                  cameraCol = "Camera",
#'                                  hasProblems = TRUE,
#'                                  byCamera = FALSE,
#'                                  allCamsOn = FALSE,
#'                                  camerasIndependent = FALSE,   # this is made up, not simulated
#'                                  dateFormat = "ymd HMS")
#'                                  
#' summary(camop_complex)
#' 
#' plot(camop_complex)

simulateCamtrapData <- function(nStations = 10,
                                camerasPerStation = 1,
                                duration_days = 40,
                                nSeasons = 1,
                                nSpecies = 8,
                                nRecords = 300,
                                startDate = "2023-01-01",
                                probProblem = 0,
                                bbox = c(xmin = 117.25, xmax = 117.5, ymin =5.3, ymax = 5.5),
                                covariates = list(continuous = list(elev = c(1000, 200)), 
                                                  categorical = list(habitat = 3)),
                                dateFormat = "ymd",       # e.g., "ymd HMS", "dmy", "ymd_hms"
                                seed = NULL
                                
) {
  
  # --------------------------------------------------------- #
  # 0. HELPER: Translate Lubridate string to base R format  ----
  # --------------------------------------------------------- #
  translate_format <- function(fmt) {
    f <- tolower(fmt)
    f <- gsub("ymd", "%Y-%m-%d", f)
    f <- gsub("dmy", "%d-%m-%Y", f)
    f <- gsub("mdy", "%m-%d-%Y", f)
    f <- gsub("hms", "%H:%M:%S", f)
    f <- gsub("hm", "%H:%M", f)
    f <- gsub("_", " ", f) # handle "ymd_hms" elegantly
    return(f)
  }
  
  stopifnot(camerasPerStation %in% c(1,2))
  
  fmt_full <- translate_format(dateFormat)
  # Extract just the date part for the 'Problem' columns
  fmt_date <- trimws(gsub("%H:%M:%S|%H:%M", "", fmt_full))
  
  # --------------------------------------------------------- #
  # 1. SETUP CAMTRAP (STATION) METADATA   ----
  # --------------------------------------------------------- #
  stations <- sprintf("Station%03d", 1:nStations)
  
  if (camerasPerStation == 2) {
    cameras <- c("CamA", "CamB")
    camtraps <- expand.grid(Station = stations, Camera = cameras, Season = 1:nSeasons, stringsAsFactors = FALSE)
  } else {
    camtraps <- expand.grid(Station = stations, Season = 1:nSeasons, stringsAsFactors = FALSE)
  }
  
  n_deployments <- nrow(camtraps)
  
  
  # set seed for RNG
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed))
    set.seed(seed)
  }
  
  
  # Coordinates in Decimal Degrees (Lat/Long) from Bounding Box
  camtraps$longitude <- round(runif(n_deployments, min = bbox["xmin"], max = bbox["xmax"]), 5)
  camtraps$latitude  <- round(runif(n_deployments, min = bbox["ymin"], max = bbox["ymax"]), 5)
  # Simulate Dates using lubridate period/duration math
  base_time <- lubridate::as_datetime(startDate, tz = "UTC")
  
  # Generate base dates per Station-Season so cameras at the same station are linked
  n_stationSeasons <- nStations * nSeasons
  events <- expand.grid(Station = stations, Season = 1:nSeasons, stringsAsFactors = FALSE)
  
  events$base_setup <- base_time + 
    lubridate::days((events$Season - 1) * 180) + 
    lubridate::ddays(runif(n_stationSeasons, -3, 3))
  
  events$base_retrieval <- events$base_setup + 
    lubridate::days(duration_days) + 
    lubridate::ddays(runif(n_stationSeasons, -2, 2))
  
  # ensure setup / retrieval in daytime hours (assign new hours)
  lubridate::hour(events$base_setup)     <- round(runif(length(events$base_setup), 7, 18))
  lubridate::hour(events$base_retrieval) <- round(runif(length(events$base_retrieval), 7, 18))
  
  # Map the shared dates back to the main camtraps dataframe
  match_idx <- match(paste(camtraps$Station, camtraps$Season), 
                     paste(events$Station, events$Season))
  
  setup_posix <- events$base_setup[match_idx]
  retrieval_posix <- events$base_retrieval[match_idx]
  
  # Add a realistic walking/setup delay (2 to 15 mins) for the second camera
  if (camerasPerStation == 2) {
    is_camB <- camtraps$Camera == "CamB"
    setup_posix[is_camB] <- setup_posix[is_camB] + lubridate::dseconds(runif(sum(is_camB), 120, 900))
    retrieval_posix[is_camB] <- retrieval_posix[is_camB] + lubridate::dseconds(runif(sum(is_camB), 120, 900))
  }

  # Format using standard base R format()
  camtraps$Setup_date <- format(setup_posix, format = fmt_full)
  camtraps$Retrieval_date <- format(retrieval_posix, format = fmt_full)
  
  # Simulate Problem periods
  if (probProblem > 0) {
    has_problem <-  runif(n_deployments) < probProblem 
    
    prob_start_posix <- setup_posix + lubridate::ddays(runif(n_deployments, min = 2, max = duration_days - 10))
    prob_end_posix <- prob_start_posix + lubridate::ddays(runif(n_deployments, 1, 10))
    
    
    # Coerce to Dates, then format string (setup/ retrieval / problems must have same format, so don't coerce to date)
    # camtraps$Problem1_from <- ifelse(has_problem, format(lubridate::as_date(prob_start_posix), format = fmt_date), "")
    # camtraps$Problem1_to   <- ifelse(has_problem, format(lubridate::as_date(prob_end_posix), format = fmt_date), "")
    
    camtraps$Problem1_from <- ifelse(has_problem, format(lubridate::as_date(prob_start_posix), format = fmt_full), "")
    camtraps$Problem1_to   <- ifelse(has_problem, format(lubridate::as_date(prob_end_posix), format = fmt_full), "")
  }

    # Add Covariates
  if (!is.null(covariates$continuous)) {
    for (cov in names(covariates$continuous)) {
      params <- covariates$continuous[[cov]] 
      camtraps[[cov]] <- round(rnorm(nStations, params[1], params[2]), 1) [as.factor(camtraps$Station)]
    }
  }
  
  if (!is.null(covariates$categorical)) {
    for (cov in names(covariates$categorical)) {
      n_levels <- covariates$categorical[[cov]]
      camtraps[[cov]] <- sample(paste0(cov, "_", 1:n_levels), nStations, replace = TRUE)[as.numeric(as.factor(camtraps$Station))]
    }
  }
  
  # sort output
  if(camerasPerStation == 2)  {
    camtraps <- camtraps[order(camtraps$Station, camtraps$Season, camtraps$Camera),]
  } else {
    camtraps <- camtraps[order(camtraps$Station, camtraps$Season),]  
  }
  
  if (nSeasons == 1) camtraps$Season <- NULL
  
  
  
  # --------------------------------------------------------- #
  # 2. SETUP RECORD TABLE  ----
  # --------------------------------------------------------- #
  species_pool <- sprintf("Sp_%02d", 1:nSpecies)
  species_probs <- 1 / (1:nSpecies)
  
  record_deps <- sample(1:n_deployments, nRecords, replace = TRUE)
  record_species <- sample(species_pool, nRecords, replace = TRUE, prob = species_probs)
  
  # Use lubridate interval to determine active seconds
  active_intervals <- lubridate::interval(setup_posix[record_deps], retrieval_posix[record_deps])
  active_duration_secs <- as.numeric(active_intervals, "seconds")
  
  record_times <- setup_posix[record_deps] + lubridate::dseconds(runif(nRecords) * active_duration_secs)
  
  recordTable <- data.frame(
    Station = camtraps$Station[record_deps],
    stringsAsFactors = FALSE
  )
  
  if (camerasPerStation == 2) {
    recordTable$Camera <- camtraps$Camera[record_deps]
  }
  if (nSeasons > 1) {
    recordTable$Season <- camtraps$Season[record_deps]
  }
  
  recordTable$Species <- record_species
  
  # Record dates strictly standardized to ISO8601 for downstream software reliability
  recordTable$DateTimeOriginal <- format(record_times, "%Y-%m-%d %H:%M:%S")
  recordTable$Date <- as.character(lubridate::as_date(record_times))
  recordTable$Time <- format(record_times, "%H:%M:%S")
  
  recordTable <- recordTable[order(recordTable$Station, recordTable$DateTimeOriginal), ]
  rownames(recordTable) <- NULL
  
  # declare specific classes and store attributes
  as_cams <- as_cams(camtraps, 
                     stationCol = "Station")
  
  recordTable <- as_records(recordTable, 
                            stationCol = "Station",
                            speciesCol = "Species")

  return(list(camtraps = camtraps, 
              recordTable = recordTable))
}


