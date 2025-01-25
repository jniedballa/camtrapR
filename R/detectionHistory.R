#' Species detection histories for occupancy analyses
#' 
#' This function generates species detection histories that can be used in
#' single-species occupancy analyses with packages \link[unmarked:unmarked-package]{unmarked} 
#' and \pkg{ubms}, as well as multi-species/community occupancy models via 
#' \code{\link{communityModel}}. It generates detection histories in different formats, 
#' with adjustable occasion length and occasion start time.
#'  
#' 
#' The function creates species detection matrices in two possible formats:
#' detection-by-date or detection-by-occasion. The start of detection histories 
#' is controlled by \code{day1}:
#' \itemize{
#'   \item \code{"station"}: Each station's history begins on its setup day
#'   \item \code{"survey"}: All stations begin on the first day of the survey
#'   \item A specific date (e.g., \code{"2015-12-31"}): All stations begin on this date
#' }
#' Dates must be in "YYYY-MM-DD" format if specified directly.
#' 
#' 
#' Two output formats are available via the \code{output} parameter:
#' \itemize{
#'   \item \code{"binary"}: Records detection (1) or non-detection (0)
#'   \item \code{"count"}: Records the number of detections per occasion
#' }
#' 
#' 
#' The \code{includeEffort} parameter determines how camera operation affects the output:
#' \itemize{
#'   \item If \code{FALSE}: Periods when cameras were not operational or only 
#'   partly operational appear as NA in the detection history. This may lose 
#'   species record from incomplete occasions.
#'   \item If \code{TRUE}: Incomplete occasions are retained. Outputs contain a 
#'   separate effort matrix that can be used as an observation covariate in occupancy models.
#' }
#' It is generally advisable to include effort as a covariate to account for 
#' uneven sampling effort.
#' 
#' 
#' \code{occasionLength} controls how many days are aggregated into each sampling occasion.
#' Note that \code{occasionStartTime} has moved to \code{\link{cameraOperation}} to ensure
#' proper calculation of daily effort.
#' 
#' The values of \code{stationCol} in \code{recordTable} must be matched by the
#' row names of \code{camOp} (case-insensitive), otherwise an error is raised.
#' 
#' For date/time formatting, \code{recordDateTimeFormat} accepts two syntax styles:
#' \itemize{
#'   \item Base R style (using \%): e.g., "\%Y-\%m-\%d \%H:\%M:\%S"
#'   \item lubridate style: e.g., "ymd HMS"
#' }
#' \pkg{lubridate} will be used if there are no "\%" characters in
#' \code{recordDateTimeFormat}. The default and recommended format is 
#' "YYYY-MM-DD HH:MM:SS" (e.g., "2014-09-30 22:59:59").
#' 
#' 
#' For multi-season studies where \code{sessionCol} was used in \code{\link{cameraOperation}},
#' the function automatically detects this structure. Set \code{unmarkedMultFrameInput = TRUE}
#' to format output for \code{unmarkedMultFrame}, with rows representing sites and columns
#' ordered by season-major, occasion-minor (e.g., season1-occasion1, season1-occasion2, etc.).
#' 
#' 
#' @param recordTable data.frame. the record table created by
#' \code{\link{recordTable}}
#' @param species character. species name(s) for which to compute detection histories. Can 
#' be either a single species name (for use with \pkg{unmarked}/\pkg{ubms}) or a vector 
#' of multiple species names (for input to \code{\link{communityModel}})
#' @param camOp The camera operation matrix as created by
#' \code{\link{cameraOperation}}
#' @param output character. Return binary detections ("binary") or counts of
#' detections ("count")
#' @param stationCol character. name of the column specifying Station ID in
#' \code{recordTable}
#' @param speciesCol character. name of the column specifying species in
#' \code{recordTable}
#' @param recordDateTimeCol character. name of the column specifying date and
#' time in \code{recordTable}
#' @param recordDateTimeFormat character. Format of column
#' \code{recordDateTimeCol} in \code{recordTable}
#' @param occasionLength integer. occasion length in days
#' @param minActiveDaysPerOccasion integer. minimum number of active trap days
#' for occasions to be included (optional)
#' @param maxNumberDays integer. maximum number of trap days per station
#' (optional)
#' @param day1 character. When should occasions begin: station setup date
#' ("station"), first day of survey ("survey"), a specific date (e.g.
#' "2015-12-31")?
#' @param buffer integer. Makes the first occasion begin a number of days after
#' station setup. (optional)
#' @param includeEffort logical. Compute trapping effort (number of active
#' camera trap days per station and occasion)?
#' @param scaleEffort logical. scale and center effort matrix to mean = 0 and
#' sd = 1?
#' @param occasionStartTime (DEPRECATED) integer. time of day (the full hour)
#' at which to begin occasions. Please use argument \code{occasionStartTime} in
#' \code{\link{cameraOperation}} instead.
#' @param datesAsOccasionNames If \code{day1 = "survey"}, occasion names in the
#' detection history will be composed of first and last day of that occasion.
#' @param timeZone character. Must be a value returned by
#' \code{\link[base:timezones]{OlsonNames}}
#' @param writecsv logical. Should the detection history be saved as a .csv?
#' @param outDir character. Directory into which detection history .csv file is
#' saved
#' @param unmarkedMultFrameInput logical. Return input for multi-season
#' occupancy models in unmarked (argument "y" in
#' \code{\link[unmarked]{unmarkedMultFrame}}?
#' 
#' @return If a single species is provided (typical for \pkg{unmarked}/\pkg{ubms} analyses), 
#' returns a list with either 1, 2 or 3 elements depending on the value of \code{includeEffort} 
#' and \code{scaleEffort}:
#' \item{detection_history}{A species detection matrix}
#' \item{effort}{A matrix giving the number of active camera trap days per station and 
#' occasion (= camera trapping effort). Only returned if \code{includeEffort = TRUE}}
#' \item{effort_scaling_parameters}{Scaling parameters of the effort matrix. Only 
#' returned if \code{includeEffort} and \code{scaleEffort} are \code{TRUE}}
#'
#' If multiple species are provided (for use with \code{\link{communityModel}}), returns 
#' a similar list structure but with \code{detection_history} containing a named list of 
#' detection matrices, one for each species. The effort matrix is identical for all 
#' species and thus returned only once.
#' 
#' 
#' @section Warning: Setting \code{output = "count"} returns a count of
#' detections, not individuals. These counts are not suitable for abundance modeling
#' (e.g., N-mixture models) as they do not represent individual animals.
#' 
#' For important information about the \code{timeZone} parameter, please refer to
#' the "Data Extraction" vignette (\code{vignette("DataExtraction")} or online at
#' \url{https://cran.r-project.org/package=camtrapR/vignettes/camtrapr3.pdf}).
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' 
#' # define image directory
#' wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
#' 
#' # load station information
#' data(camtraps)
#' 
#' # create camera operation matrix
#' camop_no_problem <- cameraOperation(CTtable      = camtraps,
#'                                     stationCol   = "Station",
#'                                     setupCol     = "Setup_date",
#'                                     retrievalCol = "Retrieval_date",
#'                                     hasProblems  = FALSE,
#'                                     dateFormat   = "dmy"
#' )
#' 
#' \dontrun{
#' if (Sys.which("exiftool") != ""){        # only run this function if ExifTool is available
#' recordTableSample <- recordTable(inDir               = wd_images_ID,
#'                                  IDfrom              = "directory",
#'                                  minDeltaTime        = 60,
#'                                  deltaTimeComparedTo = "lastRecord",
#'                                  exclude             = "UNID",
#'                                  timeZone            = "Asia/Kuala_Lumpur"
#' )
#' }
#' }
#' data(recordTableSample)    # load the record history, as created above
#' 
#' 
#' # compute detection history for a species
#' 
#' # without trapping effort
#' DetHist1 <- detectionHistory(recordTable         = recordTableSample,
#'                             camOp                = camop_no_problem,
#'                             stationCol           = "Station",
#'                             speciesCol           = "Species",
#'                             recordDateTimeCol    = "DateTimeOriginal",
#'                             species              = "VTA",
#'                             occasionLength       = 7,
#'                             day1                 = "station",
#'                             datesAsOccasionNames = FALSE,
#'                             includeEffort        = FALSE,
#'                             timeZone             = "Asia/Kuala_Lumpur"
#' )
#' 
#' DetHist1                     # this is a list with 1 element
#' DetHist1$detection_history   # this is the contained detection/non-detection matrix
#' 
#' 
#' # with effort / using base R to define recordDateTimeFormat
#' DetHist2 <- detectionHistory(recordTable          = recordTableSample,
#'                              camOp                = camop_no_problem,
#'                              stationCol           = "Station",
#'                              speciesCol           = "Species",
#'                              recordDateTimeCol    = "DateTimeOriginal",
#'                              species              = "VTA",
#'                              occasionLength       = 7,
#'                              day1                 = "station",
#'                              datesAsOccasionNames = FALSE,
#'                              includeEffort        = TRUE,
#'                              scaleEffort          = FALSE,
#'                              timeZone             = "Asia/Kuala_Lumpur"
#' )
#' 
#' DetHist2$detection_history  # detection history  (alternatively, use: DetHist2[[1]])
#' DetHist2$effort             # effort (alternatively, use: DetHist2[[2]])
#' 
#' # with effort / using lubridate package to define recordDateTimeFormat
#' DetHist2_lub <- detectionHistory(recordTable          = recordTableSample,
#'                                  camOp                = camop_no_problem,
#'                                  stationCol           = "Station",
#'                                  speciesCol           = "Species",
#'                                  recordDateTimeCol    = "DateTimeOriginal",
#'                                  recordDateTimeFormat = "ymd HMS",
#'                                  species              = "VTA",
#'                                  occasionLength       = 7,
#'                                  day1                 = "station",
#'                                  datesAsOccasionNames = FALSE,
#'                                  includeEffort        = TRUE,
#'                                  scaleEffort          = FALSE,
#'                                  timeZone             = "Asia/Kuala_Lumpur"
#' )    
#' 
#' DetHist2_lub$detection_history  # detection history  (alternatively, use: DetHist2_lub[[1]])
#' DetHist2_lub$effort             # effort (alternatively, use: DetHist2_lub[[2]])
#' 
#' 
#' # multi-season detection history
#' 
#' # load multi-season data
#' data(camtrapsMultiSeason)
#' data(recordTableSampleMultiSeason)
#' 
#' # multi-season camera operation matrix
#' camop_season <- cameraOperation(CTtable          = camtrapsMultiSeason,
#'                                     stationCol   = "Station",
#'                                     setupCol     = "Setup_date",
#'                                     sessionCol   = "session",
#'                                     retrievalCol = "Retrieval_date",
#'                                     hasProblems  = TRUE,
#'                                     dateFormat   = "dmy"
#' )
#' 
#' # multi-season detection history
#' DetHist_multi_season <- detectionHistory(recordTable      = recordTableSampleMultiSeason,
#'                             camOp                  = camop_season,
#'                             stationCol             = "Station",
#'                             speciesCol             = "Species",
#'                             species                = "VTA",
#'                             occasionLength         = 10,
#'                             day1                   = "station",
#'                             recordDateTimeCol      = "DateTimeOriginal",
#'                             includeEffort          = TRUE,
#'                             scaleEffort            = FALSE,
#'                             timeZone               = "UTC",
#'                             unmarkedMultFrameInput = TRUE
#' )
#' 
#' DetHist_multi_season
#' 
#' 
#' # Multi-species example for community occupancy analysis with communityModel()
#' DetHist_multi_species <- detectionHistory(recordTable = recordTableSample,
#'                                  species = c("VTA", "PBE", "EGY"),   # multiple species
#'                                  camOp = camop_no_problem,
#'                                  stationCol = "Station",
#'                                  speciesCol = "Species",
#'                                  recordDateTimeCol = "DateTimeOriginal",
#'                                  occasionLength = 7,
#'                                  day1 = "station",
#'                                  includeEffort = TRUE,
#'                                  scaleEffort = FALSE,
#'                                  timeZone = "Asia/Kuala_Lumpur"
#' )
#' 
#' 
#' # bundle input data for communityModel
#' data_list <- list(ylist = DetHist_multi_species$detection_history,
#'                   siteCovs = camtraps,
#'                   obsCovs = list(effort = DetHist_multi_species$effort))
#' 
#' 
#' \dontrun{
#' 
#' # create community model
#' mod.jags <- communityModel(data_list,
#'                            ...) # model specification 
#' }
#' 
#' @export detectionHistory
#' 
detectionHistory <- function(recordTable,
                             species,
                             camOp,
                             output = c("binary", "count"),
                             stationCol = "Station",
                             speciesCol = "Species",
                             recordDateTimeCol = "DateTimeOriginal",
                             recordDateTimeFormat = "ymd HMS",
                             occasionLength,
                             minActiveDaysPerOccasion,
                             maxNumberDays,
                             day1 = "survey",
                             buffer,
                             includeEffort = TRUE,
                             scaleEffort = FALSE,
                             occasionStartTime = "deprecated",
                             datesAsOccasionNames = FALSE,
                             timeZone,
                             writecsv = FALSE,
                             outDir,
                             unmarkedMultFrameInput)
{
  wd0 <- getwd()
  on.exit(setwd(wd0))
  
  
  # Check if species is vector
  is_multispecies <- length(species) > 1
  
  
  # single-species case ----
  if(!is_multispecies) { 
    
    
  #################
  # check input
  
  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, speciesCol = speciesCol, recordDateTimeCol = recordDateTimeCol)
  
  recordTable <- dataFrameTibbleCheck(df = recordTable)

  if(!stationCol %in% colnames(recordTable))  stop(paste('stationCol = "', stationCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))  stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable))  stop(paste('recordDateTimeCol = "', recordDateTimeCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  output <- match.arg (output)
  
  if(!hasArg(species)) stop("'species' is not defined", call. = FALSE)
  if(!is.character(species))  stop("species must be of class character", call. = FALSE)
  # if(!length(species) == 1)   stop("species may only contain one value", call. = FALSE)
  
  if(!hasArg(occasionLength)) stop("'occasionLength' is not defined", call. = FALSE)
  
  if(!hasArg(recordTable))         stop("'recordTable' is not defined", call. = FALSE)
  if(!is.data.frame(recordTable))  stop("'recordTable' must be a data.frame", call. = FALSE)
  if(!hasArg(camOp))               stop("'camOp' is not defined", call. = FALSE)
  if(!inherits(camOp, "matrix"))   stop ("camOp must be a matrix", call. = FALSE)
  
  if(!length(stationCol) == 1)     stop("stationCol may only contain one value", call. = FALSE)
  recordTable[,stationCol] <- as.character(recordTable[,stationCol])
  if(!is.character(stationCol))    stop("stationCol must be a character", call. = FALSE)
  
  if(!length(speciesCol) == 1)    stop("speciesCol may only contain one value", call. = FALSE)
  recordTable[,speciesCol] <- as.character(recordTable[,speciesCol])
  if(!is.character(speciesCol))   stop("speciesCol must be a character", call. = FALSE)
  
  if(!length(recordDateTimeCol) == 1)  stop("recordDateTimeCol may only contain one value", call. = FALSE)
  recordTable[,recordDateTimeCol] <- as.character(recordTable[,recordDateTimeCol])   # make character to get rid of attributes. Will later assign time zone again
  if(!is.character(recordDateTimeCol)) stop("recordDateTimeCol must be a character", call. = FALSE)
  
  
  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }
  if(!is.logical(writecsv)) stop("writecsv must be logical (TRUE / FALSE)", call. = FALSE)
  
  if(hasArg(occasionStartTime)) warning("'occasionStartTime' is deprecated and will be removed in the next release. It is now found in cameraOperation()")
  # if(length(occasionStartTime) != 1) stop("occasionStartTime must have length 1")
  # occasionStartTime <- as.integer(round(occasionStartTime))
  # if(occasionStartTime != 0 & !is.integer(occasionStartTime)) {stop ("occasionStartTime must be between 0 and 23", call. = FALSE)}
  # if(occasionStartTime < 0 | occasionStartTime >= 24){         stop ("occasionStartTime must be between 0 and 23", call. = FALSE)}
  
  occasionLength <- as.integer(round(occasionLength))
  if(length(occasionLength) != 1)  stop("occasionLength may only contain one value", call. = FALSE)
  if(!is.numeric(occasionLength))  stop("occasionLength must be a positive integer", call. = FALSE)
  if(occasionLength <= 0)          stop("occasionLength must be a positive integer and not 0", call. = FALSE)
  if(occasionLength > ncol(camOp)) stop("occasionLength must be smaller than the total number of days in camOp", call. = FALSE)
  
  
  if(hasArg(maxNumberDays)){
    maxNumberDays <- as.integer(maxNumberDays)
    if(maxNumberDays > ncol(camOp))    stop("maxNumberDays is larger than the number of columns of camOp", call. = FALSE)
    if(maxNumberDays < occasionLength) stop("maxNumberDays must be larger than or equal to occasionLength", call. = FALSE)
  }
  
  if(hasArg(buffer)) {
    if(!is.numeric(buffer)) stop("buffer must be number", call. = FALSE)
    buffer <- round(buffer)
    if(!buffer >= 1)        stop("if buffer is defined, it must be 1 or higher", call. = FALSE)
  }
  
  
  if(!speciesCol %in% colnames(recordTable))        stop(paste("speciesCol", speciesCol, "is not a column name in recordTable"), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste("recordDateTimeCol", recordDateTimeCol, "is not a column name in recordTable"), call. = FALSE)
  if(!stationCol %in% colnames(recordTable))        stop(paste("stationCol", stationCol, "is not a column name in recordTable"), call. = FALSE)
  
  
  if(!species %in% recordTable[,speciesCol]) stop("species ", species, " not found in speciesCol of recordTable", call. = FALSE)
  
  if(writecsv == TRUE){
    if(!file.exists(outDir)){stop("outDir does not exist", call. = FALSE)}
  }
  
  if(includeEffort){
    # if(!hasArg(scaleEffort))     stop("scaleEffort must be defined if includeEffort is TRUE", call. = FALSE)
    if(!is.logical(scaleEffort)) stop("scaleEffort must be logical (TRUE or FALSE)", call. = FALSE)
  } else {scaleEffort <- FALSE}
  
  if(hasArg(minActiveDaysPerOccasion)){
    if(!is.numeric(minActiveDaysPerOccasion))       stop("minActiveDaysPerOccasion must be a number", call. = FALSE)
    if(!minActiveDaysPerOccasion <= occasionLength) stop("minActiveDaysPerOccasion must be smaller than or equal to occasionLength", call. = FALSE)
  }
  
  #############
  # bring date, time, station ids into shape
  
  subset_species           <- recordTable[recordTable[,speciesCol] == species,]
  subset_species$DateTime2 <- parseDateTimeObject(inputColumn = subset_species[,recordDateTimeCol],
                                                  dateTimeFormat = recordDateTimeFormat,
                                                  timeZone = timeZone, 
                                                  checkNA_out = FALSE)
  
  if(any(is.na(subset_species$DateTime2))) stop(paste(sum(is.na(subset_species$DateTime2)), "out of",
                                                      nrow(subset_species),
                                                      "entries in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat (NA). row",
                                                      paste(rownames(subset_species)[which(is.na(subset_species$DateTime2))], collapse = ", ")))
  
  
  # check consistency of argument day1
  if(!is.character(day1)) stop("day1 must be a character", call. = FALSE)
  day1 <- tolower(day1)
  if(day1 == "survey") {day1switch <- 1} else {
    if(day1 == "station") {day1switch <- 2} else {
      try(date.test <- as.Date(day1), silent = TRUE)
      if(!exists("date.test"))       stop("day1 is not specified correctly. It can only be 'station', 'survey', or a date formatted as 'YYYY-MM-DD', e.g. '2016-12-31'")
      if(!inherits(date.test, "Date")) stop('could not interpret argument day1: can only be "station", "survey" or a specific date (e.g. "2015-12-31")')
      if(hasArg(buffer))             stop("if buffer is defined, day1 can only be 'survey' or 'station'")
      suppressWarnings(rm(date.test))
      day1switch <- 3
    }
  }
  
  # check that column names can be interpreted as day, as save
  camOp <- checkCamOpColumnNames (cameraOperationMatrix = camOp)
  # check if there's conflict in occasionStartTime. Error if so. 
  if(hasArg(occasionStartTime)){
    if(occasionStartTime != attributes(camOp)$occasionStartTime){
     stop(paste("occasionStartTime", occasionStartTime, "differs from occasionStartTime in camOp", attributes(camOp)$occasionStartTime)) 
    }
  }
  occasionStartTime <- attributes(camOp)$occasionStartTime
  
  # extract station / camera / session information from row names of camera operation matrix
  camop.info.df <- deparseCamOpRownames(camOp)
  
  # get information about station / session / camera IDs
  if("session" %in% colnames(camop.info.df)){
    
    if("camera" %in% colnames(camop.info.df)) stop("currently detectionHistory does not support camera-specific camera operation matrices when session/seasons are defined. Please run cameraOperation with byCamera = FALSE.", call. = FALSE)
    
    if(!is.logical(unmarkedMultFrameInput)) stop("unmarkedMultFrameInput must be logical (TRUE / FALSE)", call. = FALSE)
    
    # assign session IDs to record table (this will also change station ID to station__session)
    subset_species <- assignSessionIDtoRecordTable(recordTable_tmp = subset_species,
                                                    camOp = camOp,
                                                    dateTimeCol = "DateTime2",  #recordDateTimeCol,
                                                    stationCol = stationCol,
                                                    sessionCol = "session")
  } else {
    if(hasArg(unmarkedMultFrameInput)) warning("'unmarkedMultFrameInput' is defined, but I cannot find session IDs in the rownames of camOp. Check the row names format and maybe run camtrapR:::deparseCamOpRownames(camOp) to see if it can be interpreted", call. = FALSE)
  }
  
  cam.op.worked0 <- as.matrix(camOp)
  
  if(all(as.character(unique(subset_species[,stationCol])) %in% rownames(cam.op.worked0)) == FALSE){
  #if(all(as.character(unique(subset_species[,stationCol])) %in% camop.info.df$station) == FALSE){
    (stop("Not all values of stationCol in recordTable are matched by rownames of camOp"))
  }
  
  ################################################
  # compute date range of stations and records
  arg.list0 <- list(cam.op                = cam.op.worked0, 
                    subset_species_tmp    = subset_species, 
                    stationCol_tmp        = stationCol, 
                    day1_tmp              = day1, 
                    occasionStartTime_tmp = occasionStartTime, 
                    timeZone_tmp          = timeZone)
  
  if(hasArg(maxNumberDays))  arg.list0 <- c(arg.list0, maxNumberDays_tmp = maxNumberDays)
  if(hasArg(buffer))         arg.list0 <- c(arg.list0, buffer_tmp =  buffer)
  
  date_ranges <- do.call(createDateRangeTable, arg.list0)
  
  rm(arg.list0)
  
  #######################
  # adjust camera operation matrix
  
  cam.op.worked <- adjustCameraOperationMatrix(cam.op       = cam.op.worked0, 
                                               date_ranges2 = date_ranges, 
                                               timeZone_tmp = timeZone, 
                                               day1_2       = day1)
  
  # append occasionStartTime (if != 0) to column names for output table
  if(occasionStartTime != 0){
    colnames(cam.op.worked) <- paste(colnames(cam.op.worked), "+", occasionStartTime, "h", sep = "")
  }
  
  ######################
  # calculate trapping effort by station and occasion
  arg.list0 <- list(cam.op          = cam.op.worked, 
                    occasionLength2 = occasionLength, 
                    scaleEffort2    = scaleEffort, 
                    includeEffort2  = includeEffort,
                    occasionStartTime2 = occasionStartTime)
  
  if(hasArg(minActiveDaysPerOccasion))  arg.list0 <- c(arg.list0, minActiveDaysPerOccasion2 = minActiveDaysPerOccasion)
  
  effort.tmp <- do.call(calculateTrappingEffort, arg.list0)
  
  rm(arg.list0)
  
  effort <- effort.tmp[[1]]
  if(isTRUE(scaleEffort))  scale.eff.tmp.attr <- effort.tmp[[2]]
  
  ###################
  # remove records that fall into buffer period or were taken after maxNumberDays
  
  subset_species <- cleanSubsetSpecies(subset_species2 = subset_species, 
                                       stationCol2     = stationCol, 
                                       date_ranges2    = date_ranges)
  
  ############
  #  define the 1st day of the effective survey period.
  if(day1 %in% c("survey")){
    time2 <- date_ranges$start_first_occasion_survey[match(subset_species[,stationCol], rownames(date_ranges))]
  } else {
    time2 <- date_ranges$start_first_occasion[match(subset_species[,stationCol], rownames(date_ranges))]
  }
  
  # calculate the occasion each record belongs into from the time difference between records and beginning of the first occasion
  subset_species$occasion <- as.numeric(ceiling((difftime(time1  = subset_species$DateTime2,
                                                          time2  =  time2,
                                                          units  = "secs",
                                                          tz     = timeZone)
                                                 / (occasionLength * 86400))))
  
  # check if occasions are valid
  if(max(subset_species$occasion) > ncol(effort)) stop("Occasions exceeding total number of occasions calculated. This is a bug. I'm Sorry. Please report it.", call. = FALSE)
  if(any(subset_species$occasion == 0)) {
    # this is old code that works on POSIXlt objects, not POSIXct. Need to convert first
    subset_species$DateTime2_POSIXlt <- as.POSIXlt(subset_species$DateTime2)
    
    # identify records with time 00:00:00
    idx_records_without_time <- which(subset_species$DateTime2_POSIXlt$hour == 0 &
                                        subset_species$DateTime2_POSIXlt$min  == 0 &
                                        subset_species$DateTime2_POSIXlt$sec  == 0)
    
    # add 1 second and recalculate occasion
    if(length(idx_records_without_time) >= 1) { 
      warning(paste(length(idx_records_without_time), " records out of ", nrow(subset_species),
                    " seem to have no time information. Please provide time also. I will assume time of these records was occasionStartTime + 1 second (",
              occasionStartTime, ":00:01), but please treat this as experimental.", sep = ""), call. = FALSE)
      subset_species$DateTime2_POSIXlt$sec  <- 1
      subset_species$DateTime2_POSIXlt$hour <- occasionStartTime
      subset_species$DateTime2 <- as.POSIXct(subset_species$DateTime2_POSIXlt)
      # recalculate occasions with new date/time
      subset_species$occasion <- as.numeric(ceiling((difftime(time1  = subset_species$DateTime2,
                                                              time2  =  time2,
                                                              units  = "secs",
                                                              tz     = timeZone)
                                                     / (occasionLength * 86400))))
      if(any(subset_species$occasion == 0)) stop("I tried to fix the date-only recordDateTimeCol, but there is still occasions with value 0. Please provide date and time in recordDateTimeCol.", call. = FALSE)
    } else {
      stop("Occasion 0 calculated for at least one record. If recordDateTimeCol contains date and time (it seems they do), this is likely a bug. I am sorry. Please report it.", call. = FALSE)}
  }
  if(min(subset_species$occasion) < 0) {stop("Negative occasions calculated for at least one record. This is a bug. I'm Sorry. Please report it.", call. = FALSE)}
  
  ############
  # make detection history
  
  record.hist <- effort                     # start with effort (including NAs)
  record.hist <- ifelse(!is.na(record.hist), 0, record.hist)     # set all cells 0 (unless they are NA)
  rownames(record.hist) <- rownames(cam.op.worked)
  
  # identify occasions during which there were records (and how many)
  # binary: values = occasion
  # count: values = number of records, names = occasion
  if(output == "binary")  occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = unique, simplify = FALSE)
  if(output == "count")   occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = table, simplify = FALSE)
  
  
  
 # if(occasionLength == 1){                    # occasion length = 1
    # record.hist <- cam.op.worked
    # record.hist <- ifelse(record.hist == 0, NA, record.hist)    # if cameras not operational, set NA
    # record.hist <- ifelse(record.hist >= 1, 0,  record.hist)    # if cameras operational, set to 0
    # 
    # # identify occasions during which there were records (and how many)
    # if(output == "binary")  occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = unique, simplify = FALSE)
    # if(output == "count")   occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = table, simplify = FALSE)
    # 
    # fill detection matrix with 1 (or count) in appropriate cells
    for(xyz in which(sapply(occasions.by.station, FUN = function(x){!is.null(x)}))){
      
      if(output == "binary"){
        if(any(occasions.by.station[[xyz]] < 0)) stop("this is a bug in the function (DH_error1.binary). Please report it.", call. = FALSE)
        if(any(occasions.by.station[[xyz]] > ncol(record.hist))) stop("this is a bug in the function (DH_error2.binary). Please report it.", call. = FALSE)
        record.hist[match(names(occasions.by.station)[xyz], rownames(record.hist)), 
                    occasions.by.station[[xyz]]] <- 1
      } 
      if(output == "count") {
        if(any(as.numeric(names(occasions.by.station[[xyz]])) < 0)) stop("this is a bug in the function (DH_error1.count). Please report it.", call. = FALSE)
        if(any(as.numeric(names(occasions.by.station[[xyz]])) > ncol(record.hist))) stop("this is a bug in the function (DH_error2.count). Please report it.", call. = FALSE)
        record.hist[match(names(occasions.by.station)[xyz], rownames(record.hist)), 
                    as.numeric(names(occasions.by.station[[xyz]]))] <- occasions.by.station[[xyz]]
      } 
    }
    

   # ensure that occasions with NA or 0 effort are NA in detectionHistory
  if(occasionLength == 1){
    record.hist[is.na(cam.op.worked)] <- NA   # remove the records that were taken when cams were NA (redundant with above:   # remove records taken after day1 + maxNumberDays)
    if(isFALSE(scaleEffort)) record.hist[cam.op.worked == 0] <- NA
    
  } else {
    record.hist[is.na(effort)] <- NA     # just to make sure NA stays NA
    if(isFALSE(scaleEffort)) record.hist[effort == 0] <- NA
  }
    
   # rm(occasions.by.station, xyz)
    
#  } else {                                    # occasion length > 1
    # record.hist <- effort                     # start with effort (including NAs)
    # record.hist <- ifelse(!is.na(record.hist), 0, record.hist)     # set all cells 0 (unless they are NA)
    # rownames(record.hist) <- rownames(cam.op.worked)
    # 
    # # identify occasions during which there were records (and how many)
    # # binary: values = occasion
    # # count: values = number of records, names = occasion
    # if(output == "binary")  occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = unique, simplify = FALSE)
    # if(output == "count")   occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = table, simplify = FALSE)
    # 
    
    # fill detection matrix with "1" (or count) in appropriate cells
    # for(xyz in which(sapply(occasions.by.station, FUN = function(x){!is.null(x)}))){
    #   if(output == "binary")  record.hist[match(names(occasions.by.station)[xyz], rownames(record.hist)), 
    #                                       occasions.by.station[[xyz]]] <- 1
    #   if(output == "count")   record.hist[match(names(occasions.by.station)[xyz], rownames(record.hist)), 
    #                                       as.numeric(names(occasions.by.station[[xyz]]))] <- occasions.by.station[[xyz]]
    # }
    # record.hist[is.na(effort)] <- NA     # just to make sure NA stays NA
 # }
  
  # assign row names to output
  row.names(record.hist) <- row.names(effort) <- rownames(cam.op.worked)
  
  
  # assign column names
  if(isTRUE(datesAsOccasionNames)){
    
    seq.tmp <- seq(from = 1, by = occasionLength, length.out = ncol(record.hist))
    if(occasionStartTime != 0){
      colnames.tmp <- paste(colnames(cam.op.worked)[seq.tmp],
                            colnames(cam.op.worked)[seq.tmp + occasionLength], sep = "_")
    } else {
      colnames.tmp <- paste(colnames(cam.op.worked)[seq.tmp],
                            colnames(cam.op.worked)[seq.tmp + occasionLength - 1], sep = "_")
    }
    # make sure the last occasion ends on the last day
    
    if(day1 == "station"){
      colnames.tmp[length(colnames.tmp)] <- paste(colnames(cam.op.worked)[max(seq.tmp)],
                                                  colnames(cam.op.worked)[ncol(cam.op.worked)],
                                                  sep = "_")
    } else {   # if day1 = "survey" or date
      colnames.tmp[length(colnames.tmp)] <- paste(colnames(cam.op.worked)[max(seq.tmp)],
                                                  max(as.Date(date_ranges$end_last_occasion)),
                                                  sep = "_")
    }
    
    colnames(record.hist) <- colnames(effort) <- colnames.tmp
    
  }  else {
    colnames(record.hist) <- colnames(effort) <-  paste("o", seq(1,ncol(record.hist), by = 1), sep = "")
  }
  
  
  
  ## if multi-season object, make input for unmarkedMultFrame if desired
  if("session" %in% colnames(camop.info.df)){
    if(unmarkedMultFrameInput){
      
      record.hist.season.list <- split(as.data.frame(record.hist), f = camop.info.df$session)
      effort.season.list      <- split(as.data.frame(effort),      f = camop.info.df$session)
      
      all_stations <- unique(camop.info.df$station)
      
      # add missing stations for individual seasons
      record.hist.season.list2 <- lapply(record.hist.season.list, FUN = function(x){
        deparse_tmp <- deparseCamOpRownames(x)
          x2 <- matrix(NA, ncol = ncol(x), nrow = length(all_stations))
          rownames(x2) <- all_stations
          colnames(x2) <- colnames(x)
          x2[match(deparse_tmp$station, all_stations), ] <- as.matrix(x)
          x2
      })
      
      effort.season.list2 <- lapply(effort.season.list, FUN = function(x){
        deparse_tmp <- deparseCamOpRownames(x)
        x2 <- matrix(NA, ncol = ncol(x), nrow = length(all_stations))
        rownames(x2) <- all_stations
        colnames(x2) <- colnames(x)
        x2[match(deparse_tmp$station, all_stations), ] <- as.matrix(x)
        x2
      })
      
      # # get detection history and effort into separate lists
      # detHist_season_list_det    <- lapply(detHist_season_list, FUN = function(x) {x$detection_history})
      # detHist_season_list_effort <- lapply(detHist_season_list, FUN = function(x) {x$effort})
      
      
      # add NA columns to make sure all detection histories / effort matrices have same number of occasions (= identical number of columns)
      ncol_by_season <- sapply(record.hist.season.list, FUN = ncol)
      
      #detHist_season_list_det    <- lapply(detHist_season_list, FUN = function(x) {x$detection_history})
      #detHist_season_list_effort <- lapply(detHist_season_list, FUN = function(x) {x$effort})
      
      detHist_season_list_det_padded    <- lapply(record.hist.season.list2, 
                                                  FUN = padMatrixWithNA, 
                                                  ncol_desired = max(ncol_by_season))
      detHist_season_list_effort_padded <- lapply(effort.season.list2, 
                                                  FUN = padMatrixWithNA, 
                                                  ncol_desired = max(ncol_by_season))
      
      # combine detection histories for all seasons (effort also) into wide format
      record.hist <- do.call(what = "cbind",
                                  args = detHist_season_list_det_padded) 
      effort <- do.call(what = "cbind",
                                 args = detHist_season_list_effort_padded)
      
    }
  }
    
  
  
  
  ################################################
  # save output as table
  if(day1switch == 1) day1string <- "_first_day_from_survey"
  if(day1switch == 2) day1string <- "_first_day_by_station"
  if(day1switch == 3) day1string <- paste("_first_day", day1, sep = "_")
  
  effortstring <- ifelse(isTRUE(includeEffort), "with_effort__", "no_effort__")
  maxNumberDaysstring <- ifelse(hasArg(maxNumberDays), paste("max",maxNumberDays,"days_", sep = ""), "")
  if(isTRUE(includeEffort)){
    scaleEffortstring <- ifelse(isTRUE(scaleEffort), "scaled_", "not_scaled_")
  } else {
    scaleEffortstring <- ""
  }
  
  # create names for the csv files
  outtable.name <- paste(species, "__detection_history__", effortstring,
                         occasionLength, "_days_per_occasion_",
                         maxNumberDaysstring,
                         "_occasionStart", occasionStartTime,"h_",
                         day1string, "__",
                         Sys.Date(),
                         ".csv", sep = "")
  
  outtable.name.effort <- paste(species, "__effort__",
                                scaleEffortstring,
                                occasionLength, "_days_per_occasion_",
                                maxNumberDaysstring,
                                "_occasionStart", occasionStartTime,"h_",
                                day1string, "__",
                                Sys.Date(),
                                ".csv", sep = "")
  
  outtable.name.effort.scale <- paste(species, "__effort_scaling_parameters__",
                                      occasionLength, "_days_per_occasion_",
                                      maxNumberDaysstring,
                                      "_occasionStart", occasionStartTime,"h_",
                                      day1string, "__",
                                      Sys.Date(),
                                      ".csv", sep = "")
  
  if(writecsv){
    setwd(outDir)
    write.csv(record.hist, file = outtable.name)
    if(isTRUE(includeEffort)){
      write.csv(effort, file = outtable.name.effort)
      if(hasArg(scaleEffort)){
        if(scaleEffort)  write.csv(scale.eff.tmp.attr, file = outtable.name.effort.scale)
      }
    }
  }
  
  if(includeEffort){
    if(scaleEffort){
      return(list(detection_history = record.hist,
                  effort = effort,
                  effort_scaling_parameters = scale.eff.tmp.attr))
    } else {
      return(list(detection_history = record.hist,
                  effort = effort))
    }
  } else {
    return(list(detection_history = record.hist))
  }
  
  }  # end is_multispecies
  
  
  
  # Multi-species case ----

  # Check if all species are in the record table
  species_not_found <- species[!species %in% recordTable[, speciesCol]]
  if(length(species_not_found) > 0) {
    stop(paste("The following species were not found in recordTable:", 
               paste(species_not_found, collapse = ", ")), call. = FALSE)
  }
  
  # Get the original call minus the species argument which we're modifying
  call_args <- match.call(expand.dots = FALSE)
  call_args$species <- NULL  # remove species as we'll modify it
  
  # Convert to list for manipulation
  call_list <- as.list(call_args)[-1]  # -1 removes the function name
  
  
  
  # Create the lapply call with only defined arguments
  results <- lapply(species, function(sp) {
    # Add current species to argument list
    call_list$species <- sp
    
    # Execute call with only the arguments that were provided
    do.call(detectionHistory, call_list)[[1]]  
  })
  
  names(results) <- species
  

  # Get effort matrix from first species result (they're identical)
  call_list$species <- species[1]
  full_result <- do.call(detectionHistory, call_list)
  
  # Return modified structure
  if(includeEffort) {
    if(scaleEffort) {
      return(list(
        detection_history = results,
        effort = full_result$effort,
        effort_scaling_parameters = full_result$effort_scaling_parameters
      ))
    } else {
      return(list(
        detection_history = results,
        effort = full_result$effort
      ))
    }
  } else {
    return(list(detection_history = results))
  }
}
