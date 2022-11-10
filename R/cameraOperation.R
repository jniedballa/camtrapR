#' Create a camera trap station operability matrix
#' 
#' Construct a matrix of daily camera trap station operability for use in
#' \code{\link{detectionHistory}} and \code{\link{spatialDetectionHistory}},
#' where it is needed for calculating trapping effort per occasion. It is also
#' used in \code{\link{surveyReport}} to calculate the number of trap nights
#' durig a survey. If several cameras were deployed per station, the matrix can
#' contain camera- or station-specific trap operation information, or
#' information about sessions during repeated surveys.
#' 
#' \code{cameraCol} is NULL by default, meaning the function assumes there was
#' 1 camera per station in \code{CTtable}. If more than 1 camera was deployed
#' per station, \code{cameraCol} needs to be specified to identify individual
#' cameras within a station. Likewise, \code{sessionCol} can be used to if
#' camera trap stations were operated during multiple sessions / trapping
#' seasons.
#' 
#' \code{dateFormat} defaults to "YYYY-MM-DD", e.g. "2014-10-31", but can be
#' any other date format or date-time also. It can be specified either in the
#' format required by \code{\link[base]{strptime}} or the 'orders' argument in
#' \code{\link[lubridate]{parse_date_time}} in \pkg{lubridate}. In the example
#' above, "YYYY-MM-DD" would be specified as "\%Y-\%m-\%d" in base R or "ymd"
#' in \pkg{lubridate}.
#' 
#' Since version 2.1, dateFormat can be a date-time. That makes it possible to
#' specify the exact time cameras were set up / retrieved / malfunctioned /
#' worked again. This information is used to calculate the daily trapping
#' effort more precisely on days with incomplete effort.
#' 
#' Previously, setup and retrival day were counted as 1, indicating a whole day
#' of effort on those days. Since version 2.1, setup and retrieval are assumed
#' to have happened at 12 noon (resulting in daily effort of 0.5 instead of 1).
#' Users can also specify the exact time cameras were set up (by providing a
#' date-time in the setup / retrieval / problem columns). See vignette 3 for
#' more details.
#' 
#' If \code{hasProblems} is TRUE, the function tries to find columns
#' \code{ProblemX_from} and \code{ProblemX_to} in \code{CTtable}. \code{X} is a
#' consecutive number from 1 to n, specifying periods in which a camera or
#' station was not operational. If \code{hasProblems} is FALSE, cameras are
#' assumed to have been operational uninterruptedly from setup to retrieval
#' (see \code{\link{camtraps}} for details).
#' 
#' \code{allCamsOn} only has an effect if there was more than 1 camera at a
#' station. If TRUE, for the station to be considered operational, all cameras
#' at a station need to be operational. If FALSE, at least 1 active camera
#' renders the station operational. Argument \code{camerasIndependent} defines
#' if cameras record animals independently (it thus only has an effect if there
#' was more than 1 camera at a station). This is the case if an observation at
#' one camera does not increase the probability for detection at another camera
#' (cameras face different trails at a distance of one another).
#' Non-independence occurs if an animal is likely to trigger both camers (as
#' would be the case with 2 cameras facing each other).
#' 
#' If \code{camerasIndependent} is TRUE, 2 active cameras at a station will
#' result in a station operation value of 2 in the resulting matrix, i.e., 2
#' independent trap days at 1 station and day. If \code{camerasIndependent} is
#' FALSE, 2 active cameras will return value 1, i.e., 1 trap night at 1 station
#' per day.
#' 
#' Row names depend on the input arguments and contain the station name and
#' potentially session and camera names (if \code{sessionCol} and/or
#' \code{cameraCol} are defined).
#' 
#' Naming convention is (since version 1.2) \bold{Bold} information are from
#' the columns \code{stationCol}, \code{sessionCol} and \code{cameraCol} in
#' \code{CTtable}:
#' 
#' \bold{Station} \cr \bold{Station}__SESS_\bold{SessionID} \cr
#' \bold{Station}__CAM_\bold{CameraID} \cr
#' \bold{Station}__SESS_\bold{SessionID}__CAM_\bold{CameraID}
#' 
#' Session are designated with prefix "__SESS_", cameras with prefix "__CAM_".
#' Therefore, these are reserved words and may not be part of station, session
#' or camera names. Here's what it may look like in real life:
#' 
#' \bold{Station1} \cr \bold{Station1}__SESS_\bold{2019} \cr
#' \bold{Station1}__CAM_\bold{1024152} \cr
#' \bold{Station1}__SESS_\bold{2019}__CAM_\bold{1024152}
#' 
#' Functions detectionHistory and spatialDetectionHistory recognize these and
#' use the information accordingly.
#' 
#' @param CTtable data.frame containing information about location and trapping
#' period of camera trap stations
#' @param stationCol character. name of the column specifying Station ID in
#' \code{CTtable}
#' @param cameraCol character. name of the column specifying Camera ID in
#' \code{CTtable} (optional). If empty, 1 camera per station is assumed.
#' @param sessionCol character. name of the column specifying session ID in
#' \code{CTtable} (optional). Use it for creating multi-session / multi-season
#' detection histories (unmarked: \code{\link[unmarked]{unmarkedMultFrame}};
#' secr: \code{\link[secr]{capthist}} )
#' @param setupCol character. name of the column containing camera setup dates
#' in \code{CTtable}
#' @param retrievalCol character. name of the column containing camera
#' retrieval dates in \code{CTtable}
#' @param hasProblems logical. If TRUE, function will look for columns
#' specifying malfunction periods in \code{CTtable} (naming convention:
#' \code{ProblemX_from} and \code{ProblemX_to}, where X is a number)
#' @param byCamera logical. If TRUE, camera operability matrix is computed by
#' camera, not by station (requires \code{cameraCol})
#' @param allCamsOn logical. Takes effect only if \code{cameraCol} is defined
#' and if \code{byCamera} is FALSE. If \code{allCamsOn = TRUE}, all cameras at
#' a station need to be operational for the station to be operational (e.g. 1
#' camera out of 2 malfunctioning renders the station inoperational). Output
#' values can be 1/0/NA only (all cameras at a station operational/ at least 1
#' camera not operational/ no camera set up). If \code{allCamsOn = FALSE}, at
#' least 1 active camera makes a station operational.
#' @param camerasIndependent logical. Return number of active camera traps by
#' station? Only if \code{byCamera} is FALSE and \code{allCamsOn} is FALSE. If
#' \code{camerasIndependent} is TRUE, output values will be the number of
#' operational cameras at a station. If \code{camerasIndependent} is FALSE, the
#' value is 1 if at least 1 camera was operational, otherwise 0. In both cases,
#' values are NA if no camera was set up.
#' @param dateFormat character. The format of columns \code{setupCol} and
#' \code{retrievalCol} (and potential problem columns) in \code{CTtable}. Must
#' be interpretable by either \code{as.Date} or the "orders" argument
#' \code{\link[lubridate]{parse_date_time}} in \pkg{lubridate}. Can be a date
#' or (since version 2.1) a date-time.
#' @param occasionStartTime integer. time of day (the full hour) at which to
#' begin occasions. Replaces \code{occasionStartTime} from
#' \code{\link{detectionHistory}} and \code{\link{spatialDetectionHistory}}.
#' @param writecsv logical. Should the camera operability matrix be saved as a
#' .csv?
#' @param outDir character. Directory into which csv is saved
#' 
#' @return A matrix. Row names always indicate Station IDs. If
#' \code{sessionCol} and/or \code{cameraCol} are defined, they are contained in
#' the row names also (camera ID only if \code{byCamera = TRUE}). Column names
#' are dates. \cr Legend: NA: camera(s) not set up, 0: camera(s) not
#' operational, 1 (or higher): number of operational camera(s) or an indicator
#' for whether the station was operational (depending on
#' \code{camerasIndependent} and \code{allCamsOn})
#' 
#' @note Setting \code{camerasIndependent} according to the sampling situation
#' is important for the functions \code{\link{detectionHistory}} and
#' \code{\link{spatialDetectionHistory}}, if sampling effort (the number of
#' active trap nights in a occasion) is to be computed and returned.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' 
#' data(camtraps)
#' 
#' # no problems/malfunction
#' camop_no_problem <- cameraOperation(CTtable      = camtraps,
#'                                     stationCol   = "Station",
#'                                     setupCol     = "Setup_date",
#'                                     retrievalCol = "Retrieval_date",
#'                                     writecsv     = FALSE,
#'                                     hasProblems  = FALSE,
#'                                     dateFormat   = "dmy"
#' )
#' 
#' # with problems/malfunction
#' camop_problem <- cameraOperation(CTtable      = camtraps,
#'                                  stationCol   = "Station",
#'                                  setupCol     = "Setup_date",
#'                                  retrievalCol = "Retrieval_date",
#'                                  writecsv     = FALSE,
#'                                  hasProblems  = TRUE,
#'                                  dateFormat   = "dmy"
#' )
#' 
#' # with problems/malfunction / dateFormat in strptime format
#' camop_problem_lubridate <- cameraOperation(CTtable      = camtraps,
#'                                            stationCol   = "Station",
#'                                            setupCol     = "Setup_date",
#'                                            retrievalCol = "Retrieval_date",
#'                                            writecsv     = FALSE,
#'                                            hasProblems  = TRUE,
#'                                            dateFormat   = "%d/%m/%Y"
#' )
#' 
#' camop_no_problem
#' camop_problem
#' camop_problem_lubridate
#' 
#' @importFrom data.table rbindlist setDF setDT setkey foverlaps ":="
#' @importFrom lubridate as_date as_datetime ddays dhours dseconds interval int_start int_end time_length "%within%"
#' @importFrom methods hasArg is new
#' @importFrom stats aggregate na.omit start end rnorm window quantile
#' @importFrom utils capture.output modifyList write.csv zip head menu read.table
#' @export cameraOperation
#' 
cameraOperation <- function(CTtable,
                            stationCol = "Station",
                            cameraCol,
                            sessionCol,
                            setupCol,
                            retrievalCol,
                            hasProblems = FALSE,
                            byCamera,
                            allCamsOn,
                            camerasIndependent,
                            dateFormat = "ymd",
                            occasionStartTime = 0,
                            writecsv = FALSE,
                            outDir){
  
  
  # check and prepare input
  wd0 <- getwd()
  on.exit(setwd(wd0))
  
  checkForSpacesInColumnNames(stationCol = stationCol, setupCol = setupCol, retrievalCol = retrievalCol)
  
  CTtable <- dataFrameTibbleCheck(df = CTtable)
  
  if(!stationCol   %in% colnames(CTtable))  stop(paste('stationCol = "',   stationCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!setupCol     %in% colnames(CTtable))  stop(paste('setupCol = "',     setupCol,       '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!retrievalCol %in% colnames(CTtable))  stop(paste('retrievalCol = "', retrievalCol,   '" is not a column name in CTtable', sep = ''), call. = FALSE)
  
  
  stopifnot(length(stationCol) == 1)
  CTtable[,stationCol] <- as.character(CTtable[,stationCol])
  
  stopifnot(length(setupCol) == 1)
  CTtable[,setupCol] <- as.character(CTtable[,setupCol])   # parse_date_time gives error if in Date format
  
  stopifnot(length(retrievalCol) == 1)
  CTtable[,retrievalCol] <- as.character(CTtable[,retrievalCol])
  
  stopifnot(is.logical(writecsv))
  stopifnot(is.logical(hasProblems))
  
  if(hasArg(byCamera)) {
    stopifnot(is.logical(byCamera))
    if(isTRUE(byCamera) & hasArg(cameraCol) == FALSE){
      stop("if 'byCamera' is TRUE, 'cameraCol' needs to be specified")
    }
  } 
  
  if(length(occasionStartTime) != 1) stop("occasionStartTime must have length 1")
  occasionStartTime    <- as.integer(round(occasionStartTime))
  if(occasionStartTime != 0 & !is.integer(occasionStartTime)) stop ("occasionStartTime must be an integer between 0 and 23", call. = FALSE)
  if(occasionStartTime < 0 | occasionStartTime >= 24)         stop ("occasionStartTime must be between 0 and 23", call. = FALSE)
  
  myArgs <- match.call()
  cameraColInArgs  <- "cameraCol" %in% names(myArgs)
  sessionColInArgs <- "sessionCol" %in% names(myArgs)
  
  
  if(cameraColInArgs){
    checkForSpacesInColumnNames(cameraCol = cameraCol)
    if(!cameraCol %in% colnames(CTtable)) stop(paste('cameraCol = "', cameraCol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
    if(!hasArg(byCamera)) stop("if cameraCol is set, byCamera must be specified")
    stopifnot(is.logical(byCamera))
    CTtable[,cameraCol] <- as.character(CTtable[,cameraCol])
    
    if(isFALSE(byCamera)){
      if(!hasArg(allCamsOn)) stop("if cameraCol is set and byCamera is FALSE, allCamsOn must be specified")
      stopifnot(is.logical(allCamsOn))
      if(!allCamsOn){
        if(!hasArg(camerasIndependent)) stop("if cameraCol is set, byCamera is FALSE and allCamsOn is FALSE, camerasIndependent must be specified")
        stopifnot(is.logical(camerasIndependent))
      }
    } else {    # if byCamera = TRUE
      if(hasArg(allCamsOn))          warning("if cameraCol is set and byCamera is TRUE, allCamsOn will have no effect", call. = FALSE)
      if(hasArg(camerasIndependent)) warning("if cameraCol is set and byCamera is TRUE, camerasIndependent will have no effect", call. = FALSE)
    }
  } else {
    cameraCol <- "camera"
    CTtable[, cameraCol] <-  paste(CTtable[, stationCol], "Cam1", sep = "")     # add a dummy camera column
    if(hasArg(byCamera)) warning("If cameraCol is not defined, byCamera will have no effect")
  }
  
  # check argument sessionCol
  if(isTRUE(sessionColInArgs)){
    checkForSpacesInColumnNames(sessionCol = sessionCol)
    if(!sessionCol %in% colnames(CTtable)) stop(paste('sessionCol = "', sessionCol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
    if(!is.numeric(CTtable[, sessionCol])) stop("Values of sessionCol must be numeric", call. = FALSE)
  } else{
    sessionCol <- "session"
    CTtable[, sessionCol] <- 1   # add a dummy session column
  }
  
  # return error if duplicate stations (i.e. more than 1 row per station)
  if(isFALSE(cameraColInArgs) & isFALSE(sessionColInArgs)){
    if(any(duplicated(CTtable[,stationCol]))){
      tmp <- table(CTtable[,stationCol])
      stop(paste(sum(tmp >= 2)," stations have more than 1 item in CTtable. Please specify 'cameraCol' or 'sessionCol'\n", 
                 paste(names(tmp[tmp >= 2]),
                       tmp[tmp >= 2], sep = ": ", collapse = "\n"),
                 sep = ""), 
           call. = FALSE)
    }
  }
  
  if(isTRUE(cameraColInArgs) & isFALSE(sessionColInArgs)){
    if(any(duplicated(CTtable[,c(stationCol, cameraCol)]))){
      tmp <- table(paste(CTtable[,stationCol], " - ", cameraCol, " ", CTtable[, cameraCol], sep = ""))
      stop(paste(sum(tmp >= 2), " station/camera combinations have more than 1 item in CTtable. Consider specifying 'sessionCol' if you have multiple sessions / seasons\n",
                 paste(names(tmp[tmp >= 2]), tmp[tmp >= 2], sep = ": ", collapse = "\n"), sep = ""),
           call. = FALSE)
    }
  }
  
  if(isFALSE(cameraColInArgs) & isTRUE(sessionColInArgs)){
    if(any(duplicated(CTtable[,c(stationCol, sessionCol)]))){
      tmp <- table(paste(CTtable[,stationCol], " - ", sessionCol, " ", CTtable[, sessionCol], sep = ""))
      stop(paste(sum(tmp >= 2)," station/session combinations have more than 1 item in CTtable. Consider specifying 'cameraCol' if you have multiple cameras per station\n",
                 paste(names(tmp[tmp >= 2]), tmp[tmp >= 2], sep = ": ", collapse = "\n"), sep = ""),
           call. = FALSE)
    }
  }
  
  if(isTRUE(cameraColInArgs) & isTRUE(sessionColInArgs)){
    if(any(duplicated(CTtable[,c(stationCol, cameraCol, sessionCol)]))){
      tmp <- table(paste(CTtable[,stationCol], " - ", cameraCol, " ", CTtable[, cameraCol], " - ", sessionCol, " ", CTtable[, sessionCol], sep = ""))
      stop(paste(sum(tmp >= 2), " station/camera/session combination have more than 1 item in CTtable.\n",
                 paste(names(tmp[tmp >= 2]), tmp[tmp >= 2], sep = ": ", collapse = "\n"), sep = ""),
           call. = FALSE)
    }
  }
  
  if(hasArg(outDir)){
    if(!is.character(outDir)){stop("outDir must be of class 'character'")}
    if(file.exists(outDir) == FALSE) stop("outDir does not exist")
  }
  
  # check date columns and format
  
  # flag for whether to use fraction of days (if hours are provided in dateFormat), otherwise daily effort (itteger)
  effortAsFraction <- grepl("H", dateFormat)
  
  # if dateFormat contains H (hour), use parseDateTimeObject, otherwise parseDateObject
  if(effortAsFraction) {
    CTtable[,setupCol]     <- parseDateTimeObject(inputColumn = CTtable[,setupCol],     dateTimeFormat = dateFormat, checkNA = TRUE, checkEmpty = TRUE, timeZone = "UTC")
    CTtable[,retrievalCol] <- parseDateTimeObject(inputColumn = CTtable[,retrievalCol], dateTimeFormat = dateFormat, checkNA = TRUE, checkEmpty = TRUE, timeZone = "UTC")
  } else {
    CTtable[,setupCol]     <- parseDateObject(inputColumn = CTtable[,setupCol],     dateFormat = dateFormat, checkNA = TRUE, checkEmpty = TRUE)
    CTtable[,retrievalCol] <- parseDateObject(inputColumn = CTtable[,retrievalCol], dateFormat = dateFormat, checkNA = TRUE, checkEmpty = TRUE)
  }
  
  # if setup time was not defined, assume 12 noon (so effort on setup/retrieval day = 0.5)
  if(isFALSE(effortAsFraction)){
    CTtable[,setupCol]     <- CTtable[,setupCol]     + dhours(12)
    CTtable[,retrievalCol] <- CTtable[,retrievalCol] + dhours(12)
  }
  
  if(any(CTtable[,setupCol] == CTtable[,retrievalCol])) stop(paste0("row ", paste(which(CTtable[,setupCol] == CTtable[,retrievalCol]), collapse = ", "), ": setup is identical to retrieval"), call. = FALSE)
  
  # check if dates make sense
  if(any(CTtable[,setupCol]     < as.Date("1970-01-01"))) warning("setup dates begin before 1970. If this is not intended please check dateFormat", call. = FALSE)
  if(any(CTtable[,retrievalCol] < as.Date("1970-01-01"))) warning("retrieval dates are before 1970. If this is not intended please check dateFormat", call. = FALSE)
  
  if(any(CTtable[,setupCol]     > Sys.Date())) warning("setup date is in the future. If this is not intended please check dateFormat", call. = FALSE)
  if(any(CTtable[,retrievalCol] > Sys.Date())) warning("retrieval date is in the future. If this is not intended please check dateFormat", call. = FALSE)
  
  
  
  # ensure setup is before retrieval (treating sessions independently)
  CTtable_split <- split(CTtable, f = CTtable[, sessionCol])
  
  lapply(CTtable_split, FUN = function(x) {
    if(any(x[,setupCol] > x[,retrievalCol])){
      stop(paste("Setup Date after Retrieval Date:   "),
           paste(x[which(x[,setupCol] > x[,retrievalCol]), stationCol],
                 collapse = ", "), 
           ifelse(sessionColInArgs, paste0(" (Session ", unique(x[, sessionCol]), ")"), ""), call. = FALSE)
    }
  }
  )

  # get start / retrieval dates of all cameras
  date0 <- sapply(CTtable[, setupCol],     FUN =  function(x) as.character(min(x)))
  date1 <- sapply(CTtable[, retrievalCol], FUN =  function(x) as.character(max(x) - dseconds(1)))  # if not removing 1 second, last day with end on midnight when the day begins, leading to retrieval day being 0
  
  # create interval from start to end of each camera
  start_to_end <- interval(date0, date1)
  
  if(isTRUE(hasProblems)){
    
    # find problem columns
    cols.prob.from <- grep(colnames(CTtable), pattern = "Problem\\d\\Sfrom")
    cols.prob.to   <- grep(colnames(CTtable), pattern = "Problem\\d\\Sto")
    
    # convert problem column entries to character
    for(problem_col_index in c(cols.prob.from, cols.prob.to)){
      CTtable[, problem_col_index] <- as.character(CTtable[, problem_col_index])
    }
    
    # error if no Problem columns
    if(length(cols.prob.from) == 0) stop("could not find column ProblemX_from")
    if(length(cols.prob.to) == 0)   stop("could not find column ProblemX_to")
    
    if(length(cols.prob.from) != length(cols.prob.to)){
      stop("length of 'Problem..._from' and 'Problem..._to' columns differs. Check column names. Sample: 'Problem1_from', 'Problem1_to'")
    }
    
    if(effortAsFraction) {
      for(problemFromColumn in cols.prob.from){
        CTtable[, problemFromColumn] <- parseDateTimeObject(inputColumn = CTtable[, problemFromColumn], dateFormat, quiet = TRUE,
                                                            checkNA = FALSE, checkEmpty = FALSE, checkNA_out = FALSE, timeZone = "UTC",
                                                            allowEmptyOutput = T)
      }
      for(problemToColumn in cols.prob.to){
        CTtable[, problemToColumn]   <- parseDateTimeObject(inputColumn = CTtable[, problemToColumn],   dateFormat, quiet = TRUE,
                                                            checkNA = FALSE, checkEmpty = FALSE, checkNA_out = FALSE, timeZone = "UTC",
                                                            allowEmptyOutput = T)
      }
    } else {
      for(problemFromColumn in cols.prob.from){
        CTtable[, problemFromColumn] <- parseDateObject(inputColumn = CTtable[, problemFromColumn], dateFormat, 
                                                        checkNA = FALSE, checkEmpty = FALSE, returndatetime = TRUE,
                                                        allowEmptyOutput = TRUE)
      }
      for(problemToColumn in cols.prob.to){
        CTtable[, problemToColumn] <- parseDateObject(inputColumn = CTtable[, problemToColumn], dateFormat, 
                                                      checkNA = FALSE, checkEmpty = FALSE, returndatetime = TRUE,
                                                      allowEmptyOutput = TRUE)
      }
    }
    

    # check that there are some problems at all (since hasProblems = TRUE)
    if(all(is.na(CTtable[, problemFromColumn]))) warning("in problemFromColumn column(s), all values are NA", call. = FALSE)
    if(all(is.na(CTtable[, problemToColumn])))   warning("in Problem_to column(s), all values are NA", call. = FALSE)
    
    # if problems begin on setup day, make sure it's the same time as setup (if only dates are specified)
    if(isFALSE(effortAsFraction)){
      problem_begin_on_setup_day <- which(as.Date(CTtable[, setupCol]) == as.Date(CTtable[, cols.prob.from[1]]))
      if(length(problem_begin_on_setup_day) >= 1) {
        CTtable[problem_begin_on_setup_day, cols.prob.from[1]] <- CTtable[problem_begin_on_setup_day, setupCol]
      }
      problem_ends_on_retrieval_day <- which(as.Date(CTtable[, retrievalCol]) == as.Date(CTtable[, cols.prob.to[length(cols.prob.to)]]))
      if(length(problem_ends_on_retrieval_day) >= 1) {
        CTtable[problem_ends_on_retrieval_day, cols.prob.to[length(cols.prob.to)]] <- CTtable[problem_ends_on_retrieval_day, retrievalCol]
      }
    }
    
    # check that problems begin after setup
    for(cols.prob.from.index in cols.prob.from){
      if(isTRUE(effortAsFraction)){
        if(any(CTtable[,setupCol] > CTtable[,cols.prob.from.index], na.rm = TRUE)) {
          stop(paste(paste(CTtable[which(CTtable[,setupCol] > CTtable[,cols.prob.from.index]), stationCol], collapse = ", "), ": Problem begins before Setup"), call. = FALSE)
        }
      }
      if(isFALSE(effortAsFraction)){
        if(any(as.Date(CTtable[,setupCol]) > CTtable[,cols.prob.from.index], na.rm = TRUE)){
          stop(paste(paste(CTtable[which(CTtable[,setupCol] > CTtable[,cols.prob.from.index]), stationCol], collapse = ", "), ": Problem begins before Setup"), call. = FALSE)
        } 
      }
    }
    
    # check that problems end before (or on) retrieval
    for(cols.prob.to.index in cols.prob.to){
      if(any(CTtable[,retrievalCol] < CTtable[,cols.prob.to.index], na.rm = TRUE)){
        stop(paste(paste(CTtable[which(CTtable[,retrievalCol] < CTtable[,cols.prob.to.index]), stationCol], collapse = ", "), ": Problem ends after retrieval"), call. = FALSE)
      }
    }
    
    
    # make list of Problem groups
    
    # loop over problem groups, save problem start and end date(time)
    # this is a bit hacky, but ensures that it works even if Problem columns are not ordered 
    # list item = Problem index (Problem1, Problem2, ...); vector items within = cameras
    problem_colnames_index_list <- vector(mode = "list", length = length(cols.prob.from))
    
    for(Problem_group in 1:length(problem_colnames_index_list)){
      problem_colnames_index_list[[Problem_group]]$prob.from <- CTtable[, cols.prob.from [order(colnames(CTtable) [cols.prob.from])] [Problem_group]]
      problem_colnames_index_list[[Problem_group]]$prob.to   <- CTtable[, cols.prob.to   [order(colnames(CTtable) [cols.prob.to])]   [Problem_group]]
    }
    
    # loop over cameras, make and concatenate the problem intervals by station
    problem_intervals_by_row <- list()
    for(row_index in 1:nrow(CTtable)){
      
      # make data frame of problem start / end times
      tmp <- lapply(problem_colnames_index_list, FUN = function(x) {
        
        if(is.na( x$prob.from  [row_index]) & is.na( x$prob.to  [row_index])){
          return(data.frame(start = NA, end   = NA))
        }
        if(is.na( x$prob.from  [row_index]) & !is.na( x$prob.to  [row_index])){
          stop("row ", row_index, ": Problem_from is NA, but Problem_to is ", x$prob.to  [row_index], call. = FALSE)
        }
        if(!is.na( x$prob.from  [row_index]) & is.na( x$prob.to  [row_index])){
          stop("row ", row_index, ": Problem_from is ", x$prob.from  [row_index], " but Problem_to is NA", call. = FALSE)
        }
        
        # when using ifelse, output is numeric, not POSIXct
        if(effortAsFraction)  prob.to <- x$prob.to  [row_index] #- dseconds(1)           
        # if Problem_to is defined as date, add 1 day - 1 second (so problem ends just before midnight the same day)
        if(!effortAsFraction) {
          prob.to <- x$prob.to  [row_index] + ddays(1) - dseconds(1) 
          # if problem period were to end after end of camera trapping period, replace the end of Problem with end of camera trapping period
          if(prob.to > int_end(start_to_end[row_index])){
            prob.to <- int_end(start_to_end[row_index])
          }
        }
        data.frame(start = x$prob.from[row_index],
                   end   = prob.to)
      })
      
      # combine date.frames for problem start end times (if multiple problem periods defines)
      tmp.rbind <- do.call(rbind, tmp)
      
      
      # make intervals for the problem periods
      if(!all(is.na(tmp.rbind))) {
        problem_intervals_by_row[[row_index]] <- interval(start = tmp.rbind$start, tmp.rbind$end  - dseconds(1))   # -1 to avoid problems matching with camera operation date-time
        if(any(time_length(problem_intervals_by_row[[row_index]]) < 0, na.rm = TRUE)) stop("row", row_index, ": Problem ends before it starts.")
      } else {
        problem_intervals_by_row[[row_index]] <- NA
      }
    }
    
    rm(problemFromColumn, problemToColumn, cols.prob.from.index, cols.prob.to.index)
  }
  
  # create empty matrix with desired dimensions (depending on presence of camera / session columns)
  arg_list <- list(CTtable = CTtable,
                   stationCol = stationCol,
                   setupCol = setupCol,
                   retrievalCol = retrievalCol
  )
  
  if(cameraColInArgs)  arg_list <- c(arg_list, "cameraCol" = cameraCol)
  if(sessionColInArgs) arg_list <- c(arg_list, "sessionCol" = sessionCol)
  
  camOp_empty <- do.call(stationSessionCamMatrix, args = arg_list) 
  
  
  
  
  
  
  #  trapping intervals for all cameras (setup to retrieval)
  # for each day in camop, make an interval covering the entire day
  camop_daily_intervals <- lapply(as.Date(colnames(camOp_empty)),
                                  FUN = function(x) interval(start = x + dhours(occasionStartTime),  #paste0(x, " ", occasionStartTime, ":00:00"),
                                                             end =   x + ddays(1) + dhours(occasionStartTime) - dseconds(1) #paste0(x+1, " ", occasionStartTime, ":00:00")  # 86400 s. If 23:59:59 same day, it's 86399 s
                                                             #end = paste0(x, " ", occasionStartTime, ":59:59")  #  86399 s
                                  ))
  names(camop_daily_intervals) <- colnames(camOp_empty)
  
  # get start / end of the days covered by the study (+ occasionStartTime, if defined)
  int_start_daily <- lapply(camop_daily_intervals, FUN = function(x) int_start(x))
  int_end_daily   <- lapply(camop_daily_intervals, FUN = function(x) int_end(x))
  # get start / end of camera trapping period by camera
  # int_start_total <- as_date(int_start(start_to_end))
  # int_end_total   <- as_date(int_end(start_to_end))
  int_start_total <- int_start(start_to_end)
  int_end_total   <- int_end(start_to_end)
  
  
  
  # identify overlapping intervals with data.table
  # https://www.howtobuildsoftware.com/index.php/how-do/bzA9/r-intervals-lubridate-r-and-lubridate-do-the-intervals-in-x-fit-into-any-of-the-intervals-in-y
  
  #CTtable
  int.total <- data.frame(start = sapply(int_start(start_to_end), as.POSIXct), 
                          end   = sapply(int_end(start_to_end), as.POSIXct))
  
  # daily intervals
  int.daily <- data.frame(start = sapply(int_start_daily, as.POSIXct), 
                          end = sapply(int_end_daily, as.POSIXct))
  
  setDT(int.total)[, `:=`(start = start,
                          end = end)]
  setkey(setDT(int.daily)[, `:=`(start =start,
                                 end = end)], start, end)
  intervals_matched <- foverlaps(int.total, int.daily, type = "any", which = TRUE)
  #intervals_matched.within <- foverlaps(int.total, int.daily, type = "within", which = TRUE)
  
  # equivalent to avove, only x/y exchanged
  # setDT(int.daily)[, `:=`(start = start,
  #                         end = end)]
  # setkey(setDT(int.total)[, `:=`(start =start,
  #                                end = end)], start, end)
  # intervals_matched <- foverlaps(int.daily, int.total, type = "any", which = TRUE)
  
  
  # loop over cameras (= rows)
  for(i in 1:nrow(camOp_empty)){
    
    run_these <- intervals_matched[intervals_matched$xid == i,]$yid
    
    # intersect daily intervals with interval from setup to retrieval
    
    interval.tmp <- sapply(camop_daily_intervals[run_these], intersect.Interval.fast, start_to_end[i])

    # assign values to camera operation matrix
    camOp_empty[i, run_these] <- time_length(interval.tmp, unit = "days")
    
    
    # if problems are defined, subtract those from the camera operation values
    if(hasProblems) {
      if(any(!is.na(problem_intervals_by_row[[i]]))){
        if(!all(problem_intervals_by_row[[i]] %within% start_to_end[i], na.rm = TRUE)) stop(paste(CTtable[i,stationCol], ": problem intervals are not within interval from setup to retrieval"))
        
        interval.tmp.prob <- sapply(camop_daily_intervals[run_these], intersect.Interval.fast, problem_intervals_by_row[[i]])   # intersection of day and total trapping period
        # total Problem value per day
        if(inherits(interval.tmp.prob, "array")) {
          fraction_to_remove <- time_length(colSums(interval.tmp.prob, na.rm = TRUE), unit = "days")   # if mutliple problem periods are defined, they show up as rows here and are combined with colSums
        } else {
          fraction_to_remove <- time_length(interval.tmp.prob, unit = "days")
        }
        # replace NA with 0 
        fraction_to_remove <- ifelse(is.na(fraction_to_remove), 0, fraction_to_remove)
        # assign values to camera operation matrix
        camOp_empty[i,run_these] <- camOp_empty[i,run_these] - fraction_to_remove
      }
    }
    
    if(any(camOp_empty[i, run_these] < 0)) stop(paste("Negative effort calculated in", rownames(camOp_empty)[i], "on:",
                                                      paste(names(which(camOp_empty[i, run_these] < 0)), collapse = ", "), "\n",
                                                      "Check for overlapping dates in Problem columns"))
  }
  
  #camOp_filled <- camOp_empty #
  camOp_filled <- round(camOp_empty, 4)   # to account for slight imprecision because daily interval is 86399 seconds, not 86400s
  
  
  if(occasionStartTime != 0) colnames(camOp_filled) <- paste0(colnames(camOp_filled), "+", occasionStartTime, "h")
  
  if(isTRUE(cameraColInArgs)){   # there is a camera column, i.e., potentially > 1 cameras per station
    
    if(isFALSE(byCamera)){        # if aggregate to station level (byCamera = FALSE)
      
      separatorSession <- "__SESS_"
      
      # byCamera = FALSE, allCamsOn = TRUE, camerasIndependent = TRUE
      if(allCamsOn){
        if(camerasIndependent){
          # if all cameras have full effort, value = sum. 
          if(effortAsFraction){
            # If any camera is not active whole day, use minimum value x n_cameras (fraction of day that all cameras were active)
            # doesn't make much sense, but included for completeness sake
            dat2 <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                      CTtable[, c(sessionCol)]), 
                              FUN = function(x) ifelse(all(x == 1), sum(x), min(x) * length(x)))
            
          } else {
            # if all cameras have full effort, value = sum. If any camera is not active whole day, count station as inactive (0)
            dat2 <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                      CTtable[, c(sessionCol)]), 
                              FUN = function(x) ifelse(all(x == 1), sum(x), 0))
          }
        }
        
        # byCamera = FALSE, allCamsOn = TRUE, camerasIndependent = FALSE
        if(isFALSE(camerasIndependent)) {
          
          if(effortAsFraction){
            # if all cameras at station active, effort = 1. If any camera inactive, 0. If any camera partially active, the fraction of day for that particular camera.
            dat2 <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                      CTtable[, c(sessionCol)]),
                              FUN = min)
          } else {
            # if all cameras at station active, effort = 1. If any camera not active, 0
            dat2 <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                      CTtable[, c(sessionCol)]), 
                              FUN = function(x) ifelse(all(x == 1), 1, 0))  
          }
        }
        
        # asssign row names (adding session ID, if applicable)
        if(sessionColInArgs)  row.names(dat2) <- paste(dat2[,1], dat2[,2], sep = separatorSession)
        if(!sessionColInArgs) row.names(dat2) <- dat2[,1]
        
        # remove station & session columns
        dat2[,c(1,2)] <- NULL
      } 
      
      # byCamera = FALSE, allCamsOn = FALSE,
      if(isFALSE(allCamsOn)){
        # allCamsOn = FALSE means that not all cameras need to be active simultaneously for the station to be considered active.
        
        # byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = TRUE
        if(camerasIndependent){
          # sum of daily effort from multiple cameras by station (complete days or fraction days, depending on input dateFormat)
          # same for effortAsFraction = TRUE or FALSE
          dat2    <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                       CTtable[, c(sessionCol)]), 
                               FUN = sum, na.rm = TRUE)
        } 
        
        # byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE
        if(isFALSE(camerasIndependent)){
          
          if(effortAsFraction){
            # if days as fraction, effort per station = mean fraction per day of cameras
            dat2    <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                         CTtable[, c(sessionCol)]), 
                                 FUN = mean, na.rm = TRUE)
          } else {
            # if days NOT as fraction, effort per station = sum of cameras days
            dat2    <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                         CTtable[, c(sessionCol)]), 
                                 FUN = function(x) ifelse(sum(x, na.rm = TRUE) >= 1, 1, sum(x, na.rm = TRUE)))
          }
        }
        
        # find cells that are NA for all cameras
        dat2.na <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                     CTtable[, c(sessionCol)]), 
                             FUN = function(X){all(is.na(X))})     # TRUE if no camera was set up that day
        
        # asssign row names (adding session ID, if applicable)
        if(sessionColInArgs)  row.names(dat2) <- row.names(dat2.na) <- paste(dat2[,1], dat2[,2], sep = separatorSession)
        if(!sessionColInArgs) row.names(dat2) <- row.names(dat2.na) <- dat2[,1]
        
        # remove station & session columns
        dat2[,c(1,2)] <- dat2.na[,c(1,2)] <- NULL
        
        dat2     <- as.matrix(dat2)
        dat2.na  <- as.matrix(dat2.na)
        
        # fill in NAs in matrix (when all cameras were NA)
        if(any(dat2.na))  dat2[which(dat2.na)] <- NA
        
        # if cameras are not independent and effort by day (not hour), replace 2 or higher with 1
        if(camerasIndependent == FALSE & !effortAsFraction){
          dat2 <- ifelse(dat2 >= 2, 1, dat2)
        }
        dat2 <- as.data.frame(dat2)
      }
    } else {   # belongs to:    if(isFALSE(byCamera)){  
      dat2 <- as.data.frame(camOp_filled)
    }   # end if(cameraColInArgs)
  } else {    # belongs to: if(cameraColInArgs)
    # if only station information, no camera level information
    dat2 <- as.data.frame(camOp_filled)
  } # end if(cameraColInArgs)
  
  if(writecsv == TRUE){
    
    # assemble parts of outfile name (according to function arguments)
    hasProblemsString <- ifelse(isTRUE(hasProblems), "with_problems_", "")
    
    if(cameraColInArgs){
      byCameraString <- ifelse(byCamera, "by_camera", "by_station")
    } else {
      byCameraString <- "by_station"
    }
    if(sessionColInArgs) byCameraString <- paste(byCameraString, "_by_session", sep = "")
    
    filename.out <- paste("CameraOperationMatrix_", byCameraString, "_", hasProblemsString, Sys.Date(), ".csv", sep = "")
    
    if(hasArg(outDir) == FALSE){
      setwd(getwd())
      write.csv(dat2, file = filename.out,
                row.names = TRUE)
    } else {
      setwd(outDir)
      write.csv(dat2, file = filename.out,
                row.names = TRUE)
    }
    if(missing(outDir)) message(paste("writecsv was TRUE, but outDir was not defined. Saved camera operation matrix in:", getwd(), sep = "   "))
  }
  return(as.matrix(dat2))
}
