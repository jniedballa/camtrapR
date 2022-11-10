#' Create a report about a camera trapping survey and species detections
#' 
#' This function creates a report about a camera trapping survey and species
#' records. It uses a camera trap station information table and a record table
#' (generated with \code{\link{recordTable}}) as input. Output tables can be
#' saved and a zip file for simple data sharing can be created easily.
#' 
#' 
#' \code{dateFormat} defaults to "YYYY-MM-DD", e.g. "2014-10-31". It can be
#' specified either in the format required by \code{\link[base]{strptime}} or
#' the 'orders' argument in \code{\link[lubridate]{parse_date_time}} in
#' \pkg{lubridate}. In the example above, "YYYY-MM-DD" would be specified as
#' "\%Y-\%m-\%d" or "ymd".
#' 
#' \code{recordDateTimeFormat} defaults to the "YYYY-MM-DD HH:MM:SS"
#' convention, e.g. "2014-09-30 22:59:59". \code{recordDateTimeFormat} can be
#' interpreted either by base-R via \code{\link[base]{strptime}} or in
#' \pkg{lubridate} via \code{\link[lubridate]{parse_date_time}} (argument
#' "orders"). \pkg{lubridate} will be used if there are no "\%" characters in
#' \code{recordDateTimeFormat}.
#' 
#' For "YYYY-MM-DD HH:MM:SS", \code{recordDateTimeFormat} would be either
#' "\%Y-\%m-\%d \%H:\%M:\%S" or "ymd HMS". For details on how to specify date
#' and time formats in R see \code{\link[base]{strptime}} or
#' \code{\link[lubridate]{parse_date_time}}.
#' 
#' Note: as of version 2.1, argument \code{CTHasProblems} is deprecated and
#' defunct. Please use \code{camOp} instead to provide information about
#' periods of camera activity and malfunction. If \code{camOp} is not provided
#' the legacy version of surveyReport (from camtrapR 2.0.3) will be run with a
#' warning.
#' 
#' @param recordTable data.frame containing a species record table as given by
#' \code{\link{recordTable}}
#' @param CTtable data.frame containing information about location and trapping
#' period of camera trap stations (equivalent to \code{\link{camtraps}}
#' @param camOp camera operation matrix created with
#' \code{\link{cameraOperation}}
#' @param speciesCol character. name of the column specifying Species ID in
#' \code{recordTable}
#' @param stationCol character. name of the column specifying Station ID in
#' \code{CTtable} and \code{recordTable}
#' @param cameraCol character. name of the column specifying Camera ID in
#' \code{CTtable} and \code{recordTable}
#' @param setupCol character. name of the column containing camera setup dates
#' in \code{CTtable}
#' @param retrievalCol character. name of the column containing camera
#' retrieval dates in \code{CTtable}
#' @param CTDateFormat character. The format of columns \code{setupCol} and
#' \code{retrievalCol} (and potential problem columns) in \code{CTtable}. Must
#' be interpretable by either \code{as.Date} or the "orders" argument
#' \code{\link[lubridate]{parse_date_time}} in \pkg{lubridate}.
#' @param CTHasProblems deprecated (since version 2.1)
#' @param recordDateTimeCol character. The name of the column containing date
#' and time of records in \code{recordTable}
#' @param recordDateTimeFormat character. The date/time format of column
#' \code{recordDateTimeCol} in \code{recordTable}.
#' @param Xcol character. name of the column specifying x coordinates in
#' \code{CTtable}. Used to create detection maps if \code{makezip} is TRUE.
#' (optional)
#' @param Ycol character. name of the column specifying y coordinates in
#' \code{CTtable}. Used to create detection maps if \code{makezip} is TRUE.
#' (optional)
#' @param sinkpath character. The directory into which the survey report is
#' saved (optional)
#' @param makezip logical. Create a zip file containing tables, plots and maps
#' in \code{sinkpath}?
#' 
#' @return An invisible list containing 5 \code{data.frames}.
#' 
#' \item{survey_dates}{station and image date ranges, number of total and
#' active trap days (calendar days and taking into account independent effort
#' of multiple cameras, if applicable), number of cameras per station}
#' \item{species_by_station}{species numbers by station}
#' \item{events_by_species}{number of events and stations by species}
#' \item{events_by_station}{number of events for every species by station (only
#' species that were recorded)} \item{events_by_station2}{number of events for
#' all species at all stations (including species that were not recorded)}
#' 
#' The output will be saved to a .txt file if \code{sinkpath} is defined.
#' 
#' If \code{makezip} is TRUE, a zip file will be created in \code{sinkpath}. It
#' contains single-species activity plots, detection maps (if \code{Xcol} and
#' \code{Ycol} are defined), the survey report tables, the record table and the
#' camera trap station table, and an example R script.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{recordTable}}
#' 
#' @examples
#' 
#' data(camtraps)
#' data(recordTableSample)
#' 
#' # since version 2.1, camera operation matrix is required as input
#' 
#' camop_no_problem <- cameraOperation(CTtable      = camtraps,
#'                                     stationCol   = "Station",
#'                                     setupCol     = "Setup_date",
#'                                     retrievalCol = "Retrieval_date",
#'                                     writecsv     = FALSE,
#'                                     hasProblems  = FALSE,
#'                                     dateFormat   = "dmy"
#' )
#' 
#' reportTest <- surveyReport (recordTable          = recordTableSample,
#'                             CTtable              = camtraps,
#'                             camOp                = camop_no_problem,
#'                             speciesCol           = "Species",
#'                             stationCol           = "Station",
#'                             setupCol             = "Setup_date",
#'                             retrievalCol         = "Retrieval_date",
#'                             CTDateFormat         = "dmy", 
#'                             recordDateTimeCol    = "DateTimeOriginal",
#'                             recordDateTimeFormat = "ymd HMS")
#' 
#' class(reportTest)  # a list with
#' length(reportTest) # 5 elements
#' 
#' reportTest[[1]]    # camera trap operation times and image date ranges
#' reportTest[[2]]    # number of species by station
#' reportTest[[3]]    # number of events and number of stations by species
#' reportTest[[4]]    # number of species events by station
#' reportTest[[5]]    # number of species events by station including 0s (non-observed species)
#' 
#' # with camera problems
#' 
#' camop_problem <- cameraOperation(CTtable      = camtraps,
#'                                  stationCol   = "Station",
#'                                  setupCol     = "Setup_date",
#'                                  retrievalCol = "Retrieval_date",
#'                                  writecsv     = FALSE,
#'                                  hasProblems  = TRUE,
#'                                  dateFormat   = "dmy"
#' )
#' 
#' reportTest_problem <- surveyReport (recordTable          = recordTableSample,
#'                                     CTtable              = camtraps,
#'                                     camOp                = camop_problem,
#'                                     speciesCol           = "Species",
#'                                     stationCol           = "Station",
#'                                     setupCol             = "Setup_date",
#'                                     retrievalCol         = "Retrieval_date",
#'                                     CTDateFormat         = "dmy", 
#'                                     recordDateTimeCol    = "DateTimeOriginal",
#'                                     recordDateTimeFormat = "ymd HMS")
#' 
#' reportTest_problem$survey_dates
#' 
#' 
#' 
#' ## if camOp is missing, the legacy version (from 2.0.3) will be used:
#' 
#' reportTest_problem_old <- surveyReport (recordTable          = recordTableSample,
#'                                         CTtable              = camtraps,
#'                                        # camOp                = camop_problem,
#'                                         speciesCol           = "Species",
#'                                         stationCol           = "Station",
#'                                         setupCol             = "Setup_date",
#'                                         retrievalCol         = "Retrieval_date",
#'                                         CTDateFormat         = "dmy", 
#'                                         recordDateTimeCol    = "DateTimeOriginal",
#'                                         recordDateTimeFormat = "ymd HMS")
#' 
#' \dontrun{
#' # run again with sinkpath defined
#' reportTest <- surveyReport (recordTable          = recordTableSample,
#'                             CTtable              = camtraps,
#'                             camOp                = camop_no_problem,
#'                             speciesCol           = "Species",
#'                             stationCol           = "Station",
#'                             setupCol             = "Setup_date",
#'                             retrievalCol         = "Retrieval_date",
#'                             CTDateFormat         = "dmy",, 
#'                             recordDateTimeCol    = "DateTimeOriginal",
#'                             recordDateTimeFormat = "ymd HMS",
#'                             sinkpath             = getwd())
#' 
#' # have a look at the text file
#' readLines(list.files(getwd(), pattern = paste("survey_report_", Sys.Date(), ".txt", sep = ""), 
#' 					 full.names = TRUE))
#' }
#' 
#' @export surveyReport
#' 
surveyReport <- function(recordTable,
                         CTtable,
                         camOp,
                         speciesCol = "Species",
                         stationCol = "Station",
                         cameraCol,
                         setupCol,
                         retrievalCol,
                         CTDateFormat = "ymd",
                         CTHasProblems = "deprecated",
                         recordDateTimeCol = "DateTimeOriginal",
                         recordDateTimeFormat = "ymd HMS",
                         Xcol,
                         Ycol,
                         sinkpath,
                         makezip
){
  
  # To do: 
  # - setup/retrieval with hour (seems to work, when using base R date-time string is converted to date.)
  # - sessionCol? As long as camOp and CTtable match it should be fine. But all aggregate commands need a session column (dummy if only one session).
  # - explanation in vignette / help file
  
  
  
  # for users running old (<= 2.0.3) code: redirect to legacy version of surveyReport if camop not defined
  
  if(!hasArg(camOp)){
    warning("Argument 'camOp' is missing (expected since camtrapR 2.1). Will run legacy version of surveyReport (from camtrapR 2.0.3). See:  news(package = 'camtrapR') and ?surveyReport. This warning may be raised to an error in the future, so please adapt your code.",
            immediate. = TRUE)
    args_names <- ls()
    args <- mget(args_names)
    args_subset <- args[!sapply(args, is.symbol)]
    out <- do.call(surveyReport_legacy, args_subset)
    return(out)
  }
  
  
  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, 
                              setupCol = setupCol, 
                              retrievalCol = retrievalCol,
                              recordDateTimeCol = recordDateTimeCol, 
                              speciesCol = speciesCol)
  
  # check input
  if(!hasArg(recordTable))         stop("'recordTable' is not defined", call. = FALSE)
  if(!is.data.frame(recordTable))  stop("'recordTable' must be a data.frame", call. = FALSE)
  if(hasArg(CTHasProblems))        message("Since version 2.1, argument CTHasProblems is deprecated. It was replaced with the new argument camOp. Periods when cameras malfunctioned are now assinged via camOp.")
  if(!hasArg(camOp))               stop("'camOp' is not defined", call. = FALSE)
  if(!inherits(camOp, "matrix"))   stop("camOp must be a matrix", call. = FALSE)
  if(any(camOp < 0, na.rm = TRUE)) stop("camOp may only contain positive values")
  
  camop.info.df <- deparseCamOpRownames(camOp)   # only works if camOp is station-specific
  if("camera" %in% colnames(camop.info.df))  stop("camera operation matrix is camera-specific. Please provide a camera operation matrix that is by station (byCamera = FALSE).\nThis is necessary for correctly calculating the number of active days (taking into account whether cameras were independent or not).", call. = FALSE)
  if("session" %in% colnames(camop.info.df)) stop("camera operation matrix contains sessions. This is currently not supported. Please run the survey report separately for the different sessions.", call. = FALSE)
  colnames(camop.info.df) <- stationCol
  
  
  CTtable     <- dataFrameTibbleCheck(df = CTtable)
  recordTable <- dataFrameTibbleCheck(df = recordTable)
  
  if(!stationCol %in% colnames(CTtable))    stop(paste('stationCol = "',   stationCol,   '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!setupCol %in% colnames(CTtable))      stop(paste('setupCol = "',     setupCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!retrievalCol %in% colnames(CTtable))  stop(paste('retrievalCol = "', retrievalCol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
  
  if(!stationCol %in% colnames(recordTable))         stop(paste('stationCol = "',   stationCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable))  stop(paste('recordDateTimeCol = "', recordDateTimeCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))         stop(paste('speciesCol = "', speciesCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  
  # make columns character
  recordTable[,speciesCol]        <- as.character(recordTable[,speciesCol])
  recordTable[,stationCol]        <- as.character(recordTable[,stationCol])
  recordTable[,recordDateTimeCol] <- as.character(recordTable[,recordDateTimeCol])
  
  CTtable[,stationCol]    <- as.character(CTtable[,stationCol])
  CTtable[,setupCol]      <- as.character(CTtable[,setupCol])
  CTtable[,retrievalCol]  <- as.character(CTtable[,retrievalCol])
  
  
  
  if(hasArg(makezip)){
    stopifnot(is.logical(makezip))
  } else {
    makezip <- FALSE
  }
  
  if(isTRUE(makezip)){
    if(hasArg(sinkpath) == FALSE) stop("if makezip is TRUE, please define sinkpath")
    if(!dir.exists(sinkpath)) stop("Could not find sinkpath:\n", sinkpath, call. = FALSE)
    
    if(Sys.getenv("R_ZIPCMD") == "" & 
       Sys.getenv("zip") == "") {
      if(isFALSE(requireNamespace("zip", quietly = TRUE))) {
        stop('cannot create zip file. Try installing the package "zip" via:  install.packages("zip")', call. = FALSE)
      } else {
        requireNamespace("zip")
        usePackageZip <- TRUE
      }
    } else {
      usePackageZip <- FALSE
    }
  }
  
  if(hasArg(cameraCol)){
    if(cameraCol %in% colnames(CTtable) == FALSE) stop(paste(cameraCol, "is not a column of CTtable"))
  } else {
    if(any(table(CTtable[,stationCol]) > 1)){
      stop("at least 1 station has more than 1 item in CTtable. Please specify 'cameraCol'")
    }
  }
  
  if(hasArg(Xcol)){
    stopifnot(hasArg(Ycol))
    stopifnot(c(Xcol, Ycol) %in% colnames(CTtable))
    CTtable[,Xcol] <- as.numeric(as.character(CTtable[,Xcol]))
    CTtable[,Ycol] <- as.numeric(as.character(CTtable[,Ycol]))
  } 
  
  
  recordTable$DateTime2 <- parseDateTimeObject(inputColumn = recordTable[,recordDateTimeCol],
                                               dateTimeFormat = recordDateTimeFormat,
                                               timeZone = "UTC")
  recordTable$Date2 <- as.Date(recordTable$DateTime2, tz = "UTC")
  
  
  # if(all(as.character(unique(recordTable[,stationCol])) %in% CTtable[,stationCol]) == FALSE){
  #   (stop("Not all values of stationCol in recordTable are matched by values of stationCol in CTtable"))
  # }
  if(!all(as.character(unique(recordTable[,stationCol])) %in% CTtable[,stationCol])){
    stop(paste("Not all values of stationCol in recordTable are matched by values of stationCol in CTtable:\n",
               paste(as.character(unique(recordTable[,stationCol])) [!as.character(unique(recordTable[,stationCol])) %in% CTtable[,stationCol]], collapse = ", ")
    ))
  }
  
  
  # check date columns and format
  CTtable[,setupCol]     <- parseDateObject(inputColumn = CTtable[,setupCol],     CTDateFormat, checkNA = TRUE, checkEmpty = TRUE)
  CTtable[,retrievalCol] <- parseDateObject(inputColumn = CTtable[,retrievalCol], CTDateFormat, checkNA = TRUE, checkEmpty = TRUE)
  
  
  # station and image date ranges
  station.tmp1 <- aggregate(CTtable[,setupCol],
                            list(CTtable[,stationCol]),
                            FUN = min)
  station.tmp2 <- aggregate(CTtable[,retrievalCol],
                            list(CTtable[,stationCol]),
                            FUN = max)
  image.tmp1   <- aggregate(recordTable$Date2,
                            list(recordTable[,stationCol]),
                            FUN = min)
  image.tmp2   <- aggregate(recordTable$Date2,
                            list(recordTable[,stationCol]),
                            FUN = max)
  n_cameras.tmp   <- aggregate(CTtable[,stationCol],  # is this correct when sessionCol is present?
                               list(CTtable[,stationCol]),
                               FUN = length)

  colnames(n_cameras.tmp) <- c(stationCol, "n_cameras")

  
  df1  <- data.frame(station.tmp1, station.tmp2[,2])
  colnames(df1) <- c(stationCol, "setup", "retrieval")
  df2  <- data.frame(image.tmp1, image.tmp2[,2])
  colnames(df2) <- c(stationCol, "image_first", "image_last")
  
  station_dates <- merge(merge(df1, 
                               df2, 
                               by = stationCol, 
                               all = T), 
                         n_cameras.tmp)
  
  # station_dates <- data.frame(station     = station.tmp1[,1], 
  #                             setup       = station.tmp1[,2],
  #                             image_first = image.tmp1[,2],
  #                             image_last  = image.tmp2[,2],
  #                             retrieval   = station.tmp2[,2],
  #                             n_cameras   = n_cameras.tmp[,2])
  
  n_calendar_days_total <- rowSums(camOp >= 0, na.rm =TRUE)
  
  # Calendar days on which cameras were active, by row.
  n_calendar_days_active <- rowSums(camOp > 0, na.rm =TRUE)
  
  # sum of inactive calendar days (by row)
  n_calendar_days_inactive <- rowSums(camOp == 0, na.rm =TRUE)
  
  # trap-nights (counting independent cameras separately)
  # per station, number of active cameras * number of active days
  n_trap_nights_active <- rowSums(camOp, na.rm =TRUE) 
  
  
  
  #######################
  # # Legacy calculation of active days - for the sake of continuity - full old code
  
  CTHasProblems <- TRUE
  
  # check that problems are arranged in order 1,2,3,...
  cols.prob.from <- grep(colnames(CTtable), pattern = "Problem\\d\\Sfrom")
  cols.prob.to   <- grep(colnames(CTtable), pattern = "Problem\\d\\Sto")
  
  if(length(cols.prob.from) == 0 | length(cols.prob.to) == 0) CTHasProblems <- FALSE 
  if(!all(order(colnames(CTtable)[cols.prob.from]) == seq(1:length(cols.prob.from)))) CTHasProblems <- FALSE 
  if(!all(order(colnames(CTtable)[cols.prob.to])   == seq(1:length(cols.prob.to)))) CTHasProblems <- FALSE 
  if(length(cols.prob.from) != length(cols.prob.to)) CTHasProblems <- FALSE 
  if(all(is.na(CTtable[,cols.prob.from])) | all(is.na(CTtable[,cols.prob.to]))) CTHasProblems <- FALSE 
  
  if(isTRUE(CTHasProblems)){    # camera problem columns
    
    # convert problem column entries to character
    for(problem_col_index in c(cols.prob.from, cols.prob.to)){
      CTtable[, problem_col_index] <- as.character(CTtable[, problem_col_index])
    }
    
    n_days_inactive_legacy <- data.frame(matrix(NA,
                                         ncol = length(cols.prob.from),
                                         nrow = nrow(CTtable)))
    
    for(xy in 1:length(cols.prob.from)){
      
      CTtable[, cols.prob.from[xy]] <- parseDateObject(inputColumn = CTtable[, cols.prob.from[xy]], CTDateFormat, checkNA = FALSE, checkEmpty = FALSE)
      CTtable[, cols.prob.to[xy]]   <- parseDateObject(inputColumn = CTtable[, cols.prob.to[xy]],   CTDateFormat, checkNA = FALSE, checkEmpty = FALSE)
      
      n_days_inactive_legacy[,xy] <- CTtable[cols.prob.to[xy]] - CTtable[cols.prob.from[xy]]       # compute number of inactive trap nights
      # EDIT 2021-01-17: the line above computes time difference between start and end of problem. So yesterday and end today = 1 day, not 2 days as in the new version
      # basically, that's assuming problems began / ended at noon (for the sake of computing the effort)
      # hence, the number of inactive days was 1 less in the old version compared to the new version. 
      # if problem_to = retrieval, difference will only be 0.5 per camera (because in new version, retrieval is assumed to be 12 noon)
      
      n_days_inactive_legacy[,xy] <- as.integer(n_days_inactive_legacy[,xy])
    }

    n_days_inactive_rowsum_legacy <- rowSums(n_days_inactive_legacy, na.rm = TRUE)
  } else {
    n_days_inactive_rowsum_legacy <- rep(0, times = nrow(CTtable))
  }
  stopifnot(length(n_days_inactive_rowsum_legacy) == nrow(CTtable))
  
  n_days_inactive_rowsum_legacy <- aggregate(n_days_inactive_rowsum_legacy,
                                             by    = list(CTtable[,stationCol]),
                                             FUN   = sum,
                                             na.rm = TRUE)

  n_nights_total_legacy      <- as.integer(CTtable[,retrievalCol] - CTtable[,setupCol])
  n_nights_total_agg_legacy  <- aggregate(n_nights_total_legacy,
                                          by  = list(CTtable[,stationCol]),
                                          FUN = sum)
  # order properly
  n_nights_total_agg_legacy <- n_nights_total_agg_legacy[match(camop.info.df[, stationCol], n_nights_total_agg_legacy$Group.1),]
  n_days_inactive_rowsum_legacy <- n_days_inactive_rowsum_legacy[match(camop.info.df[, stationCol], n_days_inactive_rowsum_legacy$Group.1),]
  
  
  n_nights_active_legacy    <- n_nights_total_agg_legacy[,2] - n_days_inactive_rowsum_legacy[,2]

  
  ############################
  # make data frame with the number of active / inactive days
  if(!all(station_dates$station == camop.info.df$station)) stop("Mismatch between station column in CTtable and camOp")
  
  camop.info.df2 <- data.frame(camop.info.df,
                               n_calendar_days_total    = n_calendar_days_total,    # this is 1 for setup/retrieval on consecutive days
                               n_calendar_days_active   = n_calendar_days_active,   # this is 2 for setup/retrieval on consecutive days
                               n_calendar_days_inactive = n_calendar_days_inactive, # number of days stations was inactive
                               n_trap_nights_active     = n_trap_nights_active      # this is precisely how long the cameras were active ( 1 for setup/retrieval on consecutive days - for each camera).
                               # n_nights_active_legacy   = n_nights_active_legacy,
                               # n_nights_total_legacy    = n_nights_total_agg_legacy$x
                               )       
  
  
  
  station.info.combined <- merge(station_dates, 
                                 camop.info.df2, 
                                 by = stationCol)
  
  # adjust options for printing results
  options.tmp <- options()
  on.exit(options(options.tmp))
  options(max.print=1e6)
  options(width = 1000)
  
  
  # sink/print output
  if(hasArg(sinkpath)){
    sinkfile <- file.path(sinkpath, paste("survey_report_", Sys.Date(), ".txt", sep = ""))
    sink(file = sinkfile)
    print(paste("Survey Report generated", Sys.Date() ))
  }
  cat("\n-------------------------------------------------------\n")
  print(paste("Total number of stations: ", length(unique(CTtable[,stationCol]))))
  
  cat("\n-------------------------------------------------------\n")
  print(paste("Number of operational stations: ", sum(station.info.combined$n_trap_nights_active > 0)))
  
  if(hasArg(cameraCol)){
    cat("\n-------------------------------------------------------\n")
    print(paste("Total number of cameras: ", length(unique(paste(CTtable[,stationCol], CTtable[,cameraCol], sep = "_")))))
  }
  
  cat("\n-------------------------------------------------------\n")
  print(paste("Trap nights (number of active 24 hour cycles completed by independent cameras): ",
              sum(station.info.combined$n_trap_nights_active)
  ))
  
  # cat("\n-------------------------------------------------------\n")
  # print(paste("n nights with cameras set up and active (trap nights - LECAGY CALCULATION - WHOLE DAYS): ",
  #             sum(station.info.combined$n_nights_active_legacy)
  # ))
  
  # cat("\n-------------------------------------------------------\n")
  # print(paste("n nights with cameras set up (LECAGY CALCULATION - WHOLE DAYS): ",
  #             sum(station.info.combined$n_nights_total_legacy)
  # ))
  
  cat("\n-------------------------------------------------------\n")
  print(paste("Calendar days with cameras set up (operational or not): ",
              sum(station.info.combined$n_calendar_days_total)
  ))
  
  cat("\n-------------------------------------------------------\n")
  print(paste("Calendar days with cameras set up and active: ",
              sum(station.info.combined$n_calendar_days_active)
  ))
  
  cat("\n-------------------------------------------------------\n")
  print(paste("Calendar days with cameras set up but inactive: ",
              sum(station.info.combined$n_calendar_days_inactive)
  ))
  
  cat("\n-------------------------------------------------------\n")
  print(paste("total trapping period: ", paste(min(station.info.combined$setup), max(station.info.combined$retrieval), sep = " - ")))
  
  
  
  # total number of independent records by species
  species_record_table <- data.frame(species    = rep(NA, times = length(unique(recordTable[, speciesCol]))),
                                     n_events   = rep(NA, times = length(unique(recordTable[, speciesCol]))), 
                                     n_stations = rep(NA, times = length(unique(recordTable[, speciesCol]))))
  
  for(i in 1:length(unique(recordTable[, speciesCol]))){
    
    tmp                           <- unique(recordTable[, speciesCol])[i]
    subset.tmp                    <- subset(recordTable, recordTable[, speciesCol] == tmp)
    species_record_table[i, ]     <- c(tmp, nrow(subset.tmp), length(unique(subset.tmp[,stationCol])))
    rm(subset.tmp, tmp)
  }
  species_record_table2           <- species_record_table[order(species_record_table$species),]
  rownames(species_record_table2) <- NULL
  
  # total number of independent records by station
  
  # only species that were recorded
  station_record_table1           <- aggregate(recordTable[,1], by = list(recordTable[,stationCol],recordTable[,speciesCol]), FUN = length)
  colnames(station_record_table1) <- c(stationCol, speciesCol, "n_events")
  station_record_table1           <- station_record_table1[order(station_record_table1[,stationCol], station_record_table1[,speciesCol]),]
  rownames(station_record_table1) <- NULL
  
  #including all species and 0s
  station_record_table           <- expand.grid(sort(unique(recordTable[,stationCol])), sort(unique(recordTable[,speciesCol])))
  station_record_table           <- data.frame(station_record_table, n_events = 0)
  colnames(station_record_table) <- c(stationCol, speciesCol, "n_events")
  rownames(station_record_table) <- NULL
  # species lists by station
  
  n_spec_by_station           <- aggregate(station_record_table1[,speciesCol], by = list(station_record_table1[,stationCol]), FUN = length)
  colnames(n_spec_by_station) <- c(stationCol, "n_species")
  rownames(n_spec_by_station) <- NULL
  
  for(j in 1:length(unique(recordTable[, stationCol]))){
    
    tmp                      <- unique(recordTable[, stationCol])[j]
    subset.tmp               <- table(subset(recordTable, recordTable[, stationCol] == tmp)[,speciesCol] )
    
    station_record_table.tmp <- station_record_table[station_record_table[, stationCol] == tmp,]
    station_record_table.tmp$n_events[match(names(subset.tmp), station_record_table.tmp[,speciesCol])] <- subset.tmp
    
    station_record_table[station_record_table[, stationCol] == tmp,] <- station_record_table.tmp
    rm(station_record_table.tmp)
  }
  station_record_table2 <-  station_record_table[order(station_record_table[,stationCol], station_record_table[,speciesCol]),]
  rownames(station_record_table2) <- NULL
  
  if(hasArg(sinkpath)){
    cat("\n\n-------------------------------------------------------\n\n")
    print(" survey station and image date ranges")
    print(station.info.combined) #print(date_range_combined)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of species by station")
    print(n_spec_by_station)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of events and station by species")
    print(species_record_table2)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of events and species by station (only species that were recorded at stations)")
    print(station_record_table1)
    cat("\n\n-------------------------------------------------------\n\n")
    print(" number of events and species by station (all species, all stations, including species that were not recorded)")
    print(station_record_table2)
    sink()
    message("saved output to file \n",
            paste(sinkfile, "\n\n"))
  }
  
  output <- list(station.info.combined, #date_range_combined, 
                 n_spec_by_station, species_record_table2, station_record_table1, station_record_table2)
  names(output) <- c("survey_dates", "species_by_station", "events_by_species",
                     "events_by_station", "events_by_station2")
  
  # make zip file
  if(isTRUE(makezip)){
    
    arglist_zip <-  list(output               = output,
                         recordTable          = recordTable,
                         CTtable              = CTtable,
                         #camOp                = camOp,
                         speciesCol           = speciesCol,
                         stationCol           = stationCol,
                         setupCol             = setupCol,
                         retrievalCol         = retrievalCol,
                         CTDateFormat         = CTDateFormat,
                         recordDateTimeCol    = recordDateTimeCol,
                         recordDateTimeFormat = recordDateTimeFormat,
                         sinkpath             = sinkpath,
                         usePackageZip        = usePackageZip)
    
    if(hasArg(cameraCol)) arglist_zip <- c(arglist_zip,  cameraCol = cameraCol)
    if(hasArg(Xcol) & hasArg(Ycol)) arglist_zip <- c(arglist_zip,  Xcol = Xcol, Ycol = Ycol)
    
    do.call(makeSurveyZip, arglist_zip)
    
  }
  
  return(invisible(output))
}
