surveyReport <- function(recordTable,
                         CTtable,
                         camOp,
                         speciesCol = "Species",
                         stationCol = "Station",
                         cameraCol,
                         setupCol,
                         retrievalCol,
                         CTDateFormat = "%Y-%m-%d",
                         CTHasProblems = "deprecated",
                         recordDateTimeCol = "DateTimeOriginal",
                         recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                         Xcol,
                         Ycol,
                         sinkpath,
                         makezip
){
  
  # To do: 
  # - setup/retrieval with hour (seems to work, when using base R date-time string is converted to date.)
  # - sessionCol? As long as camOp and CTtable match it should be fine. But all aggregate commands need a session column (dummy if only one session).
  # - explanation in vignette / help file
  
  
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
  if(any(camOp < 0, na.rm = TRUE)) stop("camOp may only contain positive values (set scaleEffort = FALSE in cameraOperation().")
  
  camop.info.df <- deparseCamOpRownames(camOp)   # only works if camOp is station-specific
  if("camera" %in% colnames(camop.info.df))  stop("camera operation matrix is camera-specific. Please provide a camera operation matrix that is by station (byCamera = FALSE).\nThis is necessary for correctly calculating the number of active days (taking into account whether cameras were independent or not).", call. = FALSE)
  if("session" %in% colnames(camop.info.df)) stop("camera operation matrix contains sessions. This is currently not supported. Please run the survey report separately for the different sessions.", call. = FALSE)
  
  
  
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
  
  
  if(all(as.character(unique(recordTable[,stationCol])) %in% CTtable[,stationCol]) == FALSE){
    (stop("Not all values of stationCol in recordTable are matched by values of stationCol in CTtable"))
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
  n_cameras.tmp   <- aggregate(CTtable[,stationCol],
                               list(CTtable[,stationCol]),
                               FUN = length)
  
  
  station_dates <- data.frame(station     = station.tmp1[,1], 
                              setup       = station.tmp1[,2],
                              image_first = image.tmp1[,2],
                              image_last  = image.tmp2[,2],
                              retrieval   = station.tmp2[,2],
                              n_cameras   = n_cameras.tmp[,2])
  
  n_calendar_days_total <- rowSums(camOp >= 0, na.rm =TRUE)
  
  # Calendar days on which cameras were active, by row.
  n_calendar_days_active <- rowSums(camOp > 0, na.rm =TRUE)
  
  # sum of inactive calendar days (by row)
  n_calendar_days_inactive <- rowSums(camOp == 0, na.rm =TRUE)
  
  # trap-nights (counting independent cameras separately)
  # per station, number of active cameras * number of active days
  n_trap_nights_active <- rowSums(camOp, na.rm =TRUE) 
  
  # make data frame with the number of active / inactive days
  if(!all(station_dates$station == camop.info.df$station)) stop("Mismatch between station column in CTtable and camOp")
  
  camop.info.df2 <- data.frame(camop.info.df,
                               n_calendar_days_total    = n_calendar_days_total,    # this is 1 for setup/retrieval on consecutive days
                               n_calendar_days_active   = n_calendar_days_active,   # this is 2 for setup/retrieval on consecutive days
                               n_calendar_days_inactive = n_calendar_days_inactive, # number of days stations was inactive
                               n_trap_nights_active       = n_trap_nights_active)       # this is precisely how long the cameras were active ( 1 for setup/retrieval on consecutive days - for each camera).
  
  
  
  station.info.combined <- merge(station_dates, camop.info.df2, by = "station")
  
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
                         camOp                = camOp,
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
