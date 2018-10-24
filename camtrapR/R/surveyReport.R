surveyReport <- function(recordTable,
                         CTtable,
                         speciesCol = "Species",
                         stationCol = "Station",
                         cameraCol,
                         setupCol,
                         retrievalCol,
                         CTDateFormat = "%Y-%m-%d",
                         CTHasProblems = FALSE,
                         recordDateTimeCol = "DateTimeOriginal",
                         recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                         Xcol,
                         Ycol,
                         sinkpath,
                         makezip
){

  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, setupCol = setupCol, retrievalCol = retrievalCol,
                                         recordDateTimeCol = recordDateTimeCol, speciesCol = speciesCol)
  if(class(CTtable) != "data.frame") stop("CTtable must be a data.frame", call. = FALSE)
  if(class(recordTable) != "data.frame") stop("recordTable must be a data.frame", call. = FALSE)

  if(!stationCol %in% colnames(CTtable))     stop(paste('stationCol = "',   stationCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!setupCol %in% colnames(CTtable))        stop(paste('setupCol = "',   setupCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!retrievalCol %in% colnames(CTtable))  stop(paste('retrievalCol = "',   retrievalCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)

  if(!stationCol %in% colnames(recordTable))            stop(paste('stationCol = "',   stationCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable))  stop(paste('recordDateTimeCol = "', recordDateTimeCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))            stop(paste('speciesCol = "', speciesCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)


  # make columns character
  recordTable[,speciesCol]        <- as.character(recordTable[,speciesCol])
  recordTable[,stationCol]        <- as.character(recordTable[,stationCol])
  recordTable[,recordDateTimeCol] <- as.character(recordTable[,recordDateTimeCol])

  CTtable[,stationCol]    <- as.character(CTtable[,stationCol])
  CTtable[,setupCol]       <- as.character(CTtable[,setupCol])
  CTtable[,retrievalCol] <- as.character(CTtable[,retrievalCol])



  if(hasArg(makezip)){
    stopifnot(is.logical(makezip))
  } else {makezip <- FALSE}
  if(isTRUE(makezip)){
    if(hasArg(sinkpath) == FALSE) stop("if makezip is TRUE, please define sinkpath")
    if(!dir.exists(sinkpath)) stop("Could not find sinkpath:\n", sinkpath, call. = FALSE)
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
   } #else {
  #   Xcol <- Ycol <- NA
  # }

  recordTable$DateTime2 <- strptime(recordTable[,recordDateTimeCol],
                                    format = recordDateTimeFormat,
                                    tz = "UTC")
  recordTable$Date2 <- as.Date(recordTable$DateTime2, tz = "UTC")


  if("POSIXlt" %in% class(recordTable$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(recordTable$DateTime2))) stop(paste("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat. row",
                                                   paste(which(is.na(recordTable$DateTime2)), collapse = ", ")))

  if(all(as.character(unique(recordTable[,stationCol])) %in% CTtable[,stationCol]) == FALSE){
    (stop("Not all values of stationCol in recordTable are matched by values of stationCol in CTtable"))
  }

  if(any(is.na(CTtable[,setupCol])))     stop("there are NAs in setupCol")
  if(any(is.na(CTtable[,retrievalCol]))) stop("there are NAs in retrievalCol")

  if(all(is.na(as.Date(CTtable[,setupCol],     format = CTDateFormat)))) {stop("Cannot read date format in setupCol")}
  if(all(is.na(as.Date(CTtable[,retrievalCol], format = CTDateFormat)))) {stop("Cannot read date format in retrievalCol")}

  if(any(is.na(as.Date(CTtable[,setupCol],     format = CTDateFormat)))) {stop("at least one entry in setupCol cannot be interpreted using CTDateFormat")}
  if(any(is.na(as.Date(CTtable[,retrievalCol], format = CTDateFormat)))) {stop("at least one entry in retrievalCol cannot be interpreted using CTDateFormat")}


  CTtable[,setupCol]     <- as.Date(strptime(CTtable[,setupCol],     format = CTDateFormat, tz = "UTC"), tz = "UTC")
  CTtable[,retrievalCol] <- as.Date(strptime(CTtable[,retrievalCol], format = CTDateFormat, tz = "UTC"), tz = "UTC")


  if(isTRUE(CTHasProblems)){    # camera problem columns

    # check that problems are arranged in order 1,2,3,...
    cols.prob.from <- grep(colnames(CTtable), pattern = "Problem\\d\\Sfrom")
    cols.prob.to   <- grep(colnames(CTtable), pattern = "Problem\\d\\Sto")

    if(length(cols.prob.from) == 0) stop("could not find column ProblemX_from")
    if(length(cols.prob.to) == 0)   stop("could not find column ProblemX_to")

    if(all(order(colnames(CTtable)[cols.prob.from]) == seq(1:length(cols.prob.from))) == FALSE){"problem columns are not arranged correctly"}
    if(all(order(colnames(CTtable)[cols.prob.to])   == seq(1:length(cols.prob.to)))   == FALSE){"problem columns are not arranged correctly"}

    if(length(cols.prob.from) != length(cols.prob.to)){
      stop("number of 'Problem..._from' and 'Problem..._to' columns differs. Check format. Sample: 'Problem1_from', 'Problem1_to'")
    }

    n_days_inactive <- data.frame(matrix(NA,
                                         ncol = length(cols.prob.from),
                                         nrow = nrow(CTtable)))

    for(xy in 1:length(cols.prob.from)){

      if(isTRUE(unlist(strsplit(colnames(CTtable)[cols.prob.from[xy]], split = "_"))[1] !=
                unlist(strsplit(colnames(CTtable)[cols.prob.to[xy]], split = "_"))[1])) stop (
                  paste("problem columns are arranged incorrectly (",
                        colnames(CTtable)[cols.prob.from[xy]], ", ",
                        colnames(CTtable)[cols.prob.to  [xy]], ")",
                        sep = "")
                )

      CTtable[,cols.prob.from[xy]] <- as.Date(CTtable[,cols.prob.from[xy]], format = CTDateFormat, tz = "UTC")
      CTtable[,cols.prob.to[xy]]   <- as.Date(CTtable[,cols.prob.to[xy]],   format = CTDateFormat, tz = "UTC")

      if(all(is.na( CTtable[,cols.prob.from[xy]]))) stop(paste("Cannot read date format in", colnames(CTtable)[cols.prob.from[xy]]))
      if(all(is.na( CTtable[,cols.prob.to[xy]])))   stop(paste("Cannot read date format in", colnames(CTtable)[cols.prob.to[xy]]))

      n_days_inactive[,xy] <- CTtable[cols.prob.to[xy]] - CTtable[cols.prob.from[xy]]       # compute number of inactive trap nights
      n_days_inactive[,xy] <- as.integer(n_days_inactive[,xy])
    }
    for(xyz in cols.prob.from){
      if(any(CTtable[,setupCol] > CTtable[,xyz], na.rm = TRUE)){
        stop(paste(paste(CTtable[which(CTtable[,setupCol] > CTtable[,xyz]), stationCol], collapse = ", "), ": Problem begins before Setup"))
      }
    }
    for(xyz2 in cols.prob.to){
      if(any(CTtable[,retrievalCol] < CTtable[,xyz2], na.rm = TRUE)){
        stop(paste(paste(CTtable[which(CTtable[,retrievalCol] < CTtable[,xyz2]), stationCol], collapse = ", "), ": Problem ends after retrieval"))
      }
    }
    rm(xy, xyz, xyz2)

    n_days_inactive_rowsum <- rowSums(n_days_inactive, na.rm = TRUE)
  } else {
    n_days_inactive_rowsum <- rep(0, times = nrow(CTtable))
  }
  stopifnot(nrow(n_days_inactive_rowsum) == nrow(CTtable))

  n_days_inactive_rowsum <- aggregate(n_days_inactive_rowsum,
                                      by    = list(CTtable[,stationCol]),
                                      FUN   = sum,
                                      na.rm = TRUE)


  # adjust options for printing results
  options.tmp <- options()
  on.exit(options(options.tmp))
  options(max.print=1e6)
  options(width = 1000)

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


  n_nights_total      <- as.integer(CTtable[,retrievalCol] - CTtable[,setupCol])
  n_nights_total_agg  <- aggregate(n_nights_total,
                                   by  = list(CTtable[,stationCol]),
                                   FUN = sum)
  n_cameras_total_agg <- aggregate(CTtable[,stationCol],
                                   by  = list(CTtable[,stationCol]),
                                   FUN = length)
  n_nights_active     <- n_nights_total_agg[,2] - n_days_inactive_rowsum[,2]

  date_range_combined <- data.frame(station.tmp1[,1], station.tmp1[,2],
                                    image.tmp1[match(station.tmp1[,1], image.tmp1[,1]),2],
                                    image.tmp2[match(station.tmp1[,1], image.tmp2[,1]),2],
                                    station.tmp2[,2],
                                    n_nights_total_agg[,2],
                                    n_nights_active,
                                    n_cameras_total_agg[,2])
  colnames(date_range_combined) <- c(stationCol, "setup_date",  "first_image_date", "last_image_date", "retrieval_date",
                                     "n_nights_total", "n_nights_active", "n_cameras")
  rownames(date_range_combined) <- NULL


  # sink/print output


  if(hasArg(sinkpath)){
    sinkfile <- file.path(sinkpath, paste("survey_report_", Sys.Date(), ".txt", sep = ""))
    sink(file = sinkfile)
    print(paste("Survey Report generated", Sys.Date() ))
  }
  cat("\n-------------------------------------------------------\n")
  print(paste("Total number of stations: ", length(unique(CTtable[,stationCol]))))
  cat("\n-------------------------------------------------------\n")
  print(paste("Number of operational stations: ", length(which(n_nights_active >= 1))))
  cat("\n-------------------------------------------------------\n")

  if(hasArg(cameraCol)){
    print(paste("Total number of cameras: ", length(unique(paste(CTtable[,stationCol], CTtable[,cameraCol], sep = "_")))))
    cat("\n-------------------------------------------------------\n")

    print(paste("n nights with cameras set up (operational or not): ",
                sum(n_nights_total, na.rm = TRUE)))
    cat("\n-------------------------------------------------------\n")
    print(paste("n nights with cameras set up and active (trap nights): ",
                sum(n_nights_active, na.rm = TRUE)))
  } else {
    print(paste("n nights with cameras set up (operational or not. NOTE: only correct if 1 camera per station):",
                sum(n_nights_total, na.rm = TRUE)))
    cat("\n-------------------------------------------------------\n")
    print(paste("n nights with cameras set up and active (trap nights. NOTE: only correct if 1 camera per station):",
                sum(n_nights_active, na.rm = TRUE)))
  }
  cat("\n-------------------------------------------------------\n")
  print(paste("total trapping period: ", paste(min(station.tmp1[,2]), max(station.tmp2[,2]), sep = " - ")))



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
    print(date_range_combined)
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

  output <- list(date_range_combined, n_spec_by_station, species_record_table2, station_record_table1, station_record_table2)
  names(output) <- c("survey_dates", "species_by_station", "events_by_species",
                     "events_by_station", "events_by_station2")

  # make zip file
  if(isTRUE(makezip)){

    arglist_zip <-  list(output               = output,
                         recordTable          = recordTable,
                         CTtable              = CTtable,
                         speciesCol           = speciesCol,
                         stationCol           = stationCol,
                         setupCol             = setupCol,
                         retrievalCol         = retrievalCol,
                         CTDateFormat         = CTDateFormat,
                         CTHasProblems        = CTHasProblems,
                         recordDateTimeCol    = recordDateTimeCol,
                         recordDateTimeFormat = recordDateTimeFormat,
                         sinkpath             = sinkpath)

    if(hasArg(Xcol) & hasArg(Ycol)) arglist_zip <- c(arglist_zip,  Xcol = Xcol, Ycol = Ycol)

    do.call(makeSurveyZip, arglist_zip)

  }

  return(invisible(output))
}
