detectionHistory <- function(recordTable,
                             species,
                             camOp,
                             stationCol = "Station",
                             speciesCol = "Species",
                             recordDateTimeCol = "DateTimeOriginal",
                             recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                             occasionLength,
                             minActiveDaysPerOccasion,
                             maxNumberDays,
                             day1,
                             buffer,
                             includeEffort = TRUE,
                             scaleEffort = FALSE,
                             occasionStartTime = 0,
                             datesAsOccasionNames = FALSE,
                             timeZone,
                             writecsv = FALSE,
                             outDir)
{
  wd0 <- getwd()
  on.exit(setwd(wd0))
  #################
  # check input
  
  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, speciesCol = speciesCol, recordDateTimeCol = recordDateTimeCol)
  if(!is.data.frame(recordTable)) stop("recordTable must be a data frame", call. = FALSE)
  if(!stationCol %in% colnames(recordTable))  stop(paste('stationCol = "', stationCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))  stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable))  stop(paste('recordDateTimeCol = "', recordDateTimeCol,  '" is not a column name in recordTable', sep = ''), call. = FALSE)
  

  stopifnot(hasArg(species))
  stopifnot(is.character(species))
  stopifnot(length(species) == 1)

  stopifnot(hasArg(occasionLength))

  stopifnot(hasArg(recordTable))
  stopifnot(class(recordTable) == "data.frame")
  stopifnot(hasArg(camOp))

  stopifnot(hasArg(stationCol))
  stopifnot(length(stationCol) == 1)
  recordTable[,stationCol] <- as.character(recordTable[,stationCol])
  stopifnot(is.character(stationCol))

  stopifnot(hasArg(speciesCol))
  stopifnot(length(speciesCol) == 1)
  recordTable[,speciesCol] <- as.character(recordTable[,speciesCol])
  stopifnot(is.character(speciesCol))


  stopifnot(hasArg(recordDateTimeCol))
  stopifnot(length(recordDateTimeCol) == 1)
  recordTable[,recordDateTimeCol] <- as.character(recordTable[,recordDateTimeCol])   # make character to get rid of attributes. Will later assign time zone again
  stopifnot(is.character(recordDateTimeCol))


  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }
  stopifnot(is.logical(writecsv))

  if(length(occasionStartTime) != 1) stop("occasionStartTime must have length 1")
  occasionStartTime <- as.integer(round(occasionStartTime))
  if(occasionStartTime != 0 & !is.integer(occasionStartTime)) {stop ("occasionStartTime must be between 0 and 23")}
  if(occasionStartTime < 0 | occasionStartTime >= 24){stop ("occasionStartTime must be between 0 and 23")}

  occasionLength <- as.integer(round(occasionLength))
  if(occasionLength <= 0) stop("occasionLength must be a positive integer and not 0")
  if(occasionLength > ncol(camOp)/2) stop("occasionLength may not be greater than half the total number of days in camOp")
  stopifnot(is.numeric(occasionLength))

  if(hasArg(maxNumberDays)){
    maxNumberDays <- as.integer(maxNumberDays)
    if(maxNumberDays > ncol(camOp)) stop("maxNumberDays is larger than the number of columns of camOp")
    if(maxNumberDays < occasionLength) stop("maxNumberDays must be larger than or equal to occasionLength")
  }

  if(hasArg(buffer)) {
    stopifnot(is.numeric(buffer))
    buffer <- round(buffer)
    stopifnot(buffer >= 1)
  }

  stopifnot(c(speciesCol, recordDateTimeCol, stationCol) %in% colnames(recordTable))

  if(species %in% recordTable[,speciesCol] == FALSE) stop("species is not in speciesCol of recordTable")

  if(writecsv == TRUE){
    if(!file.exists(outDir)){stop("outDir does not exist")}
  }

  if(includeEffort == TRUE){
    if(!hasArg(scaleEffort)) stop("scaleEffort must be defined if includeEffort is TRUE")
    if(class(scaleEffort) != "logical") stop("scaleEffort must be logical (TRUE or FALSE)")
  } else {scaleEffort <- FALSE}

  if(hasArg(minActiveDaysPerOccasion)){
    stopifnot(is.numeric(minActiveDaysPerOccasion))
    stopifnot(minActiveDaysPerOccasion <= occasionLength)
  }
  
  #############
  # bring date, time, station ids into shape

  subset_species           <- recordTable[recordTable[,speciesCol] == species,]
  subset_species$DateTime2 <- as.POSIXlt(subset_species[,recordDateTimeCol], tz = timeZone, format = recordDateTimeFormat)

  # check consistency of argument day1
  stopifnot(class(day1) == "character")
  day1 <- tolower(day1)
  if(day1 == "survey") {day1switch <- 1} else {
    if(day1 == "station") {day1switch <- 2} else {
      try(date.test <- as.Date(day1), silent = TRUE)
      if(!exists("date.test")) stop("day1 is not specified correctly. It can only be 'station', 'survey', or a date formatted as 'YYYY-MM-DD', e.g. '2016-12-31'")
      if(class(date.test) != "Date") stop('could not interpret argument day1: can only be "station", "survey" or a specific date (e.g. "2015-12-31")')
      if(hasArg(buffer)) stop("if buffer is defined, day1 can only be 'survey' or 'station'")
      suppressWarnings(rm(date.test))
      day1switch <- 3
    }
  }

  if("POSIXlt" %in% class(subset_species$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(subset_species$DateTime2))) stop(paste("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat. row",
                                                   paste(which(is.na(subset_species$DateTime2)), collapse = ", ")))

  checkCamOpColumnNames (cameraOperationMatrix = camOp)
  cam.op.worked0 <- as.matrix(camOp)

  if(all(as.character(unique(subset_species[,stationCol])) %in% rownames(cam.op.worked0)) == FALSE){
    (stop("Not all values of stationCol in recordTable are matched by rownames of camOp"))
  }

  ################################################
  # compute date range of stations and records
  arg.list0 <- list(cam.op = cam.op.worked0, subset_species_tmp = subset_species, stationCol_tmp = stationCol, day1_tmp = day1, occasionStartTime_tmp = occasionStartTime, timeZone_tmp = timeZone)

    if(hasArg(maxNumberDays))  arg.list0 <- c(arg.list0,   maxNumberDays_tmp = maxNumberDays)
    if(hasArg(buffer))   arg.list0 <- c(arg.list0, buffer_tmp =  buffer)

  date_ranges <- do.call(createDateRangeTable, arg.list0)

  rm(arg.list0)

#######################
# adjust camera operation matrix

  cam.op.worked <- adjustCameraOperationMatrix(cam.op = cam.op.worked0, date_ranges2 = date_ranges, timeZone_tmp = timeZone, day1_2 = day1)

  # append occasionStartTime (if != 0) to column names for output table
  if(occasionStartTime != 0){
    colnames(cam.op.worked) <- paste(colnames(cam.op.worked), "+", occasionStartTime, "h", sep = "")
  }

######################
# calculate trapping effort by station and occasion
  arg.list0 <- list(cam.op = cam.op.worked, occasionLength2 = occasionLength, scaleEffort2 = scaleEffort, includeEffort2 = includeEffort)

    if(hasArg(minActiveDaysPerOccasion))  arg.list0 <- c(arg.list0, minActiveDaysPerOccasion2 = minActiveDaysPerOccasion)
  
    effort.tmp <- do.call(calculateTrappingEffort, arg.list0)
  
    rm(arg.list0)
  
  effort <- effort.tmp[[1]]
  if(isTRUE(scaleEffort))  scale.eff.tmp.attr <- effort.tmp[[2]]

###################
# remove records that fall into buffer period or were taken after maxNumberDays

  subset_species <- cleanSubsetSpecies(subset_species2 = subset_species, stationCol2 = stationCol, date_ranges2 = date_ranges)

############
  #  define the 1st day of the effective survey period.
if(day1 %in% c("survey")){
  time2 <- date_ranges$start_first_occasion_survey[match(subset_species[,stationCol], rownames(date_ranges))]
} else {
  time2 <- date_ranges$start_first_occasion[match(subset_species[,stationCol], rownames(date_ranges))]
}

  # calculate the occasion each record belongs into from the time difference between records and beginning of the first occasion
  subset_species$occasion <- as.numeric(ceiling((difftime(time1  = subset_species$DateTime2,
                                                          time2 =  time2,
                                                          units = "secs",
                                                          tz = timeZone)
                                                / (occasionLength * 86400))))


 if(max(subset_species$occasion) > ncol(effort)) {stop("encountered a bug. I'm Sorry. Please report it.")}

  ############
  # make detection history

  if(occasionLength == 1){                    # occasion length = 1
    record.hist <- cam.op.worked
    record.hist <- ifelse(record.hist == 0, NA, record.hist)    # if cameras not operational, set NA
    record.hist <- ifelse(record.hist >= 1, 0,  record.hist)    # if cameras operational, set to 0

    occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = unique, simplify = FALSE)

    # fill detection matrix with 1 in appropriate cells
    for(xyz in which(sapply(occasions.by.station, FUN = function(x){!is.null(x)}))){
      if(any(occasions.by.station[[xyz]] < 0)) stop("this is a bug in the function (DH_error1). Please report it.", call. = FALSE)
      if(any(occasions.by.station[[xyz]] > ncol(record.hist))) stop("this is a bug in the function (DH_error2). Please report it.", call. = FALSE)
      record.hist[match(names(occasions.by.station)[xyz], rownames(record.hist)), occasions.by.station[[xyz]]] <- 1
    }
    record.hist[is.na(cam.op.worked)] <- NA   # remove the records that were taken when cams were NA (redundant with above:   # remove records taken after day1 + maxNumberDays)

    rm(occasions.by.station, xyz)
    
  } else {                                    # occasion length > 1
    record.hist <- effort                     # start with effort (including NAs)
    record.hist <- ifelse(!is.na(record.hist), 0, record.hist)     # set all cells 0 (unless they are NA)
    rownames(record.hist) <- rownames(cam.op.worked)

    occasions.by.station <- tapply(X = subset_species$occasion, INDEX = subset_species[,stationCol], FUN = unique, simplify = FALSE)

    # fill detection matrix with "1" in appropriate cells
    for(xyz in which(sapply(occasions.by.station, FUN = function(x){!is.null(x)}))){
      record.hist[match(names(occasions.by.station)[xyz], rownames(record.hist)), occasions.by.station[[xyz]]] <- 1
    }
    record.hist[is.na(effort)] <- NA     # just to make sure NA stays NA
  }

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

  if(isTRUE(writecsv)){
    setwd(outDir)
    write.csv(record.hist, file = outtable.name)
    if(isTRUE(includeEffort)){
      write.csv(effort, file = outtable.name.effort)
      if(hasArg(scaleEffort)){
        if(scaleEffort == TRUE)  write.csv(scale.eff.tmp.attr, file = outtable.name.effort.scale)
      }
    }
  }

  if(isTRUE(includeEffort)){
    if(scaleEffort == TRUE){
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
}