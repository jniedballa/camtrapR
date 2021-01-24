spatialDetectionHistory <- function(recordTableIndividual,
                                    species,
                                    camOp,
                                    CTtable,
                                    output = c("binary", "count"),
                                    stationCol = "Station",
                                    speciesCol = "Species",
                                    sessionCol,
                                    Xcol,
                                    Ycol,
                                    stationCovariateCols,
                                    individualCol,
                                    individualCovariateCols,
                                    recordDateTimeCol = "DateTimeOriginal",
                                    recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                    occasionLength,
                                    minActiveDaysPerOccasion,
                                    occasionStartTime = "deprecated",
                                    maxNumberDays,
                                    day1,
                                    buffer,
                                    includeEffort = TRUE,
                                    scaleEffort   = FALSE,
                                    binaryEffort  = FALSE,
                                    timeZone,
                                    makeRMarkInput
)
{

  wd0 <- getwd()
  on.exit(setwd(wd0))

  #################
  # check input
  if(!hasArg(recordTableIndividual))  stop("'recordTableIndividual' is not defined", call. = FALSE)
  
  recordTableIndividual <- dataFrameTibbleCheck(df = recordTableIndividual)
  CTtable <- dataFrameTibbleCheck(df = CTtable)

  if(!hasArg(camOp))               stop("'camOp' is not defined", call. = FALSE)
  if(!inherits(camOp, "matrix"))   stop ("camOp must be a matrix", call. = FALSE)

  if(!hasArg(species))        stop("'species' is not defined", call. = FALSE)
  if(!is.character(species))  stop("species must be of class character", call. = FALSE)
  if(!length(species) == 1)   stop("species may only contain one value", call. = FALSE)

  if(hasArg(makeRMarkInput)) if(!is.logical(makeRMarkInput)) stop("makeRMarkInput must be logical", call. = FALSE)

  output <- match.arg(output)

  if(!hasArg(occasionLength)) stop("'occasionLength' is not defined", call. = FALSE)

  # check recordTableIndividual
  if(!speciesCol %in% colnames(recordTableIndividual))        stop(paste("speciesCol", speciesCol, "is not a column name in recordTableIndividual"), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTableIndividual)) stop(paste("recordDateTimeCol", recordDateTimeCol, "is not a column name in recordTableIndividual"), call. = FALSE)
  if(!stationCol %in% colnames(recordTableIndividual))        stop(paste("stationCol", stationCol, "is not a column name in recordTableIndividual"), call. = FALSE)
  if(!individualCol %in% colnames(recordTableIndividual))     stop(paste("individualCol", individualCol, "is not a column name in recordTableIndividual"), call. = FALSE)

  checkForSpacesInColumnNames(stationCol = stationCol, Xcol = Xcol, Ycol = Ycol, 
                                         recordDateTimeCol = recordDateTimeCol, speciesCol = speciesCol, individualCol = individualCol)
  if(!inherits(CTtable, "data.frame"))     stop("CTtable must be a data.frame", call. = FALSE)
  if(!stationCol %in% colnames(CTtable))   stop(paste('stationCol = "',   stationCol,  '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!Xcol %in% colnames(CTtable))         stop(paste('Xcol = "',   Xcol,  '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!Ycol %in% colnames(CTtable))         stop(paste('Ycol = "',   Ycol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
  
  if(!stationCol %in% colnames(recordTableIndividual))            stop(paste('stationCol = "',   stationCol,  '" is not a column name in recordTableIndividual', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTableIndividual))     stop(paste('recordDateTimeCol = "', recordDateTimeCol,  '" is not a column name in recordTableIndividual', sep = ''), call. = FALSE)
  if(!speciesCol %in% colnames(recordTableIndividual))            stop(paste('speciesCol = "', speciesCol,  '" is not a column name in recordTableIndividual', sep = ''), call. = FALSE)
  if(!individualCol %in% colnames(recordTableIndividual))         stop(paste('individualCol = "', individualCol,  '" is not a column name in recordTableIndividual', sep = ''), call. = FALSE)
  
    
  if(!length(stationCol) == 1)  stop("stationCol may only contain one value", call. = FALSE)
  recordTableIndividual[, stationCol] <- as.character(recordTableIndividual[, stationCol])
  if(!is.character(stationCol)) stop("stationCol must be a character", call. = FALSE)

  if(!length(speciesCol) == 1)  stop("speciesCol may only contain one value", call. = FALSE)
  recordTableIndividual[, speciesCol] <- as.character(recordTableIndividual[, speciesCol])
  if(!is.character(speciesCol)) stop("speciesCol must be a character", call. = FALSE)

  if(!length(individualCol) == 1)  stop("individualCol may only contain one value", call. = FALSE)
  recordTableIndividual[, individualCol] <- as.character(recordTableIndividual[, individualCol])
  if(!is.character(individualCol)) stop("individualCol must be a character", call. = FALSE)

  if(!length(recordDateTimeCol) == 1)  stop("recordDateTimeCol may only contain one value", call. = FALSE)
  recordTableIndividual[, recordDateTimeCol] <- as.character(recordTableIndividual[, recordDateTimeCol])
  if(!is.character(recordDateTimeCol)) stop("recordDateTimeCol must be a character", call. = FALSE)
  
  
  # check CTtable
  if(any(is.na(CTtable[,Xcol]))) stop("there are NAs in Xcol", call. = FALSE)
  if(any(is.na(CTtable[,Ycol]))) stop("there are NAs in Ycol", call. = FALSE)
  if(any(is.na(CTtable[,stationCol]))) stop("there are NAs in stationCol of CTtable", call. = FALSE)

  if(hasArg(stationCovariateCols)){
  checkForSpacesInColumnNames(stationCovariateCols = stationCovariateCols)
    if(!all(stationCovariateCols %in% colnames(CTtable))) stop("did not find stationCovariateCols in column names of CTtable (",
                                                               paste(stationCovariateCols[which(stationCovariateCols %in% colnames(CTtable) == FALSE)], collapse = ", "),
                                                               ")", sep = "", call. = FALSE)
  }
  if(!all(recordTableIndividual$Station %in% CTtable$Station)) stop ("there are entries in stationCol of recordTableIndividual that are not found in stationCol of CTtable")

  #####################################################################################################################
  # bring date, time, station ids into shape
  
  recordTableIndividual$DateTime2 <- parseDateTimeObject(inputColumn = recordTableIndividual[,recordDateTimeCol],
                                                  dateTimeFormat = recordDateTimeFormat,
                                                  timeZone = timeZone,
                                                  checkNA_out = FALSE)
  
  if(any(is.na(recordTableIndividual$DateTime2))) stop(paste(sum(is.na(recordTableIndividual$DateTime2)), "out of",
                                                      nrow(recordTableIndividual),
                                                      "entries in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat (NA). row",
                                                      paste(rownames(recordTableIndividual)[which(is.na(recordTableIndividual$DateTime2))], collapse = ", ")))
  
  
  
  
  # check sessionCol argument
  if(hasArg(sessionCol)) {
    checkForSpacesInColumnNames(sessionCol = sessionCol)
  
    if(sessionCol %in% colnames(recordTableIndividual) &
       sessionCol %in% colnames(CTtable)) stop(paste("both CTtable and recordTableIndividual contain sessionCol '", sessionCol, "'", sep = ""), call. = FALSE)

    if(sessionCol %in% colnames(recordTableIndividual) == FALSE &
       sessionCol %in% colnames(CTtable) == FALSE) stop(paste("neither CTtable nor recordTableIndividual contain sessionCol '", sessionCol, "'", sep = ""), call. = FALSE)

    if(sessionCol %in% colnames(recordTableIndividual)) {
      if(!is.numeric(recordTableIndividual[,sessionCol])) stop("sessionCol in recordTableIndividual must be numeric", call. = FALSE)
      if(!all(sort(unique(recordTableIndividual[,sessionCol])) == seq.int(from = 1, to = max(recordTableIndividual[,sessionCol]), by = 1)))
        stop("Problem in sessionCol of recordTableIndividual: Values must come from a gapless sequence of integer numbers starting with 1", call. = FALSE)
    }

    if(sessionCol %in% colnames(CTtable)){
      if(!is.numeric(CTtable[,sessionCol])) stop("sessionCol in CTtable must be numeric", call. = FALSE)
      if(!all(sort(unique(CTtable[,sessionCol])) == seq.int(from = 1, to = max(CTtable[,sessionCol]), by = 1)))
        stop("Problem in sessionCol of CTtable: Values must come from a gapless sequence of integer numbers starting with 1", call. = FALSE)
      
      # introduce sessionCol in recordTable by matching station IDs
      # first check if there is multiple sessions per station (with identical station IDs)
      
      # 1) if there is only 1 session per station
     # recordTableIndividual <- merge(recordTableIndividual, CTtable[,c(stationCol, sessionCol)], by = stationCol)
      
      # 2) if there are multiple entries for each station 
      
      recordTableIndividual <- assignSessionIDtoRecordTable (recordTable_tmp = recordTableIndividual,
                                               camOp = camOp,
                                               dateTimeCol = "DateTime2", # recordDateTimeCol,
                                               stationCol = stationCol,
                                               sessionCol = sessionCol
      )

      
      # see if there are duplicate station#session IDs in CTtable (meaning multiple cameras per station/session)
      
      separatorSession <- "__SESS_"
      stationCol_backup <- paste(stationCol, "backup", sep = "_")
      CTtable[, stationCol_backup] <- CTtable[,stationCol]
      
      #stationCol <- paste(recordTable_tmp$stationCol, recordTable_tmp$sessionCol, sep = "_")
      CTtable[, stationCol] <- paste(CTtable[,stationCol], CTtable[,sessionCol], sep = separatorSession)
      
      
      nonDuplicatedStationSessionIDs <- which(!duplicated(CTtable[,stationCol]))
      
      
      if(length(nonDuplicatedStationSessionIDs) != nrow(camOp)) stop("mismatch in length of nonDuplicatedStationSessionIDs and nrow(camOp)") 
      
      camOp <- camOp[match(CTtable[nonDuplicatedStationSessionIDs, stationCol], rownames(camOp)),]
      
      # check that new station column 
      if(any(rownames(camOp) != CTtable[nonDuplicatedStationSessionIDs, stationCol])) stop("error in assignSessionIDtoRecordTableIndividual: station column order doesn't match camOp matrix anymore",
                                                                                           call. = FALSE)
      
      # remove redundant cameras from CTtable
      CTtable <- CTtable[nonDuplicatedStationSessionIDs,]
      
     # rm(out_tmp)
    }
  }

  
  
  # check that length of station IDs in CTtable and camop match
  if(length(unique(CTtable[,stationCol])) != length(rownames(camOp))) stop ("there is a mismatch between station IDs in CTtable and rownames in camOp (length differs)", call. = FALSE)
  if(any(unique(CTtable[,stationCol]) != rownames(camOp)))            stop ("The order of station IDs in CTtable and rownames in camOp differs [2]", call. = FALSE)

  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()")
  }

  if(hasArg(minActiveDaysPerOccasion)){
    if(!is.numeric(minActiveDaysPerOccasion))       stop("minActiveDaysPerOccasion must be a number", call. = FALSE)
    if(!minActiveDaysPerOccasion <= occasionLength) stop("minActiveDaysPerOccasion must be smaller than or equal to occasionLength", call. = FALSE)
  }

  if(hasArg(occasionStartTime)) warning("'occasionStartTime' is deprecated and will be removed in the next release. It is now found in cameraOperation()")
  # if(length(occasionStartTime) != 1) stop("occasionStartTime must have length 1")
  # occasionStartTime    <- as.integer(round(occasionStartTime))
  # if(occasionStartTime != 0 & !is.integer(occasionStartTime)) stop ("occasionStartTime must be between 0 and 23", call. = FALSE)
  # if(occasionStartTime < 0 | occasionStartTime >= 24)         stop ("occasionStartTime must be between 0 and 23", call. = FALSE)

  occasionLength    <- as.integer(round(occasionLength))
  if(length(occasionLength) != 1)  stop("occasionLength may only contain one value", call. = FALSE)
  if(!is.numeric(occasionLength))  stop("occasionLength must be a positive integer", call. = FALSE)
  if(occasionLength <= 0)             stop("occasionLength must be a positive integer and not 0", call. = FALSE)
  if(occasionLength > ncol(camOp)/2)  stop("occasionLength may not be greater than half the total number of days in camOp", call. = FALSE)

  if(hasArg(maxNumberDays)){
    maxNumberDays    <- as.integer(maxNumberDays)
    if(maxNumberDays > ncol(camOp))    stop("maxNumberDays must be smaller than the number of columns of camOp", call. = FALSE)
    if(maxNumberDays < occasionLength) stop("maxNumberDays must be larger than or equal to occasionLength")
  }

  if(hasArg(buffer)) {
    if(!is.numeric(buffer)) stop("buffer must be number", call. = FALSE)
    buffer <- round(buffer)
    if(!buffer >= 1)        stop("if buffer is defined, it must be 1 or higher", call. = FALSE)
  }

  # check that species is in the speciesCol
  if(!species %in% recordTableIndividual[,speciesCol]) stop("species ", species, " not found in speciesCol of recordTableIndividual")
  # check all stations in recordTableIndividual are matched in CTtable
  if(!all(recordTableIndividual[,stationCol] %in% CTtable[,stationCol])) {
    stop(paste("items of stationCol in recordTableIndividual are not matched in stationCol of CTtable: ", paste(recordTableIndividual[-which(recordTableIndividual[,stationCol] %in% CTtable[,stationCol]),stationCol], collapse = ", ")))
  }


  if(includeEffort == TRUE){
    if(hasArg(scaleEffort)){
      if(!is.logical(scaleEffort)) stop("scaleEffort must be logical (currently only FALSE is allowed)")
      if(isTRUE(scaleEffort))             stop("currently scaleEffort must be FALSE")
    }
    if(hasArg(binaryEffort)){
      if(!is.logical(binaryEffort)) stop("binaryEffort must be logical (TRUE or FALSE)")
    }
    if(binaryEffort == TRUE & scaleEffort == TRUE) stop("'scaleEffort' and 'binaryEffort' cannot both be TRUE")
  } else {
    scaleEffort  <- FALSE
    binaryEffort <- FALSE
  }

  

 # subset to species of interest
  subset_species           <- recordTableIndividual[recordTableIndividual[,speciesCol] == species,]
  
  # if sessionCol is defined and present in CTtable, check if all records are within correct session. remove if not
  if(hasArg(sessionCol)){
    if(sessionCol %in% colnames(CTtable)){

      rec_station_session <- unique(subset_species[,c(stationCol, sessionCol)])
      cam_station_session <- unique(CTtable[,c(stationCol, sessionCol)])

      rec_station_session$included_rec     <- TRUE
      cam_station_session$included_station <- TRUE
      merged_tmp <- merge(rec_station_session, cam_station_session, all=TRUE)

      if(any(is.na(merged_tmp$included_station))){
        which_to_remove_mismatch <- which(subset_species[,stationCol] == merged_tmp[is.na(merged_tmp$included_station), stationCol] &
                                            subset_species[,sessionCol] == merged_tmp[is.na(merged_tmp$included_station), sessionCol])
        if(length(which_to_remove_mismatch) >= 1){
          warning("there are records at stations which were not used during sessions. They will be ignored. \nrows: ",
                  paste(rownames(subset_species)[which_to_remove_mismatch], collapse = ", "), call. = FALSE)
          subset_species <- subset_species[-which_to_remove_mismatch,]
        }
      }
    }
  }


  # check consistency of argument day1
  if(!is.character(day1)) stop("day1 must be a character", call. = FALSE)

  if(day1 == "survey") {day1switch <- 1} else {
    if(day1 == "station") {day1switch <- 2} else {
      try(date.test <- as.Date(day1), silent = TRUE)
      if(!exists("date.test")) stop("day1 is not specified correctly. It can only be 'station', 'survey', or a date formatted as 'YYYY-MM-DD', e.g. '2016-12-31'")
      if(!is(date.test, "Date")) stop('could not interpret argument day1: can only be "station", "survey" or a specific date (e.g. "2015-12-31")')
      if(hasArg(buffer)) stop("if buffer is defined, day1 can only be 'survey' or 'station'")
      suppressWarnings(rm(date.test))
      day1switch <- 3
    }
  }


  
  # check that column names can be interpreted as day, as extract occasionStartTime if relevant
  camOp <- checkCamOpColumnNames (cameraOperationMatrix = camOp)
  if(hasArg(occasionStartTime)){
    if(occasionStartTime != attributes(camOp)$occasionStartTime){
      stop(paste("occasionStartTime", occasionStartTime, "differs from occasionStartTime in camOp", attributes(camOp)$occasionStartTime)) 
    }
  }
  occasionStartTime <- attributes(camOp)$occasionStartTime
  
  cam.op.worked0 <- as.matrix(camOp)
  
  if(all(as.character(unique(subset_species[,stationCol])) %in% rownames(cam.op.worked0)) == FALSE){
    (stop("Not all values of stationCol in recordTableIndividual are matched by rownames of camOp"))
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
  if(isTRUE(scaleEffort))     scale.eff.tmp.attr <- effort.tmp[[2]]
  if(isTRUE(binaryEffort))    effort             <- ifelse(effort >= 1, 1, 0)


  ###################
  # remove records that fall into buffer period or were taken after maxNumberDays

  subset_species <- cleanSubsetSpecies(subset_species2 = subset_species, stationCol2 = stationCol, date_ranges2 = date_ranges)

  ############
  #  define the 1st day of the effective survey period.

  # for each record, what is the respective beginning of the trapping period (used to calculate the occasion), 
  if(day1 %in% c("survey")){
    time2 <- date_ranges$start_first_occasion_survey[match(subset_species[,stationCol], rownames(date_ranges))]   # difference is relative to first occasion of survey
  } else {
    time2 <- date_ranges$start_first_occasion[match(subset_species[,stationCol], rownames(date_ranges))]         # difference is relative to first occasion at each station
  }


  # calculate time difference between records and first day of detection history (the occasion each record belongs into)
  occasionCol <- "Occasion"

  subset_species[,occasionCol] <- as.numeric(ceiling((difftime(time1 = subset_species$DateTime2,
                                                               time2 =  time2,
                                                               units = "secs",
                                                               tz    = timeZone)
                                                      / (occasionLength * 86400))))


  if(max(subset_species[,occasionCol]) > ncol(effort)) {stop("encountered a bug. I'm Sorry. Please report it.")}


  # column names for effort matrix
  if(includeEffort == TRUE) {colnames(effort) <-  paste("o", seq(1,ncol(effort), by = 1), sep = "")}

  #############
  # build spatial detection history

  # get relevant columns from subset_species
  if(hasArg(individualCovariateCols)){
    columnsForCaptures <- c(individualCol, occasionCol, stationCol, individualCovariateCols)
    sdh0 <- subset_species[, columnsForCaptures]
    colnames(sdh0) <- c("ID", occasionCol, "trapID", individualCovariateCols)
  } else {
    columnsForCaptures <- c(individualCol, occasionCol, stationCol)
    sdh0 <- subset_species[, columnsForCaptures]
    colnames(sdh0) <- c("ID", occasionCol, "trapID")
  }

  # add required session column to captures
  if(hasArg(sessionCol)) {
    sdh1 <- cbind(Session = subset_species[,sessionCol], sdh0)
  } else {
    sdh1 <- cbind(Session = 1, sdh0)
  }

  # remove unidentified individuals from captures
  remove.tmp <- which(is.na(sdh1[,"ID"]))
  if(length(remove.tmp) >= 1){
    sdh1 <- sdh1[-remove.tmp,]
    warning(paste("removed", length(remove.tmp), "records because of missing individual IDs"), call. = FALSE)
  }


  # if requested,  remove duplicate records (makes capthist binary. otherwise it returns counts)
  if(output == "binary"){
    sdh2 <- unique(sdh1)                  # remove duplicate rows
  } else {
    sdh2 <- sdh1                          # keep duplicate rows
  }


  ############
  # remove records for which effort was 0 or NA
  rowindex_sdh_to_remove <- vector()

  for(rowindex_sdh in 1:nrow(sdh2)){
    if(is.na(effort[match(sdh2$trapID[rowindex_sdh], rownames(effort)), sdh2$Occasion[rowindex_sdh]]) |   # if effort = NA or
       effort[match(sdh2$trapID[rowindex_sdh], rownames(effort)), sdh2$Occasion[rowindex_sdh]] == 0){     # if effort = 0
      rowindex_sdh_to_remove <- c(rowindex_sdh_to_remove, rowindex_sdh)                                   # save row index into temporary vector
    }
  }
  
  
  
  # if there were records in occasions with effort = 0/NA, remove these records
  if(length(rowindex_sdh_to_remove) != 0){
    warning("removed ", length(rowindex_sdh_to_remove), " record(s) because of effort = 0/NA, incomplete occasions (if includeEffort = FALSE), or effort < minActiveDaysPerOccasion:\n",
            paste(capture.output(print(sdh2[rowindex_sdh_to_remove,])), collapse = "\n"),
            call. = FALSE)
    sdh2 <- sdh2[-rowindex_sdh_to_remove,]
  }
  rm(rowindex_sdh, rowindex_sdh_to_remove)


  # make sure we have no NA in effort matrix (overwrite with 0)
  # otherwise secr::verify() will crash if there are NAs in the trap usage information
  effort[which(is.na(effort))] <- 0


  ############
  # make secr traps object

  coord.ct <- CTtable[,c(Xcol, Ycol)]
  colnames(coord.ct) <- c("x", "y")
  
  rownames(coord.ct) <- CTtable[,stationCol]

  if(!all(rownames(coord.ct) == rownames(cam.op.worked))) stop("Error assigning rownames to traps data frame. Please report this bug")
  
  # set detector type according to desired output (count or binary)
  detectortype <- ifelse (output == "binary", "proximity", "count")


# make station covariate data frame

  if(hasArg(stationCovariateCols)){
    whichstationCovariateCols <- which(colnames(CTtable) %in% stationCovariateCols)
    
    stationCovsDF <- data.frame(CTtable[, whichstationCovariateCols])
    
    if(length(stationCovariateCols) == 1) {
      colnames(stationCovsDF) <- stationCovariateCols
    }
  }

  # if sessionCol is defined and it is found in CTtable, make a list of traps objects (one for each session)

  if(hasArg(sessionCol)){
    if(sessionCol %in% colnames(CTtable)){
      secr.traps <- list()

      if(any(table(CTtable[, sessionCol]) == 1)) stop ("there is a session in CTtable that has only one station.")

      for(sessionID in sort(unique(CTtable[,sessionCol]))){
        which.session.tmp <- which(CTtable[,sessionCol] == sessionID)   # index of all stations active during session
        if(includeEffort == TRUE) {
          secr.traps[[sessionID]] <- read.traps(data         = coord.ct[which.session.tmp,],
                                                detector     = detectortype,
                                                binary.usage = binaryEffort)

          effort_session_tmp <- effort[which.session.tmp,]              # subset effort to active stations only
          secr::usage(secr.traps[[sessionID]]) <- effort_session_tmp[,which(colSums(effort_session_tmp) != 0)]  # only occasions with >1 active station
          # fix occasions (if there were occasions without active cameras before an occasion)
          # if a session is empty (e.g. because day1 was after end of that session, all records will be removed from sdh2)
          # that session then needs to be deleted with a warning
          sdh2[sdh2[, "Session"] == sessionID, "Occasion"] <- sapply(sdh2[sdh2[, "Session"] == sessionID, "Occasion"],
                                                                     FUN = function(x){x - length(which(which(colSums(effort_session_tmp) == 0) <  x) == TRUE)})

          rm(effort_session_tmp)
          # assign station names as row names back
          rownames(secr.traps[[sessionID]]) <- CTtable[which.session.tmp, stationCol_backup]
          rownames(secr::usage(secr.traps[[sessionID]])) <- CTtable[which.session.tmp, stationCol_backup]
          
        } else {
          secr.traps[[sessionID]] <- read.traps(data         = coord.ct[which.session.tmp,],
                                                detector     = detectortype)
          rownames(secr.traps[[sessionID]]) <- CTtable[which.session.tmp, stationCol_backup]
        }
        if(exists("stationCovsDF")) {
          secr::covariates(secr.traps[[sessionID]]) <- stationCovsDF[which.session.tmp,]
          rownames(secr::covariates(secr.traps[[sessionID]])) <- CTtable[which.session.tmp, stationCol_backup]
        }
        rm(which.session.tmp)
      }
# remove sessions with empty effort (caused by day1 after last station retrieval)
      if(any(sapply(secr::usage(secr.traps), sum) == 0)){
        which_effort_empty <- which(sapply(secr::usage(secr.traps), sum) == 0)
        if(day1switch == 3){
          warning("will remove data from session  ", paste(which_effort_empty, collapse = ", "), "  because the effort matrix was empty.
                  This is probably due to the usage of a date outside the station operation range in argument 'day1'", call. = FALSE)
        } else {
          warning("will remove data from ", paste(which_effort_empty, collapse = ", "), " session because the effort matrix was empty.", call. = FALSE)
        }
        secr.traps <- secr.traps[-which_effort_empty]
      }
    }
    }

  # otherwise (sessionCol not in CTtable or not defined) make traps object for all stations
  if(!exists("secr.traps")){
    if(includeEffort == TRUE) {
      secr.traps <- read.traps(data         = coord.ct,
                               detector     = detectortype,
                               binary.usage = binaryEffort)
      # if there are columns in effort which have colSum = 0 (no station active), remove these from effort/usage information
      secr::usage(secr.traps) <- effort[,which(colSums(effort) != 0)]
      # consequently, occasions need to be adjusted too (if occasions were lost before a specific occasion, reduce occasion by number of removed occasions before that occasion)
      sdh2$Occasion <- sapply(sdh2$Occasion, FUN = function(x){x - length(which(which(colSums(effort) == 0) <  x) == TRUE)})

    } else {
      secr.traps <- read.traps(data     = coord.ct,
                               detector = detectortype)
    }
    if(exists("stationCovsDF")) secr::covariates(secr.traps) <- stationCovsDF
  }

  # find number of occasions
  if(includeEffort == FALSE) {  
    noccasions <- ncol(effort) 
  } else {
    if(inherits(secr.traps, "list")) {
      noccasions <- sapply(secr::usage(secr.traps), ncol)    # includeEffort = TRUE / multi-session
    } else { 
      if(is(secr::usage(secr.traps), "matrix")){
        noccasions <- ncol(secr::usage(secr.traps))          # includeEffort = TRUE / single-session
      } else {
        noccasions <- ncol(effort)                           # includeEffort = TRUE / single-session
      }
    }
  }


  ###########
  # make capthist object

  deparse_sdh2 <- deparseCamOpRownames(sdh2$trapID)
  sdh2$trapID <- deparse_sdh2$station
  
  capthist.secr <- make.capthist(captures   = sdh2,
                                 traps      = secr.traps,
                                 fmt        = "trapID",
                                 noccasions = noccasions,
                                 bysession  = TRUE)

  if(hasArg(makeRMarkInput)){
    if(isTRUE(makeRMarkInput)){
      RMarkDataframe <- RMarkInput(capthist.secr, covariates = TRUE)
      return(RMarkDataframe)
    }
  }

  return(capthist.secr)
}
