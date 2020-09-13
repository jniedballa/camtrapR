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
                            dateFormat = "%Y-%m-%d",
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
    CTtable[,setupCol]     <- parseDateTimeObject(inputColumn = CTtable[,setupCol],     dateFormat, checkNA = TRUE, checkEmpty = TRUE, timeZone = "UTC")
    CTtable[,retrievalCol] <- parseDateTimeObject(inputColumn = CTtable[,retrievalCol], dateFormat, checkNA = TRUE, checkEmpty = TRUE, timeZone = "UTC")
  } else {
    CTtable[,setupCol]     <- parseDateObject(inputColumn = CTtable[,setupCol],     dateFormat, checkNA = TRUE, checkEmpty = TRUE)
    CTtable[,retrievalCol] <- parseDateObject(inputColumn = CTtable[,retrievalCol], dateFormat, checkNA = TRUE, checkEmpty = TRUE)
  }
  
  
  # check if dates make sense
  if(any(CTtable[,setupCol]     < as.Date("1970-01-01"))) warning("setup dates begin before 1970. If this is not intended please check dateFormat", call. = FALSE)
  if(any(CTtable[,retrievalCol] < as.Date("1970-01-01"))) warning("retrieval dates are before 1970. If this is not intended please check dateFormat", call. = FALSE)
  
  if(any(CTtable[,setupCol]     > Sys.Date())) warning("setup date is in the future. If this is not intended please check dateFormat", call. = FALSE)
  if(any(CTtable[,retrievalCol] > Sys.Date())) warning("retrieval date is in the future. If this is not intended please check dateFormat", call. = FALSE)
  
  
  # ensure setup is before retrieval
  if(any(CTtable[,setupCol] > CTtable[,retrievalCol])){
    stop(paste("Setup Date after Retrieval Date:   "),
         paste(CTtable[which(CTtable[,setupCol] > CTtable[,retrievalCol]), stationCol],
               collapse = ", "))
  }
  
  if(isTRUE(hasProblems)){
    
    # find problem columns
    cols.prob.from <- grep(colnames(CTtable), pattern = "Problem\\d\\Sfrom")
    cols.prob.to   <- grep(colnames(CTtable), pattern = "Problem\\d\\Sto")
    
    # convert problem columns to character
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
        CTtable[, problemFromColumn] <- parseDateTimeObject(inputColumn = CTtable[, problemFromColumn], dateFormat,
                                                            checkNA = FALSE, checkEmpty = FALSE, checkNA_out = FALSE, timeZone = "UTC")
      }
      for(problemToColumn in cols.prob.to){
        CTtable[, problemToColumn]   <- parseDateTimeObject(inputColumn = CTtable[, problemToColumn],   dateFormat, 
                                                            checkNA = FALSE, checkEmpty = FALSE, checkNA_out = FALSE, timeZone = "UTC")
      }
      
    } else {
      
      for(problemFromColumn in cols.prob.from){
        CTtable[, problemFromColumn] <- parseDateObject(inputColumn = CTtable[, problemFromColumn], dateFormat, checkNA = FALSE, checkEmpty = FALSE)
      }
      for(problemToColumn in cols.prob.to){
        CTtable[, problemToColumn] <- parseDateObject(inputColumn = CTtable[, problemToColumn], dateFormat, checkNA = FALSE, checkEmpty = FALSE)
      }
    }
    
    # check that there are some problems at all (since hasProblems = TRUE)
    if(all(is.na(CTtable[, problemFromColumn]))) stop("in problemFromColumn column(s), all values are NA", call. = FALSE)
    if(all(is.na(CTtable[, problemToColumn])))   stop("in Problem_to column(s), all values are NA", call. = FALSE)
    
    # check that problems begin after setup
    for(cols.prob.from.index in cols.prob.from){
      if(any(CTtable[,setupCol] > CTtable[,cols.prob.from.index], na.rm = TRUE)){
        stop(paste(paste(CTtable[which(CTtable[,setupCol] > CTtable[,cols.prob.from.index]), stationCol], collapse = ", "), ": Problem begins before Setup"), call. = FALSE)
      }
    }
    # check that problems end before retrieval
    for(cols.prob.to.index in cols.prob.to){
      if(any(CTtable[,retrievalCol] < CTtable[,cols.prob.to.index], na.rm = TRUE)){
        stop(paste(paste(CTtable[which(CTtable[,retrievalCol] < CTtable[,cols.prob.to.index]), stationCol], collapse = ", "), ": Problem ends after retrieval"), call. = FALSE)
      }
    }
    rm(problemFromColumn, problemToColumn, cols.prob.from.index, cols.prob.to.index)
  }
  
  # create empty matrix with desired dimensions (depending on presence of camera / session columns)
  
  arg_list <- list(CTtable = CTtable,
                   stationCol = stationCol,
                   setupCol = setupCol,
                   retrievalCol = retrievalCol#,
                   #separator = "__"
  )
  
  if(cameraColInArgs)  arg_list <- c(arg_list, "cameraCol" = cameraCol)
  if(sessionColInArgs) arg_list <- c(arg_list, "sessionCol" = sessionCol)
  
  camOp_empty <- do.call(stationSessionCamMatrix, args = arg_list) 
  
  # for each row (station/camera), assign daily effort where needed
  # integer when input columns are a date format, fraction when they are date-time
  for(i in 1:nrow(camOp_empty)){
    
    # camera setup date (min is to ensure that we get the first day if multiple cameras were set at a station on different days)
    date0 <- as.character(as.Date(min(CTtable[i, setupCol])))
    #date0 <- as.character(min(CTtable[i, setupCol]))
    
    # camera retrieval date
    date1 <- as.character(as.Date(max(CTtable[i, retrievalCol])))
    
    # fill matrix between setup and retrieval with 1
    camOp_empty[i, seq(from = match(date0, colnames(camOp_empty)),
                       to   = match(date1, colnames(camOp_empty)), by = 1)] <- 1
    
    
    if(effortAsFraction){
      # setup day as fraction of day with camera active (instead of 1)
      camOp_empty[i, match(date0, colnames(camOp_empty))] <-  fractionOfDay(time = CTtable[i, setupCol], type = "after")
      
      # retrieval day as fraction of day with camera active (instead of 1)
      camOp_empty[i, match(date1, colnames(camOp_empty))] <- fractionOfDay(time = CTtable[i, retrievalCol], type = "before")
    }
    
    
    # set non operational times to 0
    if(hasProblems){
      # loop over all problem periods
      for(j in 1:length(cols.prob.to)){   
        
        # find first day of problem period j
        date.p0.tmp <- as.character(as.Date(min(CTtable[i, cols.prob.from[j]])))   
        # find last day of problem period j
        date.p1.tmp <- as.character(as.Date(max(CTtable[i, cols.prob.to[j]])))   
        
        if(!is.na(date.p0.tmp) & !is.na(date.p1.tmp)){
          if(date.p1.tmp < date.p0.tmp) stop(paste("row", i, ", Problem ", j, ": 'to' is smaller than 'from'", sep = ""))
          if(date.p1.tmp > date1)       stop(paste("row", i, ", Problem ", j, ": is outside date range of setup and retrieval", sep = ""))
          if(date.p0.tmp < date0)       stop(paste("row", i, ", Problem ", j, ": is outside date range of setup and retrieval", sep = ""))
          # 
          camOp_empty[i, seq(from = match(date.p0.tmp, colnames(camOp_empty)),
                             to   = match(date.p1.tmp, colnames(camOp_empty)), by = 1)] <- 0
          
          # fraction of problem days that the cameras were active
          
          # if problem began on setup day, % setup day active - % setup day affected by problem
          if(date.p0.tmp == date0){   
            camOp_empty[i, match(date.p0.tmp, colnames(camOp_empty))] <- fractionOfDay(time = CTtable[i, setupCol], type = "after") - 
              fractionOfDay(time = CTtable[i, cols.prob.from[j]], type = "after")
            
          } else {   # otherwise fraction of day before problem began
            camOp_empty[i, match(date.p0.tmp, colnames(camOp_empty))] <-  fractionOfDay(time = CTtable[i, cols.prob.from[j]], type = "before")
          }
          
          # if problem ended on retrieval day, % retrieval day active - % retrieval day affected by problem
          if(date.p1.tmp == date1){   
            camOp_empty[i, match(date.p1.tmp, colnames(camOp_empty))] <- fractionOfDay(time = CTtable[i, retrievalCol], type = "before") - 
              fractionOfDay(time = CTtable[i, cols.prob.to[j]], type = "before")
            
          } else {  # otherwise fraction of day after problem ended
            camOp_empty[i, match(date.p1.tmp, colnames(camOp_empty))] <-  fractionOfDay(time = CTtable[i, cols.prob.to[j]], type = "after")
          }
        }
        rm(date.p0.tmp, date.p1.tmp)
      }
    }   # end isTRUE(hasProblems)
    rm(date0, date1)
  }
  
  camOp_filled <- camOp_empty
  
  
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
        
        # # return lowest value at that station (station will be "off" if at least one camera was off)
        # dat2 <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
        #                                           CTtable[, c(sessionCol)]), 
        #                   FUN = min)
        #                   #FUN = function(x) ifelse(all(x == 1), sum(x), 0))
        
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
