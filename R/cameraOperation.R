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
  
  if(!stationCol %in% colnames(CTtable))   stop(paste('stationCol = "',   stationCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!setupCol %in% colnames(CTtable))     stop(paste('setupCol = "',     setupCol,       '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!retrievalCol %in% colnames(CTtable)) stop(paste('retrievalCol = "', retrievalCol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
  
  
  stopifnot(length(stationCol) == 1)
  CTtable[,stationCol] <- as.character(CTtable[,stationCol])
  
  stopifnot(length(setupCol) == 1)
  #CTtable[,setupCol] <- as.character(CTtable[,setupCol])
  
  stopifnot(length(retrievalCol) == 1)
  #CTtable[,retrievalCol] <- as.character(CTtable[,retrievalCol])
  
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
    if(!byCamera){
      if(!hasArg(allCamsOn)) stop("if cameraCol is set and byCamera is FALSE, allCamsOn must be specified")
      stopifnot(is.logical(allCamsOn))
      if(!allCamsOn){
        if(!hasArg(camerasIndependent)) stop("if cameraCol is set, byCamera is FALSE and allCamsOn is FALSE, camerasIndependent must be specified")
        stopifnot(is.logical(camerasIndependent))
      }
    }
  } else {
    cameraCol <- "camera"
    CTtable[, cameraCol] <-  paste(CTtable[, stationCol], "Cam1", sep = "")
    if(hasArg(byCamera)) warning("If cameraCol is not defined, byCamera will have no effect")
  }
  
  stopifnot(c(stationCol, setupCol, retrievalCol) %in% colnames(CTtable))
  
  # check argument sessionCol
  if(sessionColInArgs){
    checkForSpacesInColumnNames(sessionCol = sessionCol)
    if(!sessionCol %in% colnames(CTtable)) stop(paste('sessionCol = "', sessionCol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
    if(!is.numeric(CTtable[, sessionCol])) stop("Values of sessionCol must be numeric", call. = FALSE)
  } else{
    sessionCol <- "session"
    CTtable[, sessionCol] <- 1
  }
  
  # return error if duplicate stations (i.e. more than 1 row per station)
  if(!cameraColInArgs & !sessionColInArgs){
    if(any(duplicated(CTtable[,stationCol]))){
      tmp <- table(CTtable[,stationCol])
      stop(paste(sum(tmp >= 2)," stations have more than 1 item in CTtable. Please specify 'cameraCol' or 'sessionCol'\n", 
                 paste(names(tmp[tmp >= 2]),
                       tmp[tmp >= 2], sep = ": ", collapse = "\n"),
                 sep = ""), 
           call. = FALSE)
    }
  }
  
  if(cameraColInArgs & !sessionColInArgs){
    if(any(duplicated(CTtable[,c(stationCol, cameraCol)]))){
      tmp <- table(paste(CTtable[,stationCol], " - ", cameraCol, " ", CTtable[, cameraCol], sep = ""))
      stop(paste(sum(tmp >= 2), " station/camera combinations have more than 1 item in CTtable. Consider specifying 'sessionCol' if you have multiple sessions / seasons\n",
                 paste(names(tmp[tmp >= 2]), tmp[tmp >= 2], sep = ": ", collapse = "\n"), sep = ""),
           call. = FALSE)
    }
  }
  
  if(!cameraColInArgs & sessionColInArgs){
    if(any(duplicated(CTtable[,c(stationCol, sessionCol)]))){
      tmp <- table(paste(CTtable[,stationCol], " - ", sessionCol, " ", CTtable[, sessionCol], sep = ""))
      stop(paste(sum(tmp >= 2)," station/session combinations have more than 1 item in CTtable. Consider specifying 'cameraCol' if you have multiple cameras per station\n",
                 paste(names(tmp[tmp >= 2]), tmp[tmp >= 2], sep = ": ", collapse = "\n"), sep = ""),
           call. = FALSE)
    }
  }
  
  if(cameraColInArgs & sessionColInArgs){
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
  
  CTtable[,setupCol]     <- parseDateObject(inputColumn = CTtable[,setupCol],     dateFormat, checkNA = TRUE, checkEmpty = TRUE)
  CTtable[,retrievalCol] <- parseDateObject(inputColumn = CTtable[,retrievalCol], dateFormat, checkNA = TRUE, checkEmpty = TRUE)
  
  
  # check if dates make sense
  if(any(CTtable[,setupCol]     < as.Date("1970-01-01"))) warning("setup dates begin before 1970. If this is not intended please check dateFormat", call. = FALSE)
  if(any(CTtable[,retrievalCol] < as.Date("1970-01-01"))) warning("retrieval dates are before 1970. If this is not intended please check dateFormat", call. = FALSE)
  
  if(any(CTtable[,setupCol]     > Sys.Date())) warning("setup date is in the future. If this is not intended please check dateFormat", call. = FALSE)
  if(any(CTtable[,retrievalCol] > Sys.Date())) warning("retrieval date is in the future. If this is not intended please check dateFormat", call. = FALSE)
  
  
  
  if(any(CTtable[,setupCol] > CTtable[,retrievalCol])){
    stop(paste("Setup Date after Retrieval Date:   "),
         paste(CTtable[which(CTtable[,setupCol] > CTtable[,retrievalCol]), stationCol],
               collapse = ", "))
  }
  
  if(isTRUE(hasProblems)){
    
    cols.prob.from <- grep(colnames(CTtable), pattern = "Problem\\d\\Sfrom")
    cols.prob.to   <- grep(colnames(CTtable), pattern = "Problem\\d\\Sto")
    
    if(length(cols.prob.from) == 0) stop("could not find column ProblemX_from")
    if(length(cols.prob.to) == 0)   stop("could not find column ProblemX_to")
    
    if(length(cols.prob.from) != length(cols.prob.to)){
      stop("length of 'Problem..._from' and 'Problem..._to' columns differs. Check column names. Sample: 'Problem1_from', 'Problem1_to'")
    }
    
    for(problemFromColumn in cols.prob.from){
      CTtable[, problemFromColumn] <- parseDateObject(inputColumn = CTtable[, problemFromColumn], dateFormat, checkNA = FALSE, checkEmpty = FALSE)
    }
    for(problemToColumn in cols.prob.to){
      CTtable[, problemToColumn] <- parseDateObject(inputColumn = CTtable[, problemToColumn], dateFormat, checkNA = FALSE, checkEmpty = FALSE)
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
    rm(problemFromColumn, problemToColumn, xyz, xyz2)
  }
  
  # create empty matrix with desired dimensions (depending on presence of camera / session columns)
  
  arg_list <- list(CTtable = CTtable,
                    stationCol = stationCol,
                    setupCol = setupCol,
                    retrievalCol = retrievalCol,
                    separator = "__")
  
  if(cameraColInArgs)  arg_list <- c(arg_list, "cameraCol" = cameraCol)
  if(sessionColInArgs) arg_list <- c(arg_list, "sessionCol" = sessionCol)
  
  camOp_empty <- do.call(stationSessionCamMatrix, args = arg_list) 
  
   # for each row, assign 1 where needed
  for(i in 1:nrow(camOp_empty)){
    
    # camera setup date
    date0 <- as.character(min(CTtable[i, setupCol]) )
      # camera retrieval date
    date1 <- as.character(max(CTtable[i, retrievalCol])) 
   
     # fill matrix between setup and retrieval with 1
    camOp_empty[i, seq(from = match(date0, colnames(camOp_empty)),
                       to   = match(date1, colnames(camOp_empty)), by = 1)] <- 1
    
    # set non operational times to 0
    if(hasProblems){
      
      for(j in 1:length(cols.prob.to)){   # loop over all problem periods
        
        # find first day of problem period j
        date.p0.tmp <- as.character(min(CTtable[i, cols.prob.from[j]]))   
        # find last day of problem period j
        date.p1.tmp <- as.character(max(CTtable[i, cols.prob.to[j]]))   
         
        if(!is.na(date.p0.tmp) & !is.na(date.p1.tmp)){
          if(date.p1.tmp < date.p0.tmp) stop(paste("row", i, ", Problem ", j, ": 'to' is smaller than 'from'", sep = ""))
          if(date.p1.tmp > date1)       stop(paste("row", i, ", Problem ", j, ": is outside date range of setup and retrieval", sep = ""))
          if(date.p0.tmp < date0)       stop(paste("row", i, ", Problem ", j, ": is outside date range of setup and retrieval", sep = ""))
          # 
          camOp_empty[i, seq(from = match(date.p0.tmp, colnames(camOp_empty)),
                             to   = match(date.p1.tmp, colnames(camOp_empty)), by = 1)] <- 0
        }
        rm(date.p0.tmp, date.p1.tmp)
      }
    }   # end isTRUE(hasProblems)
    rm(date0, date1)
  }
  
  camOp_filled <- camOp_empty
  
  
  if(cameraColInArgs){      # there is a camera column, i.e., potentially > 1 cameras per station
    
    if(!byCamera){  # byCamera = F
      
      separatorSession <- "__SESS_"
      
      if(allCamsOn){   # byCamera = F, allCamsOn = T
        dat2 <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                  CTtable[, c(sessionCol)]), FUN = min)    # return lowest value at that station (station will be "off" if at least one camera was off)
        if(sessionColInArgs)  row.names(dat2) <- paste(dat2[,1], dat2[,2], sep = separatorSession)
        if(!sessionColInArgs) row.names(dat2) <- dat2[,1]
        
        dat2[,c(1,2)] <- NULL
      } else {     # byCamera = F, allCamsOn = F
        dat2    <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                     CTtable[, c(sessionCol)]), FUN = sum, na.rm = TRUE)     # sum of operational cameras at the station
        dat2.na <- aggregate(camOp_filled, by = list(CTtable[, c(stationCol)],
                                                     CTtable[, c(sessionCol)]), FUN = function(X){all(is.na(X))})     # TRUE if no camera was set up that day
        
        if(sessionColInArgs)  row.names(dat2) <- row.names(dat2.na) <- paste(dat2[,1], dat2[,2], sep = separatorSession)
        if(!sessionColInArgs) row.names(dat2) <- row.names(dat2.na) <- dat2[,1]
        
        dat2[,c(1,2)] <- dat2.na[,c(1,2)] <- NULL
        
        dat2     <- as.matrix(dat2)
        dat2.na  <- as.matrix(dat2.na)
        
        if(any(dat2.na))  dat2[which(dat2.na)] <- NA
        
        if(camerasIndependent == FALSE){
          dat2 <- ifelse(dat2 >= 2, 1, dat2)
        }
        dat2 <- as.data.frame(dat2)
      }
    } else {   # if ( byCamera = TRUE)
      dat2 <- as.data.frame(camOp_filled)
    }
  } else {
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
