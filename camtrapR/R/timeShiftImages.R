timeShiftImages <- function(inDir,
                            hasCameraFolders,
                            timeShiftTable,
                            stationCol,
                            cameraCol,
                            timeShiftColumn,
                            timeShiftSignColumn,
                            undo = FALSE
)
{
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool")

  stopifnot(class(timeShiftTable) == "data.frame")
  
  # convert all columns to character
  for(i in 1:ncol(timeShiftTable)){
    timeShiftTable[,i] <- as.character(timeShiftTable[,i])
  }
  rm(i)
  
  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, timeShiftColumn = timeShiftColumn, timeShiftSignColumn = timeShiftSignColumn)
  if(!stationCol %in% colnames(timeShiftTable))         stop(paste('stationCol = "',   stationCol,     '" is not a column name in timeShiftTable', sep = ''), call. = FALSE)
  if(!timeShiftColumn %in% colnames(timeShiftTable))  stop(paste('timeShiftColumn = "',   timeShiftColumn,     '" is not a column name in timeShiftTable', sep = ''), call. = FALSE)
  if(!timeShiftSignColumn %in% colnames(timeShiftTable))  stop(paste('timeShiftSignColumn = "', timeShiftSignColumn,       '" is not a column name in timeShiftTable', sep = ''), call. = FALSE)

  
  if(isTRUE(hasCameraFolders)){
    stopifnot(cameraCol %in% colnames(timeShiftTable))
  } else {
    if(any(duplicated(timeShiftTable[,stationCol]))) stop("There are duplicates in stationCol. Check argument hasCameraFolders")
  }
   
 
  stopifnot(is.logical(hasCameraFolders))
  for(xy in 1:nrow(timeShiftTable)){
    if(length(unlist(strsplit(timeShiftTable[xy,timeShiftColumn], split = " "))) != 2) stop(paste("there is more than 1 space in your timeShiftColumn string. Only 1 space is allowed (", timeShiftTable[xy,stationCol], ")"))
    if(nchar(timeShiftTable[xy,timeShiftColumn]) - nchar(gsub(":","",timeShiftTable[xy,timeShiftColumn])) != 4) stop("there should be 4 colons in timeShiftColumn (", timeShiftTable[xy,stationCol], ")")
    if(timeShiftTable[xy,timeShiftSignColumn] %in% c("+", "-") == FALSE) stop("timeShiftSignColumn can only be + or - (",
                                                                              timeShiftTable[xy,stationCol], ")")
    if(length(unlist(lapply(strsplit(timeShiftTable[xy,timeShiftColumn], split = " "), FUN = strsplit, split = ":"))) != 6) stop("there must be six numbers in timeShiftColumn (",
                                                                                                                                 timeShiftTable[xy,stationCol], ")")
  }

  
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
  
  if(isTRUE(hasCameraFolders)){
    shift.dirs <- file.path(inDir, timeShiftTable[,stationCol], timeShiftTable[,cameraCol])
  } else {
    shift.dirs <- file.path(inDir, timeShiftTable[,stationCol])
  }

  if(any(file.exists(shift.dirs) == FALSE)){
    stop(paste(shift.dirs[file.exists(shift.dirs) == FALSE], "does not exist"))
  }

  if(isTRUE(undo)){

  results_undo <- data.frame(directory = rep(NA, times = nrow(timeShiftTable)),
                             n_images_undo = rep(NA, times = nrow(timeShiftTable)))
   
  jpg2undo <- list()
  jpg2keep   <- list()
  jpg2keep_newname <- list()
  remove.tmp <- rename.tmp <- list()
  
    for(i in 1:length(shift.dirs)){
      jpg2undo[[i]]       <- list.files(shift.dirs[i], pattern = ".jpg$|.JPG$", recursive = TRUE, full.names = TRUE)
      jpg2keep[[i]]         <- list.files(shift.dirs[i], pattern = ".jpg_original$|.JPG_original$", recursive = TRUE, full.names = TRUE)
      jpg2keep_newname[[i]] <- gsub(pattern = "_original$", replacement = "", x = jpg2keep[[i]])
      
      if(length(jpg2keep[[i]]) == 0) stop(paste("found no .JPG_original files in", shift.dirs[i], "check argument 'undo'"))
      if(length(jpg2undo[[i]]) != length(jpg2keep[[i]])) stop(paste("number of jpgs in", shift.dirs[i], "is not equal to the number of JPG_original files" ))

      

	  results_undo$directory[i] <- shift.dirs[i]
	  results_undo$n_images_undo[i] <- length(jpg2undo[[i]])
    }
	
	remove.tmp <- lapply(jpg2undo, FUN = file.remove)
	for(xyz in 1:length(jpg2keep)){
		rename.tmp[[xyz]] <- file.rename(from = jpg2keep[[xyz]], to = jpg2keep_newname[[xyz]])
	}
	
	return(results_undo)
	  
  } else {

    results.tmp <- list()

    for(i in 1:nrow(timeShiftTable)){
      message(shift.dirs[i])
      list.files(shift.dirs)
      command.tmp2b <- paste('exiftool -r "-DateTimeOriginal', timeShiftTable[i,timeShiftSignColumn], '=',
                             timeShiftTable[i,timeShiftColumn], '" "', shift.dirs[i], '"', sep = "")
      results.tmp[[i]] <- system(command.tmp2b, intern=TRUE)
      rm(command.tmp2b)
    }

    results.tmp2 <- lapply(results.tmp, FUN = function(x){x[c(length(x) - 1, length(x))]})
    
    output <- data.frame(shift.dirs, matrix(unlist(results.tmp2),ncol = 2, byrow = 2))
    colnames(output) <- c("directory", "n_directories", "n_images")
    return(output)
  }
}