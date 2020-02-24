createStationFolders <- function(inDir,
                                 stations,
                                 cameras,
                                 createinDir
                                 ){

# check input

if(hasArg(createinDir) == FALSE) createinDir <- FALSE
stopifnot(is.logical(createinDir))

if(createinDir == FALSE & !dir.exists(inDir))  stop("Could not find inDir:\n", inDir, call. = FALSE)
if(createinDir == TRUE  & !dir.exists(inDir))  dir.create(inDir, recursive = TRUE)

  stopifnot(is.character(stations))
  if(hasArg(cameras)){
  stopifnot(is.character(cameras))
  stopifnot(length(stations) == length(cameras))
  }
 
 # create directories
   if(hasArg(cameras)){
  dirs.to.create <- file.path(inDir, stations, cameras)
  
  tmp.create <- suppressWarnings(sapply(dirs.to.create, FUN = dir.create, showWarnings = TRUE, recursive = TRUE))
    dat.out1 <- data.frame(station = stations, 
                           camera = cameras,
                           directory = dirs.to.create,
                           created = tmp.create,
                           exists = file.exists(dirs.to.create))
    rownames(dat.out1) <- NULL

    message(paste("created", sum(tmp.create == TRUE), "directories"))
    if(sum(tmp.create == FALSE) != 0){
      message(paste(sum(tmp.create == FALSE & file.exists(dirs.to.create)), "directories already existed"))
    }
    return(dat.out1)

 } else {
 
 if(any(duplicated(stations))) stop("duplicates in stations are not allowed if cameras is not defined")
  dirs.to.create <- file.path(inDir, stations)
  
  
    tmp.create <- suppressWarnings(sapply(dirs.to.create, FUN = dir.create, showWarnings = TRUE, recursive = TRUE))
    dat.out2 <- data.frame(station = stations, 
                            directory = dirs.to.create,
                           created = tmp.create,
                           exists = file.exists(dirs.to.create))
    rownames(dat.out2) <- NULL

    message(paste("created", sum(tmp.create == TRUE), "directories"))
    if(sum(tmp.create == FALSE) != 0){
      message(paste(sum(tmp.create == FALSE & file.exists(dirs.to.create)), "directories already existed"))
    }
    return(dat.out2)
}
}