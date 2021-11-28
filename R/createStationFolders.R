#' Create camera trap station directories for raw camera trap images
#' 
#' This function creates camera trap station directories, if needed with camera
#' subdirectories. They can be used as an initial directory structure for
#' storing raw camera trap images.
#' 
#' 
#' The empty directories serve as containers for saving raw camera trap images.
#' If more than 1 camera was set up at a station, specifying \code{cameras} is
#' required in order to keep images from different cameras separate. Otherwise,
#' generic filenames (e.g., IMG0001.JPG) from different cameras may lead to
#' accidental overwriting of images if images from these cameras are saved in
#' one station directory.
#' 
#' @param inDir character. Directory in which station directories are to be
#' created
#' @param stations character. Station IDs to be used as directory names within
#' \code{inDir}
#' @param cameras character. Camera trap IDs to be used as subdirectory names
#' in each station directory (optionally)
#' @param createinDir logical. If inDir does not exist, create it?
#' 
#' @return A \code{data.frame} with station (and possibly camera) directory
#' names and an indicator for whether they were created successfully.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # create dummy directory for tests (this will be used as inDir)
#' # (normally, you'd set up an empty directory, e.g. .../myStudy/rawImages)
#' wd_createStationDir <- file.path(tempdir(), "createStationFoldersTest")
#' 
#' # now we load the sample camera trap station data frame
#' data(camtraps)
#' 
#' # create station directories in wd_createStationDir
#' StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
#'                                               stations    = as.character(camtraps$Station),
#'                                               createinDir = TRUE)
#'   
#' StationFolderCreate1
#' 
#' # check if directories were created
#' list.dirs(wd_createStationDir)
#' 
#' }
#' 
#' @export createStationFolders
#' 
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
