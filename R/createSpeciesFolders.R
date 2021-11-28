#' Create species directories for species identification
#' 
#' This function creates species subdirectories within station directories.
#' They can be used for species identification by manually moving images into
#' the respective species directories. The function can also delete empty
#' species directories (if species were not detected at sites). It is not
#' necessary to run this function if animals will be identified by metadata
#' tagging.
#' 
#' This function should be run after \code{\link{imageRename}}. Empty
#' directories can be created as containers for species identification if
#' images are identified with the drag & drop method. After species
#' identification is complete, empty species directories can be deleted using
#' \code{removeFolders = TRUE}. The function will delete only directories which
#' are specified in \code{species}. If \code{hasCameraFolders} was set to
#' \code{TRUE} in function \code{\link{imageRename}}, \code{hasCameraFolders}
#' must be set to \code{TRUE} here too. Species directories will then be
#' created within each camera subdirectory of each station directory. if the
#' user wishes to identify species by metadata tagging, running this function
#' is not needed.
#' 
#' @param inDir character. Directory containing camera trap images sorted into
#' station subdirectories (e.g. inDir/StationA/)
#' @param hasCameraFolders logical. Do the station directories in \code{inDir}
#' have camera-subdirectories (e.g. inDir/StationA/CameraA1;
#' inDir/StationA/CameraA2)?
#' @param species character. names of species directories to be created in
#' every station (or station/camera) subdirectory of \code{inDir}
#' @param removeFolders logical. Indicating whether to create (TRUE) or remove
#' (FALSE) species directories .
#' @return A \code{data.frame} with directory names and an indicator for
#' whether directories were created or deleted.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # create dummy directories for tests
#' # (normally, you'd use directory containing renamed, unsorted images)
#' 
#' # this will be used as inDir
#' wd_createDirTest <- file.path(getwd(), "createSpeciesFoldersTest")
#' 
#' # now we create 2 station subdirectories
#' dirs_to_create <- file.path(wd_createDirTest, c("StationA", "StationB"))
#' sapply(dirs_to_create, FUN = dir.create, recursive = TRUE)
#' 
#' # species names for which we want to create subdirectories
#' species <- c("Sambar Deer", "Bay Cat")
#' 
#' # create species subdirectories
#' SpecFolderCreate1 <- createSpeciesFolders (inDir               = wd_createDirTest,
#'                                            species             = species,
#'                                            hasCameraFolders = FALSE,
#'                                            removeFolders       = FALSE)
#'   
#' SpecFolderCreate1
#' 
#' # check if directories were created
#' list.dirs(wd_createDirTest)
#' 
#' # delete empty species directories
#' SpecFolderCreate2 <- createSpeciesFolders (inDir               = wd_createDirTest,
#'                                            species             = species,
#'                                            hasCameraFolders = FALSE,
#'                                            removeFolders       = TRUE)
#' 
#' SpecFolderCreate2
#' 
#' # check if species directories were deleted
#' list.dirs(wd_createDirTest)
#' 
#' }
#' 
#' @export createSpeciesFolders
#' 
createSpeciesFolders <- function(inDir,
                                 hasCameraFolders,
                                 species,
                                 removeFolders = FALSE){

  stopifnot(is.character(species))
  stopifnot(is.logical(removeFolders))
  stopifnot(is.logical(hasCameraFolders))
  
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)

  dirs       <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE , recursive = FALSE)

  if(length(dirs) == 0) stop("inDir has no (station) subdirectories")

  if(hasCameraFolders == TRUE){
    dirs.to.create <- file.path(rep(list.dirs(dirs, recursive = FALSE), each = length(species)), species)
  } else {
    dirs.to.create <- file.path(rep(dirs, each = length(species)), species)
  }

  if(removeFolders == FALSE){
    tmp.create <- suppressWarnings(sapply(dirs.to.create, FUN = dir.create, showWarnings = TRUE, recursive = FALSE))
    dat.out1 <- data.frame(directory = dirs.to.create,
                           created = tmp.create,
                           exists = file.exists(dirs.to.create))
    rownames(dat.out1) <- NULL

    message(paste("created", sum(tmp.create == TRUE), "directories"))
    if(sum(tmp.create == FALSE) != 0){
      message(paste(sum(tmp.create == FALSE & file.exists(dirs.to.create)), "directories already existed"))
    }
    return(dat.out1)

  } else {

    dirs.to.delete <- dirs.to.create[file.exists(dirs.to.create)]

    if(length(dirs.to.delete) == 0) stop("nothing to delete: the specified species subdirectories do not exist")

    n.images.per.folder <- sapply(sapply(dirs.to.delete, FUN = list.files, recursive = TRUE), FUN = length)

    which.to.delete <- which(n.images.per.folder == 0)
    which.not.to.delete <- which(n.images.per.folder != 0)

    unlink(dirs.to.delete[which.to.delete], recursive=TRUE)

    Sys.sleep(0.2)     # because a network drive may sync too slowly and thus can still show a deleted dir as existing

    dat.out2 <- data.frame(directory = dirs.to.delete,
                           still.exists = file.exists(dirs.to.delete))

    if(length(which.to.delete)!= 0){
      message(paste("deleted", sum(dat.out2$still.exists == FALSE), "empty directories"))
    }

    if(length(which.not.to.delete)!= 0){
      cat("\n")
      message(paste("could not delete", sum(dat.out2$still.exists == TRUE), "non-empty directories"))
    }
    return(dat.out2)
  }
}
