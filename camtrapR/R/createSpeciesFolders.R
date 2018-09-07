createSpeciesFolders <- function(inDir,
                                 hasCameraFolders,
                                 species,
                                 removeFolders = FALSE){

  stopifnot(is.character(species))
  stopifnot(is.logical(removeFolders))
  stopifnot(is.logical(hasCameraFolders))
  
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)

  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
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