exifTagNames <- function(inDir, 
                         whichSubDir = 1, 
                         fileName,
                         returnMetadata = FALSE, 
                         returnTagGroup = TRUE){
  
  
  stopifnot(is.logical(returnMetadata))
  stopifnot(is.logical(returnTagGroup))
  
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  
  if(hasArg(fileName)){
    
    if(!hasArg(inDir)){
      file.tmp <- fileName
    } else {
      if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
      file.tmp <- file.path(inDir, fileName)
    }
    if(!file.exists(file.tmp)) stop(paste(file.tmp, "not found"))
    
  } else { # if fileName is not defined
    
    if(!hasArg(inDir))     stop("fileName is not defined. Please define 'inDir' then", call. = FALSE)
    if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
    
    if(is.numeric(whichSubDir)){
      dir.tmp <- list.dirs(inDir, recursive = FALSE, full.names = TRUE)[whichSubDir]
      if(!dir.exists(dir.tmp)) stop("the specified subdirectory does not exist. Check argument 'whichSubDir'")
    }
    if(is.character(whichSubDir)){
      dir.tmp <- file.path(inDir, whichSubDir)
    }
    
    file.tmp <- list.files(dir.tmp,
                           full.names = TRUE,
                           pattern    = ".JPG$|.jpg$",
                           recursive  = TRUE)[1]
    if(length(file.tmp) == 0) stop(paste("found no jpg in ", dir.tmp, sep = "\n"))
    
  }
  
  
  if(!returnMetadata)  message(paste("Metadata of:", file.tmp, sep = "\n"))
  if(returnMetadata)  message(paste("Metadata values of:", file.tmp, sep = "\n"))
  
  if(!isTRUE(returnMetadata)){
    command.tmp  <- paste('exiftool -csv', ifelse(returnTagGroup, ' -G',  ''), ' "', file.tmp, '"', sep = "")
    metadata.tmp <- system(command.tmp, intern=TRUE)
    tagnames     <- sort(unlist(strsplit(metadata.tmp[[1]], split = ",")))
    return (tagnames)
  } else {
    command.tmp  <- paste('exiftool ', ifelse(returnTagGroup, ' -G',  ''), ' "', file.tmp, '"', sep = "")
    metadata.tmp <- system(command.tmp, intern=TRUE)
    return (metadata.tmp)
  }
}