exifTagNames <- function(inDir, 
                                  whichSubDir = 1, 
                                  returnMetadata = FALSE, 
                                  returnTagGroup = TRUE){

  stopifnot(file.exists(inDir))
  stopifnot(is.logical(returnMetadata))
  stopifnot(is.logical(returnTagGroup))
  
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool")
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
  
  dirs.tmp <- list.dirs(inDir, recursive = FALSE, full.names = TRUE)

  if(file.exists(dirs.tmp[whichSubDir]) == FALSE) stop("the specified subdirectory does not exist. Check argument 'whichSubDir'")

  file.tmp <- list.files(dirs.tmp[whichSubDir],
                         full.names = TRUE,
                         pattern    = ".JPG$|.jpg$",
                         recursive  = TRUE)[1]
  if(length(file.tmp) == 0) stop(paste("found no jpg in ", dirs.tmp[whichSubDir], sep = "\n"))

  message(paste("Extracted metadata of:", file.tmp))
  
  if(returnMetadata == FALSE){
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