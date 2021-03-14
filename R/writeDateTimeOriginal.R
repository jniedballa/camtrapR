writeDateTimeOriginal <- function(DateTimeOriginal, 
                                  fileNames,
                                  parallel,
                                  overwrite = FALSE) {
  
  stopifnot(is.character(DateTimeOriginal))
  stopifnot(is.character(fileNames))
  
  if(length(DateTimeOriginal) != length(fileNames)) stop("DateTimeOriginal and fileNames must have same length")
  
  exif_call <- paste0('exiftool -datetimeoriginal="', DateTimeOriginal, '" ', 
                      ifelse(overwrite, "-overwrite_original ", " "), fileNames)
  
  
  if(hasArg(parallel)){
    if (!requireNamespace("snow", quietly = TRUE)) {
      stop("Please install the package snow to run this function in parallel")
    }
    if(!is(parallel, "cluster")) stop("parallel must be a cluster object, i.e. output of snow::makeCluster()", call. = FALSE)
    
    out <- snow::parLapply(parallel, exif_call, system)
    
  } else {
    out <- lapply(exif_call, system)
  }
  return(invisible(NULL))
}
