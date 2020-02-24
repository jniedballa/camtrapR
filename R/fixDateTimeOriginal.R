fixDateTimeOriginal <- function(inDir,
                                recursive = TRUE){

  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  stopifnot(is.logical(recursive))
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)

  command.tmp  <- paste('exiftool "-DateTimeOriginal>DateTimeOriginal" ',  ifelse(recursive, '-r ', '') , '-overwrite_original -ext JPG "', inDir, '"', sep = '')

  tmp <- system(command.tmp,  intern=TRUE)

  message(tmp[c(length(tmp) - 1, length(tmp))])

  return(invisible(tmp))
}