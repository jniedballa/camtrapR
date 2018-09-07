addCopyrightTag <- function(inDir,
                            copyrightTag,
                            askFirst = TRUE,
                            keepJPG_original = TRUE){

  stopifnot(is.character(inDir))
  stopifnot(file.exists(inDir))

  stopifnot(is.character(copyrightTag))
  stopifnot(length(copyrightTag) == 1)

  stopifnot(is.logical(keepJPG_original))

  if(!isTRUE(keepJPG_original)) {
    overwrite_command <- " -overwrite_original "
  } else {overwrite_command <- " "}

  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(isTRUE(askFirst)){
    message(paste("You are about to add the copyright tag '", copyrightTag, "' to all images in ", inDir, ". Are you sure?", sep = ""))
    switch(menu(c("YES", "NO")),
           doit <- TRUE,
           doit <- FALSE)
  } else {
    doit <- TRUE
  }

  if(isTRUE(doit)){

    command.tmp  <- paste('exiftool -r', overwrite_command  ,'-Copyright="', copyrightTag, '" "', inDir, '"', sep = "")

    output <- system(command.tmp, show.output.on.console = FALSE, intern = TRUE)
    message(paste(output[length(output) - 1],
                  output[length(output)]))
    return(invisible(output))
  } else {
    message("Did not write copyright tag")
    return(invisible(NULL))
  }
}