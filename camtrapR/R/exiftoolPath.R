exiftoolPath <- function(exiftoolDir){
  WPATH <- Sys.getenv("PATH")
  WPATH1 <- paste(exiftoolDir, WPATH, sep=";")
  Sys.setenv(PATH=WPATH1)
  return(invisible(grepl(exiftoolDir,  Sys.getenv("PATH"))))
}
