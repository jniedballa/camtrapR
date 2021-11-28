#' Add a directory to PATH temporarily
#' 
#' Temporarily adds a directory to the environmental variable PATH for system
#' calls from within R. This allows Windows users to store exiftool.exe
#' anywhere on their hard drive. It is not needed on Linux or MacOS machines.
#' 
#' Several functions within this package depend on ExifTool. Under Windows,
#' exiftool.exe cannot be used if it is not in a directory path specified in
#' PATH. This can be solved by adding the directory containing exiftool.exe for
#' temporary use within the running R process.
#' 
#' @param exiftoolDir character. the directory in the file system containing
#' exiftool.exe.
#' 
#' @return invisible logical indicating whether \code{exiftoolDir} was added to
#' PATH successfully (in the running R process).
#' 
#' @note The directories in PATH can be queried by \code{Sys.getenv("PATH")}.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' exiftool_dir <- "C:/Path/To/Exiftool"
#' exiftoolPath(exiftoolDir = exiftool_dir)
#' 
#' # check if it has been added to PATH
#' grepl(exiftool_dir,  Sys.getenv("PATH"))
#' 
#' @export exiftoolPath
#' 
exiftoolPath <- function(exiftoolDir){
  WPATH <- Sys.getenv("PATH")
  WPATH1 <- paste(exiftoolDir, WPATH, sep=";")
  Sys.setenv(PATH=WPATH1)
  return(invisible(grepl(exiftoolDir,  Sys.getenv("PATH"))))
}
