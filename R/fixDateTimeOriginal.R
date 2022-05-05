#' Fix DateTimeOriginal Exif metadata tag in Reconyx Hyperfire cameras
#' 
#' Some camera models don't store the date/time information in the standard
#' Exif metadata tag. Consequently, camtrapR cannot find that information. This
#' function uses Exiftool to update the DateTimeOriginal metadata tag in all
#' images within a directory to make them readable with camtrapR (and other
#' software).
#' 
#' Some Reconyx Hyperfire cameras (e.g. HC500) are known to show this problem.
#' 
#' @param inDir character. Name of the directory containing images to be fixed
#' @param recursive logical. Recursively find images in subdirectories of
#' \code{inDir}?
#' 
#' @return Returns invisibly the messages returned by the Exiftool call
#' (warnings etc.).
#' 
#' @section Warning : Please make a backup of your images before running this
#' function.
#' 
#' @author Juergen Niedballa
#' 
#' @references This function uses the code from: \cr Tobler, Mathias (2015).
#' Camera Base Version 1.7 User Guide
#' \url{https://www.atrium-biodiversity.org/tools/camerabase/files/CameraBaseDoc1.7.pdf}
#' 
#' @examples
#' 
#' \dontrun{
#' # a hypothetical example
#' 
#' wd_images_hyperfire <- "C:/Some/Directory"
#' 
#' fixDateTimeOriginal(inDir     = wd_images_hyperfire,
#'                     recursive = TRUE)
#' }
#' 
#' @export fixDateTimeOriginal
#' 
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
