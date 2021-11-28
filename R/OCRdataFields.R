#' Optical character recognition (OCR) from data fields in digital images
#' 
#' 
#' Extracts information from the data fields in camera trap images (not the
#' metadata). Many camera traps include data fields in camera trap images,
#' often including date and time of images, and sometimes other information.
#' This function extracts the information from these fields using optical
#' character recognition provided by the package \pkg{tesseract} after reading
#' images using the package \pkg{magick}.
#' 
#' Normally all these information should be in the image metadata. This
#' function is meant as a last resort if image metadata are unreadable or were
#' removed from images. OCR is not perfect and may misidentify characters, so
#' check the output carefully.
#' 
#' The output of this function can be used in
#' \code{\link{writeDateTimeOriginal}} to write date/time into the
#' DateTimeOriginal tag in image metadata, making these images available for
#' automatic processing with \code{\link{recordTable}} and other functions that
#' extract image metadata.
#' 
#' 
#' This function reads all images in inDir (including subdirectories), crops
#' them to the geometries in the "geometries" list, and performs optical
#' character recognition (OCR) on each of these fields (leveraging the magick
#' and tesseract packages).
#' 
#' Geometries are defined with \code{geometry_area} from \pkg{magick}. See
#' \code{\link[magick]{geometry}} for details on how to specify geometries with
#' \code{geometry_area}. The format is: "widthxheight+x_off+y_off", where:
#' 
#' \describe{ \item{width}{width of the area of interest} \item{height}{height
#' of the area of interest} \item{x_off}{offset from the left side of the
#' image} \item{y_off}{offset from the top of the image} }
#' 
#' Units are pixels for all fields. digiKam can help in identifying the correct
#' specification for geometries. Open the Image Editor, left-click and draw a
#' box around the data field of interest. Ensure the entire text field is
#' included inside the box, but nothing else. Now note two pairs of numbers at
#' the bottom of the window, showing the offsets and box size as e.g.:
#' 
#' "(400, 1800) (300 x 60)"
#' 
#' This corresponds to the geometry values as follows:
#' 
#' "(x_off, y_off) (width x height)"
#' 
#' Using these values, you'd run:
#' 
#' \code{geometry_area(x_off = 400, y_off = 1800, width = 300, height = 60)}
#' 
#' and receive
#' 
#' "300x60+400+1800"
#' 
#' as your geometry.
#' 
#' OCR in tesseract has problems with white font on black background. If that
#' is the case in your images, set \code{invert} to \code{TRUE} to invert the
#' image and ensure OCR uses black text on white background.
#' 
#' Even then, output will not be perfect. Error rates in OCR depend on multiple
#' factors, including the text size and font type used. We don't have control
#' over these, so check the output carefully and edit as required.
#' 
#' @param inDir character. Directory containing camera trap images (or
#' subdirectories containing images)
#' @param geometries list. A (possibly named) list of geometry strings defining
#' the image area(s) to extract.
#' @param invert logical. Invert colors in the image? Set to TRUE if text in
#' data field is white on black background. Leave if FALSE if text is black in
#' white background.
#' 
#' @return A \code{data.frame} with original directory and file names, and
#' additional columns for the OCR data of each extracted geometry.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{writeDateTimeOriginal}}
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' # dontrun is to avoid forcing users to install additional dependencies
#' 
#' wd_images_OCR <- system.file("pictures/full_size_for_ocr", package = "camtrapR")
#' 
#' library(magick)
#' 
#' # define geometries
#' geometry1 <- geometry_area(x_off = 0, y_off = 0, width = 183, height = 37)
#' geometry2 <- geometry_area(x_off = 196, y_off = 0, width = 200, height = 17)
#' geometry3 <- geometry_area(x_off = 447, y_off = 0, width = 63, height = 17)
#' geometry4 <- geometry_area(x_off = 984, y_off = 0, width = 47, height = 17)
#' geometry5 <- geometry_area(x_off = 0, y_off = 793, width = 320, height = 17)
#' 
#' # combine geometries into list
#' geometries <- list(date = geometry1, 
#'                    time = geometry2, 
#'                    sequence_id = geometry3,
#'                    temperature = geometry4,
#'                    camera_model = geometry5)
#' 
#' df_image_data  <- OCRdataFields(inDir = wd_images_OCR,
#'                                 geometries = geometries, 
#'                                 invert = TRUE)       
#' df_image_data
#' 
#' # note the mistake in "camera_model"
#' # it should be "PC850", not "PC8S0O"
#' # date and time are correct though
#'                    
#' }
#' 
#' 
#' 
#' @export OCRdataFields
#' 
OCRdataFields <- function(inDir,
                          geometries,
                          invert = FALSE){
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Please install the package magick to run this function")
  }
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    stop("Please install the package tesseract to run this function")
  }
  
  if(class(inDir) !=  "character") stop("inDir must be a character string")
  if(class(geometries) !=  "list") stop("geometries must be a list")
  if(!is.logical(invert)) stop("invert must be logical (TRUE/FALSE)")
  
  lf_full  <- list.files(inDir, recursive = TRUE, full.names = TRUE, pattern = ".JPG$|.jpg$|jpeg$")
  lf_short <- list.files(inDir, recursive = TRUE, full.names = FALSE, pattern = ".JPG$|.jpg$|jpeg$")
  
  if(length(lf_short) == 0) stop("Did not find any images")
  
  img <- magick::image_read(lf_full)
  
  out_list <- lapply(geometries, function (x) {
    # crop image to data field only
    img_crop <- magick::image_crop(img, 
                           geometry = x)  
    
    if(invert) {
      img_crop <- magick::image_negate(img_crop)
    }
    
    # add a white margin to help OCR
    img_crop <- magick::image_border(img_crop, color = "white")
    
    
    ocr_list <- lapply(img_crop, magick::image_ocr)
    
    # remove line break at end of string
    ocr_unlist <- gsub("\n$", "", unlist(ocr_list))
    
    return(ocr_unlist)
  })
  
  if(is.null(names(geometries))){
    names(out_list) <- paste(geometries)
  }
  
  out_df1 <- as.data.frame(out_list)
  
  out <- data.frame(filename_full = lf_full,
                    # filename_short = lf_short,
                    out_df1)
  
  return(out)
}


# NOTE: it would be good to offer the option to write those tags to the DateTimeOriginal field in image metadata
# maybe not immediately so users can first check everything is in order. 
# exiftool syntax see Q5: https://exiftool.org/faq.html
