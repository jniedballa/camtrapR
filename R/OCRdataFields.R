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
