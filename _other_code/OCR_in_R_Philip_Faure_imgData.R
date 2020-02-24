imgData <- function(inDir, topOrBottom, whiteList, outDir, name) {
  
  # Need to also add an argument for selecting the different camera types (e.g. Bushnell, Cuddeback, Reconyx, etc.) and then set the image_crop() function to crop as needed by the different types of images.
  
  library(magick)
  library(tesseract)
  # List all the files which you would like to run OCR on:
  files <- list.files(inDir, 
                      full.names = TRUE)
  
  # Convert images to smaller images, convert, crop, stack, run:
  text <- image_read(files) %>%
    image_convert(type = 'grayscale') %>% 
    image_crop( 
      if (isTRUE(topOrBottom == "top")) {invisible("1890x32")}
      else {if (isTRUE(topOrBottom == "bottom")) {invisible("1920x1080+0-1048")}}
      # Still trying to write code for an error message:
      # else {message('Please select either "top" or "bottom" to specify whether you would like to extract the information from the top or bottom of your images.')}
    ) %>% 
    
    image_append(stack=T) %>%
    image_ocr(options = list(tessedit_char_whitelist = paste(
      whiteList, "/0123456789:-°", sep = "")
    )
    )
  
  cat(text, file = paste(outDir, "/imgOCR_", name, ".", "csv", sep = ""))
  # If you use only cat(text) here, then it prints the data perfectly, but when saving it to 
  # a .csv file then it creates problems with the temperature section.
}

# Test:
imgData(inDir = "/Users/philipfaure/Desktop/test", 
        topOrBottom = "top", 
        whiteList = "ACMP", 
        outDir = "/Users/philipfaure/Desktop",
        name = "imageOCR666")

imgData(inDir = "/Users/philipfaure/Desktop/test", 
        topOrBottom = "bottom", 
        whiteList = "LAJ",
        outDir = "/Users/philipfaure/Desktop",
        name = "imageOCR2")