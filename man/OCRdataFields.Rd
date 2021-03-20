\name{OCRdataFields}
\alias{OCRdataFields}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optical character recognition (OCR) from data fields in digital images
}
\description{

Extracts information from the data fields in camera trap images (not the metadata). Many camera traps include data fields in camera trap images, often including date and time of images, and sometimes other information. This function extracts the information from these fields using optical character recognition provided by the package \pkg{tesseract} after reading images using the package \pkg{magick}.

Normally all these information should be in the image metadata. This function is meant as a last resort if image metadata are unreadable or were removed from images. OCR is not perfect and may misidentify characters, so check the output carefully. 

}
\usage{
OCRdataFields(inDir,
              geometries,
              invert = FALSE
              )
}

\arguments{
  \item{inDir}{
     character. Directory containing camera trap images (or subdirectories containing images)
}

  \item{geometries}{
     list. A (possibly named) list of geometry strings defining the image area(s) to extract.
}
 
   \item{invert}{
     logical. Invert colors in the image? Set to TRUE if text in data field is white on black background. Leave if FALSE if text is black in white background.
}
 
}
\details{

This function reads all images in inDir (including subdirectories), crops them to the geometries in the "geometries" list, and performs optical character recognition (OCR) on each of these fields (leveraging the magick and tesseract packages).

Geometries are defined with \code{geometry_area} from \pkg{magick}. See \code{\link[magick]{geometry}} for details on how to specify geometries with \code{geometry_area}. The format is: "widthxheight+x_off+y_off", where:


 \describe{
    \item{width}{width of the area of interest}
    \item{height}{height of the area of interest}
    \item{x_off}{offset from the left side of the image}
    \item{y_off}{offset from the top of the image}
}



Units are pixels for all fields. digiKam can help in identifying the correct specification for geometries. Open the Image Editor, left-click and draw a box around the data field of interest. Ensure the entire text field is included inside the box, but nothing else. Now note two pairs of numbers at the bottom of the window, showing the offsets and box size as e.g.:

"(400, 1800) (300 x 60)"

This corresponds to the geometry values as follows:

"(x_off, y_off) (width x height)"

Using these values, you'd run:

\code{geometry_area(x_off = 400, y_off = 1800, width = 300, height = 60)}

and receive

"300x60+400+1800"

as your geometry.


OCR in tesseract has problems with white font on black background. If that is the case in your images, set \code{invert} to \code{TRUE} to invert the image and ensure OCR uses black text on white background. 


Even then, output will not be perfect. Error rates in OCR depend on multiple factors, including the text size and font type used. We don't have control over these, so check the output carefully and edit as required.

}
\value{
A \code{data.frame} with original directory and file names, and additional columns for the OCR data of each extracted geometry.
}

\author{
Juergen Niedballa
}

\examples{

\dontrun{
# dontrun is to avoid forcing users to install additional dependencies

wd_images_OCR <- system.file("pictures/full_size_for_ocr", package = "camtrapR")

library(magick)

# define geometries
geometry1 <- geometry_area(x_off = 0, y_off = 0, width = 183, height = 37)
geometry2 <- geometry_area(x_off = 196, y_off = 0, width = 200, height = 17)
geometry3 <- geometry_area(x_off = 447, y_off = 0, width = 63, height = 17)
geometry4 <- geometry_area(x_off = 984, y_off = 0, width = 47, height = 17)
geometry5 <- geometry_area(x_off = 0, y_off = 793, width = 320, height = 17)

# combine geometries into list
geometries <- list(date = geometry1, 
                   time = geometry2, 
                   sequence_id = geometry3,
                   temperature = geometry4,
                   camera_model = geometry5)

df_image_data  <- OCRdataFields(inDir = wd_images_OCR,
                                geometries = geometries, 
                                invert = TRUE)       
df_image_data

# note the mistake in "camera_model"
# it should be "PC850", not "PC8S0O"
# date and time are correct though
                   
}


}