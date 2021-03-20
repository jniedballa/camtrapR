\name{writeDateTimeOriginal}
\alias{writeDateTimeOriginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write values to DateTimeOriginal tag in image metadata
}
\description{

This function assigns values to the DateTimeOriginal tag in image's EXIF metadata using Exiftool. It can be used when the original DateTimeOriginal values in the metadata were lost for whatever reason. In order to first read the Date/Time values from the data fields in the images, see the function \code{\link{OCRdataFields}}. After running \code{OCRdataFields} and checking its output you can run \code{writeDateTimeOriginal}.

}
\usage{
writeDateTimeOriginal(DateTimeOriginal, 
                      fileNames,
                      parallel,
                      overwrite = FALSE
                      ) 
}

\arguments{
  \item{DateTimeOriginal}{
     character. DateTimeOriginal values to write into EXIF:DateTimeOriginal tag of the files in fileNames.
}

  \item{fileNames}{
     character. Full file names (including directories) of images to process.
}
 
   \item{parallel}{
     A snow cluster object. Specify if you wish to run this function in parallel. Provide a cluster object (output of makeCluster()) - optional.
}
   
   \item{overwrite}{
     logical. Overwrite existing files (TRUE) or create new files while saving the original data as jpg_original files as a backup (FALSE)?
}
 
}
\details{

The first value in \code{DateTimeOriginal} will be assigned to the first image in \code{fileNames}, and so on. Both \code{DateTimeOriginal} and \code{fileNames} can be obtained from the output of \code{\link{OCRdataFields}}. 
\code{DateTimeOriginal} uses the standard "YYYY-MM-SS HH:MM:SS"" notation. If the values extracted via \code{\link{OCRdataFields}} are in a different format you'll need to reformat them first. Please provide them as character. Also, before using this function, make sure that the date/time values read by \code{\link{OCRdataFields}} are correct (sometimes OCR misreads values, so check carefully).

Parallel processing is advised since the function is rather slow (due to calling Exiftool separately on every, so about 1 second per image). If you know how to batch-assign DateTimeOriginal values in one Exiftool call, please let me know. 


The function only works on JPG images, not video files. 

}
\value{
Invisible NULL. The actual output is the JPG images which now have a DateTimeOriginal tag. 
}

\author{
Juergen Niedballa
}

\seealso{
\code{\link{OCRdataFields}}
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



library(snow)
library(lubridate)

# prepare DateTimeOriginal column (ymd_hms() automatically respects the PM indicator)
df_image_data$DateTimeOriginal <- paste(df_image_data$date, df_image_data$time)
df_image_data$DateTimeOriginal <- as.character(ymd_hms(df_image_data$DateTimeOriginal))

# create cluster (3 cores)
cl <- makeCluster(3)

# assign new DateTimeOriginal
writeDateTimeOriginal(DateTimeOriginal = df_image_data$DateTimeOriginal,
                      fileNames = df_image_data$DateTimeOriginal,
                      parallel = cl)
                   
}


}