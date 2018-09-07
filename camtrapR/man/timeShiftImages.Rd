\name{timeShiftImages}
\alias{timeShiftImages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply time shifts to JPEG image metadata
}
\description{
Change the values of digital timestamps in image metadata using ExifTool. If date/time of images were set incorrectly, they can be corrected easily in batch mode for further analyses. Please, always make a backup of your data before using this function to avoid data loss or damage. This is because ExifTool will make a copy of your images and applies the time shifts to the copies. The file extentsion of the original images (.JPG) will be renamed to ".JPG_original".
}
\usage{
timeShiftImages (inDir,
  hasCameraFolders,
  timeShiftTable,
  stationCol,
  cameraCol,
  timeShiftColumn,
  timeShiftSignColumn,
  undo = FALSE
)
}

\arguments{
  \item{inDir}{
   character. Name of directory containing station directories with images
}
  \item{hasCameraFolders}{
  logical. Do the station directories in \code{inDir} have camera subdirectories (e.g. "inDir/StationA/Camera1")?
}
  \item{timeShiftTable}{
   data.frame containing information about station-/camera-specific time shifts.
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{timeShiftTable}
}
  \item{cameraCol}{
  character. name of the column specifying Camera ID in \code{timeShiftTable} (optional)
}
  \item{timeShiftColumn}{
  character. The name of the column containing time shift values in \code{timeShiftTable}
}
  \item{timeShiftSignColumn}{
  character. The name of the column with the direction of time shifts in \code{timeShiftTable}. Can only be "-" or "+".
}

  \item{undo}{
  logical. Undo changes and restore the original images? Please be careful, this deletes any edited images if \code{TRUE}
}
}
\details{

\code{timeShiftTable} is a data frame with columns for station ID, camera ID (optional), time shift value and direction of time shift (for an example  see \code{\link{timeShiftTable}}).
Images in \code{inDir} must be sorted into station directories. If \code{hasCameraFolders = TRUE}, the function expects camera subdirectories in the station directories and will only apply time shifts to the camera subdirectories specified by \code{CameraCol} in \code{timeShiftTable}. If \code{hasCameraFolders = FALSE}, shifts will be applied to the whole station directory (including potential subdirectories).

The values of \code{timeShiftColumn} must adhere to the following pattern: "YYYY:mm:dd HH:MM:SS" ("year:month:day hour:minute:second"). Examples: "1:0:0 0:0:0" is a shift of exactly 1 year and "0:0:0 12:10:01" 12 hours and 10 minutes and 1 second. Note that stating "00" may cause problems, so use "0" instead if an entry is zero. 

\code{timeShiftSignColumn} signifies the direction of the time shift. "+" moves image dates into the future (i.e. the image date lagged behind the actual date) and "-" moves image dates back (if the image dates were ahead of actual time).

ExifTool stores the original images as \code{.JPG_original} files in the original file location. By setting \code{undo = TRUE}, any JPG files in the directories specified by \code{timeShiftTable} will be deleted and the original JPEGs will be restored from the JPG_original files. Please make a backup before using \code{undo}.
}
\value{
 A \code{data.frame} containing the information about the processed directories and the number of images.
}
\references{
\url{http://www.sno.phy.queensu.ca/~phil/exiftool/Shift.html}
}
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% }
\examples{

\dontrun{

# copy sample images to temporary directory (so we don't mess around in the package directory)
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
file.copy(from = wd_images_ID, to = tempdir(), recursive = TRUE)
wd_images_ID_copy <- file.path(tempdir(), "sample_images")

data(timeShiftTable)


timeshift_run <- timeShiftImages(inDir                = wd_images_ID_copy,
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "Station",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign",
                                 undo                 = FALSE
)


timeshift_undo <- timeShiftImages(inDir               = wd_images_ID_copy,
                                  timeShiftTable      = timeShiftTable,
                                  stationCol          = "Station",
                                  hasCameraFolders    = FALSE,
                                  timeShiftColumn     = "timeshift",
                                  timeShiftSignColumn = "sign",
                                  undo                = TRUE
)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
