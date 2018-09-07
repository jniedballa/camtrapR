\name{imageRename}
\alias{imageRename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Copy and rename images based on camera trap station ID and creation date
}
\description{
The function renames and copies raw camera trap images into a new location where they can be identified. Images are renamed with camera trap station ID, camera ID (optional), creation date and a numeric identifier for images taken within one minute of each other at a given station. Station ID and camera ID are derived from the raw image directory structure. The creation date is extracted from image metadata using ExifTool.
}
\usage{
imageRename(inDir, 
  outDir, 
  hasCameraFolders,
  keepCameraSubfolders,
  createEmptyDirectories = FALSE,
  copyImages = FALSE, 
  writecsv = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
     character. Directory containing camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
  \item{outDir}{
     character. Directory into which the renamed images will be copied
}
  \item{hasCameraFolders}{
     logical. Do the station directories in \code{inDir} have camera subdirectories (e.g. "inDir/StationA/Camera1")?
}
  \item{keepCameraSubfolders}{
     logical. Should camera directories be preserved as subdirectories of \code{outDir} (e.g. "outDir/StationA/CameraA1")?
}
  \item{createEmptyDirectories}{
     logical. If station or camera directories are empty, should they be copied nevertheless (causing empty directories in \code{inDir}, but preserving the whole directory structure)?
}
  \item{copyImages}{
  logical. Copy images to \code{outDir}?
}
  \item{writecsv}{
  logical. Save a data frame with a summary as a .csv?
}
}
\details{
Setting up the correct raw image directory structure is necessary for running the function successfully. \code{inDir} is the main directory that contains camera trap station subdirectories (e.g. inDir/StationA). If one camera was deployed per station and no camera subdirectories are used within station directories, \code{hasCameraFolders} can be set to \code{FALSE}. If more than one camera was deployed at stations, there must be subdirectories for the individual camera traps within the station directories (e.g. "inDir/StationA/CameraA1" and "inDir/StationA/CameraA2"). Even if only some stations had multiple cameras, all station will need camera subdirectories. The argument \code{hasCameraFolders} must be \code{TRUE}. Within the camera subdirectories, the directory structure is irrelevant. 

Renaming of images follows the following pattern: If \code{hasCameraFolders} is TRUE, it is: "StationID__CameraID__Date__Time(Number).JPG", e.g. "StationA__CameraA1__2015-01-31__18-59-59(1).JPG". If \code{hasCameraFolders} is FALSE, it is:  "StationID__Date__Time(Number).JPG", e.g. "StationA__2015-01-31__18-59-59(1).JPG". 

The purpose of the number in parentheses is to prevent assigning identical file names to images taken at the same station (and camera) in the same second, as can happen if cameras take sequences of images. It is a consecutive number given to all images taken at the same station by the same camera within one minute. The double underscore "__" in the image file names is for splitting and extracting information from file names in other functions (e.g. for retrieving camera IDs in \code{\link{recordTable}} if camera subdirectories are not preserved (\code{keepCameraSubfolders = FALSE})).

The function finds all JPEG images and extracts the image timestamp from the image metadata using ExifTool and copies the images (with new file names) into \code{outDir}, where it will set up a directory structure based on the station IDs and, if required by \code{keepCameraSubfolders = TRUE}, camera IDs (e.g. outDir/StationA/ or outDir/StationA/CameraA1).

\code{copyImages} can be set to FALSE to simulate the renaming and check the file names of the renamed images without copying. If you are handling large number of images (>e.g., 100,000), the function may take some time to run.

}
\value{
A \code{data.frame} with original directory and file names, new directory and file names and an indicator for whether images were copied successfully.
}
\references{
Phil Harvey's ExifTool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ }
}
\author{
Juergen Niedballa
}

\examples{

### "trial" run. create a table with file names after renaming, but don't copy images.

# first, find sample image directory in package directory:
wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")

if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available

# because copyImages = FALSE, outDir does not need to be defined
renaming.table <- imageRename(inDir               = wd_images_raw,     
                              hasCameraFolders = FALSE,
                              copyImages          = FALSE,
                              writecsv            = FALSE
  )
  } else {
  message("ExifTool is not available. Cannot test function")
  }
  
  \dontrun{
  
  # define image directories

  # raw image location
wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")      
  # destination for renamed images
wd_images_raw_renamed <- file.path(tempdir(), "raw_images_renamed")       


  if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
  
  # now we have to set outDir because copyImages = TRUE
renaming.table2 <- imageRename(inDir               = wd_images_raw,
                               outDir              = wd_images_raw_renamed,       
                               hasCameraFolders    = FALSE,
                               copyImages          = TRUE,
                               writecsv            = FALSE
  )
  }
  
  list.files(wd_images_raw_renamed, recursive = TRUE)
  
  }
}