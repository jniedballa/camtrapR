\name{addCopyrightTag}
\alias{addCopyrightTag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a copyright tag into JPEG image metadata
}
\description{
This function writes a copyright tag into the copyright field of JPEG image Exif metadata. It does so recursively, so it works both for images that are sorted into subdirectories and unsorted images. Note that all images in subdirectories of inDir will be tagged. It is not required to run this function in the camtrapR workflow, but may be desired for data sharing or publishing.
}
\usage{
addCopyrightTag(inDir, 
  copyrightTag,
  askFirst = TRUE,
  keepJPG_original = TRUE
)
}

\arguments{
  \item{inDir}{
  character. Name of the directory containing camera trap images.
}
  \item{copyrightTag}{
     character. The tag to be written into the Exif Copyright field
}
  \item{askFirst}{
     logical. Ask user to confirm before execution?
}
  \item{keepJPG_original}{
     logical. Keep original JPG files as .JPG_original files (TRUE) or overwrite JPGs (FALSE)?
}
 
}
\details{
If askFirst = TRUE, the function will show a menu and asks the user to confirm the action before execution. Type "1" to write copyright tags and "2" to abort.

By default Exiftool creates a copy of each JPG image and preserves the original images (without the copyright tag) as .JPG_original files. Note that this behaviour will instantly double the number of images in inDir and the disk space required. If this is not desired, set keepJPG_original = FALSE.
}
\value{
  An invisible list of Exiftool output.
  
  More importantly, the specified copyright tag is written into the Copyright field of the Exif metadata of all images in inDir.
}
\author{
Juergen Niedballa
}

\examples{

\dontrun{

if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available

# copy sample images to temporary directory (so we don't mess around in the package directory)
wd_images_ID <- system.file(file.path("pictures", "sample_images"), package = "camtrapR")
file.copy(from = wd_images_ID, to = tempdir(), recursive = TRUE)
wd_images_ID_copy <- file.path(tempdir(), "sample_images")

# define a sample tag
copyrightTagToAdd <- "Your Name (Your Organisation)"

# add the tag to the images
addCopyrightTag(inDir        = wd_images_ID_copy, 
                copyrightTag = copyrightTagToAdd)
1     # we choose "YES", i.e., we want to add a copyright tag
                
# you can check the outcome with function exifTagNames (find the field "Copyright")

exifTagNames(wd_images_ID_copy, returnMetadata = TRUE)
}


}
}
