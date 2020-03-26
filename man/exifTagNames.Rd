\name{exifTagNames}
\alias{exifTagNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show Exif metadata tags and tag names from JPEG images
}
\description{
The function will return metadata values, metadata tag names and group names of Exif metadata of JPEG images or other formats.
}
\usage{
exifTagNames(inDir, 
  whichSubDir = 1,
  fileName,
  returnMetadata,
  returnTagGroup)
}

\arguments{
\item{inDir}{
  character. Directory containing camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
\item{whichSubDir}{
  integer or character. Either number or name of subdirectory of \code{inDir} in which to look for an image
}
\item{fileName}{
  character. A filename, either the file name of an image in \code{inDir} or a full path with file name (in which case \code{inDir} is not needed)
}
\item{returnMetadata}{
  deprecated and ignored
}
\item{returnTagGroup}{
  deprecated and ignored
}
}
\details{
Many digital cameras record information such as ambient temperature or moon phase under maker-specific tag names in Exif metadata of JPEG images. In addition, many technical information are stored in Exif metadata. In order to extract those information from images and add them to the record tables created by the functions \code{\link{recordTable}} and \code{\link{recordTableIndividual}}, the tag names must be known so they can be passed to these functions via the \code{additionalMetadataTags} argument.

By default the function returns both metadata tag names and the metadata group they belong to (via argument \code{returnTagGroup}). This is helpful to unambiguously address specific metadata tags, because different groups can contain tags of identical names, which may cause problems executing the functions \code{\link{recordTable}} and \code{\link{recordTableIndividual}}. The format is "GROUP:tag", e.g. "EXIF:Flash". 

}
\value{
A data frame containing available metadata tag groups, descriptions, names and values.
}
\references{
Phil Harvey's ExifTool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ } \cr
}
\author{
Juergen Niedballa
}

\seealso{
\code{\link{recordTable}}
}
\examples{

\dontrun{

wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# specify directory, camtrapR will automatically take first image from first subdirectory
exifTagNames(inDir          = wd_images_ID)

# specify subdirectory by name, camtrapR will use first image
exifTagNames(inDir          = wd_images_ID,
             whichSubDir    = "StationA")

# specifying fileName only (line break due to R package policy)
exifTagNames(fileName       = file.path(wd_images_ID, "StationC", "TRA", 
                                        "StationC__2009-05-02__00-10-00(1).JPG"))
                                        
# specify inDir and fileName
exifTagNames(inDir          = wd_images_ID,
             fileName       = file.path("StationC", "TRA", "StationC__2009-05-02__00-10-00(1).JPG"))
             
# it also works this way
exifTagNames(inDir          = file.path(wd_images_ID, "StationC", "TRA"),
             fileName       = "StationC__2009-05-02__00-10-00(1).JPG")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
