\name{exifTagNames}
\alias{exifTagNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show Exif metadata tags and tag names from JPEG images
}
\description{
The function will return sample metadata and tag names of Exif metadata of JPEG images. It uses the first JPEG image it finds in a subdirectory of the specified directory.
}
\usage{
exifTagNames(inDir, 
  whichSubDir = 1,
  returnMetadata = FALSE,
  returnTagGroup = TRUE)
}

\arguments{
\item{inDir}{
  character. Directory containing camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
\item{whichSubDir}{
  integer. The number of the subdirectory of \code{inDir} in which to look for an image
}
\item{returnMetadata}{
  logical. Return actual metadata (TRUE) or metadata tag names only (FALSE)
}
\item{returnTagGroup}{
  logical. Return tag group along with tag names (e.g. EXIF, IPTC, XMP, MakerNotes)
}
}
\details{
Many digital cameras record information such as ambient temperature or moon phase under maker-specific tag names in Exif metadata of JPEG images. In addition, many technical information are stored in Exif metadata. In order to extract those information from images and add them to the record tables created by the functions \code{\link{recordTable}} and \code{\link{recordTableIndividual}}, the tag names must be known so they can be passed to these functions via the \code{additionalMetadataTags} argument.

By default the function returns both metadata tag names and the metadata group they belong to (via argument \code{returnTagGroup}). This is helpful to unambiguously address specific metadata tags, because different groups can contain tags of identical names, which may cause problems executing the functions \code{\link{recordTable}} and \code{\link{recordTableIndividual}}. The format is "GROUP:tag", e.g. "EXIF:Flash". 

}
\value{
A character vector containing available metadata or metadata tag names (and groups).
}
\references{
Phil Harvey's ExifTool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ } \cr
}
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{recordTable}}
}
\examples{
if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# return tag names only
exifTagNames(inDir          = wd_images_ID,
             returnMetadata = FALSE)

# return tag names and metadata
exifTagNames(inDir          = wd_images_ID,
             returnMetadata = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
