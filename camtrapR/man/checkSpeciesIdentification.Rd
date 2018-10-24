\name{checkSpeciesIdentification}
\alias{checkSpeciesIdentification}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Consistency check on species image identification
}
\description{
This function serves 2 purposes: 1) it assesses possible misidentification of species and 2) compares double observer species identification (only if metadata tagging was used for species identification). 

Within each station, it assesses whether there are images of a species taken within a given time interval of another species. Often, it is unlikely that different species are encountered within a very short time intervals at the same location. This type of misidentification can arise easily if some images belonging to a sequence of images were accidentally moved into different species directories or tagged incorrectly.

Double observer identification may be desirable to increase reliability of species identification. The function returns conflicts in species identification between 2 observers. These conflicts can then be corrected.
}
\usage{
checkSpeciesIdentification(inDir, 
  IDfrom,
  hasCameraFolders,
  metadataSpeciesTag,
  metadataSpeciesTagToCompare,
  metadataHierarchyDelimitor = "|",
  maxDeltaTime, 
  excludeSpecies,
  stationsToCheck,
  writecsv = FALSE
)
}

\arguments{
  \item{inDir}{
  character. Directory containing identified camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
  \item{IDfrom}{
  character. Read species ID from image metadata ("metadata") of from species directory names ("directory")?
}
  \item{hasCameraFolders}{
  logical. Do the station directories in \code{inDir} have camera subdirectories (e.g. "inDir/StationA/Camera1" or "inDir/StationA/Camera1/Species1")?
}
  \item{metadataSpeciesTag}{
  character. The species ID tag name in image metadata (if IDfrom = "metadata").
}
  \item{metadataSpeciesTagToCompare}{
  character. A second species ID tag name in image metadata (if IDfrom = "metadata"). For comparing double observer species identification.
}
  \item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":"
}
  \item{maxDeltaTime}{
  numeric. Maximum time interval between images to be returned (in seconds)
}
  \item{excludeSpecies}{
  character. vector of species to exclude from checks
}
  \item{stationsToCheck}{
  character. vector of stations to be checked (optionally)
}
  \item{writecsv}{
  logical. Should the resulting data.frame be saved as a .csv?
}
}
\details{
Images may accidentally be misidentified by assigning wrong species tags or by moving them into wrong species directories. Imagine your cameras take sequences of images each time they are triggered and one image of the sequence is misidentified. The time difference between these images (that have different species assigned to them) will be very small, usually a few seconds. This function will return all these images for you to check if they were identified correctly.

If multiple observers identify images independently using metadata tagging, their identifications can be compared by setting \code{metadataSpeciesTagToCompare}. Conflicting or missing identifications will be reported. This feature is only available if images were identified by metadata tagging.

Species like "blank" or "team" can be ignored using \code{excludeSpecies}. If only specific stations are to be checked, \code{stationsToCheck} can be set.
}

\value{
A \code{list} containing 2 data frames. The first contains a data frame with images file names, directories, time stamp and species ID that were taken within \code{maxDeltaTime} seconds of another species image at a particular station.
The second data frame contains images with conflicting species IDs (if \code{IDfrom = "metadata"} and \code{metadataSpeciesTagToCompare} is defined)
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
 \note{
The function will not be able to find "isolated" images, i.e. images that were misidentified, but were not part of a sequence of images. Likewise, if all images of a sequence were misidentified, they cannot be found either.
From version 0.99.0, the function can also handle images identied with metadata tags.
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
check.folders <- checkSpeciesIdentification(inDir             = wd_images_ID,
                                            IDfrom            = "directory",
                                            hasCameraFolders  = FALSE,
                                            maxDeltaTime      = 120,
                                            writecsv          = FALSE)
                                     
check.folders   # In the example, 2 different species were photographed within 2 minutes. 
}

\dontrun{
# now exclude one of these 2 species 
check.folders2 <- checkSpeciesIdentification(inDir             = wd_images_ID,
                                             IDfrom            = "directory",
                                             hasCameraFolders  = FALSE,
                                             maxDeltaTime      = 120,
                                             excludeSpecies    = "EGY",
                                             writecsv          = FALSE)           
  
check.folders2   # the data frame is empty

# now we check only one station
check.folders3 <- checkSpeciesIdentification(inDir             = wd_images_ID,
                                             IDfrom            = "directory",
                                             hasCameraFolders  = FALSE,
                                             maxDeltaTime      = 120,
                                             stationsToCheck   = "StationB",
                                             writecsv          = FALSE)
check.folders3   # the data frame is empty
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
