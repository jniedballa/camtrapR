\name{appendSpeciesNames}
\alias{appendSpeciesNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add or remove species names from JPEG image filenames
}
\description{
Add or remove species names from JPEG image filenames. It makes it easier to find images of a species.
}
\usage{
appendSpeciesNames(inDir, 
  IDfrom,
  hasCameraFolders,
  metadataSpeciesTag,
  metadataHierarchyDelimitor = "|",
  removeNames = FALSE, 
  writecsv = FALSE
)
}

\arguments{

  \item{inDir}{
  character. Directory containing camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
  \item{IDfrom}{
  character. Read species ID from image metadata ("metadata") of from species directory names ("directory")?
}
  \item{hasCameraFolders}{
  logical. Do the station subdirectories of \code{inDir} have camera-subdirectories (e.g. inDir/StationA/CameraA1; inDir/StationA/CameraA2)?
}
\item{metadataSpeciesTag}{
  character. The species ID tag name in image metadata (if IDfrom = "metadata").
}
\item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":". 
}
  \item{removeNames}{
 logical. remove appended species names?
}
  \item{writecsv}{
 logical. write csv table containing old and new file names into \code{inDir}?
}
}

\details{
Species names can be appended or removed from image filenames. Before running the function, you may want to run \code{\link{checkSpeciesIdentification}} to detect possible misidentifications. As an example, the function would change an image file name from "StationA__2015-05-41__20-59-59(1).JPG" to "StationA__2015-05-41__20-59-59(1)__Species Name.JPG". If species names were appended several times by accident, they can all be removed by running the function with \code{removeNames = TRUE}
}

\value{
A \code{data.frame} containing the old and new file names and directories.
}
% \references{
% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{

# copy sample images to another location (so we don't mess around in the package directory)
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
file.copy(from = wd_images_ID, to = getwd(), recursive = TRUE)       
wd_images_ID_copy <- file.path(getwd(), "sample_images")

# append species names
SpecNameAppend1 <- appendSpeciesNames(inDir            = wd_images_ID_copy,
                                      IDfrom           = "directory",
                                      hasCameraFolders = FALSE,
                                      removeNames      = FALSE,
                                      writecsv         = FALSE)
  
SpecNameAppend1

# remove species names
SpecNameRemove1 <- appendSpeciesNames(inDir            = wd_images_ID_copy,
                                      IDfrom           = "directory",
                                      hasCameraFolders = FALSE,
                                      removeNames      = TRUE,
                                      writecsv         = FALSE)
  
SpecNameRemove1
}
}
