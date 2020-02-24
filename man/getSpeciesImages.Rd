\name{getSpeciesImages}
\alias{getSpeciesImages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Collect all images of a species
}
\description{
This function will fetch all images of a particular species from all camera trap stations and copies these images to a new location. The images which are to be copied are found in one of 2 possible ways, 1) by providing an existing record table (created with \code{\link{recordTable}}) or 2) by reading species IDs from species directories or from metadata (calling ExifTool).
Earlier in the workflow, i.e., before running this function, images should have been renamed (with \code{\link{imageRename}}) to give images unique file names based on station ID and date/time.
}
\usage{
  getSpeciesImages(species,
  recordTable,
  speciesCol = "Species",
  stationCol = "Station",
  inDir,
  outDir,
  createStationSubfolders = FALSE,
  IDfrom,
  metadataSpeciesTag,
  metadataHierarchyDelimitor = "|")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
character. Species whose images are to be fetched
}
\item{recordTable}{
data frame. A data frame as returned by function \code{\link{recordTable}}. If you specify this argument, do not specify \code{inDir}
}
  \item{speciesCol}{
character. Name of the column specifying species ID in \code{recordTable}. Only required if \code{recordTable} is defined
}
  \item{stationCol}{
character. Name of the column specifying station ID in \code{recordTable}. Only required if \code{recordTable} is defined
}
  \item{inDir}{
character. Directory containing identified (species level) camera trap images sorted into station subdirectories (e.g. inDir/StationA/). If you specify this argument, do not specify \code{recordTable}.
}
  \item{outDir}{
character. Directory in which to save species images. A species subdirectory will be created in \code{outDir} automatically.
}
  \item{createStationSubfolders}{
logical. Save images in station directories within the newly created species directory in \code{outDir}?
}
\item{IDfrom}{
character. Read species ID from image metadata ("metadata") of from species directory names ("directory")? Only required if \code{inDir} is defined.
}
\item{metadataSpeciesTag}{
  character. The species ID tag name in image metadata (if IDfrom = "metadata"). Only required if \code{inDir} is defined.
}
\item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":" (if IDfrom = "metadata"). Only required if \code{inDir} is defined and \code{IDfrom = "metadata"}.
}

}
\details{

The function finds the images to be copied by either consulting a record table created with \code{\link{recordTable}} or by reading species IDs from images. The former is considerable faster because ExifTool is not called, but requires images to be in precisely the location given by the columns \code{Directory} and \code{FileName} in \code{recordTable}. To use this feature, provide the function with a record table in argument \code{recordTable}.

If you'd rather read species IDs from images within the function (to make sure all file paths are correct), images need to be in the directory structure required by the package, e.g.

> inDir/Station/Species

or

> inDir/Station/Camera/Species

if using species directories for species IDs, and

> inDir/Station

or

> inDir/Station/Camera

if reading IDs from species metadata tags. In the latter case, only station directories are needed.
In any case, the argument \code{species} must match species IDs (either the \code{speciesCol} in \code{recordTable}, species directory names or species metadata tags).

Before running the function, first rename the images using function \code{\link{imageRename}} to provide unique file names and prevent several images from having the same name (if generic names like "IMGP0001.jpg" are used). The function will not copy images if there are duplicate filenames to prevent overwriting images unintentionally.
}
\value{
A \code{data.frame} with old and new directories and file names and the copy status (\code{copy_ok}; TRUE if copying was successful, FALSE if not).
}

\author{
Juergen Niedballa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
wd_images_ID_copy <- file.path(tempdir(), "sample_species_images")

species_to_copy <- "VTA"    # = Viverra tangalunga, Malay Civet

specImagecopy <- getSpeciesImages(species                 = species_to_copy,
                                  inDir                   = wd_images_ID,
                                  outDir                  = wd_images_ID_copy,
                                  createStationSubfolders = FALSE,
                                  IDfrom                  = "directory"
                                  )
}
}