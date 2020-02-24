\name{createSpeciesFolders}
\alias{createSpeciesFolders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create species directories for species identification
}
\description{
This function creates species subdirectories within station directories. They can be used for species identification by manually moving images into the respective species directories. The function can also delete empty species directories (if species were not detected at sites). It is not necessary to run this function if animals will be identified by metadata tagging.
}
\usage{
createSpeciesFolders(inDir, 
  hasCameraFolders,
  species, 
  removeFolders = FALSE
)
}

\arguments{
  \item{inDir}{
  character. Directory containing camera trap images sorted into station subdirectories (e.g. inDir/StationA/)
}
  \item{hasCameraFolders}{
     logical. Do the station directories in \code{inDir} have camera-subdirectories (e.g. inDir/StationA/CameraA1; inDir/StationA/CameraA2)?
}
  \item{species}{
  character. names of species directories to be created in every station (or station/camera) subdirectory of \code{inDir}
}
  \item{removeFolders}{
  logical. Indicating whether to create (TRUE) or remove (FALSE) species directories .
}

}
\details{
This function should be run after \code{\link{imageRename}}.
Empty directories can be created as containers for species identification if images are identified with the drag & drop method. After species identification is complete, empty species directories can be deleted using \code{removeFolders = TRUE}. The function will delete only directories which are specified in \code{species}.
If \code{hasCameraFolders} was set to \code{TRUE} in function \code{\link{imageRename}}, \code{hasCameraFolders} must be set to \code{TRUE} here too. Species directories will then be created within each camera subdirectory of each station directory.
if the user wishes to identify species by metadata tagging, running this function is not needed.
}
\value{
  A \code{data.frame} with directory names and an indicator for whether directories were created or deleted.
}
\author{
Juergen Niedballa
}

\examples{
\dontrun{

# create dummy directories for tests
# (normally, you'd use directory containing renamed, unsorted images)

# this will be used as inDir
wd_createDirTest <- file.path(getwd(), "createSpeciesFoldersTest")

# now we create 2 station subdirectories
dirs_to_create <- file.path(wd_createDirTest, c("StationA", "StationB"))
sapply(dirs_to_create, FUN = dir.create, recursive = TRUE)

# species names for which we want to create subdirectories
species <- c("Sambar Deer", "Bay Cat")

# create species subdirectories
SpecFolderCreate1 <- createSpeciesFolders (inDir               = wd_createDirTest,
                                           species             = species,
                                           hasCameraFolders = FALSE,
                                           removeFolders       = FALSE)
  
SpecFolderCreate1

# check if directories were created
list.dirs(wd_createDirTest)

# delete empty species directories
SpecFolderCreate2 <- createSpeciesFolders (inDir               = wd_createDirTest,
                                           species             = species,
                                           hasCameraFolders = FALSE,
                                           removeFolders       = TRUE)

SpecFolderCreate2

# check if species directories were deleted
list.dirs(wd_createDirTest)

}
}
