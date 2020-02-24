\name{createStationFolders}
\alias{createStationFolders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create camera trap station directories for raw camera trap images
}
\description{
This function creates camera trap station directories, if needed with camera subdirectories. They can be used as an initial directory structure for storing raw camera trap images.
}
\usage{
createStationFolders(inDir, 
  stations, 
  cameras,
  createinDir
)
}

\arguments{
  \item{inDir}{
  character. Directory in which station directories are to be created
}
  \item{stations}{
  character. Station IDs to be used as directory names within \code{inDir}
}
  \item{cameras}{
  character. Camera trap IDs to be used as subdirectory names in each station directory (optionally)
}
\item{createinDir}{
  logical. If inDir does not exist, create it?
}


}
\details{

The empty directories serve as containers for saving raw camera trap images. If more than 1 camera was set up at a station, specifying \code{cameras} is required in order to keep images from different cameras separate. Otherwise, generic filenames (e.g., IMG0001.JPG) from different cameras may lead to accidental overwriting of images if images from these cameras are saved in one station directory.
}

\value{
  A \code{data.frame} with station (and possibly camera) directory names and an indicator for whether they were created successfully.
}
\author{
Juergen Niedballa
}

\examples{
\dontrun{

# create dummy directory for tests (this will be used as inDir)
# (normally, you'd set up an empty directory, e.g. .../myStudy/rawImages)
wd_createStationDir <- file.path(tempdir(), "createStationFoldersTest")

# now we load the sample camera trap station data frame
data(camtraps)

# create station directories in wd_createStationDir
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = as.character(camtraps$Station),
                                              createinDir = TRUE)
  
StationFolderCreate1

# check if directories were created
list.dirs(wd_createStationDir)

}
}
