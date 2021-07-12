\name{camtrapR-package}
\alias{camtrapR-package}
\alias{camtrapR}

\docType{package}

\title{
Overview of the functions in the camtrapR package
}
\description{
This package provides a streamlined workflow for processing data generated in camera trap-based wildlife studies and prepares input for further analyses, particularly in occupancy and spatial capture-recapture frameworks.
It suggests a simple data structure and provides functions for managing digital camera trap photographs (and videos), generating record tables, maps of species richness and species detections and species activity diagrams. It further helps prepare subsequent analyses by creating detection/non-detection matrices for occupancy analyses, e.g. in the \pkg{unmarked} or \pkg{ubms} packages, and \code{capthist} objects for spatial capture-recapture analyses in the \pkg{secr} package. In addition, basic survey statistics are computed. The functions build on one another in a logical sequence.
The only manual input needed is species (and individual) identification, which is achieved by moving images into species directories or by tagging images in image management software. Besides, a table holding basic information about camera trap station IDs, locations and trapping periods must be created in spreadsheet software.
}

\section{User support}{
The camtrapR Google group is an online support and help forum for camtrapR users. You can find it here: \url{https://groups.google.com/forum/#!forum/camtrapr}.
}

\details{
Image metadata (such as date and time or user-assigned tags) are extracted from the images using Phil Harvey's ExifTool (available from \url{https://exiftool.org/}) and the information is stored in a record table. An adjustable criterion for temporal independence of records can be applied. Maps of species presence and species richness can be generated. Several functions are available for plotting single- and two-species activity patterns.
Information about the camera-specific trapping periods (and periods of malfunction) are summarized into a matrix about camera trap operability. These, together with the record table, are used to generate species detection histories for occupancy and spatial capture-recapture analyses. The user has considerable freedom in generating the detection histories; sampling occasion length, beginning date and and occasion start times are adjustable. In addition, trapping effort (i.e. active trap nights per station and occasion) can be computed for use as a covariate / offset on detection probability.
}

\section{Image organisation and management}{

The functions in this section set up a directory structure for storing camera trap images and identifying species and individuals from images. They build on one another and can be run in sequential order as needed.
\tabular{ll}{
\code{\link{createStationFolders}} \tab Create camera trap station directories for raw images \cr
\code{\link{fixDateTimeOriginal}}  \tab Fix DateTimeOriginal Exif metadata tag in Reconyx Hyperfire cameras \cr
\code{\link{OCRdataFields}}        \tab Optical character recognition (OCR) from data fields in images \cr
\code{\link{writeDateTimeOriginal}} \tab Write values to DateTimeOriginal tag in image metadata \cr
\code{\link{timeShiftImages}}      \tab Apply time shifts to JPEG images \cr
\code{\link{imageRename}}          \tab Copy and rename images based on station ID and image creation date \cr
\code{\link{addCopyrightTag}}      \tab Write a copyright tag into JPEG image metadata \cr
\code{\link{appendSpeciesNames}}   \tab Add or remove species names from image filenames \cr

--------------------------- \tab ---------------------------------------------------------------------------------------------- \cr

}
}

\section{Species / individual identification}{

These functions assist in species identification and prepare individual identification of animals.
\tabular{ll}{
\code{\link{checkSpeciesNames}}          \tab Check species names against the ITIS taxonomic database \cr
\code{\link{createSpeciesFolders}}       \tab Create directories for species identification\cr
\code{\link{checkSpeciesIdentification}} \tab Consistency check on species image identification \cr
\code{\link{getSpeciesImages}}           \tab Gather all images of a species in a new directory \cr
--------------------------- \tab ---------------------------------------------------------------------------------------------- \cr
}
}


\section{Image data extraction}{

These function use the directory structure built above (Section 'Image management workflow') and a table containing basic information about camera traps and/or stations (IDs, location, trapping period).
\tabular{ll}{
\code{\link{recordTable}}           \tab Create a species record table from camera trap images and videos \cr
\code{\link{recordTableIndividual}} \tab Create a single-species record table from camera trap images and videos with individual IDs \cr
\code{\link{exifTagNames}}          \tab Return Exif metadata tags and tag names from JPEG images \cr
\code{\link{exiftoolPath}}          \tab Add the directory containing exiftool.exe to PATH temporarily (Windows only) \cr
--------------------------- \tab ---------------------------------------------------------------------------------------------- \cr

}
}

\section{Data exploration and visualisation}{

These plots are generated from the record table and the camera trap table.
\tabular{ll}{
\code{\link{detectionMaps}}     \tab Generate maps of species richness and species presence by station, export shapefiles \cr
\code{\link{activityHistogram}} \tab Single-species diel activity histograms \cr
\code{\link{activityDensity}}   \tab Single-species diel activity kernel density estimation plots \cr
\code{\link{activityRadial}}    \tab Single-species diel activity radial plot \cr
\code{\link{activityOverlap}}   \tab Two-species diel activity overlap plots and estimates \cr
--------------------------- \tab ---------------------------------------------------------------------------------------------- \cr

}
}

\section{Data export}{

\tabular{ll}{
\code{\link{cameraOperation}}         \tab Create a camera operability matrix \cr
\code{\link{detectionHistory}}        \tab Species detection histories for occupancy analyses (single and multi-season) \cr
\code{\link{spatialDetectionHistory}} \tab Detection histories of individuals for spatial capture-recapture analyses  \cr
\code{\link{surveyReport}}            \tab Create a report about camera trap surveys and species detections \cr
--------------------------- \tab ---------------------------------------------------------------------------------------------- \cr

}
}

\section{Sample data}{

\tabular{ll}{
\code{\link{camtraps}}          \tab  Sample camera trap station information table \cr
\code{\link{recordTableSample}} \tab Sample species record table \cr
\code{\link{recordTableIndividualSample}}  \tab Single-species record table with individual IDs\cr
\code{\link{camtrapsMultiSeason}}          \tab  Sample multi season camera trap station information table \cr
\code{\link{recordTableSampleMultiSeason}} \tab Sample  multi season species record table \cr
\code{\link{recordTableIndividualSampleMultiSeason}}  \tab Single-species multi season record table with individual IDs\cr
\code{\link{timeShiftTable}}    \tab Sample camera trap time shift information \cr
--------------------------- \tab ---------------------------------------------------------------------------------------------- \cr

}
}

\section{Vignettes}{

\tabular{ll}{

\href{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr1.html}{1. Organising raw camera trap images}  \cr
\href{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr2.html}{2. Identifying species and individuals}  \cr
\href{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr3.html}{3. Extracting Data from Camera Trapping Images and Videos}  \cr
\href{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr4.html}{4. Data exploration and visualisation}  \cr

}
}



\author{
Juergen Niedballa

Maintainer:Juergen Niedballa <camtrapr@gmail.com>
}

\references{

Niedballa, J., Sollmann, R., Courtiol, A., Wilting, A. (2016): camtrapR: an R package for efficient camera trap data management. Methods in Ecology and Evolution, 7(12). \url{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12600} \cr

camtrapR Google Group \url{https://groups.google.com/forum/#!forum/camtrapr} \cr

Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12. \cr

Mike Meredith and Martin Ridout (2018). overlap: Estimates of coefficient of overlapping for animal activity patterns. R package version 0.3.2. \url{https://CRAN.R-project.org/package=overlap} \cr

Phil Harvey's ExifTool \url{https://exiftool.org/} \cr
}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{package}
\seealso{
\pkg{overlap}
\pkg{unmarked}
\pkg{ubms}
\pkg{secr}
\pkg{wiqid}
}

