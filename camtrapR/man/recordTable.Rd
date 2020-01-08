\name{recordTable}
\alias{recordTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a species record table from camera trap images
}
\description{
Generates a record table from camera trap images. Images must be sorted into station directories at least. The function can read species identification from a directory structure (Station/Species or Station/Camera/Species) or from image metadata tags.
}
\usage{
recordTable(inDir,
  IDfrom,
  cameraID,
  camerasIndependent,
  exclude,
  minDeltaTime = 0,
  deltaTimeComparedTo,
  timeZone,
  stationCol,
  writecsv = FALSE,
  outDir,
  metadataHierarchyDelimitor = "|",
  metadataSpeciesTag,
  additionalMetadataTags,
  removeDuplicateRecords = TRUE,
  returnFileNamesMissingTags = FALSE,
  eventSummaryColumn,
  eventSummaryFunction
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
  character. Directory containing station directories. It must either contain images in species subdirectories (e.g. inDir/StationA/SpeciesA) or images with species metadata tags (without species directories, e.g. inDir/StationA).
}
  \item{IDfrom}{
  character. Read species ID from image metadata ("metadata") of from species directory names ("directory")?
}
  \item{cameraID}{
  character. Where should the function look for camera IDs: 'filename', 'directory'. 'filename' requires images renamed with \code{\link{imageRename}}. 'directory' requires a camera subdirectory within station directories (station/camera/species). Can be missing.
}
  \item{camerasIndependent}{
  logical. If \code{TRUE}, species records are considered to be independent between cameras at a station.
}
  \item{exclude}{
  character. Vector of species names to be excluded from the record table
}
  \item{minDeltaTime}{
  integer. Time difference between records of the same species at the same station to be considered independent (in minutes)
}
  \item{deltaTimeComparedTo}{
  character. For two records to be considered independent, must the second one be at least \code{minDeltaTime} minutes after the last independent record of the same species (\code{"lastIndependentRecord"}), or \code{minDeltaTime} minutes after the last record (\code{"lastRecord"})?
}
  \item{timeZone}{
  character. Must be a value returned by \code{\link[base:timezones]{OlsonNames}}
}
  \item{stationCol}{
  character. Name of the camera trap station column. Assuming "Station" if undefined.
}
  \item{writecsv}{
  logical. Should the record table be saved as a .csv?
}
  \item{outDir}{
  character. Directory to save csv to. If NULL and \code{writecsv = TRUE}, recordTable will be written to \code{inDir}.
}
  \item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":".
}
  \item{metadataSpeciesTag}{
  character. In custom image metadata, the species ID tag name.
}
  \item{additionalMetadataTags}{
  character. Additional camera model-specific metadata tags to be extracted. (If possible specify tag groups as returned by \code{\link{exifTagNames}})
}
  \item{removeDuplicateRecords}{
  logical. If there are several records of the same species at the same station (also same camera if cameraID is defined) at exactly the same time, show only one?
}
  \item{returnFileNamesMissingTags}{
  logical. If species are assigned with metadata and images are not tagged, return a few file names of these images as a message?
}
\item{eventSummaryColumn}{
  character. A column in the record table (e.g. from a metadata tag) by to summarise non-independent records (those within \code{minDeltaTime} of a given record)  with a user-defined function (\code{eventSummaryFunction})
}
\item{eventSummaryFunction}{
  character. The function by which to summarise \code{eventSummaryColumn} of  non-independent records, e.g. "sum", "max" (optional)
}
}
\details{
The function can handle a number of different ways of storing images, and supports species identification by moving images into species directories as well as metadata tagging. In every case, images need to be stored into station directories. If images are identified by moving them into species directories, a camera directory is optional: "Station/Species/XY.JPG" or "Station/Camera/Species/XY.JPG". Likewise, if images are identified using metadata tagging, a camera directory can be used optionally: "Station/XY.JPG" or "Station/Camera/XY.JPG".

If images are identified by metadata tagging, \code{metadataSpeciesTag} specifies the metadata tag group name that contains species identification tags. \code{metadataHierarchyDelimitor} is "|" for images tagged in DigiKam and images tagged in Adobe Bridge / Lightroom with the default settings. It is only necessary to change it if the default was changed in these programs.

\code{minDeltaTime} is a criterion for temporal independence of species recorded at the same station. Setting it to 0 will make the function return all records. Setting it to a higher value will remove records that were taken less than \code{minDeltaTime} minutes after the last record (\code{deltaTimeComparedTo = "lastRecord"}) or the last independent record (\code{deltaTimeComparedTo = "lastIndependentRecord"}).

\code{camerasIndependent} defines if the cameras at a station are to be considered independent. If \code{TRUE}, records of the same species taken by different cameras are considered independent (e.g. if they face different trails). Use \code{FALSE} if both cameras face each other and possibly \code{TRUE} ).

\code{exclude} can be used to exclude "species" directories containing irrelevant images (e.g. "team", "blank", "unidentified"). \code{stationCol} can be set to match the station column name in the camera trap station table (see \code{\link{camtraps}}).

Many digital images contain  Exif metadata tags such as "AmbientTemperature" or "MoonPhase" that can be extracted if specified in \code{metadataTags}. Because these are manufacturer-specific and not standardized, function \code{\link{exifTagNames}} provides a vector of all available tag names. Multiple names can be specified as a character vector as: \code{c(Tag1, Tag2, ...)}. The metadata tags thus extracted may be used as covariates in modelling species distributions.

\code{eventSummaryColumn} and \code{eventSummaryFunction} can be used to extract summary statistics for independent sampling events. For example, you assigned a "count" tag to your images, indicating the number of individuals in a picture. In a sequence of pictures taken within 1 minute, most pictures show one individual, but one image shows two individuals. You tagged the images accordingly (count = 1 or count = 2) and run \code{recordTable}. Set \code{eventSummaryColumn = "count"} and \code{eventSummaryFunction = "max"} to obtain the maximum number of \code{count} in all images within \code{minDeltaTime} minutes of a given record. The results is in a new column, in this example \code{count_max}. You can also calculate several statistics at the same time, by supplying vectors of values, e.g. \code{eventSummaryColumn = c("count", "count", "camera")} and \code{eventSummaryFunction = c("min", "max", "unique")} to get minimum and maximum count and all unique camera IDs for that event. Note that \code{eventSummaryColumn} and \code{eventSummaryFunction} must be of same length. 
}
\value{
A data frame containing species records and additional information about stations, date, time and (optionally) further metadata.
}

\section{Warning }{
Custom image metadata must be organised hierarchically (tag group - tag; e.g. "Species" - "Leopard Cat"). Detailed information on how to set up and use metadata tags can be found in \href{https://CRAN.R-project.org/package=camtrapR/vignettes/SpeciesIndividualIdentification.html#metadata-tagging}{vignette 2: Species and Individual Identification}.

Custom image metadata tags must be written to the images. The function cannot read tags from .xmp sidecar files. Make sure you set the preferences accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There, make sure "Write to sidecar files" is unchecked.

Please note the section about defining argument \code{timeZone} in the vignette on data extraction (accessible via \code{vignette("DataExtraction")} or online (\url{https://cran.r-project.org/package=camtrapR/vignettes/DataExtraction.html})).
}
\references{
Phil Harvey's ExifTool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ }
}
\author{
Juergen Niedballa
}
\note{
The results of a number of other function will depend on the output of this function (namely on the arguments \code{exclude} for excluding species and \code{minDeltaTime}/ \code{deltaTimeComparedTo} for temporal independence):

\tabular{l}{
\code{\link{detectionMaps}} \cr
\code{\link{detectionHistory}} \cr
\code{\link{activityHistogram}} \cr
\code{\link{activityDensity}} \cr
\code{\link{activityRadial}} \cr
\code{\link{activityOverlap}} \cr
\code{\link{activityHistogram}} \cr
\code{\link{surveyReport}} \cr
}
}

\examples{
wd_images_ID_species <- system.file("pictures/sample_images", package = "camtrapR")

if (Sys.which("exiftool") != ""){        # only run these examples if ExifTool is available


rec_table1 <- recordTable(inDir               = wd_images_ID_species,
                       IDfrom                 = "directory",
                       minDeltaTime           = 60,
                       deltaTimeComparedTo    = "lastRecord",
                       writecsv               = FALSE,
                       additionalMetadataTags = c("EXIF:Model", "EXIF:Make")
)
# note argument additionalMetadataTags: it contains tag names as returned by function exifTagNames

rec_table2 <- recordTable(inDir               = wd_images_ID_species,
                       IDfrom                 = "directory",
                       minDeltaTime           = 60,
                       deltaTimeComparedTo    = "lastRecord",
                       exclude                = "NO_ID",
                       writecsv               = FALSE,
                       timeZone               = "Asia/Kuala_Lumpur",
                       additionalMetadataTags = c("EXIF:Model", "EXIF:Make", "NonExistingTag"),
                       eventSummaryColumn     = "EXIF:Make",
                       eventSummaryFunction   = "unique"
                       )
                       
# note the warning that the last tag in "additionalMetadataTags" was not found


any(rec_table1$Species == "NO_ID")
any(rec_table2$Species == "NO_ID")


#############
# here's how the removeDuplicateRecords argument works

\dontrun{   # otherwise the example would run too long to pass CRAN tests

rec_table3a <- recordTable(inDir              = wd_images_ID_species,
                       IDfrom                 = "directory",
                       minDeltaTime           = 0,
                       exclude                = "NO_ID",
                       timeZone               = "Asia/Kuala_Lumpur",
                       removeDuplicateRecords = FALSE
)

rec_table3b <- recordTable(inDir              = wd_images_ID_species,
                       IDfrom                 = "directory",
                       minDeltaTime           = 0,
                       exclude                = "NO_ID",
                       timeZone               = "Asia/Kuala_Lumpur",
                       removeDuplicateRecords = TRUE
)


anyDuplicated(rec_table3a[, c("Station", "Species", "DateTimeOriginal")])   # got duplicates
anyDuplicated(rec_table3b[, c("Station", "Species", "DateTimeOriginal")])   # no duplicates

# after removing duplicates, both are identical:
whichAreDuplicated <- which(duplicated(rec_table3a[, c("Station", "Species", "DateTimeOriginal")]))
all(rec_table3a[-whichAreDuplicated,] == rec_table3b)

}

} else {                                # show function output if ExifTool is not available
message("ExifTool is not available. Cannot test function. Loading recordTableSample instead")
data(recordTableSample)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line