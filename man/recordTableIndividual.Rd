\name{recordTableIndividual}
\alias{recordTableIndividual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a single-species record table with individual identification from camera trap images or videos
}
\description{
The function generates a single-species record table containing individual IDs, e.g. for (spatial) capture-recapture analyses. It prepares input for the function \code{\link{spatialDetectionHistory}}.
}
\usage{
recordTableIndividual(inDir,
  hasStationFolders,
  IDfrom,
  cameraID,
  camerasIndependent,
  minDeltaTime = 0,
  deltaTimeComparedTo,
  timeZone,
  stationCol,
  writecsv = FALSE,
  outDir,
  metadataHierarchyDelimitor = "|",
  metadataIDTag,
  additionalMetadataTags,
  removeDuplicateRecords = TRUE,
  returnFileNamesMissingTags = FALSE,
  eventSummaryColumn,
  eventSummaryFunction,
  video
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inDir}{
  character. Directory containing images of individuals. Must end with species name (e.g. ".../speciesImages/Clouded Leopard")
}
  \item{hasStationFolders}{
  logical. Does \code{inDir} have station subdirectories? If \code{TRUE}, station IDs will be taken from directory names. If \code{FALSE}, they will be taken from image filenames (requires images renamed with \code{\link{imageRename}}).
}
  \item{IDfrom}{
  character. Read individual ID from image metadata ("metadata") of from directory names ("directory")?
}
  \item{cameraID}{
  character. Should the function look for camera IDs in the image file names? If so, set to 'filename'. Requires images renamed with \code{\link{imageRename}}. If missing, no camera ID will be assigned and it will be assumed there was 1 camera only per station.
}
  \item{camerasIndependent}{
  logical. If \code{TRUE}, cameras at a station are assumed to record individuals independently. If \code{FALSE}, cameras are assumed to be non-independent (e.g. in pairs). Takes effect only if there was more than 1 camera per station and cameraID = "filename".
}
  \item{minDeltaTime}{
  numeric. time difference between observation of the same individual at the same station/camera to be considered independent (in minutes)
}
  \item{deltaTimeComparedTo}{
  character. For two records to be considered independent, must the second one be at least \code{minDeltaTime} minutes after the last independent record of the same individual (\code{"lastIndependentRecord"}), or \code{minDeltaTime} minutes after the last record (\code{"lastRecord"})?
}
  \item{timeZone}{
  character. Must be a value returned by \code{\link[base:timezones]{OlsonNames}}
}
  \item{stationCol}{
  character. Name of the camera trap station column in the output table.
}
  \item{writecsv}{
  logical. Should the individual record table be saved as a .csv file?
}
  \item{outDir}{
  character. Directory to save csv file to. If NULL and \code{writecsv = TRUE}, the output csv will be written to \code{inDir}.
}
  \item{metadataHierarchyDelimitor}{
  character. The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":".
}
  \item{metadataIDTag}{
  character. In custom image metadata, the individual ID tag name.
}
  \item{additionalMetadataTags}{
  character. additional camera model-specific metadata tags to be extracted. (If possible specify tag groups as returned by \code{\link{exifTagNames}})
}
  \item{removeDuplicateRecords}{
  logical. If there are several records of the same individual at the same station (also same camera if cameraID is defined) at exactly the same time, show only one?
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
\item{video}{
  list. Contains information on how to handle video data (otional). See details.
}
}

\details{
The function can handle a number of different ways of storing images and videos. In every case, images need to be stored in a species directory first (e.g. using function \code{\link{getSpeciesImages}}). Station subdirectories are optional. Camera subdirectories are not supported. This directory structure can be created easily with function \code{\link{getSpeciesImages}}.

As with species identification, individuals can be identified in 2 different ways: by moving images into individual directories ("Species/Station/Individual/XY.JPG" or "Species/Individual/XY.JPG") or by metadata tagging (without the need for individual directories: "Species/XY.JPG" or "Species/Station/XY.JPG").

\code{minDeltaTime} is a criterion for temporal independence of records of an individual at the same station/location. Setting it to 0 will make the function return all records. \code{camerasIndependent} defines if the cameras at a station are to be considered independent (e.g. \code{FALSE} if both cameras face each other and possibly \code{TRUE} if they face different trails).
\code{stationCol} is the station column name to be used in the resulting table. Station IDs are read from the station directory names if \code{hasStationFolders = TRUE}. Otherwise, the function will try to extract station IDs from the image filenames (requires images renamed with \code{\link{imageRename}}.

If individual IDs were assigned with image metadata tags, \code{metadataIDTag} must be set to the name of the metadata tag group used for individual identification. \code{metadataHierarchyDelimitor} is "|" for images tagged in DigiKam and images tagged in Adobe Bridge/ Lightroom with the default settings.
Manufacturer-specific Exif metadata tags such as "AmbientTemperature" or "MoonPhase" can be extracted if specified in \code{additionalMetadataTags}. Multiple names can be specified as a character vector as: \code{c(Tag1, Tag2, ...)}. Because they are not standardized, function \code{\link{exifTagNames}} provides a vector of all available tag names. The metadata tags thus extracted may be used as individual covariates in spatial capture-recapture models.

\code{eventSummaryColumn} and \code{eventSummaryFunction} can be used to extract summary statistics for independent sampling events. For example, you assigned a "count" tag to your images, indicating the number of individuals in a picture. In a sequence of pictures taken within 1 minute, most pictures show one individual, but one image shows two individuals. You tagged the images accordingly (count = 1 or count = 2) and run \code{recordTable}. Set \code{eventSummaryColumn = "count"} and \code{eventSummaryFunction = "max"} to obtain the maximum number of \code{count} in all images within \code{minDeltaTime} minutes of a given record. The results is in a new column, in this example \code{count_max}. You can also calculate several statistics at the same time, by supplying vectors of values, e.g. \code{eventSummaryColumn = c("count", "count", "camera")} and \code{eventSummaryFunction = c("min", "max", "unique")} to get minimum and maximum count and all unique camera IDs for that event. Note that \code{eventSummaryColumn} and \code{eventSummaryFunction} must be of same length. 

Argument \code{video} is analogous to \code{\link{recordTable}}, a named list with 2 or 4 items. 2 items (\code{file_formats}, \code{dateTimeTag}) are always required, and are sufficent if \code{IDfrom = "directory"}. In that case, no digiKam tags will be returned. 
To return digiKam tags, two additional items are required (\code{db_directory}, \code{db_filename}). This is essential when using \code{IDfrom = "metadata"}. When using \code{IDfrom = "directory"}, it is optional, but allows to extract metadata tags assigned to videos in digiKam. This workaround is necessary because digiKam tags are not written into video metadata, but are only saved in the digiKam database. So in contrast to JPG images, they can not be extracted with ExifTool. It also requires that \code{inDir} is in your digiKam database.

The items of argument \code{video} are:

\tabular{ll}{
\code{file_formats} \tab The video formats to extract (include "jpg" if you want .JPG image metadata) \cr
\code{dateTimeTag}  \tab the metadata tag to extract date/time from (use \code{\link{exifTagNames}} to find out which tag is suitable) \cr
\code{db_directory} \tab The directory containing digiKam database (optional if \code{IDfrom = "directory"}) \cr
\code{db_filename}  \tab  The digiKam database file in \code{db_directory} (optional if \code{IDfrom = "directory"}) \cr
}


See the example below for for how to specify the argument \code{video}.


}
\value{
A data frame containing species records with individual IDs and additional information about stations, date, time and (optionally) further metadata.
}

\section{Warning }{
Be sure to read the section on individual identification in the package vignette (\url{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr2.html}).

Af you use image metadata tags for identification, the tags must be written to the image metadata. The function cannot read tags from .xmp sidecar files. Make sure you set the preferences of your image management software accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There, make sure "Write to sidecar files" is unchecked.

Please note the section about defining argument \code{timeZone} in the vignette on data extraction (accessible via \code{vignette("DataExtraction")} or online (\url{https://cran.r-project.org/package=camtrapR/vignettes/camtrapr3.html})).
}

\references{
Phil Harvey's ExifTool \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}
}

\author{
Juergen Niedballa
}

\examples{

\dontrun{   # the examples run too long to pass CRAN tests

 wd_images_ID_individual <- system.file("pictures/sample_images_indiv_tag/LeopardCat", 
                                        package = "camtrapR")
 # missing space in species = "LeopardCat" is because of CRAN package policies
 # note argument additionalMetadataTags: contains tag names as returned by function exifTagNames

 if (Sys.which("exiftool") != ""){        # only run these examples if ExifTool is available

 rec_table_pbe <- recordTableIndividual(inDir                  = wd_images_ID_individual,
                                        minDeltaTime           = 60,
                                        deltaTimeComparedTo    = "lastRecord",
                                        hasStationFolders      = FALSE,
                                        IDfrom                 = "metadata",
                                        camerasIndependent     = FALSE,
                                        writecsv               = FALSE,
                                        metadataIDTag          = "individual",
                                        additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
                                        timeZone               = "Asia/Kuala_Lumpur"
 )
 
 
 # extracting some example summary stats too 
 # a nonsensical example, get all unique cameras with which the event was photographed
 
  rec_table_pbe2 <- recordTableIndividual(inDir                = wd_images_ID_individual,
                                        minDeltaTime           = 60,
                                        deltaTimeComparedTo    = "lastRecord",
                                        hasStationFolders      = FALSE,
                                        IDfrom                 = "metadata",
                                        camerasIndependent     = FALSE,
                                        writecsv               = FALSE,
                                        metadataIDTag          = "individual",
                                        additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
                                        timeZone               = "Asia/Kuala_Lumpur",
                                        eventSummaryColumn     = "EXIF:Make",
                                        eventSummaryFunction   = "unique"
 )
 
 ### Video example (the sample data don't contain a video, this is hypothetical)
 # with JPG, video mp4, avi, mov, ID = metadata
rec_table_ind_video <- recordTableIndividual(inDir = wd_images_ID_individual,
                          hasStationFolder  = FALSE,
                          IDfrom            = "metadata", 
                          metadataIDTag     = "individual",
                          video = list(file_formats = c("jpg", "mp4", "avi", "mov"),
                                       dateTimeTag = "QuickTime:CreateDate",
                                       db_directory = "C:/Users/YourName/Pictures",
                                       db_filename = "digikam4.db")
)

  
} else {  
# show function output if ExifTool is not available
message("ExifTool is not available. Cannot test function. Loading recordTableSample instead")
data(recordTableSample)
}
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line