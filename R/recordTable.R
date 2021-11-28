#' Generate a species record table from camera trap images and videos
#' 
#' Generates a record table from camera trap images or videos. Images/videos
#' must be sorted into station directories at least. The function can read
#' species identification from a directory structure (Station/Species or
#' Station/Camera/Species) or from image metadata tags.
#' 
#' The function can handle a number of different ways of storing images, and
#' supports species identification by moving images into species directories as
#' well as metadata tagging. In every case, images need to be stored into
#' station directories. If images are identified by moving them into species
#' directories, a camera directory is optional: "Station/Species/XY.JPG" or
#' "Station/Camera/Species/XY.JPG". Likewise, if images are identified using
#' metadata tagging, a camera directory can be used optionally:
#' "Station/XY.JPG" or "Station/Camera/XY.JPG".
#' 
#' If images are identified by metadata tagging, \code{metadataSpeciesTag}
#' specifies the metadata tag group name that contains species identification
#' tags. \code{metadataHierarchyDelimitor} is "|" for images tagged in DigiKam
#' and images tagged in Adobe Bridge / Lightroom with the default settings. It
#' is only necessary to change it if the default was changed in these programs.
#' 
#' \code{minDeltaTime} is a criterion for temporal independence of species
#' recorded at the same station. Setting it to 0 will make the function return
#' all records. Setting it to a higher value will remove records that were
#' taken less than \code{minDeltaTime} minutes after the last record
#' (\code{deltaTimeComparedTo = "lastRecord"}) or the last independent record
#' (\code{deltaTimeComparedTo = "lastIndependentRecord"}).
#' 
#' \code{removeDuplicateRecords} determines whether duplicate records
#' (identical station, species, date/time, (and camera if applicable)) are are
#' all returned (TRUE) or collapsed into a single unique record (FALSE).
#' 
#' \code{camerasIndependent} defines if the cameras at a station are to be
#' considered independent. If \code{TRUE}, records of the same species taken by
#' different cameras are considered independent (e.g. if they face different
#' trails). Use \code{FALSE} if both cameras face each other and possibly
#' \code{TRUE} ).
#' 
#' \code{exclude} can be used to exclude "species" directories containing
#' irrelevant images (e.g. "team", "blank", "unidentified"). \code{stationCol}
#' can be set to match the station column name in the camera trap station table
#' (see \code{\link{camtraps}}).
#' 
#' Many digital images contain Exif metadata tags such as "AmbientTemperature"
#' or "MoonPhase" that can be extracted if specified in \code{metadataTags}.
#' Because these are manufacturer-specific and not standardized, function
#' \code{\link{exifTagNames}} provides a vector of all available tag names.
#' Multiple names can be specified as a character vector as: \code{c(Tag1,
#' Tag2, ...)}. The metadata tags thus extracted may be used as covariates in
#' modelling species distributions.
#' 
#' \code{eventSummaryColumn} and \code{eventSummaryFunction} can be used to
#' extract summary statistics for independent sampling events. For example, you
#' assigned a "count" tag to your images, indicating the number of individuals
#' in a picture. In a sequence of pictures taken within 1 minute, most pictures
#' show one individual, but one image shows two individuals. You tagged the
#' images accordingly (count = 1 or count = 2) and run \code{recordTable}. Set
#' \code{eventSummaryColumn = "count"} and \code{eventSummaryFunction = "max"}
#' to obtain the maximum number of \code{count} in all images within
#' \code{minDeltaTime} minutes of a given record. The results is in a new
#' column, in this example \code{count_max}. You can also calculate several
#' statistics at the same time, by supplying vectors of values, e.g.
#' \code{eventSummaryColumn = c("count", "count", "camera")} and
#' \code{eventSummaryFunction = c("min", "max", "unique")} to get minimum and
#' maximum count and all unique camera IDs for that event. Note that
#' \code{eventSummaryColumn} and \code{eventSummaryFunction} must be of same
#' length.
#' 
#' Argument \code{video} is a named list with 2 or 4 items. 2 items
#' (\code{file_formats}, \code{dateTimeTag}) are always required, and are
#' sufficent if \code{IDfrom = "directory"}. In that case, no digiKam tags will
#' be returned.  To return digiKam tags, two additional items are required
#' (\code{db_directory}, \code{db_filename}). This is essential when using
#' \code{IDfrom = "metadata"}. When using \code{IDfrom = "directory"}, it is
#' optional, but allows to extract metadata tags assigned to videos in digiKam.
#' This workaround is necessary because digiKam tags are not written into video
#' metadata, but are only saved in the digiKam database. So in contrast to JPG
#' images, they can not be extracted with ExifTool. It also requires that
#' \code{inDir} is in your digiKam database.
#' 
#' The items of argument \code{video} are:
#' 
#' \tabular{ll}{ \code{file_formats} \tab The video formats to extract (include
#' "jpg" if you want .JPG image metadata) \cr \code{dateTimeTag} \tab the
#' metadata tag to extract date/time from (use \code{\link{exifTagNames}} to
#' find out which tag is suitable) \cr \code{db_directory} \tab The directory
#' containing digiKam database (optional if \code{IDfrom = "directory"}) \cr
#' \code{db_filename} \tab The digiKam database file in \code{db_directory}
#' (optional if \code{IDfrom = "directory"}) \cr }
#' 
#' See the examples below for for how to specify the argument \code{video}.
#' 
#' @param inDir character. Directory containing station directories. It must
#' either contain images in species subdirectories (e.g.
#' inDir/StationA/SpeciesA) or images with species metadata tags (without
#' species directories, e.g. inDir/StationA).
#' @param IDfrom character. Read species ID from image metadata ("metadata") of
#' from species directory names ("directory")?
#' @param cameraID character. Where should the function look for camera IDs:
#' 'filename', 'directory'. 'filename' requires images renamed with
#' \code{\link{imageRename}}. 'directory' requires a camera subdirectory within
#' station directories (station/camera/species). Can be missing.
#' @param camerasIndependent logical. If \code{TRUE}, species records are
#' considered to be independent between cameras at a station.
#' @param exclude character. Vector of species names to be excluded from the
#' record table
#' @param minDeltaTime integer. Time difference between records of the same
#' species at the same station to be considered independent (in minutes)
#' @param deltaTimeComparedTo character. For two records to be considered
#' independent, must the second one be at least \code{minDeltaTime} minutes
#' after the last independent record of the same species
#' (\code{"lastIndependentRecord"}), or \code{minDeltaTime} minutes after the
#' last record (\code{"lastRecord"})?
#' @param timeZone character. Must be a value returned by
#' \code{\link[base:timezones]{OlsonNames}}
#' @param stationCol character. Name of the camera trap station column.
#' Assuming "Station" if undefined.
#' @param writecsv logical. Should the record table be saved as a .csv?
#' @param outDir character. Directory to save csv to. If NULL and
#' \code{writecsv = TRUE}, recordTable will be written to \code{inDir}.
#' @param metadataHierarchyDelimitor character. The character delimiting
#' hierarchy levels in image metadata tags in field "HierarchicalSubject".
#' Either "|" or ":".
#' @param metadataSpeciesTag character. In custom image metadata, the species
#' ID tag name.
#' @param additionalMetadataTags character. Additional camera model-specific
#' metadata tags to be extracted. (If possible specify tag groups as returned
#' by \code{\link{exifTagNames}})
#' @param removeDuplicateRecords logical. If there are several records of the
#' same species at the same station (also same camera if cameraID is defined)
#' at exactly the same time, show only one?
#' @param returnFileNamesMissingTags logical. If species are assigned with
#' metadata and images are not tagged, return a few file names of these images
#' as a message?
#' @param eventSummaryColumn character. A column in the record table (e.g. from
#' a metadata tag) by to summarise non-independent records (those within
#' \code{minDeltaTime} of a given record) with a user-defined function
#' (\code{eventSummaryFunction})
#' @param eventSummaryFunction character. The function by which to summarise
#' \code{eventSummaryColumn} of non-independent records, e.g. "sum", "max"
#' (optional)
#' @param video list. Contains information on how to handle video data
#' (optional). See details.
#' 
#' @return A data frame containing species records and additional information
#' about stations, date, time and (optionally) further metadata.
#' 
#' @note The results of a number of other function will depend on the output of
#' this function (namely on the arguments \code{exclude} for excluding species
#' and \code{minDeltaTime}/ \code{deltaTimeComparedTo} for temporal
#' independence):
#' 
#' \tabular{l}{ \code{\link{detectionMaps}} \cr \code{\link{detectionHistory}}
#' \cr \code{\link{activityHistogram}} \cr \code{\link{activityDensity}} \cr
#' \code{\link{activityRadial}} \cr \code{\link{activityOverlap}} \cr
#' \code{\link{activityHistogram}} \cr \code{\link{surveyReport}} \cr }
#' @section Warning : Custom image metadata must be organised hierarchically
#' (tag group - tag; e.g. "Species" - "Leopard Cat"). Detailed information on
#' how to set up and use metadata tags can be found in
#' \href{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr2.html#metadata-taggingvignette
#' 2: Species and Individual Identification}.
#' 
#' Custom image metadata tags must be written to the images. The function
#' cannot read tags from .xmp sidecar files. Make sure you set the preferences
#' accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There,
#' make sure "Write to sidecar files" is unchecked.
#' 
#' Please note the section about defining argument \code{timeZone} in the
#' vignette on data extraction (accessible via
#' \code{vignette("DataExtraction")} or online
#' (\url{https://cran.r-project.org/package=camtrapR/vignettes/camtrapr3.html})).
#' 
#' @author Juergen Niedballa
#' 
#' @references Phil Harvey's ExifTool \url{https://exiftool.org/}
#' 
#' @examples
#' 
#' 
#' 
#' \dontrun{   # the examples take too long to pass CRAN tests
#' 
#' 
#' # set directory with camera trap images in station directories
#' wd_images_ID_species <- system.file("pictures/sample_images_species_dir", 
#'                                     package = "camtrapR")
#' 
#' if (Sys.which("exiftool") != ""){        # only run these examples if ExifTool is available
#' 
#' 
#' rec_table1 <- recordTable(inDir               = wd_images_ID_species,
#'                        IDfrom                 = "directory",
#'                        minDeltaTime           = 60,
#'                        deltaTimeComparedTo    = "lastRecord",
#'                        writecsv               = FALSE,
#'                        additionalMetadataTags = c("EXIF:Model", "EXIF:Make")
#' )
#' # note argument additionalMetadataTags: it contains tag names as returned by function exifTagNames
#' 
#' rec_table2 <- recordTable(inDir               = wd_images_ID_species,
#'                        IDfrom                 = "directory",
#'                        minDeltaTime           = 60,
#'                        deltaTimeComparedTo    = "lastRecord",
#'                        exclude                = "UNID",
#'                        writecsv               = FALSE,
#'                        timeZone               = "Asia/Kuala_Lumpur",
#'                        additionalMetadataTags = c("EXIF:Model", "EXIF:Make", "NonExistingTag"),
#'                        eventSummaryColumn     = "EXIF:Make",
#'                        eventSummaryFunction   = "unique"
#'                        )
#'                        
#' # note the warning that the last tag in "additionalMetadataTags" ("NonExistingTag") was not found
#' 
#' 
#' any(rec_table1$Species == "UNID")    # TRUE
#' any(rec_table2$Species == "UNID")    # FALSE
#' 
#' 
#' # here's how the removeDuplicateRecords argument works
#' 
#' rec_table3a <- recordTable(inDir              = wd_images_ID_species,
#'                        IDfrom                 = "directory",
#'                        minDeltaTime           = 0,
#'                        exclude                = "UNID",
#'                        timeZone               = "Asia/Kuala_Lumpur",
#'                        removeDuplicateRecords = FALSE
#' )
#' 
#' rec_table3b <- recordTable(inDir              = wd_images_ID_species,
#'                        IDfrom                 = "directory",
#'                        minDeltaTime           = 0,
#'                        exclude                = "UNID",
#'                        timeZone               = "Asia/Kuala_Lumpur",
#'                        removeDuplicateRecords = TRUE
#' )
#' 
#' 
#' anyDuplicated(rec_table3a[, c("Station", "Species", "DateTimeOriginal")])   # got duplicates
#' anyDuplicated(rec_table3b[, c("Station", "Species", "DateTimeOriginal")])   # no duplicates
#' 
#' # after removing duplicates, both are identical:
#' whichAreDuplicated <- which(duplicated(rec_table3a[,c("Station", "Species", "DateTimeOriginal")]))
#' all(rec_table3a[-whichAreDuplicated,] == rec_table3b)
#' 
#' 
#' ### extracting species IDs from metadata
#' 
#' wd_images_ID_species_tagged <- system.file("pictures/sample_images_species_tag", 
#'                                            package = "camtrapR")
#' 
#' rec_table4 <- recordTable(inDir               = wd_images_ID_species_tagged,
#'                        IDfrom                 = "metadata",
#'                        metadataSpeciesTag     = "Species",
#'                        exclude                = "unidentified")
#' 
#' 
#' ###  Including videos
#' # sample videos are not included in package
#' 
#' # with videos, IDfrom = "directory", not extracting digiKam metadata
#' 
#' rec_table4 <- recordTable(inDir  = wd_images_ID_species,
#'                           IDfrom = "directory",
#'                           video  = list(file_formats = c("jpg", "mp4"),
#'                                         dateTimeTag  = "QuickTime:CreateDate")
#' )
#' 
#' # with videos, IDfrom = "metadata", extracting digiKam metadata
#' 
#' rec_table5 <- recordTable(inDir  = wd_images_ID_species,
#'                           IDfrom = "metadata",
#'                           metadataSpeciesTag = "Species",
#'                           video  = list(file_formats = c("jpg", "mp4", "avi", "mov"),
#'                                         dateTimeTag  = "QuickTime:CreateDate",
#'                                         db_directory = "C:/Users/YourName/Pictures",
#'                                         db_filename = "digikam4.db")
#' )
#' 
#' } else {  
#' # show function output if ExifTool is not available
#' message("ExifTool is not available. Cannot test function. Loading recordTableSample instead")
#' data(recordTableSample)
#' }
#' 
#' }
#' 
#' @export recordTable
#' 
recordTable <- function(inDir,
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
                        eventSummaryFunction,
                        video
)
{

  wd0 <- getwd()
  on.exit(setwd(wd0))

  if(!hasArg(stationCol)) stationCol <- "Station"
  if(!is.character(stationCol)) stop("stationCol must be of class 'character'")
  checkForSpacesInColumnNames(stationCol = stationCol)
  
  speciesCol <- "Species"

  if(!is.character(IDfrom)) stop("IDfrom must be of class 'character'")
  IDfrom <- match.arg(IDfrom, choices = c("metadata", "directory"))  

 if(IDfrom == "metadata"){
    metadataHierarchyDelimitor <- match.arg(metadataHierarchyDelimitor, choices = c("|", ":"))
    if(!hasArg(metadataSpeciesTag))       stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")
    if(!is.character(metadataSpeciesTag)) stop("metadataSpeciesTag must be of class 'character'")
    if(length(metadataSpeciesTag) != 1)   stop("metadataSpeciesTag must be of length 1")
  }

  multiple_tag_separator <- "_&_"

  # check input
  if(!hasArg(timeZone)) {
    message("timeZone is not specified. Assuming UTC")
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()", call. = FALSE)
  }
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(hasArg(cameraID)){
    if(!is.character(cameraID))         stop("cameraID must be of class 'character'", call. = FALSE)
    cameraID <- match.arg(cameraID, choices = c("filename", "directory"))
    if(!hasArg(camerasIndependent))     stop("camerasIndependent is not defined. It must be defined if cameraID is defined", call. = FALSE)
    if(!is.logical(camerasIndependent)) stop("camerasIndependent must be of class 'logical'", call. = FALSE)
  } else { 
    camerasIndependent <- FALSE
  }

  cameraCol <- "Camera"


  if(hasArg(outDir)){
    if(!is.character(outDir))         stop("outDir must be of class 'character'", call. = FALSE)
    if(isFALSE(file.exists(outDir)))  stop("outDir does not exist", call. = FALSE)
  }

  if(hasArg(exclude)){
    if(!is.character(exclude)) stop("exclude must be of class 'character'", call. = FALSE)
  }

  if(!is.logical(removeDuplicateRecords))     stop("'removeDuplicateRecords' must be logical (TRUE / FALSE)", call. = FALSE)
  if(!is.logical(returnFileNamesMissingTags)) stop("'returnFileNamesMissingTags' must be logical (TRUE / FALSE)", call. = FALSE)


  metadata.tagname <- "HierarchicalSubject"    # for extracting metadata assigned in tagging software

  if(hasArg(additionalMetadataTags)){
    if(!is.character(additionalMetadataTags)) stop("additionalMetadataTags must be of class 'character'", call. = FALSE)
    if(any(grep(pattern = " ", x = additionalMetadataTags, fixed = TRUE))) stop("In argument additionalMetadataTags, spaces are not allowed")
    if("HierarchicalSubject" %in% additionalMetadataTags & IDfrom == "metadata")  {
      message("'HierarchicalSubject' may not be in 'additionalMetadataTags' if IDfrom = 'metadata'. It will be ignored because the function returns it anyway.", call. = FALSE)
      additionalMetadataTags <- additionalMetadataTags[-grep(pattern = "HierarchicalSubject", x = additionalMetadataTags)]  # remove HierarchicalSubject from additionalMetadataTags
    }
  }

  minDeltaTime <- as.integer(minDeltaTime)
  if(!is.integer(minDeltaTime)) stop("'minDeltaTime' must be an integer", call. = FALSE)

  if(minDeltaTime != 0){
    if(isFALSE(removeDuplicateRecords)){
      warning("minDeltaTime is > 0. Therefore, removeDuplicateRecords was set to TRUE (otherwise there may be records taken at the same time)", call. = FALSE, immediate. = TRUE)
      removeDuplicateRecords <- TRUE
    }
    
    deltaTimeComparedTo < match.arg(deltaTimeComparedTo, choices = c("lastRecord", "lastIndependentRecord"))
    
    if(!hasArg(deltaTimeComparedTo)) {
      stop(paste("minDeltaTime is not 0. deltaTimeComparedTo must be defined"), call. = FALSE)
    }
  } else {
    if(hasArg(deltaTimeComparedTo)) {
      warning(paste("minDeltaTime is 0. deltaTimeComparedTo = '", deltaTimeComparedTo, "' will have no effect", sep = ""), call. = FALSE, immediate. = TRUE)
    } else {
      deltaTimeComparedTo <- "lastRecord"
    }
  }

  if(!is.logical(writecsv))  stop("writecsv must be logical (TRUE or FALSE)", call. = FALSE)
  if(!is.character(inDir))   stop("inDir must be of class 'character'", call. = FALSE)
  if(length(inDir) != 1)     stop("inDir may only consist of 1 element only", call. = FALSE)
  if(!dir.exists(inDir))     stop("Could not find inDir:\n", inDir, call. = FALSE)

  if(hasArg(eventSummaryColumn)) {
    if(!is.character(eventSummaryColumn))     stop("eventSummaryColumn must be of class 'character'", call. = FALSE)
    if(!is.character(eventSummaryFunction))   stop("eventSummaryFunction must be of class 'character'", call. = FALSE)
  }
  
  # find image directories
  dirs       <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  
  if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)
  max_nchar_station <- max(nchar(dirs_short))
  
  
  # process video argument (if present)
  if(hasArg(video)){
    video_out <- processVideoArgument(IDfrom = IDfrom,
                                      video  = video)
    digiKam_data <- video_out$digiKam_data
    file_formats <- video_out$file_formats
    # if(isFALSE("jpg" %in% file_formats)) file_formats <- c(file_formats, "jpg")
  } else {
    file_formats <- "jpg"   # jpg, as the default, if video not requested
  }
  
  # empty list for metadata output
  record.table.list <- list()
  
  # create command line calls
  command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal', 
                        ifelse(hasArg(video), 
                               paste(" -", video$dateTimeTag, sep = ""), 
                               ""),    # if video requested, video date time tag
                        ' -HierarchicalSubject',
                        ifelse(hasArg(additionalMetadataTags), paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ""),
                        paste(" -ext", file_formats, collapse = " ", sep = " "),    # requested file extensions
                        ' "', dirs, '"', sep = "")
  
  # construct column names for metadata table
  colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")
  if(hasArg(video)) colnames.tmp <- c(colnames.tmp, video$dateTimeTag)
  colnames.tmp <- c(colnames.tmp, "HierarchicalSubject")
  if(hasArg(additionalMetadataTags)) colnames.tmp <- c(colnames.tmp, additionalMetadataTags [nchar(additionalMetadataTags) >= 2]) 
  # only add as new column if entry of additionalMetadataTag has more than 2 characters (to allow users to specify exiftool commands, e.g. -L)
  
  
  for(i in 1:length(dirs)){   # loop through station directories

    # execute exiftool
    metadata.tmp <- runExiftool(command.tmp = command.tmp[i], colnames.tmp = colnames.tmp)

    if(class(metadata.tmp) == "NULL"){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = paste(".", file_formats, "$", collapse = "|", sep = ""), 
                                      ignore.case = TRUE, recursive = TRUE))
      message(paste(formatC(dirs_short[i], width = max_nchar_station, flag = "-"),  ":  ",
                    formatC(length.tmp, width = 5), " files      Skipping", sep = ""))
      warning(paste(dirs_short[i],  ":  contains no files of interest and was omitted\n"), call. = FALSE,  immediate. = FALSE)
    } else {

      # if video files extracted, add DateTimeOriginal and HierarchicalSubject
      if(hasArg(video)){
        metadata.tmp <- addVideoDateTimeOriginal(metadata.tmp = metadata.tmp, video = video)
        
        # add HierachicalSubject for video files
        if(!is.null(digiKam_data)){
          digiKamVideoMetadata <- digiKamVideoHierarchicalSubject(stationDir = dirs[i],
                                                                    digiKamTablesList = digiKam_data,    # output of accessDigiKamDatabase
                                                                    videoFormat = file_formats[!grepl(file_formats, pattern = "jpg")])
          # add HierarchialSubject for video files (match by filename and path)
          metadata.tmp <- addVideoHierarchicalSubject (metadata.tmp = metadata.tmp,
                                                       video = video,
                                                       digiKamVideoMetadata = digiKamVideoMetadata,
                                                       digiKamTablesList = digiKam_data,
                                                       videoFormat = file_formats[!grepl(file_formats, pattern = "jpg")])
        }
      }
      
      # check presence / consistency of DateTimeOriginal column, go to next station or remove records if necessary
      metadata.tmp <- checkDateTimeOriginal (intable    = metadata.tmp,
                                             dirs_short = dirs_short,
                                             i          = i)
      if(is.null(metadata.tmp)) next

      
      # now split HierarchicalSubject tags and add as columns to table
      metadata.tmp <- addMetadataAsColumns (intable                    = metadata.tmp,
                                            metadata.tagname           = metadata.tagname,
                                            metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                            multiple_tag_separator     = multiple_tag_separator)

      # add species names to metadata table (from folders or metadata, otherwise NA)
      metadata.tmp <- assignSpeciesID(intable                = metadata.tmp,
                                      IDfrom                 = IDfrom,
                                      metadataSpeciesTag     = metadataSpeciesTag,
                                      speciesCol             = speciesCol,
                                      dirs_short             = dirs_short,
                                      i_tmp                  = i,
                                      multiple_tag_separator = multiple_tag_separator,
                                      returnFileNamesMissingTags = returnFileNamesMissingTags)
      
      # if images in station contain no metadata species tags, skip that station
      if(!is.data.frame(metadata.tmp)){
        if(metadata.tmp == "found no species tag") {
          warning(paste(dirs_short[i], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata tag 'HierarchicalSubject'. Skipping", sep = ""), call. = FALSE, immediate. = TRUE)
        } else {
          warning(paste(dirs_short[i], ":   error in species tag extraction. Skipping. Please report this bug", sep = ""), call. = FALSE, immediate. = TRUE)
        }
        next
      }

      # remove empty metadata columns (if HierarchicalSubject is all empty or if additionalMetadataTags were not found)
      empty_cols <- which(apply(metadata.tmp, MARGIN = 2, FUN = function(X){all(X == "-")}))
      if(length(empty_cols) >= 1){
        metadata.tmp <-  metadata.tmp[, -empty_cols]
      }

      # add station and camera id to metadata table
      arg.list0 <- list(intable = metadata.tmp, dirs_short = dirs_short, stationCol = stationCol, hasStationFolders = TRUE, cameraCol = cameraCol, i = i, IDfrom = IDfrom)  # assumes station directories

      if(!hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, arg.list0)
      if( hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, c(arg.list0, cameraID = cameraID))

      # remove species in argument "excluded"
      if(hasArg (exclude)){
        if(any(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude))) {  # if there is anything to remove
          metadata.tmp <- metadata.tmp[-which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude)),]
        }
      }

      if(nrow(metadata.tmp) >= 1){   # if anything left after excluding species, do

        # convert character vector extracted from images to time object and format for outfilename
        metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

        # sort by station, (camera), species and time
        if(camerasIndependent == TRUE) {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp[,cameraCol], metadata.tmp$DateTimeOriginal),]
        } else {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp$DateTimeOriginal),]
        }



        #remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
        metadata.tmp2 <- removeDuplicatesOfRecords(metadata.tmp          = metadata.tmp,
                                                  removeDuplicateRecords = removeDuplicateRecords,
                                                  camerasIndependent     = camerasIndependent,
                                                  stationCol             = stationCol,
                                                  speciesCol             = speciesCol,
                                                  cameraCol              = cameraCol,
                                                  current                = i, 
                                                  total                  = length(dirs),
                                                  max_nchar_station      = max_nchar_station)


        # assess independence between records and calculate time differences
        args.assessTemporalIndependence <- list(intable             = metadata.tmp2,
                                                deltaTimeComparedTo = deltaTimeComparedTo,
                                                columnOfInterest    = speciesCol,
                                                cameraCol           = cameraCol,
                                                camerasIndependent  = camerasIndependent,
                                                minDeltaTime        = minDeltaTime,
                                                stationCol          = stationCol)
        
        if(hasArg(eventSummaryColumn)) {
          args.assessTemporalIndependence <- c(args.assessTemporalIndependence,
                                               list(eventSummaryColumn   = eventSummaryColumn,
                                                    eventSummaryFunction = eventSummaryFunction))
        }
        
        d1 <- do.call(assessTemporalIndependence, args = args.assessTemporalIndependence)

        record.table.list[[i]] <- d1

        suppressWarnings(rm(d1))
      }  # end      if(nrow(metadata.tmp) >= 1){} else {...}   # i.e. not all species were excluded
    }    # end      if(nrow(metadata.tmp) == 0){} else {...}   # i.e. directory i contained images
  }      # end      for(i in 1:length(dirs)){   # loop through station directories

  
  # combine all data frames from list into one data frame
  record.table <- as.data.frame(data.table::rbindlist(record.table.list, fill = TRUE, use.names = TRUE))
  
  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty. Were date/time information unreadable? Could species tags not be extracted (if IDfrom = 'metadata')? Or did you exclude too many species?"), call. = FALSE)
  }

  # rearrange table, add date and time as separate columns. add additional column names as needed.

  record.table2  <-  data.frame(record.table[,c(stationCol, speciesCol, "DateTimeOriginal")],
                                Date = as.Date (record.table$DateTimeOriginal, format = "%Y/%M/%d", tz = timeZone),
                                Time = strftime(record.table$DateTimeOriginal, format = "%H:%M:%S", tz = timeZone),
                                record.table[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
                                                "Directory", "FileName")])

  metadata_columns <- which(!colnames(record.table) %in% colnames(record.table2))

  # add metadata columns
  if(length(metadata_columns) >= 1){
    record.table3 <- cbind(record.table2, record.table[,metadata_columns])
    colnames(record.table3)[(ncol(record.table2) + 1) : ncol(record.table3)] <- colnames(record.table)[metadata_columns]
  } else {record.table3 <- record.table2}


  # add camera column (if present)
  if(hasArg(cameraID)){
    record.table3 <- data.frame(record.table3[,stationCol],
                                record.table[,cameraCol],
                                record.table3[,-which(colnames(record.table3) %in% c(stationCol, cameraCol))])
    colnames(record.table3)[1] <- stationCol
    colnames(record.table3)[2] <- cameraCol
  }

  rownames(record.table3) <- NULL


  # warning if additionalMetadataTags were not found
  if(hasArg(additionalMetadataTags)){
    #whichAdditionalMetadataTagsFound <- which(gsub(additionalMetadataTags, pattern = ":", replacement = ".") %in% colnames(record.table3))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    whichAdditionalMetadataTagsFound <- which(additionalMetadataTags %in% colnames(record.table3))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    if(length(whichAdditionalMetadataTagsFound) < length(additionalMetadataTags)){
      if(length(whichAdditionalMetadataTagsFound) == 0) {  # if none of the additionalMetadataTags was found
        warning("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags, collapse = ", "), call. = FALSE)
        } else {                                                            # if only some of the additionalMetadataTags was found
        warning("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags[-whichAdditionalMetadataTagsFound], collapse = ", "), call. = FALSE)
      }
    }
  }

  # make column "HierarchicalSubject" the last column
  col_to_move <- which(colnames(record.table3) %in% metadata.tagname)
  if(length(col_to_move) >= 1){
     record.table3 <- cbind(record.table3, record.table3[,col_to_move])
	  record.table3 <- record.table3[,-col_to_move]
    colnames(record.table3)[ncol(record.table3)] <- metadata.tagname
  }

  # convert to data.frame, in order to get all the column names right (: becomes .) 
  # NOTE: doesn't work unless data.frame is called. : in column names means they must be adressed as e.g. record.table3$`IPTC:Keywords`
  record.table3 <- data.frame(record.table3, stringsAsFactors = FALSE, check.names = TRUE)
  
  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_table_", minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    message("saving csv to  ", file.path(inDir, outtable_filename))
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
    } else {
      setwd(outDir)
    }
  write.csv(record.table3, file = outtable_filename)
  }
  return(record.table3)
}
