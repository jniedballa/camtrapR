#' Generate a single-species record table with individual identification from
#' camera trap images or videos
#' 
#' The function generates a single-species record table containing individual
#' IDs, e.g. for (spatial) capture-recapture analyses. It prepares input for
#' the function \code{\link{spatialDetectionHistory}}.
#' 
#' The function can handle a number of different ways of storing images and
#' videos. In every case, images need to be stored in a species directory first
#' (e.g. using function \code{\link{getSpeciesImages}}). Station subdirectories
#' are optional. Camera subdirectories are not supported. This directory
#' structure can be created easily with function
#' \code{\link{getSpeciesImages}}.
#' 
#' As with species identification, individuals can be identified in 2 different
#' ways: by moving images into individual directories
#' ("Species/Station/Individual/XY.JPG" or "Species/Individual/XY.JPG") or by
#' metadata tagging (without the need for individual directories:
#' "Species/XY.JPG" or "Species/Station/XY.JPG").
#' 
#' \code{minDeltaTime} is a criterion for temporal independence of records of
#' an individual at the same station/location. Setting it to 0 will make the
#' function return all records. \code{camerasIndependent} defines if the
#' cameras at a station are to be considered independent (e.g. \code{FALSE} if
#' both cameras face each other and possibly \code{TRUE} if they face different
#' trails). \code{stationCol} is the station column name to be used in the
#' resulting table. Station IDs are read from the station directory names if
#' \code{hasStationFolders = TRUE}. Otherwise, the function will try to extract
#' station IDs from the image filenames (requires images renamed with
#' \code{\link{imageRename}}.
#' 
#' If individual IDs were assigned with image metadata tags,
#' \code{metadataIDTag} must be set to the name of the metadata tag group used
#' for individual identification. \code{metadataHierarchyDelimitor} is "|" for
#' images tagged in DigiKam and images tagged in Adobe Bridge/ Lightroom with
#' the default settings. Manufacturer-specific Exif metadata tags such as
#' "AmbientTemperature" or "MoonPhase" can be extracted if specified in
#' \code{additionalMetadataTags}. Multiple names can be specified as a
#' character vector as: \code{c(Tag1, Tag2, ...)}. Because they are not
#' standardized, function \code{\link{exifTagNames}} provides a vector of all
#' available tag names. The metadata tags thus extracted may be used as
#' individual covariates in spatial capture-recapture models.
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
#' Argument \code{video} is analogous to \code{\link{recordTable}}, a named
#' list with 2 or 4 items. 2 items (\code{file_formats}, \code{dateTimeTag})
#' are always required, and are sufficent if \code{IDfrom = "directory"}. In
#' that case, no digiKam tags will be returned.  To return digiKam tags, two
#' additional items are required (\code{db_directory}, \code{db_filename}).
#' This is essential when using \code{IDfrom = "metadata"}. When using
#' \code{IDfrom = "directory"}, it is optional, but allows to extract metadata
#' tags assigned to videos in digiKam. This workaround is necessary because
#' digiKam tags are not written into video metadata, but are only saved in the
#' digiKam database. So in contrast to JPG images, they can not be extracted
#' with ExifTool. It also requires that \code{inDir} is in your digiKam
#' database.
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
#' See the example below for for how to specify the argument \code{video}.
#' 
#' @param inDir character. Directory containing images of individuals. Must end
#' with species name (e.g. ".../speciesImages/Clouded Leopard")
#' @param hasStationFolders logical. Does \code{inDir} have station
#' subdirectories? If \code{TRUE}, station IDs will be taken from directory
#' names. If \code{FALSE}, they will be taken from image filenames (requires
#' images renamed with \code{\link{imageRename}}).
#' @param IDfrom character. Read individual ID from image metadata ("metadata")
#' of from directory names ("directory")?
#' @param cameraID character. Should the function look for camera IDs in the
#' image file names? If so, set to 'filename'. Requires images renamed with
#' \code{\link{imageRename}}. If missing, no camera ID will be assigned and it
#' will be assumed there was 1 camera only per station.
#' @param camerasIndependent logical. If \code{TRUE}, cameras at a station are
#' assumed to record individuals independently. If \code{FALSE}, cameras are
#' assumed to be non-independent (e.g. in pairs). Takes effect only if there
#' was more than 1 camera per station and cameraID = "filename".
#' @param minDeltaTime numeric. time difference between observation of the same
#' individual at the same station/camera to be considered independent (in
#' minutes)
#' @param deltaTimeComparedTo character. For two records to be considered
#' independent, must the second one be at least \code{minDeltaTime} minutes
#' after the last independent record of the same individual
#' (\code{"lastIndependentRecord"}), or \code{minDeltaTime} minutes after the
#' last record (\code{"lastRecord"})?
#' @param timeZone character. Must be a value returned by
#' \code{\link[base:timezones]{OlsonNames}}
#' @param stationCol character. Name of the camera trap station column in the
#' output table.
#' @param writecsv logical. Should the individual record table be saved as a
#' .csv file?
#' @param outDir character. Directory to save csv file to. If NULL and
#' \code{writecsv = TRUE}, the output csv will be written to \code{inDir}.
#' @param metadataHierarchyDelimitor character. The character delimiting
#' hierarchy levels in image metadata tags in field "HierarchicalSubject".
#' Either "|" or ":".
#' @param metadataIDTag character. In custom image metadata, the individual ID
#' tag name.
#' @param additionalMetadataTags character. additional camera model-specific
#' metadata tags to be extracted. (If possible specify tag groups as returned
#' by \code{\link{exifTagNames}})
#' @param removeDuplicateRecords logical. If there are several records of the
#' same individual at the same station (also same camera if cameraID is
#' defined) at exactly the same time, show only one?
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
#' @return A data frame containing species records with individual IDs and
#' additional information about stations, date, time and (optionally) further
#' metadata.
#' 
#' @section Warning : Be sure to read the section on individual identification
#' in the package vignette
#' (\url{https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr2.pdf}).
#' 
#' Af you use image metadata tags for identification, the tags must be written
#' to the image metadata. The function cannot read tags from .xmp sidecar
#' files. Make sure you set the preferences of your image management software
#' accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There,
#' make sure "Write to sidecar files" is unchecked.
#' 
#' Please note the section about defining argument \code{timeZone} in the
#' vignette on data extraction (accessible via
#' \code{vignette("DataExtraction")} or online
#' (\url{https://cran.r-project.org/package=camtrapR/vignettes/camtrapr3.pdf})).
#' 
#' @author Juergen Niedballa
#' 
#' @references Phil Harvey's ExifTool \url{https://exiftool.org/}
#' 
#' @examples
#' 
#' 
#' \dontrun{   # the examples run too long to pass CRAN tests
#' 
#'  wd_images_ID_individual <- system.file("pictures/sample_images_indiv_tag/LeopardCat", 
#'                                         package = "camtrapR")
#'  # missing space in species = "LeopardCat" is because of CRAN package policies
#'  # note argument additionalMetadataTags: contains tag names as returned by function exifTagNames
#' 
#'  if (Sys.which("exiftool") != ""){        # only run these examples if ExifTool is available
#' 
#'  rec_table_pbe <- recordTableIndividual(inDir                  = wd_images_ID_individual,
#'                                         minDeltaTime           = 60,
#'                                         deltaTimeComparedTo    = "lastRecord",
#'                                         hasStationFolders      = FALSE,
#'                                         IDfrom                 = "metadata",
#'                                         camerasIndependent     = FALSE,
#'                                         writecsv               = FALSE,
#'                                         metadataIDTag          = "individual",
#'                                         additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
#'                                         timeZone               = "Asia/Kuala_Lumpur"
#'  )
#'  
#'  
#'  # extracting some example summary stats too 
#'  # a nonsensical example, get all unique cameras with which the event was photographed
#'  
#'   rec_table_pbe2 <- recordTableIndividual(inDir                = wd_images_ID_individual,
#'                                         minDeltaTime           = 60,
#'                                         deltaTimeComparedTo    = "lastRecord",
#'                                         hasStationFolders      = FALSE,
#'                                         IDfrom                 = "metadata",
#'                                         camerasIndependent     = FALSE,
#'                                         writecsv               = FALSE,
#'                                         metadataIDTag          = "individual",
#'                                         additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
#'                                         timeZone               = "Asia/Kuala_Lumpur",
#'                                         eventSummaryColumn     = "EXIF:Make",
#'                                         eventSummaryFunction   = "unique"
#'  )
#'  
#'  ### Video example (the sample data don't contain a video, this is hypothetical)
#'  # with JPG, video mp4, avi, mov, ID = metadata
#'  
#' rec_table_ind_video <- recordTableIndividual(inDir = wd_images_ID_individual,
#'                           hasStationFolder  = FALSE,
#'                           IDfrom            = "metadata", 
#'                           metadataIDTag     = "individual",
#'                           video = list(file_formats = c("jpg", "mp4", "avi", "mov"),
#'                                        dateTimeTag = "QuickTime:CreateDate",
#'                                        db_directory = "C:/Users/YourName/Pictures",
#'                                        db_filename = "digikam4.db")
#' )
#' 
#'   
#' } else {  
#' # show function output if ExifTool is not available
#' message("ExifTool is not available. Cannot test function. Loading recordTableSample instead")
#' data(recordTableSample)
#' }
#' }
#' 
#' 
#' @export recordTableIndividual
#' 
recordTableIndividual <- function(inDir,
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
{
  wd0 <- getwd()
  on.exit(setwd(wd0))

  if(!hasArg(stationCol)) stationCol <- "Station"
  if(!is.character(stationCol))     stop("stationCol must be of class 'character'", call. = FALSE)
  individualCol <- "Individual"
  speciesCol    <- "Species"

  checkForSpacesInColumnNames(stationCol = stationCol)

  # check input
  if(hasArg(timeZone) == FALSE) {
    message("timeZone is not specified. Assuming UTC")
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()", call. = FALSE)
  }
  
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(!is.logical(hasStationFolders))    stop("hasStationFolders must be of class 'logical'", call. = FALSE)

  IDfrom <- match.arg(IDfrom, choices = c("metadata", "directory") )
            
 if(IDfrom == "metadata"){
   metadataHierarchyDelimitor <- match.arg(metadataHierarchyDelimitor, choices = c("|", ":"))
    if(!hasArg(metadataIDTag))       stop('"metadataIDTag" must be defined if IDfrom = "metadata"')
    if(!is.character(metadataIDTag)) stop("metadataIDTag must be of class 'character'")
    if(length(metadataIDTag) != 1)   stop("metadataIDTag must be of length 1")
  }

  multiple_tag_separator <- "_&_"

  if(hasArg(cameraID)){
    if(!is.character(cameraID))              stop("cameraID must be of class 'character'", call. = FALSE)
    if(cameraID %in% c("filename") == FALSE) stop("cameraID can only be 'filename' or missing", call. = FALSE)
    if(!hasArg(camerasIndependent))          stop("camerasIndependent is not defined. It must be defined if cameraID is defined", call. = FALSE)
    if(!is.logical(camerasIndependent))      stop("camerasIndependent must be of class 'logical'", call. = FALSE)
  } else {
    camerasIndependent <- FALSE
    }

  cameraCol <- "Camera"


  if(hasArg(outDir)){
    if(!is.character(outDir))        stop("outDir must be of class 'character'", call. = FALSE)
    if(isFALSE(file.exists(outDir))) stop("outDir does not exist", call. = FALSE)
  }

  if(hasArg(additionalMetadataTags)){
    if(!is.character(additionalMetadataTags)) stop("additionalMetadataTags must be of class 'character'", call. = FALSE)
    if(any(grep(pattern = " ", x = additionalMetadataTags, fixed = TRUE))) stop("In argument additionalMetadataTags, spaces are not allowed")
    if("HierarchicalSubject" %in% additionalMetadataTags & IDfrom == "metadata")  {
      warning("'HierarchicalSubject' may not be in 'additionalMetadataTags' if IDfrom = 'metadata'. It will be ignored because the function returns it anyway.", call. = FALSE)
      additionalMetadataTags <- additionalMetadataTags[-grep(pattern = "HierarchicalSubject", x = additionalMetadataTags)]  # remove it
    }
  }

  if(!is.logical(removeDuplicateRecords))     stop("'removeDuplicateRecords' must be logical (TRUE / FALSE)", call. = FALSE)
  if(!is.logical(returnFileNamesMissingTags)) stop("'returnFileNamesMissingTags' must be logical (TRUE / FALSE)", call. = FALSE)

  metadata.tagname <- "HierarchicalSubject"    # for extracting metadata assigned in tagging software

  minDeltaTime <- as.integer(minDeltaTime)
  if(!is.integer(minDeltaTime)) stop("'minDeltaTime' must be an integer", call. = FALSE)

  if(minDeltaTime != 0){
    if(removeDuplicateRecords == FALSE){
      warning("minDeltaTime is > 0. Therefore, removeDuplicateRecords was set to TRUE (otherwise there may be records taken at the same time)", call. = FALSE, immediate. = TRUE)
      removeDuplicateRecords <- TRUE
    }
    deltaTimeComparedTo < match.arg(deltaTimeComparedTo, choices = c("lastRecord", "lastIndependentRecord"))
    if(!hasArg(deltaTimeComparedTo)) stop(paste("minDeltaTime is not 0. deltaTimeComparedTo must be defined"), call. = FALSE)
  } else {
    if(hasArg(deltaTimeComparedTo)) {
      warning(paste("minDeltaTime is 0. deltaTimeComparedTo = '", deltaTimeComparedTo, "' will have no effect", sep = ""), call. = FALSE, immediate. = TRUE)
    } else {
      deltaTimeComparedTo <- "lastRecord"
    }
  }

  if(!is.logical(writecsv)) stop("writecsv must be logical")

  if(!is.character(inDir))  stop("inDir must be of class 'character'", call. = FALSE)
  if(length(inDir) != 1)    stop("inDir may only consist of 1 element only", call. = FALSE)
  if(!dir.exists(inDir))    stop("Could not find inDir:\n", inDir, call. = FALSE)


  # find image directories

  if(hasStationFolders == TRUE){
    dirs       <- list.dirs(inDir, full.names = TRUE,  recursive = FALSE)
    dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
    if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)
  } else {
    dirs <- dirs_short <- inDir
  }

  max_nchar_station <- max(nchar(dirs_short))

  # Ignore the ".dtrash" folder if present (on Mac)
  if(".dtrash" %in% dirs_short) {
    message("Ignoring .dtrash folder.")
    dirs <- dirs[dirs_short != ".dtrash"]
    dirs_short <- dirs_short[dirs_short != ".dtrash"]
  }
  
  # Ignore  the ".mysql.digikam" folder if present
  if(".mysql.digikam" %in% dirs_short) {
    message("Ignoring .mysql.digikam folder.")
    dirs <- dirs[dirs_short != ".mysql.digikam"]
    dirs_short <- dirs_short[dirs_short != ".mysql.digikam"]
  }
  
  
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
  
  record.table.list <- list()

    # # create command line and execute exiftool
    #   if(hasArg(additionalMetadataTags)){
    #     command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject', paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ' -ext JPG "', dirs, '"', sep = "")
    #     colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject", additionalMetadataTags)
    #   } else {
    #     command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "',dirs, '"', sep = "")
    #     colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject")
    #   }
  
  command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal', 
                        ifelse(hasArg(video), paste(" -", video$dateTimeTag, sep = ""), ""),    # if video requested, video date time tag
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


    if(is.null(class(metadata.tmp))){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = paste(".", file_formats, "$", collapse = "|", sep = ""), 
                                      ignore.case = TRUE, recursive = TRUE))
      message(paste(formatC(dirs_short[i], width = max_nchar_station, flag = "-"),  ":  ",
                    formatC(length.tmp, width = 5), " images      Skipping", sep = ""))
      warning(paste(dirs_short[i],  ":  contains no images and was omitted\n"), call. = FALSE,  immediate. = FALSE)

    } else {

      # if video files extracted, add DateTimeOriginal and HierarchicalSubject
      if(hasArg(video)){
        metadata.tmp <- addVideoDateTimeOriginal(metadata.tmp = metadata.tmp, video = video)
        
        # add HierachicalSubject for video files
        if(!is.null(digiKam_data)){
          digiKamVideoMetadata <- digiKamVideoHierarchicalSubject(stationDir = dirs[i],
                                                                    digiKamTablesList = digiKam_data,    # output of accessDigiKamDatabase
                                                                    videoFormat = file_formats[!grepl(file_formats, pattern = "jpg")])
          # add HierarchialSubject for video files (match by filename, must be unique)
          metadata.tmp <- addVideoHierarchicalSubject (metadata.tmp = metadata.tmp,
                                                       video = video,
                                                       digiKamVideoMetadata = digiKamVideoMetadata)
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


      # add individual ID to metadata table (from folders or metadata, otherwise NA) - function is called assignSpeciesID but works on individual here

      metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,            # also works for individual IDs assuming that there is 1 species only
                                       IDfrom                 = IDfrom,
                                       metadataSpeciesTag     = metadataIDTag,
                                       speciesCol             = individualCol,           # individuals are treated as species here (so multiple individuals tagged in the same picture will be returned)
                                       dirs_short             = dirs_short,
                                       i_tmp                  = i,
                                       multiple_tag_separator = multiple_tag_separator,
                                       returnFileNamesMissingTags = returnFileNamesMissingTags,
                                       parent                 = "recordTableIndividual"
      )


      # if images in station contain no metadata individual ID tag or are not tagged, skip that station
      if(!is.data.frame(metadata.tmp)){
        if(metadata.tmp == "found no species tag") {
          # warning(paste(dirs_short[i], ":   metadataIDTag '", metadataIDTag, "' not found in image metadata tag 'HierarchicalSubject'. Skipping", sep = ""), call. = FALSE, immediate. = TRUE)
        } else {
          warning(paste(dirs_short[i], ":   error in individual tag extraction. Skipping. Please report", sep = ""), call. = FALSE, immediate. = TRUE)
        }
        next
      }


      # remove empty metadata columns (if HierarchicalSubject is all empty or if additionalMetadataTags were not found)
      empty_cols <- which(apply(metadata.tmp, MARGIN = 2, FUN = function(X){all(X == "-")}))
      if(length(empty_cols) >= 1){
        metadata.tmp <-  metadata.tmp[, -empty_cols]
      }

      # add station and camera id to metadata table
      arg.list0 <- list(intable = metadata.tmp, dirs_short = dirs_short, stationCol = stationCol, hasStationFolders = hasStationFolders, cameraCol = cameraCol, i = i, IDfrom = IDfrom)

      if(!hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, arg.list0)
      if( hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, c(arg.list0, cameraID = cameraID))   # if cameraID is defined, it will be extracted from file names

      # add species name to table (from last part of inDir)
      inDir.split <- unlist(strsplit(inDir, split = .Platform$file.sep, fixed = TRUE))
      metadata.tmp[,speciesCol] <- inDir.split[length(inDir.split)]

      if(nrow(metadata.tmp) >= 1){   # if anything left, do

        # convert character vector extracted from images to time object and format for outfilename
        metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

        # sort by station, (camera), individual and time
        if(camerasIndependent == TRUE) {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,individualCol], metadata.tmp[,cameraCol], metadata.tmp$DateTimeOriginal),]
        } else {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,individualCol], metadata.tmp$DateTimeOriginal),]
        }

        #remove duplicate records of same individual taken in same second at the same station (by the same camera, if relevant)
        metadata.tmp2 <- removeDuplicatesOfRecords(metadata.tmp           = metadata.tmp,
                                                   removeDuplicateRecords = removeDuplicateRecords,
                                                   camerasIndependent     = camerasIndependent,
                                                   stationCol             = stationCol,
                                                   speciesCol             = individualCol,    # meaning, there will be no duplicate records of the same individual at the same second and station
                                                   cameraCol              = cameraCol,
                                                   current                = i, 
                                                   total                  = length(dirs),
                                                   max_nchar_station      = max_nchar_station)
        
        # assess independence between records and calculate time differences (and possibly summarise a column)
        args.assessTemporalIndependence <- list(intable             = metadata.tmp2,
                                                deltaTimeComparedTo = deltaTimeComparedTo,
                                                columnOfInterest    = individualCol,
                                                cameraCol           = cameraCol,
                                                camerasIndependent  = camerasIndependent,
                                                minDeltaTime        = minDeltaTime,
                                                stationCol          = stationCol)
        
        if(hasArg(eventSummaryColumn)) {
          if(!is.character(eventSummaryColumn))     stop("eventSummaryColumn must be of class 'character'", call. = FALSE)
          if(!is.character(eventSummaryFunction))   stop("eventSummaryFunction must be of class 'character'", call. = FALSE)
          args.assessTemporalIndependence <- c(args.assessTemporalIndependence,
                                               eventSummaryColumn   = eventSummaryColumn,
                                               eventSummaryFunction = eventSummaryFunction)
        }
        
        d1 <- do.call(assessTemporalIndependence, args = args.assessTemporalIndependence)
        
        # save station table to list
        record.table.list[[i]] <- d1
        
       # # add potential new columns to global record.table
       #  d2 <- addNewColumnsToGlobalTable (intable      = d1,
       #                                    i            = i,
       #                                    record.table = record.table)
       # 
       # 
       # 
       #  # append table of station i's images metadata to global record table
       #  record.table <- rbind(d2[[2]], d2[[1]])

        suppressWarnings(rm(d1))
      }
    }
  }       # end loop through station directories
  
  
  # combine all data frames from list into one data frame
  record.table <- as.data.frame(data.table::rbindlist(record.table.list, fill = TRUE, use.names = TRUE))

  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty"), call. = FALSE)
  }

  # rearrange table, add date and time as separate columns. add additional column names as needed.

  record.table2  <-  data.frame(record.table[,c(stationCol, speciesCol, individualCol, "DateTimeOriginal")],
                                Date = as.Date (record.table$DateTimeOriginal, format = "%Y/%M/%d", tz = timeZone),
                                Time = strftime(record.table$DateTimeOriginal, format = "%H:%M:%S", tz = timeZone),
                                record.table[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
                                                "Directory", "FileName")])

  metadata_columns <- which(colnames(record.table) %in% colnames(record.table2) == FALSE)

  # add metadata columns
  if(length(metadata_columns) >= 1){
    record.table3 <- cbind(record.table2, record.table[,metadata_columns])
    colnames(record.table3)[(ncol(record.table2) + 1) : ncol(record.table3)] <- colnames(record.table)[metadata_columns]
  } else {record.table3 <- record.table2}


  # add camera column (if present
  if(hasArg(cameraID)){
    record.table3 <- data.frame(record.table3[,stationCol],
                                record.table[,cameraCol],
                                record.table3[,-which(colnames(record.table3) %in% c(stationCol, cameraCol))])
    colnames(record.table3)[1] <- stationCol
    colnames(record.table3)[2] <- cameraCol
  }

  record.table3 <- record.table3[with(record.table3, order(record.table3[,stationCol], record.table3[,individualCol], DateTimeOriginal)), ]
  rownames(record.table3) <- NULL

   # warning if additionalMetadataTags were not found
  if(hasArg(additionalMetadataTags)){
    #whichAdditionalMetadataTagsFound <- which(gsub(additionalMetadataTags, pattern = ":", replacement = ".") %in% colnames(record.table3))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    whichAdditionalMetadataTagsFound <- which(additionalMetadataTags %in% colnames(record.table3))
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
  record.table3 <- data.frame(record.table3, stringsAsFactors = FALSE, check.names = TRUE)
  
  # save table
  if(length(unique(record.table3[,speciesCol])) > 1){
    warning("there was more than 1 species in your images. Cannot save the record table")
    return(record.table3)
  }

  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_table_individuals", minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
    } else {
      setwd(outDir)
    }
  write.csv(record.table3, file = outtable_filename)
  }
  return(record.table3)
}
