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


    if(class(metadata.tmp) == "NULL"){            # omit station if no images found

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
          warning(paste(dirs_short[i], ":   metadataIDTag '", metadataIDTag, "' not found in image metadata tag 'HierarchicalSubject'. Skipping", sep = ""), call. = FALSE, immediate. = TRUE)
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