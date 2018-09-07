# Version Check for .onAttach()
# adapted from http://thecoatlessprofessor.com/programming/automatically-check-if-r-package-is-the-latest-version-on-package-load/. Thank you!


.pkgVersionCRAN <- function(pkg, cran_url="http://cran.r-project.org/web/packages/")
{
  # Create URL
  cran_pkg_loc <- paste0(cran_url,pkg)

  # Try to establish a connection
  suppressWarnings( conn <- try( url(cran_pkg_loc) , silent=TRUE ) )

  # If connection, try to parse values, otherwise return NULL
  if ( all( class(conn) != "try-error") ) {
    suppressWarnings( cran_pkg_page <- try( readLines(conn) , silent=TRUE ) )
    close(conn)
  } else {
    return(NULL)
  }

  # Extract version info
  version_line = cran_pkg_page[grep("Version:",cran_pkg_page)+1]
  gsub("<(td|\\/td)>","",version_line)
}


# for all functions in which user specifies column names: error if spaces in column names
checkForSpacesInColumnNames <- function(...){

  z <- list(...)

  # if all arguments are of length 1, do
  if(all(sapply(z, FUN = length) == 1)){
    if(any(grepl(pattern = " ", x = unlist(z), fixed = TRUE))) stop("column names may not contain spaces: \n ",
                                                                     paste(names(z)[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], "=",
                                                                           z[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], collapse = "\n "),
                                                                     call. = FALSE)
  }

  # if the argument is of length >1, do
  if(any(sapply(z, FUN = length) > 1)){
    if(length(z) != 1) stop("this is a bug in 'checkForSpacesInColumnNames'. I'm sorry. Please report it.")
    if(any(grepl(pattern = " ", x = unlist(z[[1]]), fixed = TRUE))) stop("column names in '", names(z) ,"' may not contain spaces: \n ",
                                                                         paste(names(unlist(z))[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], "=",
                                                                               z[[1]][which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], collapse = "\n "),
                                                                         call. = FALSE)
  }
}



# for functions reading out and tabulating image metadata

runExiftool <- function(command.tmp,
                        colnames.tmp)
{
  tmp1 <- strsplit(system(command.tmp, intern=TRUE), split = "\t")

  if(length(tmp1) == 0) return(NULL) # if nothing returned (no images, no metadata)

  metadata.tmp <- as.data.frame(matrix(unlist(lapply(tmp1, FUN = function(X){X[2]})),
                                     ncol = length(colnames.tmp),
                                     byrow = TRUE),
                              stringsAsFactors = FALSE)
  colnames(metadata.tmp) <- colnames.tmp

  # find and remove ._ files created on Macs
  strangeMacFiles <- grep("^[._]", metadata.tmp$FileName, fixed = FALSE)
  if(length(strangeMacFiles) >= 1)  {
    warning(paste("found", length(strangeMacFiles), "JPG files beginning with '._' in", paste(unique(metadata.tmp$Directory[strangeMacFiles]), collapse = ","), ". Will ignore them."), call. = FALSE, immediate. = TRUE)
    metadata.tmp <- metadata.tmp[-strangeMacFiles,]
    }
  return(metadata.tmp)
}


addMetadataAsColumns <- function(intable,
                                 metadata.tagname,
                                 metadataHierarchyDelimitor,
                                 multiple_tag_separator)
{
  intable[,metadata.tagname] <- as.character(intable[,metadata.tagname])
  tmp2 <- strsplit(intable[,metadata.tagname], split = ",")      # split items of "HierarchicalSubject" at comma
  tmp3 <- lapply(tmp2, FUN = function(X){X[grep(pattern = metadataHierarchyDelimitor, x = X, fixed = TRUE)]})   # get only the ones with values


  # find all metadata categories, remove spaces
  list.tmp <- vector()
  for(xy in 1:length(tmp3)){
    list.tmp <- c(list.tmp, gsub(pattern = " ",
                                 replacement = "",
                                 x =  unlist(lapply(strsplit(tmp3[[xy]],
                                                             split = metadataHierarchyDelimitor,
                                                             fixed = TRUE),
                                                    FUN = function(Y){Y = Y[1]}))))
  }
  cols2add <- unique(list.tmp)    # these are the columns to add

  # add as columns
  if(length(cols2add) >= 1){    # if anything to add
    intable <- data.frame(intable, matrix(NA, ncol = length(cols2add), nrow = nrow(intable)))
    colnames(intable)[seq((ncol(intable) - length(cols2add) + 1),ncol(intable))] <- cols2add

    # fill metadata columns
    for(xyz in 1:length(cols2add)){
      intable[,cols2add[xyz]] <- unlist(lapply(lapply(tmp3, FUN = function(X) {sapply(strsplit(X[grep(x = X,
                                                                                                      pattern = paste(cols2add[xyz],
                                                                                                                      metadataHierarchyDelimitor,
                                                                                                                      collapse = "",
                                                                                                                      sep      = ""),
                                                                                                      fixed = TRUE)],
                                                                                               split = metadataHierarchyDelimitor,
                                                                                               fixed = TRUE),
                                                                                      FUN = function(Y){Y[2]})}),
                                               FUN = function(Z){paste(Z, collapse = multiple_tag_separator)}))

      intable[which(intable[,cols2add[xyz]] == ""), cols2add[xyz]] <- NA
    } # end for xyz
  } # end if

  which_cols_to_rename <- which(colnames(intable) %in% cols2add)

  # remove spaces and punctuation in column names
  #colnames(intable) <- gsub(pattern = "[[:blank:]]", replacement = "", x = colnames(intable))
  #colnames(intable) <- gsub(pattern = "[[:punct:]]", replacement = "", x = colnames(intable))

  # rename metadata columns with prefix "metadata_"
  colnames(intable)[which_cols_to_rename] <- paste("metadata_", colnames(intable)[which_cols_to_rename], sep = "")

  return(intable)
}



assignSpeciesID <- function(intable,
                            IDfrom,
                            metadataSpeciesTag,
                            speciesCol,
                            dirs_short,
                            i_tmp,
                            multiple_tag_separator)
{

  file.sep <- .Platform$file.sep


  if(IDfrom == "directory"){
    intable[,speciesCol] <-  sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X)]})
    return(intable)
  } else {
    if(hasArg(metadataSpeciesTag)){
      metadataSpeciesTag2 <- paste("metadata", metadataSpeciesTag, sep = "_")
      if(metadataSpeciesTag2 %in% colnames(intable)){

        intable[,speciesCol] <- intable[,metadataSpeciesTag2]
        nrow.intable <- nrow(intable)
        species_records_to_remove <- which(is.na(intable[,speciesCol]))
        if(length(species_records_to_remove) >= 1){
          intable <- intable[-species_records_to_remove,]      #remove records without species tag
          warning(paste( dirs_short[i_tmp],":  removed", length(species_records_to_remove), "records out of", nrow.intable,
                         "because of missing species metadata tag"), call. = FALSE, immediate. = TRUE)
        }

        intable <- separateMultipleSpecies (intable                = intable,
                                            speciesCol             = speciesCol,
                                            multiple_tag_separator = multiple_tag_separator)

        return(intable)
      } else {
        warning(paste(dirs_short[i_tmp], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata tag 'HierarchicalSubject'.", sep = ""), call. = FALSE, immediate. = TRUE)
        return("found no species tag")
      }
    } else {
      stop(paste("station", dirs_short[i_tmp], ":   cannot figure out species names. Is metadataSpeciesTag defined?"), call. = FALSE)
    }
  }
}


# find and separate multiple species in same image (only if using metadata ID)
separateMultipleSpecies <- function(intable,
                                    speciesCol,
                                    multiple_tag_separator)
{

  records0                 <- intable[,speciesCol]
  records_duplicate        <- strsplit(intable[,speciesCol], split = multiple_tag_separator, fixed = TRUE)
  records_duplicate_length <- sapply(records_duplicate, length)

  if(any(records_duplicate_length > 1)){
    intable <- intable[rep(row.names(intable), records_duplicate_length), ]                # replicate rows with >1 species
    intable[,speciesCol] <- unlist(strsplit (records0, split = multiple_tag_separator))    # assign species anew
  }
  return(intable)
}




# add station and camera id to metadata table

addStationCameraID <- function(intable,
                               dirs_short,
                               stationCol,
                               cameraCol,
                               cameraID,
                               hasStationFolders,
                               i,
                               IDfrom)
{

  file.sep <- .Platform$file.sep

  # append station ID

  if(isTRUE(hasStationFolders)) {       # take station ID from station directories
    intable <- cbind(intable, dirs_short[i])
    colnames(intable)[ncol(intable)] <- stationCol

  } else  {                             # take station ID from image filenames

    station.tmp  <- try(sapply(strsplit(as.character(intable$FileName), split = "__"), FUN = function(X){X[1]}))      # assumes filenames: STATION__Camera__Date/Time(Number).JPG)
    if(length(station.tmp) == nrow(intable)){
      intable <- cbind(intable, station.tmp)
      colnames(intable)[ncol(intable)] <- stationCol
    } else {
      stop(paste(dirs_short[i], ": numbers of images and station ID extracted from image names do not match. Do image filenames begin with station IDs?"))
    }
  }

  # append camera ID

  if(hasArg(cameraID)){
    if(cameraID == "filename"){
      camera.tmp  <- try(sapply(strsplit(as.character(intable$FileName), split = "__"), FUN = function(X){X[2]}))      # assumes filenames: Station__CAMERA__Date/Time(Number).JPG)
      if(length(camera.tmp) == nrow(intable)){
        intable <- cbind(intable, camera.tmp)
        colnames(intable)[ncol(intable)]     <- cameraCol
      }
    }
    if(cameraID == "directory"){            # this can only happen in recordTable. Not in recordTableIndividual
      if(IDfrom == "directory"){             # assumes directory structure: Station/Camera/Species
        intable <- cbind(intable,
                         sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X) - 1]}))  
        } else {                                    # assumes directory structure: Station/Camera
        intable <- cbind(intable,
                         sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X)]}))  
        }
      colnames(intable)[ncol(intable)]     <- cameraCol
    }
  }
  return(intable)
}

# check if date/time information is present and was readable

checkDateTimeOriginal <- function (intable, dirs_short, i){
     # if all date/time information is missing, go to next station
    if(all(intable$DateTimeOriginal == "-")){
      warning(paste(dirs_short[i], ": no readable date/time information. Skipping"), call. = FALSE,  immediate. = TRUE)
      intable <- NULL
    } else {
    
    # if date/time information is missing for some records only
     if(any(intable$DateTimeOriginal == "-")){
      which_no_time <- which(intable$DateTimeOriginal == "-")
      warning(paste(dirs_short[i], ": omitting", length(which_no_time), "images because of missing/unreadable date/time information."), call. = FALSE,  immediate. = TRUE)
     intable <-  intable[-which_no_time,]    # removing rows with missing date/time information
    }
    }
    return(intable)
    }
  # remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
  # Note to self: this may also be done outside the station loop, after the final record table is assembled. Saves a few executions of this function.

  removeDuplicatesOfRecords <- function(metadata.tmp, removeDuplicateRecords, camerasIndependent, stationCol, speciesCol, cameraCol){
          if(isTRUE(removeDuplicateRecords)){
            if(isTRUE(camerasIndependent)){
              remove.tmp <- which(duplicated(metadata.tmp[,c("DateTimeOriginal", stationCol, speciesCol, cameraCol)]))
              if(length(remove.tmp >= 1)){
                metadata.tmp <- metadata.tmp[-remove.tmp,]
                message(paste(unique(metadata.tmp[,stationCol]), collapse = ", "), ": removed ", length(remove.tmp), " duplicate records")
              }
            } else {
              remove.tmp <- which(duplicated(metadata.tmp[,c("DateTimeOriginal", stationCol, speciesCol)]))
              if(length(remove.tmp >= 1)) {
                metadata.tmp <- metadata.tmp[-remove.tmp,]
                message(paste(unique(metadata.tmp[,stationCol]), collapse = ", "), ": removed ", length(remove.tmp), " duplicate records")
              }
            }
          }
          return(metadata.tmp)
        }


#### assess temporal independence between records

assessTemporalIndependence <- function(intable,
                                       deltaTimeComparedTo,
                                       columnOfInterest,     # species/individual column
                                       cameraCol,
                                       camerasIndependent,
                                       stationCol,
                                       minDeltaTime)
{
# check if all Exif DateTimeOriginal tags were read correctly
  if(any(is.na(intable$DateTimeOriginal))){
    which.tmp <- which(is.na(intable$DateTimeOriginal))
    if(length(which.tmp) == nrow(intable)) stop("Could not read any Exif DateTimeOriginal tag at station: ", paste(unique(intable[which.tmp, stationCol])), " Consider checking for corrupted Exif metadata.")
    warning(paste("Could not read Exif DateTimeOriginal tag of", length(which.tmp),"image(s) at station", paste(unique(intable[which.tmp, stationCol]), collapse = ", "), ". Will omit them. Consider checking for corrupted Exif metadata. \n",
      paste(file.path(intable[which.tmp, "Directory"],
                      intable[which.tmp, "FileName"]), collapse = "\n")), call. = FALSE, immediate. = TRUE)
    intable <- intable[-which.tmp ,]
    rm(which.tmp)
  }

   # prepare to add time difference between observations columns
        intable <- data.frame(intable,
                              delta.time.secs  = NA,
                              delta.time.mins  = NA,
                              delta.time.hours = NA,
                              delta.time.days  = NA)

   # introduce column specifying independence of records
        if(minDeltaTime == 0) {
          intable$independent <- TRUE    # all independent if no temporal filtering
        } else {
          intable$independent <- NA
        }


  for(xy in 1:nrow(intable)){     # for every record

    # set independent = TRUE if it is the 1st/only  record of a species / individual

    if(camerasIndependent == TRUE){
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                                                             intable[, stationCol]       == intable[xy, stationCol] &
                                                                             intable[, cameraCol]        == intable[xy, cameraCol]) ])){    # cameras at same station assessed independently
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    } else {
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which(intable[, columnOfInterest] == intable[xy, columnOfInterest] &
                                                                             intable[, stationCol]       == intable[xy, stationCol]) ])){
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    }

    if(is.na(intable$delta.time.secs[xy])) {   # if not the 1st/only record, calculate time difference to previous records of same species at this station

      if(deltaTimeComparedTo == "lastIndependentRecord"){

        if(camerasIndependent == TRUE){
          which_time2 <- which(intable[, columnOfInterest]       == intable[xy, columnOfInterest] &    # same species/individual
                              intable[, stationCol]              == intable[xy, stationCol] &          # at same station
                              intable[, cameraCol]               == intable[xy, cameraCol] &           # at same camera
                              intable$independent                == TRUE &                             # independent (first or only record of a species at a station)
                              intable$DateTimeOriginal           <  intable$DateTimeOriginal[xy])      # earlier than record xy
        } else {
          which_time2 <- which(intable[, columnOfInterest]       == intable[xy, columnOfInterest] &
                               intable[, stationCol]             == intable[xy, stationCol] &
                               intable$independent               == TRUE &
                               intable$DateTimeOriginal          <  intable$DateTimeOriginal[xy])
        }
      }  else {
        if(camerasIndependent  == TRUE){
          which_time2 <- which(intable[, columnOfInterest]       == intable[xy, columnOfInterest] &
                               intable[, stationCol]             == intable[xy, stationCol] &
                               intable[, cameraCol]              == intable[xy, cameraCol] &
                               intable$DateTimeOriginal          <  intable$DateTimeOriginal[xy])
        } else {
          which_time2 <- which(intable[, columnOfInterest]       == intable[xy, columnOfInterest] &
                               intable[, stationCol]             == intable[xy, stationCol] &
                               intable$DateTimeOriginal          <  intable$DateTimeOriginal[xy])
        }
      }

      #intable$DateTimeOriginal[which_time2] + (minDeltaTime * 60) < intable$DateTimeOriginal[xy]

      # time difference to last (independent) record
      diff_tmp <- min(na.omit(difftime(time1 = intable$DateTimeOriginal[xy],            # delta time to last independent record
                                       time2 = intable$DateTimeOriginal[which_time2],
                                       units = "secs")))

      # save delta time in seconds
      intable$delta.time.secs[xy] <-  diff_tmp
      if(intable$delta.time.secs[xy] >= (minDeltaTime * 60) | intable$delta.time.secs[xy] == 0){
          intable$independent[xy] <- TRUE
        } else {
          intable$independent[xy] <- FALSE
        }

    }   # end   if(intable$DateTimeOriginal[xy] == min(...)} else {...}
  }     # end for(xy in 1:nrow(intable))


  # keep only independent records
  outtable <- intable[intable$delta.time.secs >= (minDeltaTime * 60) |
                      intable$delta.time.secs == 0,]

  return(outtable)
}


# add potential new columns to global record.table

addNewColumnsToGlobalTable <- function(intable,
                                       i,
                                       record.table)
{

  if( nrow(record.table) >= 1){
    which_cols_to_add_to_d1 <- seq(1, ncol(record.table))[-which(colnames(record.table) %in% colnames(intable))]   # columns in record.table but not in intable

    # if intable lacks columns present in record.table, add them here (filled with NA)
    if(length(which_cols_to_add_to_d1) >= 1){
      intable <- data.frame(intable, as.list(rep(NA, each = length(which_cols_to_add_to_d1))))
      colnames(intable)[(ncol(intable) - length(which_cols_to_add_to_d1) + 1) :  ncol(intable)] <- colnames(record.table)[which_cols_to_add_to_d1]
    }

    # now check which columns are present in intable but not in record.table (new tag groups) and add these (filled with NA)
    which_cols_to_add_to_record.table <- seq(1, ncol(intable))[-which(colnames(intable) %in% colnames(record.table))]  # columns present in intable but not in record.table
    if(length(which_cols_to_add_to_record.table) >= 1){
      record.table <- data.frame(record.table, as.list(rep(NA, each = length(which_cols_to_add_to_record.table))))
      colnames(record.table)[(ncol(record.table) - length(which_cols_to_add_to_record.table) + 1) :  ncol(record.table)] <- colnames(intable)[which_cols_to_add_to_record.table]
    }
    outtable <- intable[,match(colnames(record.table), colnames(intable))]
  } else {
    outtable <- intable
  }
  return(list(outtable, record.table))
}



#####################################################
# for detectionHistory functions

checkCamOpColumnNames <- function(cameraOperationMatrix){
camopTest <- try(as.Date(colnames(cameraOperationMatrix)), silent = TRUE)
if(class(camopTest) == "try-error") stop(paste('could not interpret column names in camOp as Dates. Desired format is YYYY-MM-DD, e.g. "2016-12-31". First column name in your camera operation matrix is "', colnames(cameraOperationMatrix)[1], '"', sep = '' ), call. = FALSE)
}


createDateRangeTable <- function(cam.op,
                                 subset_species_tmp,
                                 buffer_tmp,
                                 stationCol_tmp,
                                 day1_tmp,
                                 occasionStartTime_tmp,
                                 maxNumberDays_tmp,
                                 timeZone_tmp)
                                 {

  cam.tmp.min <- apply(cam.op, MARGIN = 1, function(X){min(which(!is.na(X)))})    # 1st day of each station
  cam.tmp.max <- apply(cam.op, MARGIN = 1, function(X){max(which(!is.na(X)))})    # last day of each station

  rec.tmp.min  <- aggregate(as.Date(subset_species_tmp$DateTime2, tz = timeZone_tmp),
                            list(subset_species_tmp[,stationCol_tmp]),
                            FUN = min)
  rec.tmp.max  <- aggregate(as.Date(subset_species_tmp$DateTime2, tz = timeZone_tmp),
                            list(subset_species_tmp[,stationCol_tmp]),
                            FUN = max)



  date_ranges <- data.frame(rec.min = rec.tmp.min[match(rownames(cam.op), rec.tmp.min[,1]), 2],       # first record
                            rec.max = rec.tmp.max[match(rownames(cam.op), rec.tmp.max[,1]), 2],       # last record
                            cam.min = as.POSIXct(colnames(cam.op)[cam.tmp.min], tz = timeZone_tmp),   # station setup date
                            cam.max = as.POSIXct(colnames(cam.op)[cam.tmp.max], tz = timeZone_tmp)    # station retrieval date
  )

  rownames(date_ranges) <- rownames(cam.op)

    # check if images were taken between setup and retrieval dates (Error if images outside station date range)
  if(any(date_ranges$rec.min < as.Date(date_ranges$cam.min, tz = timeZone_tmp), na.rm = TRUE)) stop(paste("record date outside camera operation date range: ",
                                                                                                                          paste(rownames(date_ranges)[which(date_ranges$rec.min < as.Date(date_ranges$cam.min, tz = timeZone_tmp))], collapse = ", " )), call. = FALSE)
  if(any(date_ranges$rec.max > as.Date(date_ranges$cam.max, tz = timeZone_tmp), na.rm = TRUE)) stop(paste("record date outside camera operation date range: ",
                                                                                                                          paste(rownames(date_ranges)[which(date_ranges$rec.max > as.Date(date_ranges$cam.max, tz = timeZone_tmp))], collapse = ", " )), call. = FALSE)

  # define when first occasion begins (to afterwards remove prior records in function    cleanSubsetSpecies)
  if(!hasArg(buffer_tmp)) buffer_tmp <- 0

  #if(day1_tmp == "station") {
    date_ranges$start_first_occasion <- date_ranges$cam.min + buffer_tmp * 86400 + occasionStartTime_tmp * 3600     #each stations setup  + buffer + starttime
  #  } else {
  #    if(day1_tmp == "survey") {
    date_ranges$start_first_occasion_survey <- min(date_ranges$cam.min) + buffer_tmp * 86400 + occasionStartTime_tmp * 3600    # first station's setup  + buffer + starttime
  #      } else {

        if(day1_tmp %in% c("survey", "station") == FALSE) {
          if(as.Date(day1_tmp, tz = timeZone_tmp) < min(as.Date(date_ranges$cam.min,  tz = timeZone_tmp))) stop(paste("day1 (", day1_tmp, ") is before the first station's setup date (",  min(as.Date(date_ranges$cam.min,  tz = timeZone_tmp)), ")", sep = ""))
          if(as.Date(day1_tmp, tz = timeZone_tmp) > max(as.Date(date_ranges$cam.max,  tz = timeZone_tmp))) stop(paste("day1 (", day1_tmp, ") is after the last station's retrieval date (",  max(as.Date(date_ranges$cam.max,  tz = timeZone_tmp)), ")", sep = ""))
          date_ranges$start_first_occasion <- as.POSIXlt(day1_tmp, tz = timeZone_tmp) + occasionStartTime_tmp * 3600
        }



    # define when last occasion ends
    date_ranges$end_of_retrieval_day <- as.POSIXct(paste(date_ranges$cam.max, "23:59:59"), tz = timeZone_tmp, format = "%Y-%m-%d %H:%M:%S")    # end of retrieval day

 # if maxNumberDays is defined, find which is earlier: start + maxNumberDays or station retrieval?
  if(hasArg(maxNumberDays_tmp)) {

    if(day1_tmp %in% c("survey", "station") == FALSE){
      # count maximum number days from the beginning of each station's 1st occasion
      date_ranges$start_first_occasion_plus_maxNumberDays <- date_ranges$start_first_occasion_survey + (maxNumberDays_tmp * 86400) - 1   # -1 second ensures that last occasion does not spill into next day if occasionStartTime = 0
    } else {
    # count maximum number days from the beginning of survey's 1st occasion
      date_ranges$start_first_occasion_plus_maxNumberDays <- date_ranges$start_first_occasion + (maxNumberDays_tmp * 86400) - 1   # -1 second ensures that last occasion does not spill into next day if occasionStartTime = 0
    }

   for(xy in 1:nrow(date_ranges)){
      date_ranges$end_last_occasion[xy] <- min(date_ranges$end_of_retrieval_day[xy], date_ranges$start_first_occasion_plus_maxNumberDays[xy])   # use smaller value
      }
      attributes(date_ranges$end_last_occasion) <- attributes(date_ranges$start_first_occasion)   # assign the attributes: POSIX + time zone (to convert from numeric value back to date/time)
    } else {
      date_ranges$end_last_occasion <- date_ranges$end_of_retrieval_day
    }


  return(date_ranges)

  }





adjustCameraOperationMatrix <- function(cam.op,
                                        date_ranges2,
                                        timeZone_tmp,
                                        day1_2
                                        ){



if(any(date_ranges2$start_first_occasion > date_ranges2$end_last_occasion)){
  remove.these.stations <- which(date_ranges2$start_first_occasion > date_ranges2$end_last_occasion)
  if(length(remove.these.stations) == nrow(date_ranges2)) stop("In all stations, the occasions begin after retrieval. Choose a smaller buffer argument.")
    cam.op [remove.these.stations, ] <- NA
}


##################################
# set values before beginning of first occasion NA in camera operation matrix (so effort is 0 before): only relevant if buffer was used
 first_col_to_keep2  <- match(as.character(as.Date(date_ranges2$start_first_occasion, tz = timeZone_tmp)), colnames(cam.op))


   for(xxx in 1:length(first_col_to_keep2)){
     if(first_col_to_keep2[xxx] > 1){         # if it does not begin on 1st day of camera operation matrix
        cam.op[xxx, seq(1, first_col_to_keep2[xxx]-1)] <- NA
     }
  }

# set values after end of last occasion NA in camera operation matrix (so effort is 0 afterwards)
 last_col_to_keep2  <- match(as.character(as.Date(date_ranges2$end_last_occasion, tz = timeZone_tmp)), colnames(cam.op))


   for(yyy in 1:length(last_col_to_keep2)){
     if(last_col_to_keep2[yyy]+1 < ncol(cam.op)){   # if it does not end on last day of camera operation matrix
        cam.op[yyy, seq(last_col_to_keep2[yyy]+1, ncol(cam.op))] <- NA
     }
  }


####################################
# trim camera operation matrix (taking into account buffer, occasionStartTime, maxNumberDays)
# based on data frame   date_ranges   computed by    createDateRangeTable

  if(day1_2 == "station") {    # 1st day of each station OR some specified date

      cam.tmp.min <- apply(cam.op, MARGIN = 1, function(X){min(which(!is.na(X)))})    # first occasion of each station
      cam.tmp.max <- apply(cam.op, MARGIN = 1, function(X){max(which(!is.na(X)))})    # last occasion of each station

      diff.days.tmp <- cam.tmp.max - cam.tmp.min

      cam.op2 <- matrix(NA,
                        nrow = nrow(cam.op),
                        ncol = max(diff.days.tmp)+1)

      # make all stations begin in 1st column
      for(l in 1:nrow(cam.op)){
        if(is.finite(diff.days.tmp[l])){
          cam.op2[l, 1:(diff.days.tmp[l]+1)] <- as.vector(cam.op[l,cam.tmp.min[l]:cam.tmp.max[l]])
        }
      }

      if(day1_2 == "station") {
        colnames(cam.op2) <- paste("day", 1:ncol(cam.op2), sep = "")
      }

      rownames(cam.op2) <- rownames(cam.op)

      cam.op <- cam.op2


  } else {


# remove all columns of cam.op that were before beginning of 1st occasion
 first_col_to_keep <- match(as.character(min(as.Date(date_ranges2$start_first_occasion, tz = timeZone_tmp))), colnames(cam.op))
if(!is.na(first_col_to_keep)){
 if(first_col_to_keep != 1){
  cam.op <- cam.op[,-seq(1, (first_col_to_keep-1))]
 }
}

# remove all columns of cam.op that were after end of last occasion / after retrieval of last camera
 last_col_to_keep  <- match(as.character(max(as.Date(date_ranges2$end_last_occasion, tz = timeZone_tmp))), colnames(cam.op))
 if(!is.na(last_col_to_keep)){
   if(last_col_to_keep != ncol(cam.op)){
    cam.op <- cam.op[,-seq((last_col_to_keep + 1), ncol(cam.op))]
   }
}

}
return(cam.op)
}



cleanSubsetSpecies <- function(subset_species2 ,
                               stationCol2,
                               date_ranges2
                               ){

  nrow_subset_species2 <- nrow(subset_species2)


    # remove records that were taken before beginning of first occasion (because of buffer, occasionStartTime, day1)
  corrected_start_time_by_record <- date_ranges2$start_first_occasion[match(subset_species2[,stationCol2], rownames(date_ranges2))]

  remove.these <- which(subset_species2$DateTime2 < corrected_start_time_by_record)
    if(length(remove.these) >= 1){
      subset_species2 <- subset_species2[-remove.these,]
      warning(paste(length(remove.these), "records out of", nrow_subset_species2, "were removed because they were taken within the buffer period, before day1 (if a date was specified), or before occasionStartTime on the 1st day"), call. = FALSE)
      if(nrow(subset_species2) == 0) stop("No more records left. The detection history would be empty.")
      rm(corrected_start_time_by_record, remove.these)
    }

# remove records that were taken after end of last occasion (because of maxNumberDays)
corrected_end_time_by_record <- date_ranges2$end_last_occasion[match(subset_species2[,stationCol2], rownames(date_ranges2))]

  remove.these2 <- which(subset_species2$DateTime2 > corrected_end_time_by_record)
    if(length(remove.these2) >= 1){
      subset_species2 <- subset_species2[-remove.these2,]
      warning(paste(length(remove.these2), "records out of", nrow_subset_species2, "were removed because they were taken after the end of the last occasion"), call. = FALSE)
      if(nrow(subset_species2) == 0) stop("No more records left. The detection history would be empty.")
      rm(corrected_end_time_by_record, remove.these2)
    }

  return(subset_species2)
}



calculateTrappingEffort <- function(cam.op,
                                    occasionLength2,
                                    scaleEffort2,
                                    includeEffort2,
                                    minActiveDaysPerOccasion2){

  ######################
  # calculate trapping effort by station and occasion

  if(occasionLength2 == 1){
    effort <- cam.op          # if occasionLength2 = 1 day, it is identical
  } else {
    effort <- matrix(NA, nrow = nrow(cam.op), ncol = ceiling(ncol(cam.op) / occasionLength2 ))

    index <- 1
    for(m in 1:ncol(effort)){    # for every occasion in the effort matrix
      # index for columns to aggregate
      if(index + occasionLength2 <= ncol(cam.op)){
        index.tmp <- index : (index + occasionLength2 - 1)
      } else {
        index.tmp <- index : ncol(cam.op)
      }

      # calculate effort as sum of active days per occasion
      effort[, m] <- apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = sum, na.rm = TRUE)
      # if full occasion NA in cam.op, make effort NA
      effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {sum(is.na(X))}) == length(index.tmp), NA, effort[,m])
      # if full occasion = 0 in cam.op, make effort NA
      effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {all(X == 0)}),   NA, effort[,m])
      # if full occasion is not 1 (i.e. all 0 or NA), set effort NA
      effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {all(X != 1)}),   NA, effort[,m])

      # set cells in effort matrix NA (according to input arguments)
      # this is later used to adjust the detection/non-detection matrix
      if(includeEffort2 == FALSE){
        if(hasArg(minActiveDaysPerOccasion2)){   # includeEffort = FALSE and minActiveDays is defined
          # if occasion has less active days than minActiveDays, set NA
          effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {sum(X, na.rm = TRUE) < minActiveDaysPerOccasion2}), NA, effort[,m])
        } else {                                 # includeEffort = FALSE and minActiveDays not defined
          # if any day NA in cam.op, make  occasion effort NA
          effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {any(is.na(X))}), NA, effort[,m])
          # if any day = 0 in cam.op, make occasion effort NA
          effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {any(X == 0)}),   NA, effort[,m])

          if(length(index.tmp) < occasionLength2){  # if occasion is shorter than occasionLength (i.e. last occasion), set NA.
            effort[, m] <- NA
          }

        }
      } else {
        if(hasArg(minActiveDaysPerOccasion2)){   # includeEffort = TRUE and minActiveDays is defined
          # if occasion has less actice days than minActiveDays, set NA
          effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {sum(X, na.rm = TRUE) < minActiveDaysPerOccasion2}), NA, effort[,m])
        } else {                                 # includeEffort = TRUE and minActiveDays not defined
          # if all days of occasion NA in cam.op, make  occasion effort NA
          #effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {all(is.na(X))}), NA, effort[,m])
          # if all days of occasion = 0 in cam.op, make occasion effort NA

        }
      }
      index <- index + occasionLength2
    }
    rm(index, index.tmp)
  }

# scale effort, if required
  if(isTRUE(scaleEffort2)){
    if(occasionLength2 == 1) stop("cannot scale effort if occasionLength is 1", call. = FALSE)
    if(length(table(effort)) == 1) stop(paste("all values of effort are identical (", names(table(effort)), "). Cannot scale effort", sep = ""), call. = FALSE)
    
    scale.eff.tmp <- scale(as.vector(effort))                       # scale effort (as a vector, not matrix)
    scale.eff.tmp.attr <- data.frame(effort.scaled.center = NA,     # prepare empty data frame
                                     effort.scaled.scale = NA)
    scale.eff.tmp.attr$effort.scaled.center[1] <- attr(scale.eff.tmp, which = "scaled:center")   # write scaling parameters to data frame
    scale.eff.tmp.attr$effort.scaled.scale [1] <- attr(scale.eff.tmp, which = "scaled:scale")
    effort <- matrix(scale.eff.tmp, nrow = nrow(effort), ncol = ncol(effort))                    # convert effort vector to matrix again
  }

  rownames(effort) <- rownames(cam.op)

  # return objects
  if(isTRUE(scaleEffort2)){
    return(list(effort, scale.eff.tmp.attr))
  } else {
    return(list(effort))
  }
}



##########################################################################################################
# for function surveyReport

makeSurveyZip <- function(output,
                          recordTable,
                          CTtable ,
                          speciesCol,
                          stationCol,
                          setupCol,
                          retrievalCol,
                          CTDateFormat,
                          CTHasProblems,
                          Xcol,
                          Ycol,
                          recordDateTimeCol,
                          recordDateTimeFormat,
                          sinkpath){

  wd0 <- getwd()
  on.exit(setwd(wd0))

  dir.tmp <- tempdir()

  file.sep <- .Platform$file.sep


  dir.zip <- file.path(dir.tmp, paste("surveyReport_", Sys.Date(), sep = ""))
  dir.zip.short <- paste("surveyReport_", Sys.Date(), sep = "")
  unlink(dir.zip, recursive = TRUE)
  dir.create(dir.zip, showWarnings = FALSE, recursive = TRUE)


  # create directories
  invisible(sapply(file.path(dir.zip, c("surveyReport", "activity", "scripts", "detectionMaps")), dir.create, showWarnings = FALSE))


  ######
  # save input tables

  write.csv(recordTable, file = file.path(dir.zip, "recordTable.csv"), row.names = FALSE)
  write.csv(CTtable, file = file.path(dir.zip, "CTtable.csv"), row.names = FALSE)


  ######
  # save surveyReport tables

  dir.tmp2 <- file.path(dir.zip, "surveyReport")

  for(xyz in 1:length(output)){
    write.csv(output[[xyz]],
              file = file.path(dir.tmp2, paste(names(output)[[xyz]], ".csv", sep = "")),
              row.names = FALSE)
  }
  rm(xyz)



  ######
  # make activity plots


  activityDensity(recordTable          = recordTable,
                  allSpecies           = TRUE,
                  speciesCol           = speciesCol,
                  recordDateTimeCol    = recordDateTimeCol,
                  recordDateTimeFormat = recordDateTimeFormat,
                  plotR                = FALSE,
                  writePNG             = TRUE,
                  plotDirectory        = file.path(dir.zip, "activity"))



  ######
  # make detection maps

  if(hasArg(Xcol) & hasArg(Ycol)){
    detectionMaps(CTtable       = CTtable,
                  recordTable   = recordTable,
                  Xcol          = Xcol,
                  Ycol          = Ycol,
                  stationCol    = stationCol,
                  speciesCol    = speciesCol,
                  richnessPlot  = TRUE,
                  speciesPlots  = TRUE,
                  printLabels   = TRUE,
                  plotR         = FALSE,
                  writePNG      = TRUE,
                  plotDirectory = file.path(dir.zip, "detectionMaps"),
                  pngMaxPix     = 1000
    )
  }

  ########################################################################################
  # prepare scripts

  scriptfile <- file.path(dir.zip, "scripts", "camtrapR_scripts.R")
  file.create(scriptfile, showWarnings = FALSE)

  # load basic data

  sink(file = scriptfile)
  cat("###  load data tables  ### \n\n")

  cat("directory.data <- PLEASE_SPECIFY        # this is the directory you got after unzipped the zip file to (e.g. .../surveyReport_2016-02-29/)  \n\n")


  cat("CTtable     <- read.csv(paste(directory.data, 'CTtable.csv', sep = '/'))\n")
  cat("recordTable <- read.csv(paste(directory.data, 'recordTable.csv', sep = '/'))\n\n\n")

  sink()


  # make detection maps   # no, because of coordinate columns

  sink(file = scriptfile, append = TRUE)
  cat("###  plot species detections  ### \n\n")
  if(hasArg(Xcol) & hasArg(Ycol)){
    cat(paste("Xcol <- '", Xcol, "'\n", sep = ""))
    cat(paste("Ycol <- '", Ycol, "'\n\n", sep = ""))
  } else {
    cat("Xcol <- PLEASE_SPECIFY\n")
    cat("Ycol <- PLEASE_SPECIFY\n\n")
  }

  cat(paste("detections <- detectionMaps(CTtable = CTtable,
            recordTable  = recordTable,
            Xcol         = Xcol,
            Ycol         = Ycol,
            stationCol   = '", stationCol, "',
            speciesCol   = '", speciesCol, "',
            writePNG     = FALSE,
            plotR        = TRUE,
            printLabels  = TRUE,
            richnessPlot = TRUE,
            addLegend    = TRUE
  ) \n\n\n", sep = ""))

  sink()


  # camera operation matrix

  sink(file = scriptfile, append = TRUE)
  cat("###  camera operation matrix  ### \n\n")

  cat(paste("cameraOperation <- cameraOperation(CTtable = CTtable,
            stationCol                                  = '", stationCol, "',
            #cameraCol,
            setupCol                                    = '", setupCol, "',
            retrievalCol                                = '", retrievalCol, "',
            hasProblems                                 = '", CTHasProblems, "',
            #byCamera,
            #allCamsOn,
            #camerasIndependent,
            dateFormat                                  = '", CTDateFormat, "' #,
            #writecsv                                   = FALSE,
            #outDir
  ) \n\n\n", sep = ""))

  sink()


  # make detection histories

  sink(file = scriptfile, append = TRUE)
  cat("###  detection histories  ### \n\n")

  cat("day1              <- PLEASE_SPECIFY \n")
  cat("occasionLength    <- PLEASE_SPECIFY\n")
  cat("speciesOfInterest <- PLEASE_SPECIFY\n")
  cat("timeZone          <- PLEASE_SPECIFY \n\n")

  cat(paste("detHist <- detectionHistory(recordTable = recordTable,
            species                                  = speciesOfInterest,
            camOp                                    = cameraOperation,
            stationCol                               = '", stationCol, "',
            speciesCol                               = '", speciesCol, "',
            recordDateTimeCol                        = '", recordDateTimeCol, "',
            recordDateTimeFormat                     = '", recordDateTimeFormat, "',
            occasionLength                           = occasionLength,
            #maxNumberDays,
            day1                                     = day1,
            #buffer                                  = 0,
            includeEffort                            = TRUE,
            scaleEffort                              = FALSE,
            occasionStartTime                        = 0,
            datesAsOccasionNames                     = FALSE,
            timeZone                                 = timeZone,
            writecsv                                 = FALSE #,
            # outDir
  ) \n\n\n", sep = ""))

  sink()


  ### write description text file ###

  readmefile <- file.path(dir.zip, "readme.txt")
  file.create(readmefile, showWarnings = FALSE)

  sink(file = readmefile)


  cat(paste("this zip file contains a summary of a camera trapping survey:\n\n"))
  cat(paste("total survey period:              ", min(output$survey_dates$setup_date), "-", max(output$survey_dates$retrieval_date), "\n"))
  cat(paste("Total number of stations:         ", nrow(output$survey_dates), "\n"))
  cat(paste("Number of operational stations:   ", length(which(output$survey_dates$n_nights_active >= 1)), "\n"))
  cat(paste("Total number of cameras:          ", sum(output$survey_dates$n_cameras), "\n"))
  cat(paste("Total number of active trap days: ", sum(output$survey_dates$n_nights_active), "\n\n"))

  cat("\n-------------------------------------------------------\n\n")

  cat(paste("the following table shows a summary of survey period for each station \n\n"))
  print(output$survey_dates, row.names = FALSE)
  cat("\n-------------------------------------------------------\n\n")

  cat("legend to the data structure in the zip file:\n\n")
  cat(".../activity/              plots of activity density estimations for each species created with activityDensity\n")
  if(hasArg(Xcol) & hasArg(Ycol)){cat(".../detectionMaps/         maps of observed species richness and numbers of species records\n")}
  cat(".../scripts/               a prepared R script for occupancy analyses\n")
  cat(".../surveyReport/          the tables created by surveyReport summarising the survey\n")
  cat(".../CTtable.csv            table of camera trap station IDs, operation times and coordinates\n")
  cat(".../recordTable.csv        table of species records\n")
  cat(".../readme.txt             this file\n\n")

  cat("\n-------------------------------------------------------\n\n")

  cat(paste("species images are located in:\n\n"))
  cat(paste(as.character(recordTable$Directory)[1], "          # the first record\n"))
  cat(paste(as.character(recordTable$Directory)[nrow(recordTable)], "          # the last record\n"))

  cat("\n-------------------------------------------------------\n\n")

  cat("information about who created this report:\n\n")
  print(as.data.frame(Sys.info()))

  cat(paste("\n\n\n\n ***   report created by function surveyReport on",  Sys.time(),  "   ***\n\n\n"))


  sink()

  ######
  # make final zip file

  # list files
  files2zip <- dir(path = dir.zip, full.names = FALSE, recursive = TRUE)
  files2zip <- file.path(dir.zip.short, files2zip)

  # write zip
  setwd(dir.tmp)
  cat("compiling zip file \n",
      paste(sinkpath, paste(dir.zip.short, ".zip\n\n", sep = ""), sep = file.sep))

  suppressMessages(zip(zipfile = file.path(sinkpath,
                                           paste(dir.zip.short, ".zip", sep = "")),
                       files   = files2zip,
                       flags   = ""))



  # remove temporary directory
  #unlink(dir.zip, recursive = TRUE)

}