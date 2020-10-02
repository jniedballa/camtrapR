# Version Check for .onAttach()  ####
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


# for all functions in which user specifies column names: error if spaces in column names    ####
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
    if(length(z) == 1) {
      if(any(grepl(pattern = " ", x = unlist(z[[1]]), fixed = TRUE))) stop("column names in '", names(z) ,"' may not contain spaces: \n ",
                                                                           paste(names(unlist(z))[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], "=",
                                                                                 z[[1]][which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], collapse = "\n "),
                                                                           call. = FALSE)
    } 
    if(length(z) > 1) {
      which_is_the_culprit <- which(sapply(z, FUN = length) > 1)
      stop(paste("Argument", names(z)[which_is_the_culprit], "is not of length 1"), call. = FALSE)
    }
  }
}



# run exiftool for functions reading out and tabulating image metadata    ####

runExiftool <- function(command.tmp,
                        colnames.tmp)
{
  tmp1 <- strsplit(system(command.tmp, intern=TRUE), split = "\t")
  
  if(length(tmp1) == 0) return(NULL) # if nothing returned (no images, no metadata)
  if(length(tmp1) == 1) {
    if(grepl(tmp1[[1]], pattern = "^Error: ", perl = TRUE)){
      warning(paste(unlist(tmp1)), call. = FALSE)
      return(NULL)
      }
    }
  # if first entry is exiftool warning about FileName encoding remove (happens when there's special characters in directory name)
  if(any(grepl(pattern = "FileName encoding not specified", tmp1[[1]]))) tmp1[[1]] <- NULL
  
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


# add image metadata as columns in record table ####
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
    intable <- data.frame(intable, matrix(NA, ncol = length(cols2add), nrow = nrow(intable)), check.names = FALSE)
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


# assign species IDs from metadata tags or directory names   ####
assignSpeciesID <- function(intable,
                            IDfrom,
                            metadataSpeciesTag,
                            speciesCol,
                            dirs_short,
                            i_tmp,
                            multiple_tag_separator,
                            returnFileNamesMissingTags)
{
  
  file.sep <- .Platform$file.sep
  
  
  if(IDfrom == "directory"){
    intable[,speciesCol] <-  sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X)]})
    return(intable)
  } else {           # if IDfrom = "metadata"
    if(hasArg(metadataSpeciesTag)){
      metadataSpeciesTag2 <- paste("metadata", metadataSpeciesTag, sep = "_")
      
      # if the metadata_Species tag is found in metadata.tmp1
      if(metadataSpeciesTag2 %in% colnames(intable)){
        
        # copy to species column proper
        intable[,speciesCol] <- intable[,metadataSpeciesTag2]
        
        # find records without proper species tag, to be removes
        species_records_to_remove <- which(is.na(intable[,speciesCol]))
        
        # if there's records to remove
        if(length(species_records_to_remove) >= 1){    
          
          # give warnings
          if(isTRUE(returnFileNamesMissingTags)){
            warning(paste(paste( dirs_short[i_tmp],":  removed", length(species_records_to_remove), "records out of", nrow(intable),
                                 "because of missing species metadata tag:\n"),
                          paste(head(paste(intable$Directory[species_records_to_remove], intable$FileName[species_records_to_remove], sep = file.sep)), collapse = "\n")),
                    call. = FALSE, immediate. = TRUE)
          } else {
            warning(paste(paste( dirs_short[i_tmp],":  removed", length(species_records_to_remove), "records out of", nrow(intable),
                                 "because of missing species metadata tag")),
                    call. = FALSE, immediate. = TRUE)
          }
          #remove records without species tag
          intable <- intable[-species_records_to_remove,]      
        }
        
        # duplicate records with multiple species (separate row for each species)
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


# find and separate multiple species in same image (only if using metadata ID)    ####
separateMultipleSpecies <- function(intable,
                                    speciesCol,
                                    multiple_tag_separator)
{
  
  records0                 <- intable[,speciesCol]
  records_duplicate        <- strsplit(intable[,speciesCol], split = multiple_tag_separator, fixed = TRUE)
  records_duplicate_length <- sapply(records_duplicate, length)
  
  if(any(records_duplicate_length > 1)){
    intable <- intable[rep(row.names(intable), records_duplicate_length), ]                # replicate rows with >1 species / individual
    intable[,speciesCol] <- unlist(strsplit (records0, split = multiple_tag_separator))    # assign species anew
  }
  return(intable)
}




# add station and camera id to metadata table   ####

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

# check if date/time information is present and was readable    ####

checkDateTimeOriginal <- function (intable, dirs_short, i){
  # if all date/time information is missing, go to next station
  if(all(intable$DateTimeOriginal == "-")){
    warning(paste(dirs_short[i], ": no readable date/time information. Skipping"), call. = FALSE,  immediate. = TRUE)
    intable <- NULL
  } else {
    
    # if date/time information is missing for some records only
    if(any(intable$DateTimeOriginal == "-")){
      which_no_time <- which(intable$DateTimeOriginal == "-")
      warning(paste(dirs_short[i], ": omitting", length(which_no_time), "images because of missing/unreadable date/time information."), call. = FALSE,  immediate. = FALSE)
      intable <-  intable[-which_no_time,]    # removing rows with missing date/time information
    }
  }
  return(intable)
}

# remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)   ####
# Note to self: this may also be done outside the station loop, after the final record table is assembled. Saves a few executions of this function.
# edit 2019-12-04: but then the messages are useless, so just leave it as is for now

removeDuplicatesOfRecords <- function(metadata.tmp, 
                                      removeDuplicateRecords, 
                                      camerasIndependent, 
                                      stationCol, 
                                      speciesCol, 
                                      cameraCol, 
                                      current, 
                                      total,
                                      max_nchar_station){
  metadata.tmp0 <- metadata.tmp
  
  pb <- makeProgressbar(current = current, total = total)
  
  if(isTRUE(removeDuplicateRecords)){
    if(isTRUE(camerasIndependent)){
      remove.tmp <- which(duplicated(metadata.tmp[,c("DateTimeOriginal", stationCol, speciesCol, cameraCol)]))
      if(length(remove.tmp >= 1)){
        metadata.tmp <- metadata.tmp[-remove.tmp,]
      }
    } else {
      remove.tmp <- which(duplicated(metadata.tmp[,c("DateTimeOriginal", stationCol, speciesCol)]))
      if(length(remove.tmp >= 1)) {
        metadata.tmp <- metadata.tmp[-remove.tmp,]
      }
    }
    
    if(length(unique(metadata.tmp[,stationCol])) == 1) {                  # 1 station per exiftool call
      message(formatC(as.character(unique(metadata.tmp[,stationCol])), 
                      width = max_nchar_station, 
                      flag = "-"), ":  ",
              formatC(nrow(metadata.tmp0), width = 5), " images ", 
              formatC(length(remove.tmp),  width = 4), " duplicates removed",
              pb)
    } else {                                                               # > 1 station per exiftool call (recordTableIndividual)
      message(paste(unique(metadata.tmp[,stationCol]), collapse = ", "), ":  ",
              formatC(nrow(metadata.tmp0), width = 5), " images ", 
              formatC(length(remove.tmp),  width = 4), " duplicates removed",
              pb)
    }
  } else {
    if(length(unique(metadata.tmp[,stationCol])) == 1) {                  # 1 station per exiftool call
      message(formatC(as.character(unique(metadata.tmp[,stationCol])), 
                      width = max_nchar_station, 
                      flag = "-"), ":  ",
              formatC(nrow(metadata.tmp0), width = 5), " images", 
              pb)
    } else {                                                               # > 1 station per exiftool call (recordTableIndividual)
      message(paste(unique(metadata.tmp[,stationCol]), collapse = ", "), ":  ", 
              formatC(nrow(metadata.tmp0), width = 5), " images", 
              pb)
    }
  }
  return(metadata.tmp)
}


# assess temporal independence between records   ####

assessTemporalIndependence <- function(intable,
                                       deltaTimeComparedTo,
                                       columnOfInterest,     # species/individual column
                                       cameraCol,
                                       camerasIndependent,
                                       stationCol,
                                       minDeltaTime,
                                       eventSummaryColumn,
                                       eventSummaryFunction)
{
  # check if all Exif DateTimeOriginal tags were read correctly
  if(any(is.na(intable$DateTimeOriginal))){
    which.tmp <- which(is.na(intable$DateTimeOriginal))
    if(length(which.tmp) == nrow(intable)) stop("Could not read any Exif DateTimeOriginal tag at station: ", paste(unique(intable[which.tmp, stationCol])), " Consider checking for corrupted Exif metadata.")
    warning(paste("Could not read Exif DateTimeOriginal tag of", length(which.tmp),"image(s) at station", paste(unique(intable[which.tmp, stationCol]), collapse = ", "), ". Will omit them.\nConsider checking for corrupted Exif metadata. Or does your selected time zone have daylight saving time and the image(s) fall in the misisng hour at spring formward (cameras don't usually record DST)?. \n",
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
                        delta.time.days  = NA,
                        independent      = ifelse(minDeltaTime == 0, TRUE, NA),   # all independent if no temporal filtering
                        stringsAsFactors = FALSE,
                        check.names      = FALSE)        # to prevent ":" being converted to ".", e.g. in EXIF:Make
  
  # sort records by station, species, then time
  intable <- intable[order(intable[, stationCol], intable[, columnOfInterest], intable$DateTimeOriginal),]
  
  for(xy in 1:nrow(intable)){     # for every record
    
    
    which.columnOfInterest <- which(intable[, columnOfInterest]  == intable[xy, columnOfInterest])          # same species/individual
    which.stationCol       <- which(intable[, stationCol]        == intable[xy, stationCol])                # at same station
    which.independent      <- which(intable$independent          == TRUE)                                   # independent (first or only record of a species at a station)
    which.earlier          <- which(intable$DateTimeOriginal     <  intable$DateTimeOriginal[xy])          # earlier than record xy (takes long)
    #which.earlier          <- 1: (xy-1)                                                                  # earlier than record xy  (fast alternative, relies on table being sorted by date/time before anything else)
    if(camerasIndependent) {
      which.cameraCol      <- which(intable[, cameraCol]  == intable[xy, cameraCol])                        # at same camera
    }
    
    # set independent = TRUE and delta.time = 0 if it is the 1st/only  record of a species / individual
    
    if(camerasIndependent == TRUE){
      which.tmp <- Reduce(intersect, list(which.columnOfInterest, 
                                          which.stationCol, 
                                          which.cameraCol))
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp])){    # cameras at same station assessed independently
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    } else {
      which.tmp <- Reduce(intersect, list(which.columnOfInterest, 
                                          which.stationCol))
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp])){
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    }
    
    # calculate time difference to previous records of same species at this station (if not the 1st/only record)
    if(is.na(intable$delta.time.secs[xy])) {
      
      if(deltaTimeComparedTo == "lastIndependentRecord"){
        
        if(camerasIndependent == TRUE){
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                              which.stationCol,
                                              which.cameraCol,
                                              which.independent,
                                              which.earlier))
        } else {
          
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.independent,
                                                which.earlier))
        }
      }  else {    # if(deltaTimeComparedTo == "lastRecord"){'
        if(camerasIndependent  == TRUE){
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.cameraCol,
                                                which.earlier))
        } else {
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.earlier))
        }
      }
      
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
  
  # summarise some column by independent event
  
  n_imagesColumn <- "n_images"
  intable[, n_imagesColumn] <- NA
  
  which_independent <- which(intable$independent)
  
  if(hasArg(eventSummaryColumn)){
    if(all(eventSummaryColumn %in% colnames(intable))){
      if(length(eventSummaryColumn) != length(eventSummaryFunction)) stop('"eventSummaryColumn" and "eventSummaryFunction" must have same length', call. = FALSE)
      
    } else {
      warning(paste(unique(intable[, stationCol]), ": eventSummaryColumn(s) " ,
                    paste(eventSummaryColumn[!eventSummaryColumn %in% colnames(intable)], collapse = ", "),
                    " not found in column names of recordtable"), call. = FALSE)
    }
    
    summary_column_name <- paste(eventSummaryColumn, eventSummaryFunction, sep = "_")
    intable[, summary_column_name[eventSummaryColumn %in% colnames(intable)]] <- NA
    
    
    # summary_column_name <- paste(eventSummaryColumn, eventSummaryFunction, sep = "_")[which(eventSummaryColumn %in% colnames(intable))]
    # intable[, summary_column_name] <- NA
  }    # end    if(hasArg(eventSummaryColumn)){
  
  
  for(xy in 1:length(which_independent)){     # for every independent record (the ones that end up in record table)
    
    current_row <- which_independent[xy]
    
    if(camerasIndependent){
      which_records_to_group <- which(intable[, columnOfInterest]     == intable[current_row, columnOfInterest] &   # same species
                                      intable[, stationCol]           == intable[current_row, stationCol]  &        # same station
                                      intable[, cameraCol]            == intable[current_row, cameraCol]   &        # same camera
                                      intable$DateTimeOriginal        >= intable$DateTimeOriginal[current_row] &    # later than current record
                                      !isTRUE(intable$independent))                                        # not independent
      
    } else {
      which_records_to_group <- which(intable[, columnOfInterest]     == intable[current_row, columnOfInterest] &   # same species
                                      intable[, stationCol]           == intable[current_row, stationCol]  &        # same station
                                      intable$DateTimeOriginal        >= intable$DateTimeOriginal[current_row] &
                                      !isTRUE(intable$independent))                                        # not independent
    }
    
    # subset to records before the next independent record
    if(xy < length(which_independent)){
      which_records_to_group <- which_records_to_group[which_records_to_group < which_independent[xy + 1]] #which_records_to_group[which_records_to_group %in% seq(current_row, (which_independent[xy + 1] - 1))]
    } else {
      which_records_to_group <-  which_records_to_group[which_records_to_group <= nrow(intable)]   # which_records_to_group[which_records_to_group %in% seq(current_row, nrow(intable))]
    }
    
    if(hasArg(eventSummaryColumn)){
      
      for(eventSummaryIndex in 1:length(eventSummaryColumn)) {
        
        if(eventSummaryColumn[eventSummaryIndex] %in% colnames(intable)){
          
          summary_value <- do.call(what = eventSummaryFunction[eventSummaryIndex], 
                                   args = list(intable[which_records_to_group, eventSummaryColumn[eventSummaryIndex]], 
                                               na.rm = TRUE))
          if(!is.infinite(summary_value)){
            intable[current_row, summary_column_name[eventSummaryIndex]] <- ifelse(length(summary_value) > 1, 
                                                                                   paste(summary_value, collapse = ", "),
                                                                                   summary_value)
          }
          rm(summary_value)
        }
        
        
      }    # end for(eventSummaryIndex in 1:length(eventSummaryColumn)
    }      # end if(hasArg(eventSummaryColumn))
    
    intable[ current_row, n_imagesColumn] <- length(which_records_to_group)
    rm(which_records_to_group, current_row)
    
  }        # end for(xy in 1:length(which_independent))
  
  
  # keep only independent records
  outtable <- intable[intable$independent,]
  
  
  # compute delta time in hours and days
  outtable$delta.time.secs  <- round(outtable$delta.time.secs, digits = 0)
  outtable$delta.time.mins  <- round(outtable$delta.time.secs  / 60, digits = 1)
  outtable$delta.time.hours <- round(outtable$delta.time.mins  / 60, digits = 1)
  outtable$delta.time.days  <- round(outtable$delta.time.hours / 24, digits = 1)
  
  # remove "independent" column
  
  outtable <- outtable[, !colnames(outtable) %in% "independent"]
  
  return(outtable)
}


# add potential new columns to global record.table   ####

addNewColumnsToGlobalTable <- function(intable,
                                       i,
                                       record.table)
{
  
  if( nrow(record.table) >= 1){    # if there is a record table already (i.e., it is not the first station with images)
    which_cols_to_add_to_d1 <- seq(1, ncol(record.table))[-which(colnames(record.table) %in% colnames(intable))]   # columns in record.table but not in intable
    
    # if intable lacks columns present in record.table, add them here (filled with NA)
    if(length(which_cols_to_add_to_d1) >= 1){
      intable <- data.frame(intable, as.list(rep(NA, each = length(which_cols_to_add_to_d1))))
      colnames(intable)[(ncol(intable) - length(which_cols_to_add_to_d1) + 1) :  ncol(intable)] <- colnames(record.table)[which_cols_to_add_to_d1]
    }
    
    # now check which columns are present in intable but not in record.table (new tag groups) and add these (filled with NA)
    which_cols_to_add_to_record.table <- seq(1, ncol(intable))[-which(colnames(intable) %in% colnames(record.table))]  # columns present in intable but not in record.table
    if(length(which_cols_to_add_to_record.table) >= 1){
      record.table <- data.frame(record.table, 
                                 as.list(rep(NA, each = length(which_cols_to_add_to_record.table))),
                                 check.names = FALSE)
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

# check column names of camera operation matrix  ####
# if there's a time shift from cameraOperation, extract return number as attribute
checkCamOpColumnNames <- function(cameraOperationMatrix){
  
  if(any(is.na(colnames(cameraOperationMatrix)))) stop("There are NAs in the column names of camOp", call. = FALSE)
  
  # check if camera operration matrix has time shift
  if(all(grepl(pattern = "+", colnames(cameraOperationMatrix), fixed = TRUE))){
    colnames_as_dates <- sapply(strsplit(colnames(cameraOperationMatrix), split = "+", fixed = TRUE), FUN = function(x)x[1])
    camopTest <- try(as.Date(colnames_as_dates), silent = TRUE)
    occasionStartTime <- as.numeric(gsub("h", "", unique(sapply(strsplit(colnames(cameraOperationMatrix), split = "+", fixed = TRUE), FUN = function(x)x[2]))))
  } else {
    occasionStartTime <- 0
    camopTest <- try(as.Date(colnames(cameraOperationMatrix)), silent = TRUE)
  }
  
  if(class(camopTest) == "try-error") stop(paste('Could not interpret column names in camOp as Dates. Desired format is YYYY-MM-DD (e.g. "2016-12-31") YYYY-MM-DD+Xh (X being a number, e.g. "2016-12-31+12h"). First column name in your camera operation matrix is "', colnames(cameraOperationMatrix)[1], '"', sep = '' ), call. = FALSE)
  
  
  colnames(cameraOperationMatrix) <- as.character(camopTest)
  
  attr(cameraOperationMatrix, "occasionStartTime") <- occasionStartTime   # return time shift extracted from the column names
  return(cameraOperationMatrix)    
}


# create table of station / camera deployment and image data ranges  ####
createDateRangeTable <- function(cam.op,
                                 subset_species_tmp,
                                 buffer_tmp,
                                 stationCol_tmp,
                                 day1_tmp,
                                 occasionStartTime_tmp,
                                 maxNumberDays_tmp,
                                 timeZone_tmp)
{
  
  # first day of each station
  cam.tmp.min <- apply(cam.op, MARGIN = 1, function(X){min(which(!is.na(X)))})
  # last day of each station
  cam.tmp.max <- apply(cam.op, MARGIN = 1, function(X){max(which(!is.na(X)))})
  
  # date of first / last record by station
  rec.tmp.min  <- aggregate(as.Date(subset_species_tmp$DateTime2, tz = timeZone_tmp),
                            list(subset_species_tmp[,stationCol_tmp]),
                            FUN = min)
  rec.tmp.max  <- aggregate(as.Date(subset_species_tmp$DateTime2, tz = timeZone_tmp),
                            list(subset_species_tmp[,stationCol_tmp]),
                            FUN = max)
  
  
  # combine record dates and camera operation dates in one table
  date_ranges <- data.frame(rec.min = rec.tmp.min[match(rownames(cam.op), rec.tmp.min[,1]), 2],       # first record
                            rec.max = rec.tmp.max[match(rownames(cam.op), rec.tmp.max[,1]), 2],       # last record
                            cam.min = as.POSIXct(colnames(cam.op)[cam.tmp.min], tz = timeZone_tmp),   # station setup date
                            cam.max = as.POSIXct(colnames(cam.op)[cam.tmp.max], tz = timeZone_tmp)    # station retrieval date
  )
  
  rownames(date_ranges) <- rownames(cam.op)
  
  # check if images were taken between setup and retrieval dates (warning if images outside station date range)
  if(any(date_ranges$rec.min < as.Date(date_ranges$cam.min, tz = timeZone_tmp), na.rm = TRUE)){
    warning(paste("At", sum(date_ranges$rec.min < as.Date(date_ranges$cam.min, tz = timeZone_tmp), na.rm = TRUE), "stations",
                  "there were records before camera operation date range: ",
                  paste(rownames(date_ranges)[which(date_ranges$rec.min < as.Date(date_ranges$cam.min, tz = timeZone_tmp))], 
                        sep = "\n", collapse = ", " )), call. = FALSE)
  }
  if(any(date_ranges$rec.max > as.Date(date_ranges$cam.max, tz = timeZone_tmp), na.rm = TRUE)) {
    warning(paste("At", sum(date_ranges$rec.max > as.Date(date_ranges$cam.max, tz = timeZone_tmp), na.rm = TRUE), "stations",
                  "there records after camera operation date range: ",
                  paste(rownames(date_ranges)[which(date_ranges$rec.max > as.Date(date_ranges$cam.max, tz = timeZone_tmp))], 
                        sep = "\n", collapse = ", " )), call. = FALSE)
  }
  
  # define when first occasion begins (to afterwards remove prior records in function    cleanSubsetSpecies)
  if(!hasArg(buffer_tmp)) buffer_tmp <- 0
  
  
  date_ranges$start_first_occasion <- date_ranges$cam.min + buffer_tmp * 86400 + occasionStartTime_tmp * 3600     #each stations setup  + buffer + starttime
  date_ranges$start_first_occasion_survey <- min(date_ranges$cam.min) + buffer_tmp * 86400 + occasionStartTime_tmp * 3600    # first station's setup  + buffer + starttime

  
  if(day1_tmp %in% c("survey", "station") == FALSE) {
    if(as.Date(day1_tmp, tz = timeZone_tmp) < min(as.Date(date_ranges$cam.min,  tz = timeZone_tmp))) stop(paste("day1 (", day1_tmp, ") is before the first station's setup date (",  min(as.Date(date_ranges$cam.min,  tz = timeZone_tmp)), ")", sep = ""))
    if(as.Date(day1_tmp, tz = timeZone_tmp) > max(as.Date(date_ranges$cam.max,  tz = timeZone_tmp))) stop(paste("day1 (", day1_tmp, ") is after the last station's retrieval date (",  max(as.Date(date_ranges$cam.max,  tz = timeZone_tmp)), ")", sep = ""))
    date_ranges$start_first_occasion <- as.POSIXlt(day1_tmp, tz = timeZone_tmp) + occasionStartTime_tmp * 3600
  }
  
  
  
  # # define when last occasion ends 
  # the old way:
  date_ranges$end_of_retrieval_day <- as.POSIXct(paste(date_ranges$cam.max, "23:59:59"), tz = timeZone_tmp, format = "%Y-%m-%d %H:%M:%S")    # end of retrieval day
  
  # new possible solution:
  # # Option 1: end of retrieval day
  # end_of_retrieval_day <- as.POSIXct(paste(date_ranges$cam.max, "23:59:59"), tz = timeZone_tmp, format = "%Y-%m-%d %H:%M:%S")    # end of retrieval day
  # # Option 2: exactly at retrieval hour (this only makes sense if value in camOp on retrieval day is fraction (if time of day was provided))
  # # if there was a problem on the last day, it will be incorrect (return 0:0:0 if effort = 0)
  # end_of_retrieval_day_hour <- date_ranges$cam.max + dhours(apply(cam.op, MARGIN = 1, function(X){X[max(which(!is.na(X)))] * 24}))
  # 
  # # define end of survey: exact retrieval time (if effort is a fraction of day)
  # if(all(end_of_retrieval_day_hour < end_of_retrieval_day)) {
  #   date_ranges$end_of_retrieval_day <- end_of_retrieval_day_hour
  # } else{
  #   date_ranges$end_of_retrieval_day <- end_of_retrieval_day
  # }
  
  # if maxNumberDays is defined, find which is earlier: start + maxNumberDays or station retrieval?
  if(hasArg(maxNumberDays_tmp)) {
    
    if(day1_tmp %in% c("survey", "station") == FALSE){
      # count maximum number days from the beginning of each station's 1st occasion
      date_ranges$start_first_occasion_plus_maxNumberDays <- date_ranges$start_first_occasion_survey + (maxNumberDays_tmp * 86400) - 1   # -1 second ensures that last occasion does not spill into next day if occasionStartTime = 0
    } else {
      # count maximum number days from the beginning of survey's 1st occasion
      date_ranges$start_first_occasion_plus_maxNumberDays <- date_ranges$start_first_occasion + (maxNumberDays_tmp * 86400) - 1   # -1 second ensures that last occasion does not spill into next day if occasionStartTime = 0
    }
    
    # end of last occasion by staion (either end of retrieval day, or after maximum number of days)
    for(xy in 1:nrow(date_ranges)){
      date_ranges$end_last_occasion[xy] <- min(date_ranges$end_of_retrieval_day[xy], date_ranges$start_first_occasion_plus_maxNumberDays[xy])   # use smaller value
    }
    # assign the attributes: POSIX + time zone (to convert from numeric value back to date/time)
    attributes(date_ranges$end_last_occasion) <- attributes(date_ranges$start_first_occasion)   
  } 
  
  # if maxNumberDays is not defined, occasions end on station retrieval?
  if(!hasArg(maxNumberDays_tmp)) {
    date_ranges$end_last_occasion <- date_ranges$end_of_retrieval_day
  }
  
  return(date_ranges)
  
}




# check camera operation matrix with date range table  ####
adjustCameraOperationMatrix <- function(cam.op,
                                        date_ranges2,
                                        timeZone_tmp,
                                        day1_2
){
  # remove stations where occasions begin after end of last occasion (if buffer argument is too large)
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
  # trim camera operation matrix (taking into account buffer, occasionStartTime(?), maxNumberDays)
  # based on data frame   "date_ranges"   computed by function   createDateRangeTable
  
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


# check consistency of species record table before creating detection history (remove records outside date range etc)  ####
cleanSubsetSpecies <- function(subset_species2,
                               stationCol2,
                               date_ranges2
){
  
  nrow_subset_species2 <- nrow(subset_species2)
  
  
  # remove records that were taken before beginning of first occasion (because of buffer, occasionStartTime, day1)
  corrected_start_time_by_record <- date_ranges2$start_first_occasion[match(subset_species2[,stationCol2], rownames(date_ranges2))]
  
  remove.these <- which(subset_species2$DateTime2 < corrected_start_time_by_record)
  if(length(remove.these) >= 1){
    
    warning(paste(length(remove.these), 
                  " records (out of ", 
                  nrow_subset_species2, 
                  ") were removed because they were taken before day1 (if a date was specified),  within the buffer period, or before occasionStartTime on the 1st day, e.g.:\n", sep = ""),
            paste(head(subset_species2[remove.these, stationCol2]), 
                       head(subset_species2$DateTime2[remove.these]), 
                       collapse = "\n", 
                       sep = ": "),
            call. = FALSE)
    
    subset_species2 <- subset_species2[-remove.these,]
    if(nrow(subset_species2) == 0) stop("No more records after removing records before survey begin. The detection history would be empty.")
    
    rm(corrected_start_time_by_record, remove.these)
  }
  
  # remove records that were taken after end of last occasion (because of maxNumberDays)
  corrected_end_time_by_record <- date_ranges2$end_last_occasion[match(subset_species2[,stationCol2], rownames(date_ranges2))]
  
  remove.these2 <- which(subset_species2$DateTime2 > corrected_end_time_by_record)
  if(length(remove.these2) >= 1){
    
    warning(paste(paste(length(remove.these2), 
                        " records (out of ", 
                        nrow_subset_species2, 
                        ") were removed because they were taken after the end of the last occasion, e.g.:", sep = ""),
                  paste(head(subset_species2[remove.these2, stationCol2]), 
                        head(subset_species2$DateTime2[remove.these2]), 
                        collapse = "\n", 
                        sep = ": "), 
                  sep = "\n"), 
            call. = FALSE)
    
    subset_species2 <- subset_species2[-remove.these2,]
    
    if(nrow(subset_species2) == 0) stop("No more records left. The detection history would be empty.")
    rm(corrected_end_time_by_record, remove.these2)
  }
  
  return(subset_species2)
}


# calculate trapping effort matrix by day   ####
calculateTrappingEffort <- function(cam.op,
                                    occasionLength2,
                                    scaleEffort2,
                                    includeEffort2,
                                    minActiveDaysPerOccasion2,
                                    occasionStartTime2){
  
  ######################
  # calculate trapping effort by station and occasion
  
  if(occasionLength2 == 1){
    effort <- cam.op          # if occasionLength2 = 1 day, it is identical
  } else {
    effort <- matrix(NA, nrow = nrow(cam.op), ncol = ceiling(ncol(cam.op) / occasionLength2 ))
    
    index <- 1
    for(m in 1:ncol(effort)){    # for every occasion in the effort matrix
      # index for columns in camera operation matrix to aggregate
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
      
      ## if full occasion is smaller than 1 (i.e. all 0 or NA), set effort NA
      ## EDIT: 2020-09-12: This can cause problems if cameraOperation calculated fraction of days.
      ## Also, the cases with 0 and NA are covered above. So I comment it for now.
      #effort[, m] <- ifelse(apply(as.matrix(cam.op[,index.tmp]), MARGIN = 1, FUN = function(X) {all(X < 1)}),   NA, effort[,m])
      
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
        
        ## includeEffort = TRUE
      } else {    
        # if minActiveDays is defined
        if(hasArg(minActiveDaysPerOccasion2)){   
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
                          sinkpath,
                          usePackageZip){
  
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
  
  cat(paste("camOp <- cameraOperation(CTtable = CTtable,
            stationCol                        = '", stationCol, "',
            #cameraCol,
            setupCol                          = '", setupCol, "',
            retrievalCol                      = '", retrievalCol, "',
            hasProblems                       = '", CTHasProblems, "',
            #byCamera,
            #allCamsOn,
            #camerasIndependent,
            dateFormat                        = '", CTDateFormat, "' #,
            #writecsv                         = FALSE,
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
  
  if(isFALSE(usePackageZip)) {
  zip(zipfile = file.path(sinkpath,
                                           paste(dir.zip.short, ".zip", sep = "")),
                       files   = files2zip,
                       flags   = "")
  }
  
  if(isTRUE(usePackageZip)) {
    zip::zipr(zipfile = file.path(sinkpath,
                                  paste(dir.zip.short, ".zip", sep = "")),
              files   = files2zip)
  }
  
  # check if output was created
  if(file.exists(file.path(sinkpath,
                           paste(dir.zip.short, ".zip", sep = "")))){
  message("zip file compiled \n",
      paste(sinkpath, paste(dir.zip.short, ".zip\n\n", sep = ""), sep = file.sep))
  } else {
    message("zip file creation failed")
  }
  
  # remove temporary directory
  #unlink(dir.zip, recursive = TRUE)
}



##########################################################################################################
# assign session IDs to records in recordTableIndividual based on a session column in camera trap table    ####
# This is for when there are several entries for each station, one for each session
# in other words, if stations were operated continuously and session start/end dates are defined in camera trap table

assignSessionIDtoRecordTableIndividual <- function(recordTableIndividual_tmp,
                                                   cameraTrapTable_tmp,
                                                   stationCol,
                                                   sessionCol,
                                                   setup_date_col,
                                                   retrieval_date_col){
  
  # check input
  if(sessionCol %in% colnames(recordTableIndividual_tmp)) stop("recordTableIndividual has a session column already. Cannot assign new session ID",
                                                               call. = FALSE)
  if(!setup_date_col %in% colnames(cameraTrapTable_tmp)) stop("CTtable setup column not found",
                                                              call. = FALSE)
  if(!retrieval_date_col %in% colnames(cameraTrapTable_tmp)) stop("CTtable retrieval column not found",
                                                                  call. = FALSE)
  
  # define "between" function
  `%between%` <- function(x, interval) x >= interval[1] & x <= interval[2]
  
  # add an empty session column to the record table
  recordTableIndividual_tmp[, sessionCol] <- NA
  
  # for each station, find the respective sessions and assign the session IDs to the records based on their date.
  for(i in 1:length(unique(cameraTrapTable_tmp[,stationCol]))){
    
    # find index of records in station i and respective dates
    record_rows_id_tmp <- which(recordTableIndividual_tmp[,stationCol] == unique(cameraTrapTable_tmp[,stationCol])[i])
    rec_dates_tmp <- recordTableIndividual_tmp$Date [record_rows_id_tmp]
    
    # NOTE: output may be assigned to wrong session if occasionStartTime != 0!!!
    # There is also an issue when station ID is a combination of station and session IDs.
    
    # extract session start/end dates (by session)
    session_id    <- cameraTrapTable_tmp[, sessionCol]         [cameraTrapTable_tmp[,stationCol] == unique(cameraTrapTable_tmp[,stationCol])[i]]
    session_start <- cameraTrapTable_tmp[, setup_date_col]     [cameraTrapTable_tmp[,stationCol] == unique(cameraTrapTable_tmp[,stationCol])[i]]
    session_end   <- cameraTrapTable_tmp[, retrieval_date_col] [cameraTrapTable_tmp[,stationCol] == unique(cameraTrapTable_tmp[,stationCol])[i]]
    
    # assign session ID to recordTable
    for(session_id_tmp in session_id){
      record_rows_session_id_tmp <- which(rec_dates_tmp %between% c(session_start[session_id_tmp], session_end[session_id_tmp]))
      recordTableIndividual_tmp[record_rows_id_tmp, sessionCol]  [record_rows_session_id_tmp] <- session_id_tmp
    }
    rm(session_id, session_start, session_end, record_rows_id_tmp, rec_dates_tmp)
  }
  
  # create new station columns based on station + session (and create backup first)
  
  recordTableIndividual_tmp$station_backup <- recordTableIndividual_tmp[, stationCol]
  cameraTrapTable_tmp$station_backup       <- cameraTrapTable_tmp[, stationCol]
  
  recordTableIndividual_tmp[, stationCol] <- paste(recordTableIndividual_tmp[, stationCol], recordTableIndividual_tmp[, sessionCol], sep = "_session")
  cameraTrapTable_tmp[, stationCol]       <- paste(cameraTrapTable_tmp[, stationCol], cameraTrapTable_tmp[, sessionCol], sep = "_session")
  
  
  return(list(recordTable = recordTableIndividual_tmp,
              CTtable     = cameraTrapTable_tmp))
}



assignSessionIDtoRecordTable <- function(recordTable_tmp,
                                         camOp,
                                         dateTimeCol,
                                         stationCol,
                                         sessionCol
){
  
  
  camop.info.df <- deparseCamOpRownames(camOp)
  
  # if there's records at stations that are not in the camop row names
  if(!any(recordTable_tmp[, stationCol] %in% unique(camop.info.df$station))) {
    stop("Stations in record table station column don't match stations in row names of camera operation matrix.")
  }
    
  if(!all(recordTable_tmp[, stationCol] %in% unique(camop.info.df$station))) {
    warning(paste("Some stations in the record table are not matched by stations in camOp row names:\n",
                  paste(unique(recordTable_tmp[!recordTable_tmp[, stationCol] %in% unique(camop.info.df$station), stationCol]), collapse = ", ")))
  }
  
  recordTable_tmp[, sessionCol] <- NA
  
  record_dates_tmp <- as.Date(recordTable_tmp[, dateTimeCol])
  record_dates_backup <- record_dates_tmp
  camOp_dates_tmp  <- as.Date(colnames(camOp))
  
  # remove records before / after camOp date range
  records_too_early <- which(sapply(record_dates_tmp, FUN = function(x) x < min(camOp_dates_tmp)))
  
  if(length(records_too_early) > 0) {
    recordTable_tmp <- recordTable_tmp[-records_too_early, ]
    warning(paste(length(records_too_late), "records were removed because they were before the first day in the camera operation matrix"), call. = FALSE)
    record_dates_tmp <- as.Date(recordTable_tmp[, dateTimeCol])
  }
  
  records_too_late  <- which(sapply(record_dates_tmp, FUN = function(x) x > max(camOp_dates_tmp)))
  
  if(length(records_too_late) > 0) {
    recordTable_tmp <- recordTable_tmp[- records_too_late, ]
    warning(paste(length(records_too_late), "records were removed because they were after the last day in the camera operation matrix"), call. = FALSE)
    record_dates_tmp <- as.Date(recordTable_tmp[, dateTimeCol])
  }
  
  if(nrow(recordTable_tmp) == 0) stop(paste0("Date range mismatch between records (", paste(range(record_dates_backup), collapse = " - "),
                                            ") and camera operation matrix (", paste(range(camOp_dates_tmp), collapse = " - "), ")"))
  
  for(i in 1:nrow(camop.info.df)){
    # for every row in camOp, find all records at that station  
    which_tmp <- which(recordTable_tmp[, stationCol] == camop.info.df$station[i])
    
    if(length(which_tmp) >= 1){
      # of the records at that station, which are not NA in that row of the camOp (there are different rows for different sessions)
      which_tmp2 <- which(!is.na(camOp[i, as.character(record_dates_tmp[which_tmp])]))
      
      recordTable_tmp[which_tmp[which_tmp2], sessionCol] <- camop.info.df$session[i]
    }
  }
  
  which_na <- which(is.na(recordTable_tmp[, sessionCol]))
  
  if(length(which_na) > 0) {
    recordTable_tmp_na <- recordTable_tmp[which_na, ]
    recordTable_tmp <- recordTable_tmp[-which_na, ]
    if(length(which_na) == nrow(recordTable_tmp)) {
      stop("Could not assign session to any record. Please check format of column names of camOp and station column of record table")
    } else {
      warning(paste0(length(which_na), " records were removed because they could not be assigned to an active station / session:\n",
                    paste(recordTable_tmp_na[, stationCol], recordTable_tmp_na[, dateTimeCol], sep = ": ", collapse = "\n")), call. = FALSE)
    }
  }
  
  separatorSession <- "__SESS_"
  stationCol_backup <- paste(stationCol, "backup", sep = "_")
  
  recordTable_tmp[, stationCol_backup] <- recordTable_tmp[, stationCol]
  
  recordTable_tmp[, stationCol] <- paste(recordTable_tmp[, stationCol], recordTable_tmp[, sessionCol], sep = separatorSession)
  
  return(recordTable_tmp)
}



# checks if input is data.frame or tibble. If tibble, either convert to data.frame or stop  ####

dataFrameTibbleCheck <- function(df, 
                                 tibble_allowed = TRUE, 
                                 data_table_allowed = TRUE){
  
  # check if it is a data.frame at all
  if(!is.data.frame(df)) stop(paste(substitute(df), "must be a data.frame"), call. = FALSE)
  
  # handling tibbles (tidyverse)
  if (requireNamespace("tibble", quietly = TRUE)) {
    
    if(tibble::is_tibble(df)) {
      if(tibble_allowed) {
        message (paste(substitute(df), "was converted from tibble to data.frame"))
        df <- as.data.frame(df)
      } else {
        stop (paste(substitute(df), "is a tibble. Please provide a data.frame instead (use read.csv() or as.data.frame())"), call. = FALSE)
      }
    }
  }
  
  # handling data.tables
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    
    if(data.table::is.data.table(df)) {
      if(data_table_allowed) {
        message (paste(substitute(df), "was converted from data.table to data.frame"))
        #df <- as.data.frame(df)
        df <- setDF(df)
      } else {
        stop (paste(substitute(df), "is a data.table Please provide a data.frame instead (use read.csv() or as.data.frame())"), call. = FALSE)
      }
    }
  }
  return(df)
}


# check and convert dates (character) to date objects, either with base functions or lubridate    ####

# CHECK: timeZone argument needed in surveyReport?

parseDateObject <- function(inputColumn,     
                            dateFormat,   
                            checkNA,      # throw error if there are NAs in input (only setup / retrieval, not problems)
                            checkEmpty    # throw error if there are blank values in input  (only setup / retrieval, not problems)
){
  
  #if(!class(inputColumn) %in% c("factor", "character")) stop(paste("date column must be a factor or character:", deparse(substitute(inputColumn))), call. = FALSE)
  
  if(checkNA & any(is.na(inputColumn)))   stop(paste("there are NAs in", deparse(substitute(inputColumn))), call. = FALSE)
  if(checkEmpty & any(inputColumn == "")) stop(paste("there are blank values in", deparse(substitute(inputColumn))), call. = FALSE)
  
  inputColumn.char <- as.character(inputColumn)
  
  # option 1: base functions for dates as per strptime (identified by "%")
  if(grepl(pattern = "%", x = dateFormat, fixed = TRUE)){
    out <- as.Date(inputColumn.char,     format = dateFormat)
  } else {
  # option 2: lubridate functions (identified by absence of "%")
    if(!requireNamespace("lubridate", quietly = TRUE)) stop(paste("package 'lubridate' is required for the specified dateFormat", dateFormat))
    
    out <- lubridate::date(lubridate::parse_date_time(inputColumn.char, orders = dateFormat))
  }
  
 # if(all(is.na(out))) stop(paste0("Cannot read date format in", deparse(substitute(inputColumn)), ". Output is all NA."), call. = FALSE)
  
  if(all(is.na(out))) stop(paste0("Cannot read date format in ", deparse(substitute(inputColumn)), ". Output is all NA.\n",
                                  "expected:  ", dateFormat, "\nactual:    ", inputColumn[1]), call. = FALSE)
  
  if(checkNA & any(is.na(out))) stop(paste("At least one entry in", deparse(substitute(inputColumn)), "cannot be interpreted using dateFormat:", dateFormat, "\n",
                                           "rows", paste(which(is.na(out)), collapse = ", ")), call. = FALSE)
  return(out)
}


# check and convert date - time (character) to datetime objects (POSIXlt), either with base functions or lubridate    ####

parseDateTimeObject <- function(inputColumn,     
                                dateTimeFormat,
                                timeZone,
                                checkNA = TRUE,      # throw error if there are NAs in input (only setup / retrieval, not problems)
                                checkEmpty = TRUE,    # throw error if there are blank values in input  (only setup / retrieval, not problems)
                                checkNA_out = TRUE  # throw error when there is NAs in output (FALSE so reporting is done by detectionHistory, which returns correct row numbers)
){
  
  
  if(!class(inputColumn) %in% c("factor", "character")) stop(paste("datetime column must be a factor or character:", deparse(substitute(inputColumn))), call. = FALSE)
  
  if(checkNA & any(is.na(inputColumn)))   stop(paste("there are NAs in", deparse(substitute(inputColumn))), call. = FALSE)
  if(checkEmpty & any(inputColumn == "")) stop(paste("there are blank values in", deparse(substitute(inputColumn))), call. = FALSE)
  
  inputColumn.char <- as.character(inputColumn)
  
  # option 1: base functions for dates as per strptime (identified by "%")
  if(grepl(pattern = "%", x = dateTimeFormat, fixed = TRUE)){
    out <- as.POSIXlt(inputColumn.char, tz = timeZone, format = dateTimeFormat)
  } else {
    # option 2: lubridate functions (identified by absence of "%")
    if(!requireNamespace("lubridate", quietly = TRUE)) stop(paste("package 'lubridate' is required for the specified dateTimeFormat", dateTimeFormat))
    
    out <- lubridate::parse_date_time(inputColumn.char, orders = dateTimeFormat, tz = timeZone)
  }
  
  if(all(is.na(out))) stop(paste0("Cannot read datetime format in ", deparse(substitute(inputColumn)), ". Output is all NA.\n",
  "expected:  ", dateTimeFormat, "\nactual:    ", inputColumn[1]), call. = FALSE)
  
  if(checkNA_out & any(is.na(out))) stop(paste(sum(is.na(out)), "out of", length(out), "records in",
                                               deparse(substitute(inputColumn)), "cannot be interpreted using dateTimeFormat:", dateTimeFormat, "\n",
                                               "rows", paste(which(is.na(out)), collapse = ", ")), call. = FALSE)
  
  
  if(!any(class(out) %in% c("POSIXct", "POSIXlt"))) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat. Output is not POSIX object")
  return(out)
}

## make a new empty matrix, a row for each unique station / camera combination
stationSessionCamMatrix <- function(CTtable,
                                    stationCol,
                                    cameraCol, 
                                    sessionCol, 
                                    setupCol,
                                    retrievalCol#,
                                    #separator
){
  
  separatorCam     <- "__CAM_"
  separatorSession <- "__SESS_"
  
  double_underscore <- "__"
  
  if(length(grep(pattern = double_underscore, x = CTtable[,stationCol])) >= 1) stop(paste("Station IDs may not contain double underscores", double_underscore), call. = FALSE)
  
  
  # convert setup and retrievalCol to Dates (in case they contain times)
  CTtable[,setupCol]     <- as.Date(as.character(CTtable[,setupCol]))
  CTtable[,retrievalCol] <- as.Date(as.character(CTtable[,retrievalCol]))
  
  
  if(hasArg(sessionCol)){
    if(any(CTtable[,sessionCol] == "")) stop("there are empty cells in sessionCol Please provide camera IDs for all cameras",
                                             call. = FALSE)    
    if(any(is.na(CTtable[,sessionCol]))) stop("there are NAs in sessionCol Please provide camera IDs for all cameras",
                                              call. = FALSE)    
    if(length(grep(pattern = double_underscore, x = CTtable[,sessionCol])) >= 1)  stop(paste("Session IDs may not contain double underscores", double_underscore),  call. = FALSE)
    
    stationsession <- paste(CTtable[,stationCol], CTtable[,sessionCol], sep = separatorSession)
    m <- matrix(ncol = abs(as.integer(max(CTtable[,retrievalCol]) - min(CTtable[,setupCol]))) + 1,
                nrow  = length(stationsession))
    colnames(m) <- as.character(as.Date(min(CTtable[,setupCol]):max(CTtable[,retrievalCol]), origin = "1970-01-01"))
    rownames(m) <- stationsession
  }
  
  if(hasArg(cameraCol)){
    if(any(CTtable[,cameraCol] == "")) stop("there are empty cells in cameraCol. Please provide camera IDs for all cameras",
                                            call. = FALSE)    
    if(any(is.na(CTtable[,cameraCol]))) stop("there are NAs in cameraCol. Please provide camera IDs for all cameras",
                                             call. = FALSE)    
    if(length(grep(pattern = double_underscore, x = CTtable[,cameraCol])) >= 1)  stop(paste("Camera IDs may not contain double underscores", double_underscore),  call. = FALSE)
    
    
    stationcam <- paste(CTtable[,stationCol], CTtable[,cameraCol], sep = separatorCam)
    m <- matrix(ncol = abs(as.integer(max(CTtable[,retrievalCol]) - min(CTtable[,setupCol]))) + 1,
                nrow  = length(stationcam))
    colnames(m) <- as.character(as.Date(min(CTtable[,setupCol]):max(CTtable[,retrievalCol]), origin = "1970-01-01"))
    rownames(m) <- stationcam
  }
  
  if(hasArg(sessionCol) & hasArg(cameraCol)){
    
    stationsessioncam <- paste(CTtable[,stationCol], separatorSession, CTtable[,sessionCol], separatorCam, CTtable[,cameraCol], sep = "")
    m <- matrix(ncol = abs(as.integer(max(CTtable[,retrievalCol]) - min(CTtable[,setupCol]))) + 1,
                nrow  = length(stationsessioncam))
    colnames(m) <- as.character(as.Date(min(CTtable[,setupCol]):max(CTtable[,retrievalCol]), origin = "1970-01-01"))
    rownames(m) <- stationsessioncam
  }
  
  if(!hasArg(sessionCol) & !hasArg(cameraCol)){
    
    m <- matrix(ncol = abs(as.integer(max(CTtable[,retrievalCol]) - min(CTtable[,setupCol]))) + 1,
                nrow  = length(CTtable[, stationCol]))
    colnames(m) <- as.character(as.Date(min(CTtable[,setupCol]):max(CTtable[,retrievalCol]), origin = "1970-01-01"))
    rownames(m) <- CTtable[, stationCol]
  }
  
  # # assign attributes to rownames
  # attr(rownames(m), "station") <- CTtable[,stationCol]
  # 
  # if(hasArg(sessionCol)) attr(rownames(m), "session") <- CTtable[, sessionCol]
  # if(hasArg(cameraCol))  attr(rownames(m), "camera")  <- CTtable[, cameraCol]
  
  return(m)
}

# analyse row names of camera operation matrix to extract station, camera and session information (the latter two only if applicable)
deparseCamOpRownames <- function(camOp){
  
  separatorCam <- "__CAM_"
  separatorSession <- "__SESS_"
  
  if(is.data.frame(camOp)) camOp <- as.matrix(camOp)
  
  # extract rownames, if it is a matrix (if not I assume it is a vector, the result of rownames(camOp))
  if(is.matrix(camOp)){
    x <- rownames(camOp)
  } else {
    x <- camOp
  }
  
  if(any(grepl(separatorSession, x)) &
     any(grepl(separatorCam, x))){
    x2 <- strsplit(x, split = separatorSession)
    
    stationIDs <- sapply(x2, FUN = function(x)x[1])
    
    sessionCameraIDs <- sapply(x2, FUN = function(x)x[2])
    
    x3 <- strsplit(sessionCameraIDs, split = separatorCam)
    
    sessionIDs <- sapply(x3, FUN = function(x)x[1])
    cameraIDs  <- sapply(x3, FUN = function(x)x[2])
    
    return(data.frame(station = stationIDs,
                      session = sessionIDs,
                      camera = cameraIDs, 
                      stringsAsFactors = FALSE))
  }
  
  if(any(grepl(separatorSession, x))){
    x2 <- strsplit(x, split = separatorSession)
    
    stationIDs <- sapply(x2, FUN = function(x)x[1])
    sessionIDs <- sapply(x2, FUN = function(x)x[2])
    
    return(data.frame(station = stationIDs,
                      session = sessionIDs, 
                      stringsAsFactors = FALSE))
  }
  
  if(any(grepl(separatorCam, x))){
    x2 <- strsplit(x, split = separatorCam)
    
    stationIDs <- sapply(x2, FUN = function(x)x[1])
    cameraIDs  <- sapply(x2, FUN = function(x)x[2])
    
    return(data.frame(station = stationIDs,
                      camera = cameraIDs, 
                      stringsAsFactors = FALSE))
  }
  return(data.frame(station = x, 
                    stringsAsFactors = FALSE))
}



# add NA columns to matrix to achieve a certain number of columns (for creating multi-seasons unmarked frames)
padMatrixWithNA <- function(mat, ncol_desired){
  
  if(ncol_desired < ncol(mat)){
    mat.out <- matrix(c(mat, 
                        rep(NA, times = nrow(mat) * (ncol_desired - ncol(mat)))),
                      ncol = ncol_desired, 
                      nrow = nrow(mat)
    )  
    rownames(mat.out) <- rownames(mat)
    colnames(mat.out) <- colnames(mat)
    return(mat.out)
  } else {
    return(mat)
  }
}

# for progress indicator in user messages (28 charcters wide in total)
makeProgressbar <- function(current, 
                            total){
  progress_bar_width <- 20
  perc <- current / total
  
  pb <- paste("      |", 
              paste(rep("=", times = round(perc * progress_bar_width)), collapse = ""), 
              paste(rep(" ", times = progress_bar_width - round(perc * progress_bar_width)), collapse = ""), 
              "|", 
              "  ", formatC(round(perc * 100), width = 3), "%", sep = "")
}


# access digiKam database and provide tables to extract species tags for videos
# call before exiftool

accessDigiKamDatabase <- function(db_directory,   # database directory 
                                  db_filename     # database filename
)
{
  # ensure database directory and file exist
  if(!dir.exists(db_directory)) stop("Could not find directory 'db_directory'", call. = FALSE)
  if(!file.exists(file.path(db_directory, db_filename))) stop("Could not find db_filename in db_directory", call. = FALSE)
  
  # establish database connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(db_directory, db_filename))
  
  # read tables
  Images      <- RSQLite::dbReadTable(con, 'Images')
  Tags        <- RSQLite::dbReadTable(con, 'Tags')
  ImageTags   <- RSQLite::dbReadTable(con, 'ImageTags')
  Albums      <- RSQLite::dbReadTable(con, 'Albums')
  AlbumRoots  <- RSQLite::dbReadTable(con, 'AlbumRoots')
  
#  ImageInformation  <- RSQLite::dbReadTable(con, 'ImageInformation')
#  ImageMetadata     <- RSQLite::dbReadTable(con, 'ImageMetadata')
  
  # disconnect database  
  RSQLite::dbDisconnect(con)
  
  # make output
  return(list(Albums           = Albums,
              AlbumRoots       = AlbumRoots,
              Images           = Images, 
              Tags             = Tags,
              ImageTags        = ImageTags#,
#              ImageInformation = ImageInformation,
#              ImageMetadata    = ImageMetadata
))
}



# extract species tags of videos from digiKam database tables

digiKamVideoHierarchicalSubject <- function(stationDir,
                                    digiKamTablesList,    # output of accessDigiKamDatabase
                                    videoFormat           # character vector of desired video formats
)
{
  
  Albums           <- digiKamTablesList$Albums
  AlbumRoots       <- digiKamTablesList$AlbumRoots
  Images           <- digiKamTablesList$Images
  Tags             <- digiKamTablesList$Tags
  ImageTags        <- digiKamTablesList$ImageTags

  # guess which AlbumRoot is correct, based on string distance (choose the smallest one)
  # adist(x = stationDir, y = AlbumRoots$specificPath)
  
  # add platform file separator to stationDir
  stationDir0 <- stationDir
  stationDir <- paste0(stationDir, .Platform$file.sep)
  
  # combine album root and album path (match by albumRoot id, not index position)
  Albums <- merge(Albums, AlbumRoots, by.x = "albumRoot", by.y = "id", sort = FALSE)
  Albums$albumPath_full <- paste0(Albums$specificPath, Albums$relativePath)
  
  # add drive letter (only relevant on Windows, and can potentially be wrong if there's Album roots on different drives)
  # also not sure if this works on Mac / Linux due to missing drive letters
  Albums$albumPath_full2 <- paste(substr(stationDir, 1,2),   # the Drive letter, digiKam doesn't return it
                                  Albums$albumPath_full,
                                  sep = "")
  
  # add "/" to ensure Station1 doesn't include Station10 also. Also ensure all folders end with one / only
  Albums$albumPath_full2 <- ifelse(endsWith(Albums$albumPath_full2, .Platform$file.sep),
                                   Albums$albumPath_full2,
                                   paste0(Albums$albumPath_full2, .Platform$file.sep)) 
  
  pathColumn <- "albumPath_full2"
  
  # see if stationDir exists in database
  if(!stationDir %in% Albums[, pathColumn]){
    stop(paste("station directory", stationDir,  "was not found in digiKam albums. Skipping"), call. = FALSE)
    # try to handle with a warning instead
  }
  
  # find current station in albums
  #album_of_interest <-Albums [which(Albums[, pathColumn] == stationDir),]   # only return the station directory, not camera subdirectories
  album_of_interest <- Albums [grep(pattern = stationDir, Albums[, pathColumn]),]   # This one returns Station directory and camera subdirectories
  if(nrow(album_of_interest) == 0) {
    warning("Could not locate album for ", stationDir, ". Skipping", call. = FALSE)   # NOTE TO SELF: DOESN'T SKIP OR BREAK. CHANGE?
  }
  
  # keep only images in the current album
  #image_subset <- Images[Images$album == album_of_interest$id,]   # only returns matches for 1 directory, not many (only station dir, not camera subdirs). Also, may return NAs
  image_subset <- Images[Images$album %in% album_of_interest$id,]  # returns matches for all directories. Also, no NAs apparently
  
  # add stationDirectory 
  image_subset$stationDir <- stationDir0
  
  # to do: handle situation where there's no images in image_subset
  
  # NAs are possible, so remove them
  if(any(is.na(image_subset$id))) {
    image_subset <- image_subset[!is.na(image_subset$id),]
  }
  
  # keep only desired video files
  image_subset2 <- image_subset[tolower(substr(image_subset$name, 
                                               nchar(image_subset$name) - 3, 
                                               nchar(image_subset$name))) %in% 
                                  paste(".", videoFormat, sep = ""),]
  
  image_subset_others <- image_subset[!tolower(substr(image_subset$name, 
                                               nchar(image_subset$name) - 3, 
                                               nchar(image_subset$name))) %in% 
                                  paste(".", videoFormat, sep = ""),]
  
  #warning if no videos found
  if(nrow(image_subset2) == 0) {
    warning("Could not find any ", paste(videoFormat, collapse = "/"), " files in ", stationDir, call. = FALSE)
  }
  
  # find "Species" tag group and its children
   
   # subset image tags
   ImageTags <- ImageTags[ImageTags$tagid %in% Tags$id,]
   
   # get proper labels for image tags (and their parent labels = tag group names)
   ImageTags$cleartext_child  <- Tags$name [match(ImageTags$tagid, Tags$id)]
   #ImageTags$cleartext_parent <- Tags$name [Tags$pid[match(ImageTags$tagid, Tags$id)]]        # this is wrong!
   # issue here: Tags$pid can be 0 if tag has no parent! Then indexing fails and functions errors
   
   Tags$parent_name <- Tags$name[match(Tags$pid, Tags$id)]
   ImageTags$cleartext_parent <- Tags$parent_name[match(ImageTags$tagid, Tags$id)]    #alternative to above, seems to work (and above seems wrong suddenly)
   
   
   # # # solution by Joel Ruprecht (Google group 2020-07-21) - if a station has no tags with parent ID, doesn't seem to work yet, but should be identical to above solution
   # parentNA <- which(is.na(Tags$name.parent))
   # Tags$name.parent[parentNA] <- Tags$name[parentNA]
   # ImageTags$cleartext_parent_Joel <- Tags$name.parent[match(ImageTags$tagid, Tags$id)]
   
   
   
   # combine parent and child to create HierarchicalTags
   ImageTags$cleartext_full <- paste(ImageTags$cleartext_parent, ImageTags$cleartext_child, sep = "|")
   
   
   # remove unnecessary (internal) tags (not essential)
    remove1 <- grep(Tags$name, pattern = "_Digikam_Internal_Tags_")
    remove2 <- grep(Tags$name, pattern = "Color Label ")
    remove3 <- grep(Tags$name, pattern = "Pick Label ")

    Tags <- Tags[!Tags$id  %in% c(remove1, remove2, remove3),]
    Tags <- Tags[!Tags$pid %in% c(remove1, remove2, remove3),]
   
   ImageTags <- ImageTags[!ImageTags$tagid %in% c(remove1, remove2, remove3),]

   # # if parent ID = NA (the tag has no parent), then save it without the NA|  (below works but will lead to problems parsing HierarchicalSubject). Requires some more testing
   # ImageTags$cleartext_full[is.na(ImageTags$cleartext_parent)] <- gsub(pattern = "NA|",
   #                                                                     replacement = "",
   #                                                                     ImageTags$cleartext_full[is.na(ImageTags$cleartext_parent)],
   #                                                                     fixed = TRUE)
   
   # combine multiple tags for images into single field "HierarchicalSubject"
   ImageTags_aggregate <- aggregate(ImageTags$cleartext_full,
                                    by = list(ImageTags$imageid),
                                    FUN = paste, sep  = "", collapse = ", ")

   # assign column names to output
   colnames(ImageTags_aggregate) <- c("imageid", "HierarchicalSubject")
   
   # assign HierarchicalSubject to matching images
   image_subset2$HierarchicalSubject <- ImageTags_aggregate$HierarchicalSubject[match(image_subset2$id, ImageTags_aggregate$imageid)]
   
   #data.frame(rbindlist(list(image_subset2, image_subset_others), fill = TRUE))


  return(image_subset2) 
}

# process the "video" argument of recordTable

processVideoArgument <- function(IDfrom = IDfrom,
                                 video = video){
  
  if(!exists("file_formats",  where = video)) stop("'file_formats' is missing in argument 'video'", call. = FALSE)
  if(!exists("dateTimeTag",   where = video)) stop("'dateTimeTag' is missing in argument 'video'", call. = FALSE)
  
  file_formats <- video$file_formats
  
  if(any(duplicated(file_formats))) stop("There are duplicates in file_formats (in the video argument")
  
  # check file_formats argument
  if(!is.character(file_formats)) stop("'file_formats' in argument 'video' must be of class 'character'", call. = FALSE)
  file_formats <- tolower(file_formats)
  
  # access digiKam database, if required
  if(IDfrom == "metadata"){
    if(!exists("db_directory", where = video)) stop("'db_directory' is missing in argument 'video'", call. = FALSE)
    if(!exists("db_filename",  where = video)) stop("'db_filename' is missing in argument 'video'", call. = FALSE)
    if(!dir.exists(video$db_directory)) stop("directory 'db_directory' does not exist", call. = FALSE)
    if(!file.exists(file.path(video$db_directory, video$db_filename))) stop(paste("file 'db_filename' in directory 'db_directory' does not exist\n",
                                                                                  file.path(video$db_directory, video$db_filename)), call. = FALSE)
    
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
      stop("the package 'RSQLite' is needed for extracting data from digiKam database,  you can install it via: install.packages('RSQLite')")
    } 
   
    # read digiKam database with helper function
    digiKam_data <- accessDigiKamDatabase (db_directory = video$db_directory,
                                           db_filename = video$db_filename)
  } else {    # if IDfrom != "metadata"
    digiKam_data <- NULL
  }
  return(list(digiKam_data = digiKam_data,
              file_formats = file_formats))
}


# if video files extracted, add DateTimeOriginal

addVideoDateTimeOriginal <- function(metadata.tmp,
                                     video){
  # if there's missing entries in DateTimeOriginal that are present in the video date time tag, copy the video tags over
  rows_of_interest1 <- which(metadata.tmp$DateTimeOriginal == "-" & 
                               metadata.tmp[,video$dateTimeTag] != "-")
  if(length(rows_of_interest1) >= 1) {
    metadata.tmp$DateTimeOriginal[rows_of_interest1] <- metadata.tmp[rows_of_interest1, video$dateTimeTag] 
  }
  metadata.tmp[, video$dateTimeTag] <- NULL
  
  return(metadata.tmp) 
}

# add HierachicalSubject for video files
addVideoHierarchicalSubject <- function(metadata.tmp,
                                        video,
                                        digiKamVideoMetadata,
                                        digiKamTablesList,
                                        videoFormat){
  
  if(nrow(digiKamVideoMetadata) == 0) return(metadata.tmp)
  # add HierarchialSubject for video files (match by filename, must be unique)
  # Old Version, this doesn't work if filenames are not unique
  #metadata.tmp$HierarchicalSubject_video <- digiKamVideoMetadata$HierarchicalSubject [match(metadata.tmp$FileName, digiKamVideoMetadata$name)]
  
  # new version, should match filenames AND paths in digiKamVideoMetadata with metadata.tmp (can deal with duplicate file names in separate folders)
  # get (partial) album path for videos in digiKamVideoMetadata (for station / camera directory)
  # NOTE: no idea how to get the drive letter from volumeid / uuid in AlbumRoots, so it is missing here
  album_name_tmp  <- digiKamTablesList$Albums[match(digiKamVideoMetadata$album, digiKamTablesList$Albums$id), ]
  album_name_tmp2 <- merge(digiKamTablesList$AlbumRoots, album_name_tmp, by.x = "id", by.y = "albumRoot")
  album_name_tmp3 <- paste0(album_name_tmp2$specificPath, album_name_tmp2$relativePath)
  
  # combine partial path and filename of extracted videos 
  digiKamVideoMetadata$almostFullPath <-  file.path(album_name_tmp3, digiKamVideoMetadata$name)
  
  
  # subset metadata.tmp to video files only
  metadata.tmp.video <- metadata.tmp[tolower(substr(metadata.tmp$FileName, 
                                                    nchar(metadata.tmp$FileName) - 3, 
                                                    nchar(metadata.tmp$FileName))) %in% 
                                       paste(".", videoFormat, sep = ""), ]
  
  metadata.tmp.notvideo <- metadata.tmp[!tolower(substr(metadata.tmp$FileName, 
                                                    nchar(metadata.tmp$FileName) - 3, 
                                                    nchar(metadata.tmp$FileName))) %in% 
                                       paste(".", videoFormat, sep = ""), ]
  
  
  metadata.tmp.video$fullPath <- file.path(metadata.tmp.video$Directory, metadata.tmp.video$FileName)
  
  # order metadata.tmp.video.fullPath and metadata.tmp.video.fullPath
  
  metadata.tmp.video <- metadata.tmp.video[order(metadata.tmp.video$fullPath),]
  digiKamVideoMetadata <- digiKamVideoMetadata[order(digiKamVideoMetadata$almostFullPath),]
  #match(metadata.tmp.video.fullPath, digiKamVideoMetadata$almostFullPath)
  
  
  # in metadata.tmp.video, strip drive letter (everything left of first forward slash)
  metadata.tmp.almostFullPath <- substr(metadata.tmp.video$fullPath, 
        start = (nchar(metadata.tmp.video$fullPath) - nchar(digiKamVideoMetadata$almostFullPath) + 1),
        stop = nchar(metadata.tmp.video$fullPath))

  # add metadata from digiKamVideoMetadata to metadata.tmp
  metadata.tmp.video$HierarchicalSubject_video <- digiKamVideoMetadata$HierarchicalSubject [match(metadata.tmp.almostFullPath, 
                                                                                              digiKamVideoMetadata$almostFullPath)]
  
  
  # find rows that have video metadata
  rows_of_interest2 <- which(!is.na(metadata.tmp.video$HierarchicalSubject_video) & 
                               metadata.tmp.video$HierarchicalSubject == "-")
  # write HierarchicalSubject of videos to the normal HierarchicalSubject column
  if(length(rows_of_interest2) >= 1) {
    metadata.tmp.video$HierarchicalSubject[rows_of_interest2] <- metadata.tmp.video$HierarchicalSubject_video[rows_of_interest2] 
  }
  
  
  metadata.tmp.out <- data.frame(rbindlist(list(metadata.tmp.video, metadata.tmp.notvideo), fill = TRUE))
  
  # remove column HierarchicalSubject_video
  metadata.tmp.out$HierarchicalSubject_video <- NULL
  metadata.tmp.out$fullPath <- NULL
  
  return(metadata.tmp.out)
}


# function to return fraction of day that has passed already at a given time (or its inverse, fraction of day remaining)
fractionOfDay <- function(time, type) {
  # time difference between time and midnight that day (fraction of the day that has passed already)
  delta <- as.numeric(difftime(time, as.Date(time), units = "days"))
  
  # return fraction of day that remains (for setup day)
  if(type == "after") return( 1 - delta)
  # return fraction of day that has passed (for retrieval day)
  if(type == "before")  return(delta)
}

# plot camera operation matrix (function from vignette 3)
camopPlot <- function(camOp, 
                      palette = "viridis",
                      lattice = FALSE){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  values_tmp <- sort(na.omit(unique(c(camOp))))
  
  # hcl.colors only exists in R >3.6.0, use heat.colors for earlier versions
  if(getRversion() >= "3.6.0") {
    image_colors <- grDevices::hcl.colors(n = length(values_tmp), palette = palette, rev = TRUE)
  } else {
    image_colors <- heat.colors(n = length(values_tmp), rev = TRUE)
  }

  
  # transpose and reverse x axis to make sure it plots in the same row/column order as toe original matrix
  if(nrow(camOp) > 1) {
    camop_for_plotting <- t(as.matrix(camOp)[seq(nrow(camOp) ,1),])
  } else {
    camop_for_plotting <- t(as.matrix(camOp))
  }
  
  # lattice::levelplot
  if(isTRUE(lattice)) {
    
    if (!requireNamespace("tibble", quietly = TRUE)) stop("package 'lattice' is required for levelplot (lattice = TRUE)")
    # generate color ramp 
    image_colors_lattice <- grDevices::hcl.colors(n = 100, palette = palette, rev = TRUE)
    
    
    # levelplot in lattice
    # to be improved: y axis labels (station IDs) are all drawn and can overlap. 
    # this may help: https://stat.ethz.ch/R-manual/R-devel/library/lattice/html/axis.default.html
    lattice::levelplot(camop_for_plotting,
              col.regions = image_colors_lattice,
              xlab = "", ylab ="",
              scales = list(x = list(at = which.tmp,
                                     labels = label.tmp, rot = 45)),
              aspect = "fill", 
    )

  } else {
    
    # grDevices::image()    quite basic and no legend
    image(camop_for_plotting, xaxt = "n", yaxt = "n", col = image_colors)
    axis(1, at = at.tmp, labels = label.tmp)
    axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rev(rownames(camOp)), las = 1)
    abline(v = at.tmp, col = rgb(0,0,0, 0.2))
    box()
  }
}

# intersect intervals, fast (adapted from lubridate)
#https://github.com/tidyverse/lubridate/blob/master/R/intervals.r
intersect.Interval.fast <- function(int1, int2, ...) {  # (x, y, ...) {
  #int1 <- int_standardize(x)
  #int2 <- int_standardize(y)
  
  starts <- pmax(int1@start, int2@start)
  ends <- pmin(int1@start + int1@.Data, int2@start + int2@.Data)
  spans <- as.numeric(ends) - as.numeric(starts)
  
  spans[spans < 0] <- NA
  spans
  # no.int <- ends < starts
  # no.int <- is.na(no.int) | no.int
  # spans[no.int] <- NA
  # starts[no.int] <- NA
  # 
  # new.int <- new("Interval", spans, start = starts, tzone = x@tzone)
  # negix <- !is.na(x@.Data) & (sign(x@.Data) == -1)
  # new.int[negix] <- int_flip(new.int[negix])
  # new.int
}
