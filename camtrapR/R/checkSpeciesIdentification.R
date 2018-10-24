checkSpeciesIdentification <- function(inDir,
                                       IDfrom,
                                       hasCameraFolders,
                                       metadataSpeciesTag,
                                       metadataSpeciesTagToCompare,
                                       metadataHierarchyDelimitor = "|",
                                       maxDeltaTime,
                                       excludeSpecies,
                                       stationsToCheck,
                                       writecsv = FALSE
)
{
  wd0 <- getwd()
  on.exit(setwd(wd0))

  if(Sys.which("exiftool") == "") stop("cannot find ExifTool")
  if(hasArg(excludeSpecies)){
    if(class(excludeSpecies) != "character") stop("excludeSpecies must be of class 'character'")
  }
  if(hasArg(stationsToCheck)){
    if(class(stationsToCheck) != "character") stop("stationsToCheck must be of class 'character'")
  }
  stopifnot(is.logical(hasCameraFolders))

  stopifnot(is.numeric(maxDeltaTime))

  file.sep <- .Platform$file.sep

 if(class(IDfrom) != "character"){stop("IDfrom must be of class 'character'")}
 if(IDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

 if(IDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")
    metadata.tagname <- "HierarchicalSubject"

    if(!hasArg(metadataSpeciesTag)) {stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")}
    if(class(metadataSpeciesTag) != "character"){stop("metadataSpeciesTag must be of class 'character'")}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1")}

    if(hasArg(metadataSpeciesTagToCompare)) {
      if(class(metadataSpeciesTagToCompare) != "character"){stop("metadataSpeciesTagToCompare must be of class 'character'")}
      if(length(metadataSpeciesTagToCompare) != 1){stop("metadataSpeciesTagToCompare must be of length 1")}
    }
  }

  multiple_tag_separator <- "_&_"

  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
  # find station directories
  dirs       <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)

  check_table <- conflict_ID_table <-  data.frame(stringsAsFactors = FALSE)

  # if only checking certain station, subset dirs/dirs_short
  if(hasArg(stationsToCheck)){
    whichStationToCheck <- which(dirs_short %in% stationsToCheck)
    if(length(whichStationToCheck) == 0) {stop("found no directories of names specified in stationsToCheck")} else {
      dirs       <- dirs      [whichStationToCheck]
      dirs_short <- dirs_short[whichStationToCheck]
    }
  }


  for(i in 1:length(dirs)){

    if(IDfrom == "directory"){
      dirs.to.check.sho <- list.dirs(dirs[i], full.names = FALSE)[-1]
      dirs.to.check     <- list.dirs(dirs[i], full.names = TRUE)[-1]
      if(hasArg(excludeSpecies)){
        dirs.to.check     <- dirs.to.check    [!dirs.to.check.sho %in% excludeSpecies]
        dirs.to.check.sho <- dirs.to.check.sho[!dirs.to.check.sho %in% excludeSpecies]
      }
    }

    # remove empty species directories
#     empty_dirs <- sapply(dirs.to.check, FUN = function(X){length(list.files(X)) == 0})
#     if(any(empty_dirs)){
#       dirs.to.check <- dirs.to.check[-empty_dirs]
#       dirs.to.check.sho <- dirs.to.check.sho[-empty_dirs]
#     }

    # create command line for exiftool execution

    if(IDfrom == "directory"){
      if(hasArg(excludeSpecies)) {   # under some rare circumstances, this caused an error if directories were empty
        command.tmp <- paste('exiftool -t -q -r -f -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "', paste(dirs.to.check, collapse = '" "'), '"', sep = "")
      } else {
        command.tmp <- paste('exiftool -t -q -r -f -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "', dirs[i], '"', sep = "")
      }
    } else {
      command.tmp <- paste('exiftool -t -q -r -f -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "', dirs[i], '"', sep = "")
    }
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject")

    # run exiftool and make data frame
    metadata.tmp <- runExiftool(command.tmp = command.tmp, colnames.tmp = colnames.tmp)


    if(class(metadata.tmp) == "data.frame"){
		if(IDfrom == "directory"){
      message(paste(dirs_short[i], ": checking", nrow(metadata.tmp), "images in", length(dirs.to.check.sho), "directories"))
		}
      # write metadata from HierarchicalSubject field to individual columns
      if(IDfrom == "metadata"){
        metadata.tmp <- addMetadataAsColumns (intable                    = metadata.tmp,
                                              metadata.tagname           = metadata.tagname,
                                              metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                              multiple_tag_separator     = multiple_tag_separator)
      }


      # assign species ID
      metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,
                                       IDfrom                 = IDfrom,
                                       metadataSpeciesTag     = metadataSpeciesTag,
                                       speciesCol             = "species",
                                       dirs_short             = dirs_short,
                                       i_tmp                  = i,
                                       multiple_tag_separator = multiple_tag_separator
      )

      # if images in station contain no metadata species tags, skip that station
      if(class(metadata.tmp) != "data.frame"){
        if(metadata.tmp == "found no species tag") {
          warning(paste(dirs_short[i], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata tag 'HierarchicalSubject'. Skipping", sep = ""), call. = FALSE, immediate. = TRUE)
        } else {
          warning(paste(dirs_short[i], ":   error in species tag extraction. Skipping. Please report", sep = ""), call. = FALSE, immediate. = TRUE)
        }
        next
      }

      # exclude species if using metadata tags (if using IDfrom = "directory", they were removed above already)
      if(IDfrom == "metadata"){
        if(hasArg(excludeSpecies)){
          metadata.tmp <- metadata.tmp[!metadata.tmp$species %in% excludeSpecies,]
        }
      }

      # assign camera ID
      if(IDfrom == "directory" & hasCameraFolders == TRUE){
        metadata.tmp$camera  <- sapply(strsplit(metadata.tmp$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X) - 1]})
      }
      if(IDfrom == "metadata" & hasCameraFolders == TRUE){
        metadata.tmp$camera  <- sapply(strsplit(metadata.tmp$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X)]})
      }


      # make date/time R-readable
      metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"))

      # add station ID and assemble table
      metadata.tmp <- cbind(station = rep(dirs_short[i], times = nrow(metadata.tmp)),
                            metadata.tmp)


      # compare ID between different observers
      if(hasArg(metadataSpeciesTagToCompare)){
        metadataSpeciesTag2 <- paste("metadata", metadataSpeciesTag, sep = "_")
        metadataSpeciesTagToCompare2 <- paste("metadata", metadataSpeciesTagToCompare, sep = "_")
        if(metadataSpeciesTagToCompare2 %in% colnames(metadata.tmp)){
          metadata.tmp.conflict <- metadata.tmp[metadata.tmp[,metadataSpeciesTag2] != metadata.tmp[,metadataSpeciesTagToCompare2] |
                                                is.na(metadata.tmp[,metadataSpeciesTag2] != metadata.tmp[,metadataSpeciesTagToCompare2]) ,]
          metadata.tmp.conflict <- metadata.tmp.conflict[,which(colnames(metadata.tmp.conflict) %in% c("station", "Directory", "FileName", metadataSpeciesTag2, metadataSpeciesTagToCompare2))]
          # if anything to report, append to main table
          if(nrow(metadata.tmp.conflict) >= 1){
            conflict_ID_table <- rbind(conflict_ID_table, metadata.tmp.conflict)
          }
        } else {warning(paste("metadata tag '", metadataSpeciesTagToCompare, "' was not found in image metadata in Station ", dirs_short[i], sep = ""), call. = FALSE, immediate. = TRUE)}
        suppressWarnings(rm(metadataSpeciesTag2, metadataSpeciesTagToCompare2, metadata.tmp.conflict))
      }

      # calculate minimum delta time between image and all images in other species folders at station i
      if(length(unique(metadata.tmp$species)) >= 2){

        for(rowindex in 1:nrow(metadata.tmp)){

          if(hasCameraFolders == TRUE){
            # only compare within a camera folder if there was >1 camera per station
            which.tmp1 <- which(metadata.tmp$species != metadata.tmp$species[rowindex] &
                                metadata.tmp$camera  == metadata.tmp$camera[rowindex])
            if(length(which.tmp1) >= 1){
              metadata.tmp$min.delta.time[rowindex] <- round(min(abs(difftime(time1 = metadata.tmp$DateTimeOriginal[rowindex],
                                                                              time2 = metadata.tmp$DateTimeOriginal[which.tmp1],
                                                                              units = "secs"))))
            } else {
              metadata.tmp$min.delta.time[rowindex] <- NA
            }
            rm(which.tmp1)
          } else {         # if no camera subfolders
            # compare to other species
            which.tmp2 <- which(metadata.tmp$species != metadata.tmp$species[rowindex])
            if(length(which.tmp2) >= 1){
              metadata.tmp$min.delta.time[rowindex] <- round(min(abs(difftime(time1 = metadata.tmp$DateTimeOriginal[rowindex],
                                                                              time2 = metadata.tmp$DateTimeOriginal[which.tmp2],
                                                                              units = "secs"))))
            } else {
              metadata.tmp$min.delta.time[rowindex] <- NA
            }
            rm(which.tmp2)
          } # end ifelse hasCameraFolders
        }   # end for

        if(hasCameraFolders == TRUE){
          check_table_tmp <- metadata.tmp[metadata.tmp$min.delta.time <= maxDeltaTime & !is.na(metadata.tmp$min.delta.time), c("station", "Directory", "FileName", "species", "DateTimeOriginal", "camera")]
        } else {
          check_table_tmp <- metadata.tmp[metadata.tmp$min.delta.time <= maxDeltaTime & !is.na(metadata.tmp$min.delta.time), c("station", "Directory", "FileName", "species", "DateTimeOriginal")]
        }
        # order output
        check_table_tmp <- check_table_tmp[order(check_table_tmp$DateTimeOriginal),]

        # if anything to report, append to main table
        if(nrow(check_table_tmp) >= 1){
          check_table <- rbind(check_table, check_table_tmp)
        }
        suppressWarnings(rm(metadata.tmp, check_table_tmp))

      }  # end  if(length(unique(metadata.tmp$species)) >= 2){
    }    # end if(class(metadata.tmp) == "data.frame"){
  }      # end for (i ...)

  if(writecsv == TRUE){
    check_table_filename <- paste("species_ID_check_", Sys.Date(), ".csv", sep = "")
    conflict_table_filename <- paste("species_ID_conflicts_", Sys.Date(), ".csv", sep = "")
    setwd(inDir)
    write.csv(check_table, file = check_table_filename)
    write.csv(conflict_ID_table, file = conflict_table_filename)
  }

  # make output list
    outlist <- list(check_table, conflict_ID_table)
    names(outlist) <- c("temporalIndependenceCheck", "IDconflictCheck")

  return(outlist)

}