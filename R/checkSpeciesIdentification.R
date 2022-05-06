#' Consistency check on species image identification
#' 
#' This function serves 2 purposes: 1) it assesses possible misidentification
#' of species and 2) compares double observer species identification (only if
#' metadata tagging was used for species identification).
#' 
#' Within each station, it assesses whether there are images of a species taken
#' within a given time interval of another species. Often, it is unlikely that
#' different species are encountered within a very short time intervals at the
#' same location. This type of misidentification can arise easily if some
#' images belonging to a sequence of images were accidentally moved into
#' different species directories or tagged incorrectly.
#' 
#' Double observer identification may be desirable to increase reliability of
#' species identification. The function returns conflicts in species
#' identification between 2 observers. These conflicts can then be corrected.
#' 
#' Images may accidentally be misidentified by assigning wrong species tags or
#' by moving them into wrong species directories. Imagine your cameras take
#' sequences of images each time they are triggered and one image of the
#' sequence is misidentified. The time difference between these images (that
#' have different species assigned to them) will be very small, usually a few
#' seconds. This function will return all these images for you to check if they
#' were identified correctly.
#' 
#' If multiple observers identify images independently using metadata tagging,
#' their identifications can be compared by setting
#' \code{metadataSpeciesTagToCompare}. Conflicting or missing identifications
#' will be reported. This feature is only available if images were identified
#' by metadata tagging.
#' 
#' Species like "blank" or "team" can be ignored using \code{excludeSpecies}.
#' If only specific stations are to be checked, \code{stationsToCheck} can be
#' set.
#' 
#' @param inDir character. Directory containing identified camera trap images
#' sorted into station subdirectories (e.g. inDir/StationA/)
#' @param IDfrom character. Read species ID from image metadata ("metadata") of
#' from species directory names ("directory")?
#' @param hasCameraFolders logical. Do the station directories in \code{inDir}
#' have camera subdirectories (e.g. "inDir/StationA/Camera1" or
#' "inDir/StationA/Camera1/Species1")?
#' @param metadataSpeciesTag character. The species ID tag name in image
#' metadata (if IDfrom = "metadata").
#' @param metadataSpeciesTagToCompare character. A second species ID tag name
#' in image metadata (if IDfrom = "metadata"). For comparing double observer
#' species identification.
#' @param metadataHierarchyDelimitor character. The character delimiting
#' hierarchy levels in image metadata tags in field "HierarchicalSubject".
#' Either "|" or ":"
#' @param maxDeltaTime numeric. Maximum time interval between images to be
#' returned (in seconds)
#' @param excludeSpecies character. vector of species to exclude from checks
#' @param stationsToCheck character. vector of stations to be checked
#' (optionally)
#' @param writecsv logical. Should the resulting data.frame be saved as a .csv?
#' 
#' @return A \code{list} containing 2 data frames. The first contains a data
#' frame with images file names, directories, time stamp and species ID that
#' were taken within \code{maxDeltaTime} seconds of another species image at a
#' particular station. The second data frame contains images with conflicting
#' species IDs (if \code{IDfrom = "metadata"} and
#' \code{metadataSpeciesTagToCompare} is defined)
#' 
#' @note The function will not be able to find "isolated" images, i.e. images
#' that were misidentified, but were not part of a sequence of images.
#' Likewise, if all images of a sequence were misidentified, they cannot be
#' found either. From version 0.99.0, the function can also handle images
#' identied with metadata tags.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' 
#' \donttest{
#' wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
#' 
#' if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
#' check.folders <- checkSpeciesIdentification(inDir             = wd_images_ID,
#'                                             IDfrom            = "directory",
#'                                             hasCameraFolders  = FALSE,
#'                                             maxDeltaTime      = 120,
#'                                             writecsv          = FALSE)
#'                                      
#' check.folders   # In the example, 2 different species were photographed within 2 minutes. 
#' }
#' }
#' 
#' \dontrun{
#' # now exclude one of these 2 species 
#' check.folders2 <- checkSpeciesIdentification(inDir             = wd_images_ID,
#'                                              IDfrom            = "directory",
#'                                              hasCameraFolders  = FALSE,
#'                                              maxDeltaTime      = 120,
#'                                              excludeSpecies    = "EGY",
#'                                              writecsv          = FALSE)           
#'   
#' check.folders2   # the data frame is empty
#' 
#' # now we check only one station
#' check.folders3 <- checkSpeciesIdentification(inDir             = wd_images_ID,
#'                                              IDfrom            = "directory",
#'                                              hasCameraFolders  = FALSE,
#'                                              maxDeltaTime      = 120,
#'                                              stationsToCheck   = "StationB",
#'                                              writecsv          = FALSE)
#' check.folders3   # the data frame is empty
#' }
#' 
#' 
#' @export checkSpeciesIdentification
#' 
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
    if(!is.character(excludeSpecies)) stop("excludeSpecies must be of class 'character'")
  }
  if(hasArg(stationsToCheck)){
    if(!is.character(stationsToCheck)) stop("stationsToCheck must be of class 'character'")
  }
  stopifnot(is.logical(hasCameraFolders))

  stopifnot(is.numeric(maxDeltaTime))

  file.sep <- .Platform$file.sep

 if(!is.character(IDfrom)){stop("IDfrom must be of class 'character'")}
 if(IDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

 if(IDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")
    metadata.tagname <- "HierarchicalSubject"

    if(!hasArg(metadataSpeciesTag)) {stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")}
    if(!is.character(metadataSpeciesTag)){stop("metadataSpeciesTag must be of class 'character'")}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1")}

    if(hasArg(metadataSpeciesTagToCompare)) {
      if(!is.character(metadataSpeciesTagToCompare)){stop("metadataSpeciesTagToCompare must be of class 'character'")}
      if(length(metadataSpeciesTagToCompare) != 1){stop("metadataSpeciesTagToCompare must be of length 1")}
    }
  }

  multiple_tag_separator <- "_&_"

  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
  # find station directories
  dirs       <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)

  if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)

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


    if(inherits(metadata.tmp, "data.frame")){
		if(IDfrom == "directory"){
      message(paste(dirs_short[i], ": checking", nrow(metadata.tmp), "images in", length(dirs.to.check.sho), "directories"))
		}
      # write metadata from HierarchicalSubject field to individual columns
      if(IDfrom == "metadata"){
        message(paste(dirs_short[i], ": ", formatC(nrow(metadata.tmp), width = 4), " images", 
                      makeProgressbar(current = i, total = length(dirs_short)), sep = ""))
        
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
                                       multiple_tag_separator = multiple_tag_separator,
                                       returnFileNamesMissingTags = FALSE
      )

      # if images in station contain no metadata species tags, skip that station
      if(!inherits(metadata.tmp, "data.frame")){
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
    }    # end  if(inherits(metadata.tmp, "data.frame")){
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
