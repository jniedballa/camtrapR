#' Add or remove species names from JPEG image filenames
#' 
#' Add or remove species names from JPEG image filenames. It makes it easier to
#' find images of a species.
#' 
#' Species names can be appended or removed from image filenames. Before
#' running the function, you may want to run
#' \code{\link{checkSpeciesIdentification}} to detect possible
#' misidentifications. As an example, the function would change an image file
#' name from "StationA__2015-05-41__20-59-59(1).JPG" to
#' "StationA__2015-05-41__20-59-59(1)__Species Name.JPG". If species names were
#' appended several times by accident, they can all be removed by running the
#' function with \code{removeNames = TRUE}
#' 
#' @param inDir character. Directory containing camera trap images sorted into
#' station subdirectories (e.g. inDir/StationA/)
#' @param IDfrom character. Read species ID from image metadata ("metadata") of
#' from species directory names ("directory")?
#' @param hasCameraFolders logical. Do the station subdirectories of
#' \code{inDir} have camera-subdirectories (e.g. inDir/StationA/CameraA1;
#' inDir/StationA/CameraA2)?
#' @param metadataSpeciesTag character. The species ID tag name in image
#' metadata (if IDfrom = "metadata").
#' @param metadataHierarchyDelimitor character. The character delimiting
#' hierarchy levels in image metadata tags in field "HierarchicalSubject".
#' Either "|" or ":".
#' @param removeNames logical. remove appended species names?
#' @param writecsv logical. write csv table containing old and new file names
#' into \code{inDir}?
#' 
#' @return A \code{data.frame} containing the old and new file names and
#' directories.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # copy sample images to another location (so we don't mess around in the package directory)
#' wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
#' file.copy(from = wd_images_ID, to = getwd(), recursive = TRUE)       
#' wd_images_ID_copy <- file.path(getwd(), "sample_images_species_dir")
#' 
#' # append species names
#' SpecNameAppend1 <- appendSpeciesNames(inDir            = wd_images_ID_copy,
#'                                       IDfrom           = "directory",
#'                                       hasCameraFolders = FALSE,
#'                                       removeNames      = FALSE,
#'                                       writecsv         = FALSE)
#'   
#' SpecNameAppend1
#' 
#' # remove species names
#' SpecNameRemove1 <- appendSpeciesNames(inDir            = wd_images_ID_copy,
#'                                       IDfrom           = "directory",
#'                                       hasCameraFolders = FALSE,
#'                                       removeNames      = TRUE,
#'                                       writecsv         = FALSE)
#'   
#' SpecNameRemove1
#' }
#' 
#' @export appendSpeciesNames
#' 
appendSpeciesNames <- function(inDir,
                               IDfrom,
                               hasCameraFolders,
                               metadataSpeciesTag,
                               metadataHierarchyDelimitor = "|",
                               removeNames = FALSE,
                               writecsv = FALSE
)
{
  wd0 <- getwd()
  on.exit(setwd(wd0))
  # check inpuit
  stopifnot(is.logical(removeNames))
  stopifnot(is.logical(writecsv))
  stopifnot(is.logical(hasCameraFolders))

  file.sep <- .Platform$file.sep

  if(!is.character(IDfrom)){stop("IDfrom must be of class 'character'")}
  if(IDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

  if(IDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")
    metadata.tagname <- "HierarchicalSubject"

    if(!hasArg(metadataSpeciesTag))       stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")
    if(!is.character(metadataSpeciesTag)) stop("metadataSpeciesTag must be of class 'character'")
    if(length(metadataSpeciesTag) != 1)   stop("metadataSpeciesTag must be of length 1")
    if(Sys.which("exiftool") == "")       stop("cannot find ExifTool")
  }

multiple_tag_separator = "__"

  ## find station directories
  dirs       <- list.dirs(inDir, full.names = TRUE,  recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)

  if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)

  renaming.table <- data.frame()
  nrow.metadata.tmp <- vector()


  # loop over all station directories
  for(i in 1:length(dirs)){

    if(IDfrom == "directory"){     # ID via species directories

      dirlist.i <- list.dirs(dirs[i], full.names = TRUE, recursive = FALSE)      # list directories in station directory (camera or species)


      if(hasCameraFolders == FALSE){
        filenames.by.folder <- lapply(dirlist.i,                 # list species directories
                                      FUN         = list.files,
                                      pattern     = ".jpg$|.JPG$",
                                      recursive   = TRUE,
                                      ignore.case = TRUE)
        names(filenames.by.folder) <- dirlist.i

      } else {
        dirlist.k <- list.dirs(dirlist.i, full.names = TRUE, recursive = FALSE)

        filenames.by.folder <- lapply(dirlist.k,                   # list species directories in camera directories
                                      FUN         = list.files,
                                      pattern     = ".jpg$|.JPG$",
                                      recursive   = TRUE,
                                      ignore.case = TRUE)
        names(filenames.by.folder) <- dirlist.k
        rm(dirlist.k)
      }

      rm(dirlist.i)

      folders.tmp <- rep(names(filenames.by.folder),
                         times = lapply(filenames.by.folder, FUN = length))

      species.tmp <- rep(unlist(lapply(strsplit(names(filenames.by.folder),
                                                split = file.sep,
                                                fixed = TRUE),
                                       FUN = function(X){X[length(X)]})),
                         times = lapply(filenames.by.folder, length))

      if(hasCameraFolders == TRUE){
        camera.tmp <- rep(unlist(lapply(strsplit(names(filenames.by.folder),
                                                 split = file.sep,
                                                 fixed = TRUE),
                                        FUN = function(X){X[length(X) - 1]})),
                          times = lapply(filenames.by.folder, length))
      }

      if(hasCameraFolders == FALSE){
        renaming.table <- rbind(renaming.table, data.frame(directory    = folders.tmp,
                                                           filename_old = unlist(filenames.by.folder),
                                                           species      = species.tmp))
      } else {
        renaming.table <- rbind(renaming.table, data.frame(directory    = folders.tmp,
                                                           filename_old = unlist(filenames.by.folder),
                                                           species      = species.tmp,
                                                           camera       = camera.tmp))
      }
      rownames(renaming.table) <- NULL


    } else {     #  if ID via metadata


      command.tmp  <- paste('exiftool -t -q -r -f -Directory -FileName -HierarchicalSubject -ext JPG "', dirs[i], '"', sep = "")
      colnames.tmp <- c("Directory", "FileName", "HierarchicalSubject")


      # run exiftool and make data frame
      metadata.tmp <- runExiftool(command.tmp = command.tmp, colnames.tmp = colnames.tmp)


      if(is.data.frame(metadata.tmp)){
        # if(IDfrom == "directory"){
          # message(paste(dirs_short[i], ": ", nrow(metadata.tmp), "images"))
        # }

        # store number of images
        nrow.metadata.tmp[[i]] <- nrow(metadata.tmp)

        # add metadata
        metadata.tmp <- addMetadataAsColumns (intable                    = metadata.tmp,
                                              metadata.tagname           = metadata.tagname,
                                              metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                              multiple_tag_separator     = multiple_tag_separator)

        # assign species ID
        metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,
                                         IDfrom                 = "metadata",
                                         metadataSpeciesTag     = metadataSpeciesTag,
                                         speciesCol             = "species",
                                         dirs_short             = dirs_short,
                                         i_tmp                  = i,
                                         multiple_tag_separator = "_&_",   # this is different from the one defined above to prevent (!) separating multiple entries in the same image: Species will be something like "Leopard Cat__Malay Badger"
                                         returnFileNamesMissingTags = FALSE)

      # if images in station contain not metadata species tags, skip that station
      if(metadata.tmp == "found no species tag") {
        warning(paste(dirs_short[i], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata tag 'HierarchicalSubject'. Skipping", sep = ""), call. = FALSE, immediate. = TRUE)
        next
      }

        # assign camera ID
        if(hasCameraFolders == TRUE){
          if(IDfrom == "directory"){
            metadata.tmp$camera  <- sapply(strsplit(metadata.tmp$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X) - 1]})
          } else {
            metadata.tmp$camera  <- sapply(strsplit(metadata.tmp$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X)]})
          }
        }

        # add station ID and assemble table
        metadata.tmp <- cbind(station = rep(dirs_short[i], times = nrow(metadata.tmp)),
                              metadata.tmp)



        if(hasCameraFolders == TRUE){
          renaming.table <- rbind(renaming.table, data.frame(directory    = metadata.tmp$Directory,
                                                             filename_old = metadata.tmp$FileName,
                                                             species      = metadata.tmp$species,
                                                             camera       = metadata.tmp$camera))
        } else {
          renaming.table <- rbind(renaming.table, data.frame(directory    = metadata.tmp$Directory,
                                                             filename_old = metadata.tmp$FileName,
                                                             species      = metadata.tmp$species))
        }
        rownames(renaming.table) <- NULL

      } # end if(class(metadata.tmp) == "data.frame"){
    }   # end ID via metadata
  }     # end for (i...)

      # construct new filename
      if(removeNames == FALSE){
        renaming.table$filename_new <- paste(paste(gsub(pattern     = ".jpg|.JPG$",
                                                        replacement = "",
                                                        x           = renaming.table$filename_old,
                                                        ignore.case = TRUE),
                                                   renaming.table$species,
                                                   sep   = "__"),
                                             ".JPG", sep = "")
      }
      if(removeNames == TRUE){
        # find all the species names ("__SpeciesName") and remove
        # it not  work if you changed species ID after assigning species names
        renaming.table$filename_new <- gsub(pattern     = paste(paste("__", unique(renaming.table$species),
                                                                      sep      = ""),
                                                                      collapse = "|"),
                                            replacement = "",
                                            x           = renaming.table$filename_old,
                                            ignore.case = TRUE)
      }


  # rename
  file.rename(from = file.path(renaming.table$directory, renaming.table$filename_old),
              to   = file.path(renaming.table$directory, renaming.table$filename_new))

  renaming.table$renamed <- renaming.table$filename_old != renaming.table$filename_new

  # write outtable
  if(writecsv == TRUE){
    if(removeNames == TRUE){
      filename.tmp <- paste("renaming_table_UNDO_", Sys.Date(), ".csv", sep = "")
    } else {
      filename.tmp <- paste("renaming_table_", Sys.Date(), ".csv", sep = "")
    }
    setwd(inDir)
    write.csv(renaming.table, file = filename.tmp, row.names = FALSE)
  }



  if(IDfrom == "directory"){
    message(paste("renamed", sum(renaming.table$renamed), "out of", nrow(renaming.table), "images in", inDir))
  } else {
    message(paste("renamed", sum(renaming.table$renamed),
      "out of", nrow(renaming.table), "images with species ID out of", sum(unlist(nrow.metadata.tmp), na.rm = TRUE), "images in", inDir))
  }

  return(renaming.table)

}
