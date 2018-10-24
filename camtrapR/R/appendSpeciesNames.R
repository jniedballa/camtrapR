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
  
  if(class(IDfrom) != "character"){stop("IDfrom must be of class 'character'")}
  if(IDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

  if(IDfrom == "metadata"){    
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")
    metadata.tagname <- "HierarchicalSubject"

    if(!hasArg(metadataSpeciesTag)) {stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")}
    if(class(metadataSpeciesTag) != "character"){stop("metadataSpeciesTag must be of class 'character'")}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1")}
  }

multiple_tag_separator = "__"

  ## find station directories
  dirs       <- list.dirs(inDir, full.names = TRUE,  recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)

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


      if(class(metadata.tmp) == "data.frame"){
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
                                         multiple_tag_separator = "_&_"   # this is different from the one defined above to prevent (!) separating multiple entries in the same image: Species will be something like "Leopard Cat__Malay Badger"
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