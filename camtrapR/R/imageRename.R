imageRename <- function(inDir,
                        outDir,
                        hasCameraFolders,
                        keepCameraSubfolders,
                        createEmptyDirectories = FALSE,
                        copyImages = FALSE,
                        writecsv = FALSE){

  wd0 <- getwd()
  on.exit(setwd(wd0))

  file.sep <- .Platform$file.sep

  stationCol <- "Station"
  cameraCol  <- "Camera"

  # check call for consistency
  stopifnot(is.logical(copyImages))
  stopifnot(is.logical(writecsv))
  stopifnot(is.logical(hasCameraFolders))

    if(isTRUE(hasCameraFolders)){
      stopifnot(hasArg(keepCameraSubfolders))
      stopifnot(is.logical(keepCameraSubfolders))
   } else {
    keepCameraSubfolders <- FALSE
   }
   if(hasArg(hasCameraFolders) & hasArg(keepCameraSubfolders)){
    if(keepCameraSubfolders == TRUE & hasCameraFolders == FALSE){stop("If hasCameraFolders is FALSE, keepCameraSubfolders must be FALSE too", call. = FALSE)}
   }


  stopifnot(length(inDir) == 1)
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)

  if(hasArg(outDir)){
    stopifnot(length(outDir) == 1)
    if(isTRUE(all(unlist(strsplit(tolower(inDir), split = file.sep)) %in%
                  unlist(strsplit(tolower(outDir), split = file.sep))))) stop("outDir may not be identical to or a subdirectory of inDir", call. = FALSE)
    }
  if(copyImages == TRUE){

    if(any(c(grep("/$", inDir) == 1, grep("/$", outDir) == 1))) stop("inDir and outDir may not end with /", call. = FALSE)

  } else {
    if(isTRUE(grepl("/$", inDir))) stop("inDir may not end with /", call. = FALSE)
  }
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  if(isTRUE(writecsv) & hasArg(outDir) == FALSE) stop("writecsv is TRUE. Please specify outDir", call. = FALSE)




  # list of subdirectories of inDir
  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE , recursive = FALSE)

  # make sure none is empty
  list_n_files <- lapply(dirs, list.files, pattern = ".jpg$|.JPG$", recursive = TRUE)

  if(any(unlist(lapply(list_n_files, length)) == 0)){
    warning("at least one station directory contains no JPEGs:  ", paste(dirs_short[which(lapply(list_n_files, length) == 0)], collapse = "; "), call. = FALSE, immediate. = TRUE)
  }

  # remove dirs and dirs_short if they contain no images (and if user overrides default createEmptyDirectories = FALSE)
  if(!isTRUE(createEmptyDirectories)){
    stations2remove  <- which(lapply(list_n_files, length) == 0)
    if(length(stations2remove) >= 1){
      dirs          <- dirs[-stations2remove]
      dirs_short  <- dirs_short[-stations2remove]
    }
  }

  # function body

  copy.info.table <- data.frame()

  for(i in 1:length(dirs_short)){     # for all non-empty directories

    # check if there are any images not in camera trap subfolders
    if(hasCameraFolders == TRUE && length(list.files(dirs[i], pattern = ".jpg$|.JPG$", recursive = FALSE)) >= 1){
      stop(paste("Directory ", dirs[i], " contains images not sorted into Camera Trap subfolders. Check argument 'hasCameraFolders'"), call. = FALSE)
    }

    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")

    # run exiftool to get image date and time
    metadata.tmp <- runExiftool(command.tmp = command.tmp, colnames.tmp = colnames.tmp)


    if(length(metadata.tmp) == 0){
      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      warning(paste(dirs_short[i], "seems to contain no images;", " found", length.tmp, "jpgs"), call. = FALSE, immediate. = TRUE)    # give message if station directory contains no jpgs
    } else {

      message(paste(dirs_short[i], ":", nrow(metadata.tmp), "images"))

      if(isTRUE(hasCameraFolders)){
        filenames_by_subfolder <- lapply(as.list(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)),
                                         FUN = list.files, pattern = ".jpg$|.JPG$", recursive = TRUE, ignore.case = TRUE)
        metadata.tmp$CameraID <- rep(list.dirs(dirs[i], full.names = FALSE, recursive = FALSE),
                                     times = unlist(lapply(filenames_by_subfolder, length)))
        metadata.tmp$Station <- rep(dirs_short[i], times = nrow(metadata.tmp))
        colnames(metadata.tmp)[grep("CameraID", colnames(metadata.tmp))] <- cameraCol
        colnames(metadata.tmp)[grep("Station", colnames(metadata.tmp))] <- stationCol
      } else {
        metadata.tmp$CameraID <- "NA"                                                    # "camera" name if no camera id available (only station ids)
        metadata.tmp$Station <- rep(dirs_short[i], times = nrow(metadata.tmp))
        colnames(metadata.tmp)[grep("CameraID", colnames(metadata.tmp))] <- cameraCol
        colnames(metadata.tmp)[grep("Station", colnames(metadata.tmp))] <- stationCol
      }

      # make time readable
      metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = as.character(metadata.tmp$DateTimeOriginal),
                                                           format = "%Y:%m:%d %H:%M:%S", tz = "UTC"))
      metadata.tmp$DateTimeOriginal2 <- as.POSIXlt(metadata.tmp$DateTimeOriginal)

      # exclude images for which no DateTimeOriginal was found
      suppressWarnings(na.date.rows <- which(is.na(metadata.tmp$DateTimeOriginal)))
      if(length(na.date.rows) != 0){
        #metadata.tmp.na.date <- metadata.tmp[na.date.rows,]
        warning(paste("could not read DateTimeOriginal tag of: \n",
                    paste(paste(metadata.tmp$Directory,  metadata.tmp$FileName, sep = file.sep)[na.date.rows], collapse = "\n")),
                call. = FALSE, immediate. = TRUE)
        metadata.tmp <- data.frame(metadata.tmp, DateReadable = NA)
        metadata.tmp$DateReadable[-na.date.rows] <- TRUE
        metadata.tmp$DateReadable[na.date.rows]  <- FALSE
      } else {
        metadata.tmp$DateReadable <- TRUE
      }
      rm(na.date.rows)

      # rearrange column order
      metadata.tmp <- metadata.tmp[,c("Directory",  "FileName", stationCol, cameraCol,
                                      "DateTimeOriginal", "DateTimeOriginal2", "DateReadable")]

      # find images taken within 1 minute of one another (to append number)
      metadata.tmp$DateTimeOriginal2$sec <- 0

      if(isTRUE(hasCameraFolders)){
        metadata.tmp.split <- split(x = metadata.tmp, f = list(metadata.tmp[,cameraCol],
                                                               as.POSIXct(metadata.tmp$DateTimeOriginal2)
        ), drop = TRUE)
      } else {
        metadata.tmp.split <- split(x = metadata.tmp, f = list(metadata.tmp[,stationCol],
                                                               as.POSIXct(metadata.tmp$DateTimeOriginal2)
        ), drop = TRUE)
      }

      metadata.tmp.split2 <- lapply(metadata.tmp.split, FUN = function(X){
        X2 <- X[with(X, order(DateTimeOriginal)), ]
        cbind(X2, minute.append = seq(from = 1, to = nrow(X2), by = 1))
      })

      metadata.tmp2 <- do.call("rbind", metadata.tmp.split2)    # reassemble
      if(any(metadata.tmp$DateReadable == FALSE)){
        metadata.tmp2 <- rbind(cbind(metadata.tmp[metadata.tmp$DateReadable == FALSE,], minute.append = NA) ,metadata.tmp2)
      }
      rm(metadata.tmp.split, metadata.tmp.split2)

      # convert time object to character vector and format for outfilename
      time.tmp <- gsub(pattern = ":", replacement = "-", metadata.tmp2$DateTimeOriginal)
      time.tmp2 <- gsub(pattern = " ", replacement = "__", time.tmp)
      metadata.tmp2$DateTime_for_filename <- time.tmp2
      rm(time.tmp, time.tmp2)

      # create outfilename
      if(hasArg(outDir)){
        if(keepCameraSubfolders == TRUE){
          metadata.tmp2$outDir <- file.path(outDir, metadata.tmp2[,stationCol], metadata.tmp2[,cameraCol])
        } else {
          metadata.tmp2$outDir <- file.path(outDir, metadata.tmp2[,stationCol])
        }
      }

      if(isTRUE(hasCameraFolders)){
        metadata.tmp2$filename_new <- paste(paste(metadata.tmp2[,stationCol],
                                                  metadata.tmp2[,cameraCol],
                                                  paste(metadata.tmp2$DateTime_for_filename, "(",metadata.tmp2$minute.append, ")", sep = ""),
                                                  sep = "__"),
                                            ".JPG", sep = "")
      } else {
        metadata.tmp2$filename_new <- paste(paste(metadata.tmp2[,stationCol],
                                                  paste(metadata.tmp2$DateTime_for_filename, "(",metadata.tmp2$minute.append, ")", sep = ""),
                                                  sep = "__"),
                                            ".JPG", sep = "")
      }

      copy.info.table <- rbind(copy.info.table, metadata.tmp2)
      rm(metadata.tmp2)
    }
  }

  if(!any(copy.info.table$DateReadable)) stop("could not read DateTimeOriginal tag of any image. Check if the DateTimeOriginal tag is present in metadata with exifTagNames(..., returnMetadata = TRUE). If not, try fixing it with fixDateTimeOriginal()",
                                                      call. = FALSE)

    # create directory structure in outDir
      if(isTRUE(copyImages)){
          #sapply(unique(copy.info.table$outDir), dir.create, recursive = TRUE)   # old

          if(!isTRUE(keepCameraSubfolders))   dir2create <- file.path (outDir, dirs_short)    # outDir with station subdirectories

          if(isTRUE(keepCameraSubfolders)) {
            # create list of directories to create
            dirs_recursive <- lapply(dirs, FUN = list.dirs, recursive = TRUE, full.names = FALSE)
            names(dirs_recursive) <- dirs_short
            for(xyz in 1:length(dirs_recursive)){
              dirs_recursive[[xyz]] <- file.path(names(dirs_recursive)[[xyz]], dirs_recursive[[xyz]])
            }
            dirs_recursive <- unlist(dirs_recursive)

            dirs_recursive2 <- dirs_recursive[lapply(strsplit(dirs_recursive, file.sep), length) == 1 |  # find all entries with 1 items (all Stations) and
                                                           lapply(strsplit(dirs_recursive, file.sep), length) == 2]   # find all entries with 2 items (Station + Camera)
            dir2create <- file.path (outDir, dirs_recursive2)                                                                         # outDir with station and camera subdirectories
            }

            #if(isTRUE(createEmptyDirectories)) sapply(file.path(outDir, dirs_short), FUN = dir.create, recursive = TRUE, showWarnings = FALSE)     # create station directories
            sapply(dir2create, FUN = dir.create, recursive = TRUE, showWarnings = FALSE)           # create directories (recursively)
        }

        # check if renamed images exist already
        if(hasArg(outDir)) {
          copy.info.table$fileExistsAlready <- file.exists(file.path(copy.info.table$outDir, copy.info.table[,stationCol], copy.info.table$filename_new))
        } else {
          copy.info.table$fileExistsAlready <- FALSE
        }

          if(any(copy.info.table$fileExistsAlready)) {
          message(paste(sum(copy.info.table$fileExistsAlready), "out of", nrow(copy.info.table), "images existed already in outDir. They will not be copied"))
          }


      # copy images
      if(isTRUE(copyImages)){

         if(any(copy.info.table$fileExistsAlready)) {
          switch(menu(choices = c("Copy only images that are not in outDir", "Copy nothing"), title =  "outDir is not empty. What should I do?"),
                 proceed <- TRUE ,
                 proceed <- FALSE)
             } else {
                 proceed <- TRUE
             }

      if(isTRUE(proceed)){
      # find items to copy
          items_to_copy <- which(copy.info.table$DateReadable == TRUE & copy.info.table$fileExistsAlready == FALSE)

          message(paste("copying", length(items_to_copy), "images to", outDir, " ... This may take some time."))

          copy.info.table$CopyStatus[items_to_copy] <- file.copy(from      = apply(copy.info.table[items_to_copy, c("Directory", "FileName")],  MARGIN = 1, FUN = paste, collapse = file.sep),
                                                                                       to        = apply(copy.info.table[items_to_copy, c("outDir", "filename_new")], MARGIN = 1, FUN = paste, collapse = file.sep),
                                                                                       overwrite = FALSE)
          copy.info.table$CopyStatus[-items_to_copy] <- FALSE
        } else {
          copy.info.table$CopyStatus <- FALSE
        }
      } else {
        copy.info.table$CopyStatus <- FALSE
      }

  rownames(copy.info.table) <- NULL
  copy.info.table <- copy.info.table[,-which(names(copy.info.table) %in%
                                               c("DateTimeOriginal2", "DateTime_for_filename", "minute.append"))]
  # save table
  if(writecsv == TRUE){
    dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
    setwd(outDir)
    write.csv(copy.info.table, file = paste("_renaming_table_", Sys.Date(), ".csv", sep = ""),
              row.names = FALSE)
  }
  return(copy.info.table)
}