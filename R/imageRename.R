#' Copy and rename images based on camera trap station ID and creation date
#' 
#' The function renames and copies raw camera trap images into a new location
#' where they can be identified. Images are renamed with camera trap station
#' ID, camera ID (optional), creation date and a numeric identifier for images
#' taken within one minute of each other at a given station. Station ID and
#' camera ID are derived from the raw image directory structure. The creation
#' date is extracted from image metadata using ExifTool.
#' 
#' Setting up the correct raw image directory structure is necessary for
#' running the function successfully. \code{inDir} is the main directory that
#' contains camera trap station subdirectories (e.g. inDir/StationA). If one
#' camera was deployed per station and no camera subdirectories are used within
#' station directories, \code{hasCameraFolders} can be set to \code{FALSE}. If
#' more than one camera was deployed at stations, there must be subdirectories
#' for the individual camera traps within the station directories (e.g.
#' "inDir/StationA/CameraA1" and "inDir/StationA/CameraA2"). Even if only some
#' stations had multiple cameras, all station will need camera subdirectories.
#' The argument \code{hasCameraFolders} must be \code{TRUE}. Within the camera
#' subdirectories, the directory structure is irrelevant.
#' 
#' Renaming of images follows the following pattern: If \code{hasCameraFolders}
#' is TRUE, it is: "StationID__CameraID__Date__Time(Number).JPG", e.g.
#' "StationA__CameraA1__2015-01-31__18-59-59(1).JPG". If
#' \code{hasCameraFolders} is FALSE, it is:
#' "StationID__Date__Time(Number).JPG", e.g.
#' "StationA__2015-01-31__18-59-59(1).JPG".
#' 
#' The purpose of the number in parentheses is to prevent assigning identical
#' file names to images taken at the same station (and camera) in the same
#' second, as can happen if cameras take sequences of images. It is a
#' consecutive number given to all images taken at the same station by the same
#' camera within one minute. The double underscore "__" in the image file names
#' is for splitting and extracting information from file names in other
#' functions (e.g. for retrieving camera IDs in \code{\link{recordTable}} if
#' camera subdirectories are not preserved (\code{keepCameraSubfolders =
#' FALSE})).
#' 
#' The function finds all JPEG images (optionally, also videos) and extracts 
#' the image timestamp from the image metadata using ExifTool (digiKam database 
#' for videos) and copies the images with new file names
#' into \code{outDir}, where it will set up a directory structure based on the
#' station IDs and, if required by \code{keepCameraSubfolders = TRUE}, camera
#' IDs (e.g. outDir/StationA/ or outDir/StationA/CameraA1).
#' 
#' \code{copyImages} can be set to FALSE to simulate the renaming and check the
#' file names of the renamed images without copying. If you are handling large
#' number of images (>e.g., 100,000), the function may take some time to run.
#' 
#' Argument \code{video} is a named list 4 items 
#' (\code{file_formats}, \code{dateTimeTag}, (\code{db_directory}, 
#' \code{db_filename}). Video date/time is read from video metadata stored in
#' the digiKam database. Hence, \code{inDir} must be in your digiKam database.
#' 
#' The items of argument \code{video} are:
#' 
#' \tabular{ll}{ \code{file_formats} \tab The video formats to extract (include
#' "jpg" if you want .JPG image metadata) \cr \code{dateTimeTag} \tab the
#' metadata tag to extract date/time from (use \code{\link{exifTagNames}} to
#' find out which tag is suitable) \cr \code{db_directory} \tab The directory
#' containing digiKam database  \cr
#' \code{db_filename} \tab The digiKam database file in \code{db_directory} \cr }
#' 
#' See the examples in \code{\link{recordTable}} for for how to specify the 
#' argument \code{video}.
#' 
#' 
#' @param inDir character. Directory containing camera trap images sorted into
#' station subdirectories (e.g. inDir/StationA/)
#' @param outDir character. Directory into which the renamed images will be
#' copied
#' @param hasCameraFolders logical. Do the station directories in \code{inDir}
#' have camera subdirectories (e.g. "inDir/StationA/Camera1")?
#' @param keepCameraSubfolders logical. Should camera directories be preserved
#' as subdirectories of \code{outDir} (e.g. "outDir/StationA/CameraA1")?
#' @param createEmptyDirectories logical. If station or camera directories are
#' empty, should they be copied nevertheless (causing empty directories in
#' \code{outDir}, but preserving the whole directory structure)?
#' @param copyImages logical. Copy images to \code{outDir}?
#' @param writecsv logical. Save a data frame with a summary as a .csv? The csv
#' will be saved in \code{outDir}.
#' @param video list. Contains information on how to handle video data
#' (optional). See details.
#' 
#' @return A \code{data.frame} with original directory and file names, new
#' directory and file names and an indicator for whether images were copied
#' successfully.
#' 
#' @author Juergen Niedballa
#' 
#' @references Phil Harvey's ExifTool \url{https://exiftool.org/}
#' 
#' @examples
#' 
#' 
#' 
#'   \dontrun{
#'   
#' ### "trial" run. create a table with file names after renaming, but don't copy images.
#' 
#' # first, find sample image directory in package directory:
#' wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")
#' 
#' # because copyImages = FALSE, outDir does not need to be defined
#' renaming.table <- imageRename(inDir               = wd_images_raw,     
#'                               hasCameraFolders = FALSE,
#'                               copyImages          = FALSE,
#'                               writecsv            = FALSE
#'   )
#' 
#' 
#'   
#' ### a real example in which images are copied and renamed 
#' 
#'   # define raw image location
#' wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR") 
#' 
#'   # define destination for renamed images
#' wd_images_raw_renamed <- file.path(tempdir(), "raw_images_renamed")       
#' 
#' 
#'   # now we have to define outDir because copyImages = TRUE
#' renaming.table2 <- imageRename(inDir               = wd_images_raw,
#'                                outDir              = wd_images_raw_renamed,       
#'                                hasCameraFolders    = FALSE,
#'                                copyImages          = TRUE,
#'                                writecsv            = FALSE
#'   )
#'   
#'   # show output files
#'   list.files(wd_images_raw_renamed, recursive = TRUE)
#'   
#'   # output table
#'   renaming.table2
#'   
#'   }
#' 
#' @export imageRename
#' 
imageRename <- function(inDir,
                        outDir,
                        hasCameraFolders,
                        keepCameraSubfolders,
                        createEmptyDirectories = FALSE,
                        copyImages = FALSE,
                        writecsv = FALSE,
                        video){
  
  wd0 <- getwd()
  on.exit(setwd(wd0))
  
  file.sep <- .Platform$file.sep
  
  stationCol <- "Station"
  cameraCol  <- "Camera"
  
  # check inDir / outDir
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
  
  # Check input file path lengths
  max_length <- 255
  all_input_files <- list.files(inDir, recursive = TRUE, full.names = TRUE)
  if (any(nchar(all_input_files) > max_length)) {
    long_files <- all_input_files[nchar(all_input_files) > max_length]
    stop(paste("The following input file paths exceed", max_length, "characters:", 
               paste(long_files, collapse = ", ")), call. = FALSE)
  }
  
  
  # list of subdirectories of inDir
  dirs       <- list.dirs(inDir, full.names = TRUE,  recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  
  # Remove the ".dtrash" folder if present (on Mac)
  dirs <- dirs[dirs_short != ".dtrash"]
  dirs_short <- dirs_short[dirs_short != ".dtrash"]

  # Check for Umlaute in path names
  paths_to_check <- inDir
  if (hasArg(outDir)) {
    paths_to_check <- c(paths_to_check, outDir)
  }
  
  unicode_vector <- c("\u{00E4}", "\u{00F6}", "\u{00FC}", "\u{00C4}", "\u{00D6}", "\u{00DC}")

  if (any(grepl(paste0("[", paste(unicode_vector, collapse = ""), "]"), list.dirs(paths_to_check)))) {
    warning("Umlaute (", 
            paste(unicode_vector[1:3], collapse = ", "),
    ") detected in path names. This may cause issues on certain operating systems.", call. = FALSE)
  }
  
  
  if(length(dirs) == 0) stop("inDir contains no station directories", call. = FALSE)
  
  # make sure none is empty
  fileext_pattern <- ".jpg$|.JPG$"
  names(fileext_pattern) <- "jpg"
  if(hasArg(video))  fileext_pattern <- c(fileext_pattern, 
                                          sapply(video$file_formats, FUN = function(x) paste0(tolower(x), "$|", toupper(x), "$"))
                                          )
  
  list_n_files <- lapply(dirs, list.files, recursive = TRUE)   # need to adapt this to include video formats also (in fileext_pattern)
  list_n_files <- lapply(list_n_files, FUN = function(x) sapply(fileext_pattern, grepl, x))

  
  # if(any(unlist(lapply(list_n_files, length)) == 0) && !hasArg(video)){
  #     warning("at least one station directory contains no JPEGs:  ", paste(dirs_short[which(lapply(list_n_files, length) == 0)], collapse = "; "), call. = FALSE, immediate. = TRUE)
  # }
  
  if(any(sapply(list_n_files, sum) == 0)) {
    if(hasArg(video)){
      warning("at least one station directory contains no JPEGs or ", paste(video$file_formats, collapse = ' / '), " files:  ", paste(dirs_short[which(sapply(list_n_files, sum) == 0)], collapse = "; "), call. = FALSE, immediate. = TRUE)
    } else {
      warning("at least one station directory contains no JPEGs:  ", paste(dirs_short[which(sapply(list_n_files, sum) == 0)], collapse = "; "), call. = FALSE, immediate. = TRUE)
    }
  }
  
  
  
  stopifnot(is.logical(createEmptyDirectories))
  
  # remove dirs and dirs_short if they contain no images (and if user overrides default createEmptyDirectories = FALSE)
  if(isFALSE(createEmptyDirectories)){
    stations2remove  <- which(lapply(list_n_files, length) == 0)
    if(length(stations2remove) >= 1){
      dirs        <- dirs      [-stations2remove]
      dirs_short  <- dirs_short[-stations2remove]
    }
  }
  # for pretty console messages
  max_nchar_station <- max(nchar(dirs_short))
  
  # process video argument (if present)
  if(hasArg(video)){
    # Check if the video argument is a list with the required elements
    if(!is.list(video) || !all(c("file_formats", "dateTimeTag") %in% names(video))) {
      stop("The 'video' argument must be a list with elements 'file_formats' and 'dateTimeTag'")
    }
    
    video_out <- processVideoArgument(IDfrom = "metadata",   # to ensure digikam DB is read
                                      video  = video)
    digiKam_data <- video_out$digiKam_data
    file_formats <- video_out$file_formats
  } else {
    file_formats <- "jpg"   # jpg, as the default, if video not requested
  }
  
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  
  # check call for consistency
  stopifnot(is.logical(copyImages))
  stopifnot(is.logical(writecsv))
  stopifnot(is.logical(hasCameraFolders))
  
  if(isTRUE(writecsv) & isFALSE(hasArg(outDir))) stop("writecsv is TRUE. Please specify outDir", call. = FALSE)
  
  if(isTRUE(hasCameraFolders)){
    if(!hasArg(keepCameraSubfolders)) stop("If hasCameraFolders is TRUE, keepCameraSubfolders must be defined")
    if(!is.logical(keepCameraSubfolders)) stop("atgument keepCameraSubfolders must be logical (TRUE or FALSE)")
  } else {
    keepCameraSubfolders <- FALSE
  }
  if(hasArg(hasCameraFolders) & hasArg(keepCameraSubfolders)){
    if(keepCameraSubfolders == TRUE & hasCameraFolders == FALSE){stop("If hasCameraFolders is FALSE, keepCameraSubfolders must be FALSE too", call. = FALSE)}
  }
  
  
  # function body
  
  copy.info.table <- data.frame()
  
  for(i in 1:length(dirs_short)){     # for all non-empty directories
    
    # check if there are any images not in camera trap subfolders
    if(hasCameraFolders == TRUE && length(list.files(dirs[i], pattern = ".jpg$|.JPG$", recursive = FALSE)) >= 1){
      stop(paste("Directory ", dirs[i], " contains images not sorted into Camera Trap subfolders. Check argument 'hasCameraFolders'"), call. = FALSE)
    }
    
    # create Exiftool call (photos only)
    # command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
    
    # create Exiftool call (photos and videos)
    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal', 
                         ifelse(hasArg(video),
                                 paste(" -", video$dateTimeTag, sep = ""), 
                                ""),    # if video requested, video date time tag
                          paste(" -ext", file_formats, collapse = " ", sep = " "),    # requested file extensions
                          ' "', dirs[i], '"', sep = "")
    
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")
    if(hasArg(video)) colnames.tmp <- c(colnames.tmp, video$dateTimeTag)
    
    # run exiftool to get image date and time
    metadata.tmp <- runExiftool(command.tmp = command.tmp, colnames.tmp = colnames.tmp)
    
    
    if(length(metadata.tmp) == 0){
      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      warning(paste(dirs_short[i], "seems to contain no images;", " found", length.tmp, "jpgs"), call. = FALSE, immediate. = TRUE)    # give message if station directory contains no jpgs
    } else {
      
      message(paste(formatC(dirs_short[i], width = max_nchar_station, flag = "-"), ": ", formatC(nrow(metadata.tmp), width = 4), " images", 
                    makeProgressbar(current = i, total = length(dirs_short)), sep = ""))
      
      if(isTRUE(hasCameraFolders)){
        # sort by camera, then image name (in case cameras are not sorted alphabetically)
        metadata.tmp <- metadata.tmp[order(metadata.tmp$Directory, metadata.tmp$FileName),]
        
        filenames_by_subfolder <- lapply(list.dirs(dirs[i], full.names =TRUE, recursive = FALSE),
                                         FUN = list.files, pattern = ".jpg$|.JPG$", recursive = TRUE, ignore.case = TRUE)
        metadata.tmp$CameraID <- rep(list.dirs(dirs[i], full.names = FALSE, recursive = FALSE),
                                     times = sapply(filenames_by_subfolder, length))
        metadata.tmp$Station <- rep(dirs_short[i], times = nrow(metadata.tmp))
        colnames(metadata.tmp)[grep("CameraID", colnames(metadata.tmp))] <- cameraCol
        colnames(metadata.tmp)[grep("Station", colnames(metadata.tmp))]  <- stationCol
      } else {
        metadata.tmp$CameraID <- "NA"                                                    # "camera" name if no camera id available (only station ids)
        metadata.tmp$Station <- rep(dirs_short[i], times = nrow(metadata.tmp))
        colnames(metadata.tmp)[grep("CameraID", colnames(metadata.tmp))] <- cameraCol
        colnames(metadata.tmp)[grep("Station", colnames(metadata.tmp))]  <- stationCol
      }
      
      # make time readable
      if(hasArg(video)){
        metadata.tmp <- addVideoDateTimeOriginal(metadata.tmp = metadata.tmp, video = video)
        metadata.tmp$DateTimeOriginal <- lubridate::ymd_hms(metadata.tmp$DateTimeOriginal)
      } else {
        metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = as.character(metadata.tmp$DateTimeOriginal),
                                                           format = "%Y:%m:%d %H:%M:%S", tz = "UTC"))
      }
      metadata.tmp$DateTimeOriginal2 <- as.POSIXlt(metadata.tmp$DateTimeOriginal)
      
      
      # exclude images for which no DateTimeOriginal was found
      if(any(is.na(metadata.tmp$DateTimeOriginal))){
        na.date.rows <- which(is.na(metadata.tmp$DateTimeOriginal))
        warning(paste("could not read DateTimeOriginal tag of: \n",
                      paste(paste(metadata.tmp$Directory,  metadata.tmp$FileName, sep = file.sep)[na.date.rows], collapse = "\n")),
                call. = FALSE, immediate. = TRUE)
        metadata.tmp <- data.frame(metadata.tmp, DateReadable = NA)
        metadata.tmp$DateReadable[-na.date.rows] <- TRUE
        metadata.tmp$DateReadable[ na.date.rows] <- FALSE
        rm(na.date.rows)
      } else {
        metadata.tmp$DateReadable <- TRUE
      }

      
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
        metadata.tmp2 <- rbind(cbind(metadata.tmp[metadata.tmp$DateReadable == FALSE,], minute.append = NA), metadata.tmp2)
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
                                            ".", tools::file_ext(metadata.tmp2$FileName), 
                                            #"JPG", 
                                            sep = "")
      } else {
        metadata.tmp2$filename_new <- paste(paste(metadata.tmp2[,stationCol],
                                                  paste(metadata.tmp2$DateTime_for_filename, "(",metadata.tmp2$minute.append, ")", sep = ""),
                                                  sep = "__"),
                                            ".", tools::file_ext(metadata.tmp2$FileName), 
                                            # ".JPG", 
                                            sep = "")
      }
      
      copy.info.table <- rbind(copy.info.table, metadata.tmp2)
      rm(metadata.tmp2)
    }
  }
  
  # error if no date/time tag is readable
  if(!any(copy.info.table$DateReadable)) stop("could not read DateTimeOriginal tag of any image. Check if the DateTimeOriginal tag is present in metadata with exifTagNames(). If not, fixDateTimeOriginal() might help.",
                                              call. = FALSE)
  # warning if at least one date/time tag is not readable
  if(!all(copy.info.table$DateReadable)) warning(paste("Some DateTimeOriginal tags are unreadable, e.g. \n", 
                                                   paste(apply(copy.info.table[!copy.info.table$DateReadable, c("Directory", "FileName")],  MARGIN = 1, FUN = paste, collapse = file.sep), collapse = "\n")),
                                              call. = FALSE)
  
  # Check output file path lengths
  if (hasArg(outDir)) {
    all_output_files <- file.path(copy.info.table$outDir, copy.info.table$filename_new)
    if (any(nchar(all_output_files) > max_length)) {
      long_files <- all_output_files[nchar(all_output_files) > max_length]
      stop(paste("The following output file paths would exceed", max_length, "characters:", 
                 paste(long_files, collapse = ", ")), call. = FALSE)
    }
  }
  
  # create directory structure in outDir
  if(isTRUE(copyImages)){
    
    if(isFALSE(keepCameraSubfolders))   dir2create <- file.path (outDir, dirs_short)    # outDir with station subdirectories
    
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
    
    
    sapply(dir2create, FUN = dir.create, recursive = TRUE, showWarnings = FALSE)           # create directories (recursively)
  }
  
  # check if renamed images exist already
  if(hasArg(outDir)) {
    copy.info.table$fileExistsAlready <- file.exists(file.path(copy.info.table$outDir, copy.info.table$filename_new))
  } else {
    copy.info.table$fileExistsAlready <- FALSE
  }
  
  # exit if no new images to be copied
  if(all(copy.info.table$fileExistsAlready)){
    stop("No new images to be copied. Exiting.", call. = FALSE)
  }
  
  # message if there are images in outDir already
  if(any(copy.info.table$fileExistsAlready)) {
    message(paste(sum(copy.info.table$fileExistsAlready), "out of", nrow(copy.info.table), "images exist already in outDir. They will not be copied"))
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
      # if not all images are copied, set copystatus of the images that were not copied to FALSE
      if(length(items_to_copy) < nrow(copy.info.table)) copy.info.table$CopyStatus[-items_to_copy] <- FALSE
    } else {
      copy.info.table$CopyStatus <- FALSE     # if copyImages = TRUE & proceed = FALSE
    }
  } else {
    copy.info.table$CopyStatus <- FALSE       # if copyImages = FALSE
  }
  
  rownames(copy.info.table) <- NULL     # remove rownames
  
  # remove unused columns
  copy.info.table <- copy.info.table[,-which(names(copy.info.table) %in%
                                               c("DateTimeOriginal2", "DateTime_for_filename", "minute.append"))]
  # save table
  if(writecsv == TRUE){
    dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
    setwd(outDir)
    write.csv(copy.info.table, file =file.path(outDir, paste0("_renaming_table_", Sys.Date(), ".csv")),
              row.names = FALSE)
  }
  return(copy.info.table)
}
