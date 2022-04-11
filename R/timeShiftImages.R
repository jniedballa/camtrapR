#' Apply time shifts to JPEG image metadata
#' 
#' Change the values of digital timestamps in image metadata using ExifTool. If
#' date/time of images were set incorrectly, they can be corrected easily in
#' batch mode for further analyses. Please, always make a backup of your data
#' before using this function to avoid data loss or damage. This is because
#' ExifTool will make a copy of your images and applies the time shifts to the
#' copies. The file extension of the original images (.JPG) will be renamed to
#' ".JPG_original".
#' 
#' 
#' \code{timeShiftTable} is a data frame with columns for station ID, camera ID
#' (optional), time shift value and direction of time shift (for an example see
#' \code{\link{timeShiftTable}}). Images in \code{inDir} must be sorted into
#' station directories. If \code{hasCameraFolders = TRUE}, the function expects
#' camera subdirectories in the station directories and will only apply time
#' shifts to the camera subdirectories specified by \code{CameraCol} in
#' \code{timeShiftTable}. If \code{hasCameraFolders = FALSE}, shifts will be
#' applied to the whole station directory (including potential subdirectories).
#' 
#' The values of \code{timeShiftColumn} must adhere to the following pattern:
#' "YYYY:mm:dd HH:MM:SS" ("year:month:day hour:minute:second"). Examples:
#' "1:0:0 0:0:0" is a shift of exactly 1 year and "0:0:0 12:10:01" 12 hours and
#' 10 minutes and 1 second. Note that stating "00" may cause problems, so use
#' "0" instead if an entry is zero.
#' 
#' \code{timeShiftSignColumn} signifies the direction of the time shift. "+"
#' moves image dates into the future (i.e. the image date lagged behind the
#' actual date) and "-" moves image dates back (if the image dates were ahead
#' of actual time).
#' 
#' ExifTool stores the original images as \code{.JPG_original} files in the
#' original file location. By setting \code{undo = TRUE}, any JPG files in the
#' directories specified by \code{timeShiftTable} will be deleted and the
#' original JPEGs will be restored from the JPG_original files. Please make a
#' backup before using \code{undo}.
#' 
#' Years can have 365 or 366 days, and months 28 to 31 days. Here is how the
#' function handles these (from the exiftool help page): "The ability to shift
#' dates by Y years, M months, etc, conflicts with the design goal of
#' maintaining a constant shift for all time values when applying a batch
#' shift. This is because shifting by 1 month can be equivalent to anything
#' from 28 to 31 days, and 1 year can be 365 or 366 days, depending on the
#' starting date. The inconsistency is handled by shifting the first tag found
#' with the actual specified shift, then calculating the equivalent time
#' difference in seconds for this shift and applying this difference to
#' subsequent tags in a batch conversion."
#' 
#' \code{ignoreMinorErrors} is useful if image timestamps are not updated
#' correctly (entries in column "n_images" of the output are "... files weren't
#' updated due to errors"). This can be caused by bad MakerNotes and so far was
#' only observed in Panthera V4 cameras. In that case, set
#' \code{ignoreMinorErrors} to \code{TRUE}. This will add the "-m" option to
#' the Exiftool call, thereby ignoring minor errors and warnings and applying
#' the time shift nevertheless.
#' 
#' @param inDir character. Name of directory containing station directories
#' with images
#' @param hasCameraFolders logical. Do the station directories in \code{inDir}
#' have camera subdirectories (e.g. "inDir/StationA/Camera1")?
#' @param timeShiftTable data.frame containing information about
#' station-/camera-specific time shifts.
#' @param stationCol character. name of the column specifying Station ID in
#' \code{timeShiftTable}
#' @param cameraCol character. name of the column specifying Camera ID in
#' \code{timeShiftTable} (optional)
#' @param timeShiftColumn character. The name of the column containing time
#' shift values in \code{timeShiftTable}
#' @param timeShiftSignColumn character. The name of the column with the
#' direction of time shifts in \code{timeShiftTable}. Can only be "-" or "+".
#' @param undo logical. Undo changes and restore the original images? Please be
#' careful, this deletes any edited images if \code{TRUE}
#' @param ignoreMinorErrors logical. Ignore minor errors that would cause the
#' function to fail (set TRUE for images with bad MakerNotes, observed in
#' Panthera V4 cameras)
#' 
#' @return A \code{data.frame} containing the information about the processed
#' directories and the number of images.
#' 
#' @author Juergen Niedballa
#' 
#' @references \url{https://exiftool.org/#shift}
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' # copy sample images to temporary directory (so we don't mess around in the package directory)
#' wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
#' file.copy(from = wd_images_ID, to = tempdir(), recursive = TRUE)
#' wd_images_ID_copy <- file.path(tempdir(), "sample_images_species_dir")
#' 
#' data(timeShiftTable)
#' 
#' 
#' timeshift_run <- timeShiftImages(inDir                = wd_images_ID_copy,
#'                                  timeShiftTable       = timeShiftTable,
#'                                  stationCol           = "Station",
#'                                  hasCameraFolders     = FALSE,
#'                                  timeShiftColumn      = "timeshift",
#'                                  timeShiftSignColumn  = "sign",
#'                                  undo                 = FALSE
#' )
#' 
#' 
#' timeshift_undo <- timeShiftImages(inDir               = wd_images_ID_copy,
#'                                   timeShiftTable      = timeShiftTable,
#'                                   stationCol          = "Station",
#'                                   hasCameraFolders    = FALSE,
#'                                   timeShiftColumn     = "timeshift",
#'                                   timeShiftSignColumn = "sign",
#'                                   undo                = TRUE
#' )
#' }
#' 
#' @export timeShiftImages
#' 
timeShiftImages <- function(inDir,
                            hasCameraFolders,
                            timeShiftTable,
                            stationCol,
                            cameraCol,
                            timeShiftColumn,
                            timeShiftSignColumn,
                            undo = FALSE,
                            ignoreMinorErrors = FALSE
)
{
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool")

  timeShiftTable <- dataFrameTibbleCheck(df = timeShiftTable)
  
  # convert all columns to character
  for(i in 1:ncol(timeShiftTable)){
    timeShiftTable[,i] <- as.character(timeShiftTable[,i])
  }
  rm(i)
  
  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, timeShiftColumn = timeShiftColumn, timeShiftSignColumn = timeShiftSignColumn)
  if(!stationCol %in% colnames(timeShiftTable))           stop(paste('stationCol = "',   stationCol,     '" is not a column name in timeShiftTable', sep = ''), call. = FALSE)
  if(!timeShiftColumn %in% colnames(timeShiftTable))      stop(paste('timeShiftColumn = "',   timeShiftColumn,     '" is not a column name in timeShiftTable', sep = ''), call. = FALSE)
  if(!timeShiftSignColumn %in% colnames(timeShiftTable))  stop(paste('timeShiftSignColumn = "', timeShiftSignColumn,       '" is not a column name in timeShiftTable', sep = ''), call. = FALSE)

  
  if(isTRUE(hasCameraFolders)){
    stopifnot(cameraCol %in% colnames(timeShiftTable))
  } else {
    if(any(duplicated(timeShiftTable[,stationCol]))) stop("There are duplicates in stationCol. Check argument hasCameraFolders")
  }
   
 
  stopifnot(is.logical(hasCameraFolders))
  for(xy in 1:nrow(timeShiftTable)){
    if(length(unlist(strsplit(timeShiftTable[xy,timeShiftColumn], split = " "))) != 2) stop(paste("there is more than 1 space in your timeShiftColumn string. Only 1 space is allowed (", timeShiftTable[xy,stationCol], ")"))
    if(nchar(timeShiftTable[xy,timeShiftColumn]) - nchar(gsub(":","",timeShiftTable[xy,timeShiftColumn])) != 4) stop("there should be 4 colons in timeShiftColumn (", timeShiftTable[xy,stationCol], ")")
    if(timeShiftTable[xy,timeShiftSignColumn] %in% c("+", "-") == FALSE) stop(paste0('timeShiftSignColumn can only be "+" or "-". Found value "', timeShiftTable[xy,timeShiftSignColumn], '" at station ',
                                                                              timeShiftTable[xy,stationCol]))
    if(length(unlist(lapply(strsplit(timeShiftTable[xy,timeShiftColumn], split = " "), FUN = strsplit, split = ":"))) != 6) stop("there must be six numbers in timeShiftColumn (",
                                                                                                                                 timeShiftTable[xy,stationCol], ")")
  }

  
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
  
  if(isTRUE(hasCameraFolders)){
    shift.dirs <- file.path(inDir, timeShiftTable[,stationCol], timeShiftTable[,cameraCol])
  } else {
    shift.dirs <- file.path(inDir, timeShiftTable[,stationCol])
  }

  if(any(file.exists(shift.dirs) == FALSE)){
    stop(paste("Station directory does not exist:\n", 
               paste(shift.dirs[file.exists(shift.dirs) == FALSE], collapse = "\n"), sep = ""), call. = FALSE)
  }

  if(isTRUE(undo)){

  results_undo <- data.frame(directory = rep(NA, times = nrow(timeShiftTable)),
                             n_images_undo = rep(NA, times = nrow(timeShiftTable)))
   
  jpg2undo <- list()
  jpg2keep   <- list()
  jpg2keep_newname <- list()
  remove.tmp <- rename.tmp <- list()
  
    for(i in 1:length(shift.dirs)){
      jpg2undo[[i]]       <- list.files(shift.dirs[i], pattern = ".jpg$|.JPG$", recursive = TRUE, full.names = TRUE)
      jpg2keep[[i]]         <- list.files(shift.dirs[i], pattern = ".jpg_original$|.JPG_original$", recursive = TRUE, full.names = TRUE)
      jpg2keep_newname[[i]] <- gsub(pattern = "_original$", replacement = "", x = jpg2keep[[i]])
      
      if(length(jpg2keep[[i]]) == 0) stop(paste("found no .JPG_original files in", shift.dirs[i], "check argument 'undo'"))
      if(length(jpg2undo[[i]]) != length(jpg2keep[[i]])) stop(paste("number of jpgs in", shift.dirs[i], "is not equal to the number of JPG_original files" ))

      

	  results_undo$directory[i] <- shift.dirs[i]
	  results_undo$n_images_undo[i] <- length(jpg2undo[[i]])
    }
	
	remove.tmp <- lapply(jpg2undo, FUN = file.remove)
	for(xyz in 1:length(jpg2keep)){
		rename.tmp[[xyz]] <- file.rename(from = jpg2keep[[xyz]], to = jpg2keep_newname[[xyz]])
	}
	
	return(results_undo)
	  
  } else {

    results.tmp <- list()

    for(i in 1:nrow(timeShiftTable)){
      message(shift.dirs[i])
      command.tmp2b <- paste0('exiftool -r ', ifelse(ignoreMinorErrors, "-m ", ""), '"-DateTimeOriginal', timeShiftTable[i,timeShiftSignColumn], '=',
                             timeShiftTable[i,timeShiftColumn], '" "', shift.dirs[i], '"')
      results.tmp[[i]] <- system(command.tmp2b, intern=TRUE)
      rm(command.tmp2b)
    }

    results.tmp2 <- lapply(results.tmp, FUN = function(x){x[c(length(x) - 1, length(x))]})
    
    output <- data.frame(shift.dirs, matrix(unlist(results.tmp2),ncol = 2, byrow = 2))
    colnames(output) <- c("directory", "n_directories", "n_images")
    if(any(grepl("files weren't updated due to errors", output$n_images))){
      warning("There were problems changing the time stamps in:\n", 
              paste(output$directory[grepl("files weren't updated due to errors", output$n_images)], collapse = "\n"),
              "\n\nTry setting ignoreMinorErrors = TRUE", call. = FALSE)
    }
    
    return(output)
  }
}
