## ----eval = TRUE---------------------------------------------------------
library(camtrapR)

## ----eval = FALSE--------------------------------------------------------
#  # Check which directories are in PATH (not shown here)
#  Sys.getenv("PATH")

## ------------------------------------------------------------------------
# Check if the system can find ExifTool
Sys.which("exiftool")

## ------------------------------------------------------------------------
# this is a dummy example assuming the directory structure: C:/Path/To/Exiftool/exiftool.exe
exiftool_dir <- "C:/Path/To/Exiftool"        
exiftoolPath(exiftoolDir = exiftool_dir)

## ------------------------------------------------------------------------
# here is a sample camera trap station table
data("camtraps")
camtraps

## ------------------------------------------------------------------------
# create a temporary dummy directory for tests
# (normally, you'd set up an empty directory in a proper place, e.g. .../myStudy/rawImages)
wd_createStationDir <- file.path(tempdir(), "createStationFoldersTest")

# create station directories in  wd_createStationDir
StationFolderCreate1 <- createStationFolders (inDir       = wd_createStationDir,
                                              stations    = as.character(camtraps$Station), 
                                              createinDir = TRUE)
  
StationFolderCreate1


## ------------------------------------------------------------------------
data(timeShiftTable)
timeShiftTable

## ------------------------------------------------------------------------
# copy sample images to another location (so we don't mess around in the package directory)
wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")
file.copy(from = wd_images_raw, to = tempdir() , recursive = TRUE)
wd_images_raw_copy <- file.path(tempdir(), "raw_images")

timeshift_run <- timeShiftImages(inDir                = wd_images_raw_copy,
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "Station",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign"
)

timeshift_run


## ------------------------------------------------------------------------
timeshift_undo <- timeShiftImages(inDir               = wd_images_raw_copy,
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "Station",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign",
                                 undo                 = TRUE
)

timeshift_undo

## ------------------------------------------------------------------------
 # raw image location
wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")      
  # destination for renamed images to be copied to
wd_images_raw_renamed <- file.path(tempdir(), "raw_images_renamed")       

renaming.table2 <- imageRename(inDir               = wd_images_raw,
                               outDir              = wd_images_raw_renamed,       
                               hasCameraFolders    = FALSE,
                               copyImages          = FALSE
  )

# here is the information for a few images 
head(renaming.table2)

## ------------------------------------------------------------------------
# copy sample images to temporary directory (so we don't mess around in the package directory)
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
file.copy(from = wd_images_ID, to = tempdir(), recursive = TRUE)
wd_images_ID_copy <- file.path(tempdir(), "sample_images")

# define an example copyright tag
copyrightTagToAdd <- "Your Name (Your Organisation)"

# write the tag to images
addCopyrightTag(inDir        = wd_images_ID_copy, 
                copyrightTag = copyrightTagToAdd, 
                askFirst     = FALSE)                  # askFirst = FALSE prevents function from asking user to confirm action (which doesn't work in vignettes)


# you can check the outcome with function exifTagNames (you'll find field "Copyright" in line 20)
exifTagNames(wd_images_ID_copy, returnMetadata = TRUE)

