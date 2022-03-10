# camtrapR 2.1.1

## new functions
* communityModel: create community occupancy models for JAGS and Nimble. Comes with summary(), fit(), and predict() methods
* plot_effects(): for plotting marginal effect plots (response curves) from community models
* plot_coef(): for plotting coefficient estimates from community models

## bugfixes
* reading digiKam database now works for Mac + Linux systems

# camtrapR 2.1.0

## new functions
* OCRdataFields: allows automatic reading of text from data fields in images via optical character recognition (OCR), requires magick and tesseract packages
* writeDateTimeOriginal: Write values to DateTimeOriginal tag to image metadata

## new features
* cameraOperation: setup/ retrieval / problem columns can be interpreted as date-time format, then computes effort as fraction of day. (Thank you to Zoe Siegel for the suggestion, Fridolin Zimmermann and the Department of Ecology & Evolution from the University of Lausanne for funding the development of the feature)
* cameraOperation: gained argument occasionStartTime (deprecated in detectionHistory / spatialDetectionHistory)
* function camopPlot is now accessible via camtrapR:::camopPlot(). It also gained an argument "lattice", which can be TRUE or FALSE (if TRUE, it uses levelplot() from lattice)
* surveyReport: new argument camOp (for a camera operation matrix), argument CTHasProblems is deprecated. Improved calculation of survey effort.
* timeShiftImages / addCopyrightTag: added argument "ignoreMinorErrors", set it to TRUE if functions fails due to bad MakerNotes (reported by An Nguyen)
* detectionMaps uses package sf instead of sp and rgdal

## bugfixes
* cameraOperation now works when Problem columns are of class date or POSIX. Now being converted to character (reported by David Nicholls)
* recordTable: supports extraction of metadata tags from video files in camera subdirectories, and allows duplicate video file names in different camera subdirectories (reported by Sian Green)
* recordTable: fix bug that assigned Albums to wrong AlbumRoots in the digiKam database (when reading video metadata, reported by Sian Green)
* imageRename: fixed error when hasCameraFolders is TRUE, but a station has only 1 camera (reported by Camille Coudrat and Ezekiel Gading)
* detectionHistory: fixed a bug that caused errors when using multiple sessions and non-standard date/time formats (reported by Daniele Barcelos)
* checkSpeciesNames: fixed error due to change in taxize::get_tsn (reported by Ivonne Oddoy)


## other changes
* lubridate is a dependency now (for cameraOperation)
* additional Suggests: magick and tesseract (for OCRdataFields), lattice (for camopPlot)
* roxygen2 documentation

# camtrapR 2.0.3

## bugfixes
* fixed error messages if date or date/time columns contain blank values or NA
* fixed DH_error2 in detectionHistory() with output = "count" (reported by Maddy Uetrecht)
* cameraOperation failed if setupCol or retrievalCol were of class "Date". Now being converted to character.
* imageRename: fixed camera IDs being assigned incorrectly in very rare cases on Mac (reported by Camille Coudrat)
* recordTable: fixed error when videos had metadata tags that were not in tag groups

## other changes
* recordTable / recordTableIndividual: users can specify 1-character Exiftool arguments in additionalMetadataTags (only tested with "L" for Latin character set, everything else will most likely break the code, suggested by Saloni Salaria)
* improved error messages in recordTable/recordTableIndividual and detectionHistory/spatialDetectionHistory functions

# camtrapR 2.0.2

## bugfixes

* vignette creation conditional on availability of Exiftool (for CRAN checks)
* tests with testthat conditional on availability of Exiftool (for CRAN checks)
* appendSpeciesNames / getSpeciesImages: error message if exiftool is not available


# camtrapR 2.0.1

## bugfixes

* exifTagNames: doesn't return tag description, it caused an error on some systems

## other changes

* included tests with testthat


# camtrapR 2.0.0

## new features

* recordTable / recordTableIndividual: video support via new argument "video" (requires package "RSQLite", thanks to Ana Gracanin for a crucial tip that made date/time extraction possible)
* exifTagNames returns a data frame containing all relevant information at once: tag group names, tag descriptions, tag names and values. Arguments returnMetadata and returnTagGroup are ignored now.
* surveyReport: reliable creation of zip files via package "zip" (if installed)
* added sample images with species tags
* cameraOperation: detailed error messages if duplicate station / camera / session names


## bugfixes

* detectionHistory: when recordDateTimeCol is a date without time, function assigns a time to avoid calculating occasion 0 for records taken on the camera setup day (with a warning)
* error message in functions that call Exiftool when inDir is empty (no subdirectories)
* surveyReport: error message when zip file can't be created
* special characters in directory names now supported (but still best avoided; requires ExifTool 9.79 or later)
* detectionMaps: fixed error when trying to plot backgroundPolygon (reported by Oliver Wearn)

## other changes

* vignette filenames changed, document type changed to html_document, added floating table of contents
* added ORCID for authors
* packages "RSQLite" and "zip" in Suggests
* renamed sample picture folders


# camtrapR 1.2.3


## other changes

* checkSpeciesNames: no error when ITIS server is not available
* minor fixes in vignettes
* detectionHistory: no error if occasionLength is greater than half the number of columns in camera operation matrix, as long as it is smaller than the total number of columns


# camtrapR 1.2.2


## bugfixes

* recordTable: fixed error: cameraID must be of class 'character' when it is character
* recordTable: fixed error: metadataSpeciesTag must be of class 'character' when it is character

## other changes

* cameraOperation: warning if date can be interpreted but is likely wrong (due to incorrectly set dateFormat argument)
  
  
# camtrapR 1.2.1


## bugfixes

* recordTable: fixed error: metadataSpeciesTag must be of class 'character' when it is character and IDfrom = "metadata"
* recordTable: fixed: 'Error in `[.data.frame`(intable, , match(colnames(record.table), colnames(intable))) :
  undefined columns selected' (when additionalMetadataTags are defined)
  
## new features

* multi-season sample data sets: camtrapsMultiSeason, recordTableSampleMultiSeason, recordTableIndividualSampleMultiSeason
  
## other changes

* package "data.table" is a dependency now (for more reliable assembly of record table when additionalMetadataTags are defined, see ## bugfixes)


# camtrapR 1.2


## bugfixes

* imageRename: fixed bug where fileExistsAlready in output table was FALSE when it should be TRUE

## new features

* detectionHistory: support multi-season input and create detection histories for unmarkedFrameMult (from where users can create multi-season occupancy models with unmarked::colext), new argument "unmarkedFrameMultInput" (developed with support from Kirsten Weingarth)
* cameraOperation: new argument: sessionCol, to specify trapping sessions / seasons (with some changes to the row names in camera operation matrices if there are session and / or camera information)
* spatialDetectionHistory: supports camera operation matrices with session information
* tibbles (tidyverse) or data.tables support: all functions that require input data frames don't throw errors if these are are tibbles or data.tables, instead they automatically convert them to data.frames with a message
* lubridate support: date and date-time formats as defined by lubridate are now supported (e.g. "ymd" instead of "%Y-%m-%d" or "ymd HMS" instead of "%Y-%m-%d %H:%M:%S")
* activityOverlap: main plot title contains species names by default without being specified
* recordTable / recordTableIndividual: 2 new arguments, "eventSummaryColumn"" and "eventSummaryFunction" for statistics by event (e.g. max count)
* recordTable / recordTableIndividual: return additional column "n_images" indicating the number of images in an event
* progress bar in console for all functions that extract image metadata
* exifTagNames: more flexible, users can define subdirectories by name and give desired file names
  
## other changes

* detectionHistory / spatialDetectionHistory: improved error message if date/time values are not readable (correct row in original recordTable is returned)
* detectionMaps: new error message when XCol or YCol are not numeric
* package "rgdal" is no longer a dependency. Function "detectionMaps" will throw an error if user tries to write shapefile but package is missing
* packages "taxize" and "ritis" are no longer dependencies. Function "checkSpeciesNames" will throw an error if either is missing
* simplified data format of sample data: only character and numeric/integer in camtraps, recordTableSample, recordTableIndividualSample and timeShiftTable


# camtrapR 1.1


## bugfixes

* appendSpeciesNames / checkSpeciesIdentification / getSpeciesImages: fixed error 'argument "returnFileNamesMissingTags" is missing, with no default'
* recordTableIndividual: fixed multiple individuals from same image being removed (except for one) when removeDuplicates = TRUE

## new features

* detectionHistory can return counts of records per occasion (new argument "output")
* activityOverlap can return different overlap estimators (new argument "overlapEstimator")
* detectionHistory / spatialDetectionHistory: improved error messages when records fall outside camera trap operation range

## other changes

* added citation (see: citation("camtrapR"))
* added URLs to description


# camtrapR 1.0


## bugfixes

* activityDensity: fixed problem with argument allSpecies = TRUE that caused output list to return values for one species only (reported by Sally Soo).
* appendSpeciesNames, checkSpeciesIdentification,getSpeciesImages, recordTableIndividual: fixed error in extracting species IDs from image metadata in directories in which no images are tagged. These stations are now skipped with a warning.
* detectionHistory / spatialDetectionHistory: fixed error with detection histories and effort matrix full of NAs arising if camera operation matrix had values > 2 (after argument camerasIndependent = TRUE in cameraOperation)

## other changes

* imageRename: meaningful error message if no DateTimeOriginal tag was readable (e.g. all cameras were Reconyx HC500)
* recordTable/ recordTableIndividual: new argument: returnFileNamesMissingTags: file name of images with missing species tag are returned if TRUE (default = FALSE)
* surveyReport: in sample R script enclosed in survey report zip file, object cameraOperation was renamed to camOp (to avoid the clash with the function name cameraOperation)
* detectionHistory: the detection history output csv was renamed from record_history... to detection_history...


# camtrapR 0.99.9


## bugfixes

* camera operation: fixed bug with empty entries in cameraCol when cameraCol is defined (reported by Partha Sarathi Ghose)
* recordTable / recordTableIndividual: verbose warnings and better handling of situations in which date/time is unreadable (empty Exif:DateTimeOriginal metadata tag)

## new features

* detectionHistory / activity plot functions: if date/time is unreadable function returns row IDs to facilitate checks
* activityDensity: warning instead of error if bandwidth estimation fails and argument allSpecies = TRUE (problem occurred with single records, or few records at similar time of day)

## other changes

* exifTagNames: returns name of image from which metadata were extracted as a message


# camtrapR 0.99.8


## bugfixes

* recordTable / recordTableIndividual: column "HierarchicalSubject" (containing digiKam metadata tags in their native format) was called "record.table3[, col_to_move]" by mistake (thanks to Ross Pitman for reporting)

## other changes

* improved checks that function input has correct class (to make sure data.frames are provided where the functions expect them)

 
# camtrapR 0.99.7


## bugfixes

* recordTable: returned station ID as camera ID if IDfrom = "metadata" and cameraID = "directory"
 
## new features

* detectionMaps: gained argument 'backgroundPolgygon' for plotting a SpatialPolygon object in the background of the maps
* imageRename: new argument 'createEmptyDirectories' to control whether empty directories should be copied or not
* imageRename: if outDir contains images already, the function will not stop with an error, but asks you whether or not to copy new images (useful if data come in over time)
  
## other changes

* recordTable / recordTableIndividual: improved handling and reporting of stations without images (warning instead of error)
* recordTable / recordTableIndividual: improved handling and reporting of stations where all images lack species/individual metadata tags (only if using metadata ID, functions will throw warning instead of error, reported by Aditya Malgaonkar)



# camtrapR 0.99.6


## bugfixes

* fixed crash in cameraOperation function caused by underscores in station / camera IDs (reported by Wyatt Petersen)

## new functions

* fixDateTimeOriginal: Makes DateTimeOriginal Exif metadata tag in Reconyx Hyperfire cameras readable 

## new features

* spatialDetectionHistory: argument 'sessionCol' can now create multi-session capthist objects by either assigning individuals to different sessions or assigning stations to different sessions
* exifTagNames: gained argument 'returnTagGroup', and now by default returns metadata tag group in addition to metadata tag names (to unambiguously address specific metadata tags)

## other changes

* new dependency: "ritis" - this is because parts of package taxize (used internally by function "checkSpeciesNames") were migrated to ritis
* detectionHistory / spatialDetectionHistory: occasionLength can be any value between 1 and the number of columns in camOp (not half the number of columns in camOp as before)
* functions check for spaces in user-provided column names and report them as errors

  
  
# camtrapR 0.99.5


## bugfixes

* recordTable/recordTableIndividual: give warning if Exif DateTimeOriginal tag of images is unreadable (error if all tags of a station are unreadable). May happen if images files are corrupted due to camera malfunctioning.
* recordTable/recordTableIndividual: 2 records of the same species at the same station and same time by different cameras are not collapsed into one record if camerasIndependent = TRUE
* all functions using Exiftool: JPG files beginning with "._" are now ignored with a warning (these are hidden, uninformative system files created on Macs and were quite capable of messing up function calls)
  
## new features

* getSpeciesImages can now also read image file paths from a record table provided by the user instead of reading species IDs anew in every function call. It gained 3 new arguments for that purpose.
* recordTable and recordTableIndividual both gained an argument (removeDuplicateRecords) to control whether duplicate records (same station, same species, exactly same time) should be collapsed into one record (former versions did that without asking)
  
## other changes

* imageRename will first check all directories before actually renaming anything. So the function won't break halfway through, leaving you with a half-full directory of renamed images if something goes wrong (e.g. unreadable date/time tags).
  
  
# camtrapR 0.99.4


## bugfixes

* spatialDetectionHistory: trap usage now codes malfunctioning cameras and cameras that were not set up as 0 instead of NA (as expected by the secr package)
* spatialDetectionHistory: argument "scaleEffort" can only be FALSE now (to avoid negative trapping effort values, which secr does not accept)
  
## new features

* spatialDetectionHistory gained argument "sessionCol". It can be used to split data (i.e., create a list with multiple capthist objects) if captures come from different sessions).
  
## other changes

* package shows startup message and checks automatically if local package version is up to date

  
# camtrapR 0.99.3


## new functions

* addCopyrightTag: Write a copyright tag into image Exif metadata

## bugfixes

* detectionHistory: malfunctioning cameras (0 in camera operation matrix) did not cause detection history matrix cells to be NA (even if includeEffort = FALSE; reported by Hsiang Ling Chen)
* detectionHistory: if the last occasion is shorter than occasionLength, the corresponding detection history matrix cells will now be NA if includeEffort = FALSE
* surveyReport: species column names other than "Species" are now supported, i.e. argument "speciesCol" is now respected (thanks to Valentine Herrmann and Stefano Anile for reporting)

## new features

* detectionHistory gained argument "minActiveDaysPerOccasion". It can be used to define the minimum number of active trap days per occasion for that occasion be included (0/1 instead of NA)
  
  
  
# camtrapR 0.99.2


## bugfixes

* recordTable: fixed problem with appending camera IDs
* cameraOperation: improved messages to user

## other changes

* added section about defining date formats in camera station table to vignette 1 (Image Organisation)


  
# camtrapR 0.99.1


## bugfixes

* detectionHistory and spatialDetectionHistory did not run correctly under some rare circumstances (combinations of input arguments)
* column names of detection histories were incorrect if occasionStartTime != 0 and datesAsOccasionNames = TRUE
* argument maxNumberDays in detectionHistory / spatialDetectionHistory works on each station individually if day1 = "survey" or "station"; and from the specified date if day1 is a date

## other changes

* additional checks for consistency of input in a number of functions
* code simplification invisible to the user

  

# camtrapR 0.99.0


## new functions

* checkSpeciesNames: checks a vector of species names (common or scientific) against the ITIS taxonomic database via package 'taxize'
* checkSpeciesIdentification: expanded from former function checkSpeciesFolders (now defunct). Can check identification if metadata tags were used for species identification. 
* createStationFolders: Create camera trap station directories, possibly with camera subdirectories, for storing raw images (was not included in NAMESPACE of version 0.98.0, therefore invisible. Thanks to Patrick Lorch and Brent Pease for notification)
  
## renamed functions

* recordDatabase           -> recordTable
* recordDatabaseIndividual -> recordTableIndividual
  
## renamed data sets

* recordDatabaseSample           -> recordTableSample
* recordDatabaseIndividualSample -> recordTableIndividualSample
  
## new features

* detectionHistory and spatialDetectionHistory: 
	- detection histories can begin on any specified date, station-specific setup date or day the first station was set up (to that end argument "beginWithDay1" was changed to "day1")
	- a buffer between setup and the first occasion can be applied (argument "buffer").
* spatialDetectionHistory: 
	- output can be given as counts or binary (argument "output")
	- an RMark data frame can be created using argument makeRMarkInput
* extended support for identification via metadata tags (in functions appendSpeciesNames, getSpeciesImages)
* exifTagNames: can return metadata, not only metadata tag names (new argument "returnMetadata")
* createStationFolders: new argument "createinDir".
* detecionMaps: new argument "speciesToShow" to make maps for certain species only
* activity plot functions show number of records that were used
  
## bugfixes
  
* surveyReport returned an error when there were stations without records.
* detecionMaps: Xcol and Ycol have arbitrary names now.
  
## other changes

* arguments "customMetadataTags" and hasSpeciesFolders/ hasIndividualFolders was removed and replaced with argument "IDfrom"
* metadata tags assigned in image management software are always extracted and tabulated in recordTable functions (making argument "customMetadataTags" obsolete)
* some functions changed the user's working directory. Fixed. Thanks to Patrick Lorch for notification.
* argument "hasCameraSubfolders" was renamed to "hasCameraFolders" (functions appendSpeciesNames, checkSpeciesIdentification, createSpeciesFolders, imageRename, timeShiftImages)
* in all 4 activity plot functions, argument "all.species" was renamed to "allSpecies"
* argument "recordTable" in spatialDetectionHistory was renamed to "recordTableIndividual" to make the difference from "recordTable" in "detectionHistory" clearer


  
# camtrapR 0.98.0


## new functions

* createStationFolders: Create camera trap station directories, possibly with camera subdirectories, for storing raw images
* recordDatabaseIndividual: Single-species record databases from camera trap images with custom metadata tags
* spatialDetectionHistory: prepare a capthist object for spatial capture-recapture analyses

## new features

* support for camera subdirectories in species identification: Station/Camera/Species in the following function:
	- appendSpeciesFolders
  - createSpeciesFolders
  - checkSpeciesFolders
  - createStationFolders
  - getSpeciesImages
  - imageRename
  - recordDatabase
  - recordDatabaseIndividual
      
* recordDatabase: can handle different types of directory structures and metadata tags (both custom and manufacturer-specific) and has gained new arguments: 
  - hasSpeciesFolders
  - cameraID (expanded from includeCameras)
  - camerasIndependent
  - stationCol
  - customMetadataTags
  - metadataHierarchyDelimitor
  - metadataSpeciesTag
  - additionalMetadataTags (renamed from metadataTags)
      
* detectionMaps: shapefile export using packages sp and rgdal
* checkSpeciesFolders: new argument stationsToCheck
  
## other changes

* checkSpeciesFolders: renamed argument exclude -> excludeSpecies
* detectionHistory: "session" in argument names was renamed to "occasion" (to be consistent with secr nomenclature)
* getSpeciesImages: removed argument emptyOutDir (nothing can be deleted anymore)
* recordDatabase: row names of object are 1:nrow

  

# camtrapR 0.97.1

* surveyReport: fixed bug which caused wrong numbers of active trap nights to be returned
* surveyReport: does not depend on function cameraOperation anymore
* renamed function TimeShiftImages to timeShiftImages
* minor fixes in cameraOperation
* some rephrasing in help files
* changes to author list
  