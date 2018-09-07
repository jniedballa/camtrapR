## ----results = "hide"----------------------------------------------------
library(camtrapR)

## ------------------------------------------------------------------------
# find the directory with sample images contained in the package
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR", lib.loc = .libPaths())

## ------------------------------------------------------------------------
length(list.files(wd_images_ID, pattern = "JPG", recursive = TRUE))

## ------------------------------------------------------------------------
rec.db.species0 <- recordTable(inDir  = wd_images_ID,
                               IDfrom = "directory")

head(rec.db.species0)

## ------------------------------------------------------------------------
list.files(file.path(wd_images_ID, "StationB", "MNE"))

## ------------------------------------------------------------------------
rec.db.species60 <- recordTable(inDir               = wd_images_ID,
                                IDfrom              = "directory",
                                minDeltaTime        = 60,
                                deltaTimeComparedTo = "lastRecord",
                                timeZone            = "Asia/Kuala_Lumpur")

nrow(rec.db.species60)

## ------------------------------------------------------------------------
# see what species  we recorded
table(rec.db.species60$Species)

# remove "NO_ID" by setting argument exclude = "NO_ID"
rec.db.species60.exclude <- recordTable(inDir               = wd_images_ID,
                                        IDfrom              = "directory",
                                        minDeltaTime        = 60,
                                        deltaTimeComparedTo = "lastIndependentRecord",
                                        timeZone            = "Asia/Kuala_Lumpur",
                                        exclude             = "NO_ID")

# note that "NO_ID" is gone now
table(rec.db.species60.exclude$Species)


## ------------------------------------------------------------------------
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
exifTagNames(inDir = wd_images_ID, returnMetadata = FALSE)

## ------------------------------------------------------------------------
exifTagNames(inDir = wd_images_ID, returnMetadata = TRUE)

## ------------------------------------------------------------------------
rec.db.species.metadata1 <- recordTable(inDir                  = wd_images_ID,
                                        IDfrom                 = "directory",
                                        timeZone               = "Asia/Kuala_Lumpur",
                                        additionalMetadataTags = c("EXIF:Model", "EXIF:Make"))

head(rec.db.species.metadata1)

## ------------------------------------------------------------------------
# find the directory with tagged sample images contained in the package
wd_images_individual_ID <- system.file("pictures/sample_images_tagged/LeopardCat", package = "camtrapR")
 # missing space in species = "LeopardCat" is because of CRAN package policies

 rec.db.pbe <- recordTableIndividual(inDir                  = wd_images_individual_ID,
                                     IDfrom                 = "metadata",
                                     minDeltaTime           = 60,
                                     deltaTimeComparedTo    = "lastIndependentRecord",
                                     hasStationFolders      = FALSE,         # images are not in station directories
                                     metadataIDTag          = "individual",  # the name of the metadata tag containing individual IDs
                                     timeZone               = "Asia/Kuala_Lumpur"
 )


## ------------------------------------------------------------------------
head(rec.db.pbe)

## ------------------------------------------------------------------------
 # first load the camera trap station table
data(camtraps)
 
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "%d/%m/%Y"
)

# as a reminder, these are the dates in our station information table
camtraps[,-which(colnames(camtraps) %in% c("utm_y", "utm_x"))]
# now let's have a look at the first few columns of the camera operation matrix
camop_problem[, 1:5]
# and the last few
camop_problem[, (ncol(camop_problem)-6):ncol(camop_problem)]

## ------------------------------------------------------------------------
camopPlot <- function(camOp){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = c("red", "grey70"))
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}

## ------------------------------------------------------------------------
camopPlot(camOp = camop_problem)

## ----eval = FALSE--------------------------------------------------------
#  camOp <- read.csv(file = ..., row.names = 1, check.names = FALSE)

## ------------------------------------------------------------------------

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "%d/%m/%Y"
)

# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# make record table
recordTableSample <- recordTable(inDir               = wd_images_ID,
                                 IDfrom              = "directory",
                                 minDeltaTime        = 60,
                                 deltaTimeComparedTo = "lastIndependentRecord",
                                 timeZone            = "Asia/Kuala_Lumpur"
)

# make detection history (without trapping effort)
DetHist1 <- detectionHistory(recordTable         = recordTableSample,
                            camOp                = camop_no_problem,
                            stationCol           = "Station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "VTA",
                            occasionLength       = 7,
                            day1                 = "station",
                            includeEffort        = FALSE
)

DetHist1


## ------------------------------------------------------------------------

# make detection history (with trapping effort)
DetHist2 <- detectionHistory(recordTable          = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             timeZone             = "Asia/Kuala_Lumpur",
                             occasionLength       = 7,
                             day1                 = "station",
                             includeEffort        = TRUE,
                             scaleEffort          = FALSE
)

DetHist2[[1]]  # detection history
DetHist2[[2]]  # effort (in days per occasion)

## ------------------------------------------------------------------------

DetHist3 <- detectionHistory(recordTable          = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             timeZone             = "Asia/Kuala_Lumpur",
                             occasionLength       = 7,
                             day1                 = "station",
                             includeEffort        = TRUE,
                             scaleEffort          = TRUE
)

DetHist3[[2]]  # effort (scaled)
DetHist3[[3]]  # scaling parameters for back-transformation

# backtransform scaled effort like this if needed
(DetHist3[[2]] * DetHist3[[3]]$effort.scaled.scale) + DetHist3[[3]]$effort.scaled.center

## ----eval = FALSE--------------------------------------------------------
#  detHist <- read.csv(file = ..., row.names = 1)
#  effort  <- read.csv(file = ..., row.names = 1)

## ------------------------------------------------------------------------

data(recordTableIndividualSample)
data(camtraps)

# create camera operation matrix (with problems/malfunction)
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "%d/%m/%Y"
)

sdh <- spatialDetectionHistory(recordTableIndividual = recordTableIndividualSample, 
                               species               = "LeopardCat",  
                               output                = "binary",
                               camOp                 = camop_problem, 
                               CTtable               = camtraps,
                               stationCol            = "Station", 
                               speciesCol            = "Species",
                               Xcol                  = "utm_x",
                               Ycol                  = "utm_y",
                               individualCol         = "Individual",
                               recordDateTimeCol     = "DateTimeOriginal",
                               recordDateTimeFormat  = "%Y-%m-%d %H:%M:%S",
                               occasionLength        = 10, 
                               day1                  = "survey",
                               includeEffort         = TRUE,
                               timeZone              = "Asia/Kuala_Lumpur"
  )
  
# missing space in species = "LeopardCat" was introduced by recordTableIndividual 
# (because of CRAN package policies. You can have spaces in your directory names)

  summary(sdh)
  plot(sdh, tracks = TRUE)


