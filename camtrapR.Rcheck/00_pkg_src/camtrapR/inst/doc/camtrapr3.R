## ----message = FALSE, results = "hide"----------------------------------------
library(camtrapR)
library(secr)

## -----------------------------------------------------------------------------
# find the directory with sample images contained in the package
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR", lib.loc = .libPaths())

## -----------------------------------------------------------------------------
length(list.files(wd_images_ID, pattern = "JPG", recursive = TRUE))

## -----------------------------------------------------------------------------
rec.db.species0 <- recordTable(inDir  = wd_images_ID,
                               IDfrom = "directory")

head(rec.db.species0)

## -----------------------------------------------------------------------------
list.files(file.path(wd_images_ID, "StationB", "MNE"))

## -----------------------------------------------------------------------------
rec.db.species60 <- recordTable(inDir               = wd_images_ID,
                                IDfrom              = "directory",
                                minDeltaTime        = 60,
                                deltaTimeComparedTo = "lastRecord",
                                timeZone            = "Asia/Kuala_Lumpur")

nrow(rec.db.species60)

## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
exifTagNames(inDir = wd_images_ID, returnMetadata = FALSE)

## -----------------------------------------------------------------------------
exifTagNames(inDir = wd_images_ID, returnMetadata = TRUE)

## -----------------------------------------------------------------------------
rec.db.species.metadata1 <- recordTable(inDir                  = wd_images_ID,
                                        IDfrom                 = "directory",
                                        timeZone               = "Asia/Kuala_Lumpur",
                                        additionalMetadataTags = c("EXIF:Model", "EXIF:Make"))

head(rec.db.species.metadata1)

## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
head(rec.db.pbe)

## -----------------------------------------------------------------------------
 # first load the camera trap station table

data(camtraps)

dateFormat <- "%d/%m/%Y"
 
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = dateFormat
)

# as a reminder, these are the dates in our station information table
camtraps[,-which(colnames(camtraps) %in% c("utm_y", "utm_x"))]
# now let's have a look at the first few columns of the camera operation matrix
camop_problem[, 1:5]
# and the last few
camop_problem[, (ncol(camop_problem)-6):ncol(camop_problem)]

## -----------------------------------------------------------------------------
camopPlot <- function(camOp, 
                      palette = "Red-Yellow"){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  values_tmp <- na.omit(unique(c(camOp)))

  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = hcl.colors(n = length(values_tmp), palette = palette, rev = TRUE))
  
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}

## -----------------------------------------------------------------------------
camopPlot(camOp = camop_problem)

## ----eval = FALSE-------------------------------------------------------------
#  camOp <- read.csv(file = ..., row.names = 1, check.names = FALSE)

## -----------------------------------------------------------------------------

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = dateFormat
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


## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------

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

## ----eval = FALSE-------------------------------------------------------------
#  detHist <- read.csv(file = ..., row.names = 1)
#  effort  <- read.csv(file = ..., row.names = 1)

## -----------------------------------------------------------------------------

data(recordTableIndividualSample)

# create camera operation matrix (with problems/malfunction)
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = dateFormat
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
# (because of CRAN package policies). 
# In your own data you can have spaces in your directory names.

  summary(sdh)
  plot(sdh, tracks = TRUE)


## -----------------------------------------------------------------------------
# load multi-season data
data(camtrapsMultiSeason)
data(recordTableSampleMultiSeason)

# also, for clarity, lets remove all unnecessary columns from the record table
recordTableSampleMultiSeason <- recordTableSampleMultiSeason[, c("Station", "Species", "DateTimeOriginal")]

# create camera operation matrix
camop_season <- cameraOperation(CTtable         = camtrapsMultiSeason,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    sessionCol   = "session",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = TRUE,
                                    dateFormat   = dateFormat
)

# plot camera operation matrix
par(oma = c(0,7,0,0))
camopPlot(camop_season)


# make multi-season detection history
DetHist_multi <- detectionHistory(recordTable   = recordTableSampleMultiSeason,
                            camOp                = camop_season,
                            stationCol           = "Station",
                            speciesCol           = "Species",
                            species              = "VTA",
                            occasionLength       = 10,
                            day1                 = "station",
                            recordDateTimeCol     = "DateTimeOriginal",
                            includeEffort        = TRUE,
                            scaleEffort          = FALSE,
                            timeZone             = "UTC",
                            unmarkedMultFrameInput = TRUE
)

DetHist_multi

## -----------------------------------------------------------------------------

year_matrix <- matrix(unique(as.character(camtrapsMultiSeason$session)),
               ncol = length(unique(as.character(camtrapsMultiSeason$session))),
               nrow = length(unique(camtrapsMultiSeason$Station)),
               byrow = TRUE)

# this is a made up example table with station covariates for demonstration
site_covariates <- data.frame(Station = rownames(DetHist_multi$detection_history),
                              elevation = c(100, 200, 500, 300),
                              treecover = c(80, 100, 50, 10))

umf <- unmarked::unmarkedMultFrame(y = DetHist_multi$detection_history,
                                   siteCovs = site_covariates,
                                   yearlySiteCovs = list(year = year_matrix),
                                   obsCovs = list(effort = DetHist_multi$effort),
                                   numPrimary = 2)



colext_example <- unmarked::colext(psiformula = ~ treecover,    # First-year occupancy
                                   gammaformula = ~ 1,          # Colonization
                                   epsilonformula = ~ 1,        # Extinction
                                   pformula = ~ effort,         # Detection
                                   data = umf,
                                   method="BFGS")
summary(colext_example)

## -----------------------------------------------------------------------------
camtrapsMultiSeason$session[camtrapsMultiSeason$session == 2009] <- 1
camtrapsMultiSeason$session[camtrapsMultiSeason$session == 2010] <- 2

## -----------------------------------------------------------------------------

# we also want a few records in season 2
recordTableIndividualSample_season2 <- recordTableIndividualSample[1:10,]
recordTableIndividualSample_season2$DateTimeOriginal <- gsub("2009", "2010", recordTableIndividualSample_season2$DateTimeOriginal)
recordTableIndividualSample_season <- rbind(recordTableIndividualSample, recordTableIndividualSample_season2)



# for clarity, lets remove all unnecessary columns
recordTableIndividualSample_season <- recordTableIndividualSample_season[, c("Station", "Species", "Individual", "DateTimeOriginal")]


## -----------------------------------------------------------------------------

# create camera operation matrix (with problems/malfunction), same as above for multi-season occupancy
camop_season <- cameraOperation(CTtable         = camtrapsMultiSeason,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    sessionCol   = "session",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = TRUE,
                                    dateFormat   = dateFormat
)


# create capthist object
sdh_multi <- spatialDetectionHistory(recordTableIndividual = recordTableIndividualSample_season,
                               species                 = "LeopardCat",
                               output                  = "binary",
                               camOp                   = camop_season,
                               CTtable                 = camtrapsMultiSeason,
                               stationCol              = "Station",
                               speciesCol              = "Species",
                               sessionCol              = "session",
                               Xcol                    = "utm_x",
                               Ycol                    = "utm_y",
                               individualCol           = "Individual",
                               recordDateTimeCol       = "DateTimeOriginal",
                               recordDateTimeFormat    = "%Y-%m-%d %H:%M:%S",
                               occasionLength          = 10,
                               day1                    = "survey",
                               includeEffort           = TRUE,
                               timeZone                = "Asia/Kuala_Lumpur",
                               stationCovariateCols    = "utm_y",       # made up, a potential site covariate from camtrapsMultiSeason
                               individualCovariateCols = "Individual"   # made up, a potential individual covariate from recordTable
  )


  summary(sdh_multi)
  par(mfrow = c(1,2))
  plot(sdh_multi)

  secr.fit.example <- secr.fit(capthist = sdh_multi,
                               start = c(-1, -2, 10))   # with starting values, since its only very little data
  summary(secr.fit.example)


