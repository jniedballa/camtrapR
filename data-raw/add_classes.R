# Adding classes to existing datasets

rm(list = ls())
devtools::load_all()

## camtraps.rda ----
load("./data/camtraps.rda")
camtraps
class(camtraps) <- unique(c("cams", class(camtraps)))
attr(camtraps, "stationCol") <- "Station"
camtraps
save(camtraps, file = "./data/camtraps.rda")
rm(list = ls())


## camtrapsMultiSeason.rda ----
load("./data/camtrapsMultiSeason.rda")
camtrapsMultiSeason
class(camtrapsMultiSeason) <- unique(c("cams", class(camtrapsMultiSeason)))
attr(camtrapsMultiSeason, "stationCol") <- "Station"
camtrapsMultiSeason
save(camtrapsMultiSeason, file = "./data/camtrapsMultiSeason.rda")
rm(list = ls())


## recordTableSampleMultiSeason.rda ----
## Note: it would be better to have code creating the file from scratch
load("./data/recordTableSampleMultiSeason.rda")
recordTableSampleMultiSeason
class(recordTableSampleMultiSeason) <- unique(c("records", class(recordTableSampleMultiSeason)))
attr(recordTableSampleMultiSeason, "stationCol") <- "Station"
attr(recordTableSampleMultiSeason, "speciesCol") <- "Species"
recordTableSampleMultiSeason
save(recordTableSampleMultiSeason, file = "./data/recordTableSampleMultiSeason.rda")
rm(list = ls())


## recordTableSample.rda ----
wd_images_ID <- system.file("pictures/sample_images_species_dir", package = "camtrapR")
load("./data/camtraps.rda")

camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "dmy")

recordTableSample <- recordTable(inDir               = wd_images_ID,
                                 IDfrom              = "directory",
                                 minDeltaTime        = 60,
                                 deltaTimeComparedTo = "lastRecord",
                                 exclude             = "UNID",
                                 timeZone            = "Asia/Kuala_Lumpur")
recordTableSample
save(recordTableSample, file = "./data/recordTableSample.rda")
rm(list = ls())


## recordTableIndividualSampleMultiSeason.rda ----
## Note: it would be better to have code creating the file from scratch
load("./data/recordTableIndividualSampleMultiSeason.rda")
recordTableIndividualSampleMultiSeason
class(recordTableIndividualSampleMultiSeason) <- unique(c("records", class(recordTableIndividualSampleMultiSeason)))
attr(recordTableIndividualSampleMultiSeason, "stationCol") <- "Station"
attr(recordTableIndividualSampleMultiSeason, "speciesCol") <- "Species"
recordTableIndividualSampleMultiSeason
save(recordTableIndividualSampleMultiSeason, file = "./data/recordTableIndividualSampleMultiSeason.rda")
rm(list = ls())


## recordTableIndividualSample.rda ----
load("./data/recordTableIndividualSample.rda")
recordTableIndividualSample
class(recordTableIndividualSample) <- unique(c("records", class(recordTableIndividualSample)))
attr(recordTableIndividualSample, "stationCol") <- "Station"
attr(recordTableIndividualSample, "speciesCol") <- "Species"
recordTableIndividualSample
save(recordTableIndividualSample, file = "./data/recordTableIndividualSample.rda")
rm(list = ls())


