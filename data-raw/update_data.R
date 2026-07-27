# Adding classes to existing datasets
## FIXME: this script should ultimately create all datasets from scratch
## rather than loading existing data to modify them

rm(list = ls())
devtools::load_all()

## camtraps.rda ----
load("./data/camtraps.rda")
camtraps
camtraps <- as_cams(camtraps, stationCol = "Station")
camtraps
save(camtraps, file = "./data/camtraps.rda")
rm(list = ls())


## camtrapsMultiSeason.rda ----
load("./data/camtraps.rda")
camtraps_season2 <- camtraps
# change 2009 to 2010
camtraps_season2[, "Setup_date"]     <- gsub("2009", "2010", camtraps_season2[, "Setup_date"])
camtraps_season2[, "Retrieval_date"] <- gsub("2009", "2010", camtraps_season2[, "Retrieval_date"])
camtraps_season2[, "Problem1_from"]  <- gsub("2009", "2010", camtraps_season2[, "Problem1_from"])
camtraps_season2[, "Problem1_to"]    <- gsub("2009", "2010", camtraps_season2[, "Problem1_to"])
# add an extra station with different dates in session 2010
camtraps_season2 <- rbind(camtraps_season2, NA)
camtraps_season2$Station[4] <- "StationD"
camtraps_season2$utm_y[4]  <- 607050
camtraps_season2$utm_x[4]  <- 525000
camtraps_season2$Setup_date[4]      <- "04/04/2010"
camtraps_season2$Retrieval_date[4]  <- "17/06/2010"
camtraps_season2$Problem1_from[4]   <- "20/05/2010"
camtraps_season2$Problem1_to[4]     <- "30/05/2010"
# add season column
camtraps$session         <- 2009
camtraps_season2$session <- 2010
# combine the tables for 2 seasons
camtrapsMultiSeason <- rbind(camtraps, camtraps_season2)
camtrapsMultiSeason
save(camtrapsMultiSeason, file = "./data/camtrapsMultiSeason.rda")
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


## recordTableSampleMultiSeason.rda ----
load("./data/recordTableSample.rda")
recordTableSample_season2 <- recordTableSample
# substitute 2009 with 2010
recordTableSample_season2$DateTimeOriginal <- gsub("2009", "2010", 
                                                   recordTableSample_season2$DateTimeOriginal) 
# combine with season 2009
recordTableSampleMultiSeason <- rbind(recordTableSample, recordTableSample_season2)  
recordTableSampleMultiSeason
save(recordTableSampleMultiSeason, file = "./data/recordTableSampleMultiSeason.rda")
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


## recordTableIndividualSampleMultiSeason.rda ----
load("./data/recordTableIndividualSample.rda")
recordTableIndividualSample_season2 <- recordTableIndividualSample[1:10,]
recordTableIndividualSample_season2$DateTimeOriginal <- gsub("2009", "2010", 
     recordTableIndividualSample_season2$DateTimeOriginal)
recordTableIndividualSampleMultiSeason <- rbind(recordTableIndividualSample, 
                                                recordTableIndividualSample_season2)
recordTableIndividualSampleMultiSeason
save(recordTableIndividualSampleMultiSeason, file = "./data/recordTableIndividualSampleMultiSeason.rda")
rm(list = ls())
