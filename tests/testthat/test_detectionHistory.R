context("detectionHistory")
library(camtrapR)


# load station information
data(camtraps)

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "%d/%m/%Y"
)

data(recordTableSample)    # load the record history

# compute detection history for a species

# without trapping effort
DetHist1 <- detectionHistory(recordTable          = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             occasionLength       = 7,
                             day1                 = "station",
                             datesAsOccasionNames = FALSE,
                             includeEffort        = FALSE,
                             timeZone             = "Asia/Kuala_Lumpur"
)


# with effort / using base R to define recordDateTimeFormat
DetHist2 <- detectionHistory(recordTable          = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             occasionLength       = 7,
                             day1                 = "station",
                             datesAsOccasionNames = FALSE,
                             includeEffort        = TRUE,
                             scaleEffort          = FALSE,
                             timeZone             = "Asia/Kuala_Lumpur"
)

DetHist2a <- detectionHistory(recordTable          = recordTableSample,
                              camOp                = camop_no_problem,
                              stationCol           = "Station",
                              speciesCol           = "Species",
                              recordDateTimeCol    = "DateTimeOriginal",
                              species              = "VTA",
                              occasionLength       = 7,
                              day1                 = "station",
                              datesAsOccasionNames = FALSE,
                              includeEffort        = TRUE,
                              scaleEffort          = TRUE,
                              timeZone             = "Asia/Kuala_Lumpur"
)


# with effort / using lubridate package to define recordDateTimeFormat
DetHist2_lub <- detectionHistory(recordTable          = recordTableSample,
                                 camOp                = camop_no_problem,
                                 stationCol           = "Station",
                                 speciesCol           = "Species",
                                 recordDateTimeCol    = "DateTimeOriginal",
                                 recordDateTimeFormat = "ymd HMS",
                                 species              = "VTA",
                                 occasionLength       = 7,
                                 day1                 = "station",
                                 datesAsOccasionNames = FALSE,
                                 includeEffort        = TRUE,
                                 scaleEffort          = FALSE,
                                 timeZone             = "Asia/Kuala_Lumpur"
)



DetHist1day <- detectionHistory(recordTable          = recordTableSample,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "VTA",
                                occasionLength       = 1,
                                day1                 = "station",
                                datesAsOccasionNames = F,
                                includeEffort        = T,
                                scaleEffort          = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"
)

# day1 = "survey", datesAsOccasionNames = TRUE
DetHist1day_survey <- detectionHistory(recordTable          = recordTableSample,
                                       camOp                = camop_no_problem,
                                       stationCol           = "Station",
                                       speciesCol           = "Species",
                                       recordDateTimeCol    = "DateTimeOriginal",
                                       species              = "VTA",
                                       occasionLength       = 1,
                                       day1                 = "survey",
                                       datesAsOccasionNames = TRUE,
                                       includeEffort        = TRUE,
                                       scaleEffort          = FALSE,
                                       timeZone             = "Asia/Kuala_Lumpur"
)

# multi-season detection history

# load multi-season data
data(camtrapsMultiSeason)
data(recordTableSampleMultiSeason)

# multi-season camera operation matrix
camop_season <- cameraOperation(CTtable      = camtrapsMultiSeason,
                                stationCol   = "Station",
                                setupCol     = "Setup_date",
                                sessionCol   = "session",
                                retrievalCol = "Retrieval_date",
                                hasProblems  = TRUE,
                                dateFormat   = "%d/%m/%Y"
)

# multi-season detection history
DetHist_multi <- detectionHistory(recordTable            = recordTableSampleMultiSeason,
                                  camOp                  = camop_season,
                                  stationCol             = "Station",
                                  speciesCol             = "Species",
                                  species                = "VTA",
                                  occasionLength         = 10,
                                  day1                   = "station",
                                  recordDateTimeCol      = "DateTimeOriginal",
                                  includeEffort          = TRUE,
                                  scaleEffort            = FALSE,
                                  timeZone               = "UTC",
                                  unmarkedMultFrameInput = TRUE
)

# multi, unmarkedMultFrameInput = FALSE  
DetHist_multi2 <- detectionHistory(recordTable            = recordTableSampleMultiSeason,
                                   camOp                  = camop_season,
                                   stationCol             = "Station",
                                   speciesCol             = "Species",
                                   species                = "VTA",
                                   occasionLength         = 10,
                                   day1                   = "station",
                                   recordDateTimeCol      = "DateTimeOriginal",
                                   includeEffort          = TRUE,
                                   scaleEffort            = FALSE,
                                   timeZone               = "UTC",
                                   unmarkedMultFrameInput = FALSE  
)

# multi, occasionLength = 1, day1 = "survey"
DetHist_multi3 <- detectionHistory(recordTable            = recordTableSampleMultiSeason,
                                   camOp                  = camop_season,
                                   stationCol             = "Station",
                                   speciesCol             = "Species",
                                   species                = "VTA",
                                   occasionLength         = 1,
                                   day1                   = "survey",
                                   recordDateTimeCol      = "DateTimeOriginal",
                                   includeEffort          = TRUE,
                                   scaleEffort            = FALSE,
                                   timeZone               = "UTC",
                                   datesAsOccasionNames   = TRUE,
                                   unmarkedMultFrameInput = TRUE
)



# Test section

test_that("recordTable output has correct class and names", {
  expect_is(DetHist1,   "list")
  expect_is(DetHist2,   "list")
  expect_is(DetHist2a,   "list")
  expect_is(DetHist2_lub, "list")
  expect_is(DetHist_multi, "list")
  
  expect_equal(length(DetHist1), 1)
  expect_equal(length(DetHist2),   2)
  expect_equal(length(DetHist2a),   3)
  expect_equal(length(DetHist2_lub), 2)
  expect_equal(length(DetHist_multi), 2)
  
  expect_equal(names(DetHist1),   "detection_history")
  expect_equal(names(DetHist2),   c("detection_history", "effort"))
  expect_equal(names(DetHist2a),   c("detection_history", "effort",  "effort_scaling_parameters"))
  expect_equal(names(DetHist2_lub), c("detection_history", "effort"))
  expect_equal(names(DetHist_multi), c("detection_history", "effort"))
  
})

test_that("detectionHistory output has correct dimensions", {
  expect_equal(dim(DetHist1$detection_history),   c(3,7))
  expect_equal(dim(DetHist2$detection_history),   c(3,7))
  expect_equal(dim(DetHist2$effort), c(3,7))
  expect_equal(dim(DetHist_multi$detection_history), c(4,16))
  expect_equal(dim(DetHist_multi$effort),  c(4,16))
  expect_equal(dim(DetHist1day$detection_history),  c(3,44))
  expect_equal(dim(DetHist1day_survey$detection_history),  c(3,46))
  expect_equal(dim(DetHist_multi2$detection_history),  c(7,8))
  expect_equal(dim(DetHist_multi3$detection_history),  c(4,884))
})

test_that("detectionHistory output has correct row names", {
  
  expect_rownames <- c("StationA", "StationB", "StationC")
  expect_rownames_multi <- c("StationA", "StationB", "StationC", "StationD")
  expect_rownames_multi2 <- c("StationA__SESS_2009", "StationB__SESS_2009", "StationC__SESS_2009", 
                              "StationA__SESS_2010", "StationB__SESS_2010", "StationC__SESS_2010", "StationD__SESS_2010")
  
  expect_equal(rownames(DetHist1$detection_history),  expect_rownames)
  expect_equal(rownames(DetHist2$detection_history),   expect_rownames)
  expect_equal(rownames(DetHist2$effort), expect_rownames)
  expect_equal(rownames(DetHist1day$detection_history),  expect_rownames)
  expect_equal(rownames(DetHist1day_survey$detection_history),  expect_rownames)
  expect_equal(rownames(DetHist_multi$detection_history), expect_rownames_multi)
  expect_equal(rownames(DetHist_multi$effort),  expect_rownames_multi)
  expect_equal(rownames(DetHist_multi2$detection_history),  expect_rownames_multi2)
  expect_equal(rownames(DetHist_multi3$detection_history),  expect_rownames_multi)
})


test_that("occasionLength error message works", {
  expect_error(detectionHistory(recordTable          = recordTableSample,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "VTA",
                                occasionLength       = 47,
                                day1                 = "station",
                                datesAsOccasionNames = FALSE,
                                includeEffort        = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"), 
               "occasionLength must be smaller than the total number of days in camOp")
  
  expect_error(detectionHistory(recordTable          = recordTableSample,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "VTA",
                                occasionLength       = 0,
                                day1                 = "station",
                                datesAsOccasionNames = FALSE,
                                includeEffort        = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"), 
               "occasionLength must be a positive integer and not 0")
})






test_that("Warnings when day1 is a date and there are records before day1", {
  expect_warning(detectionHistory(recordTable          = recordTableSample,
                                  camOp                = camop_no_problem,
                                  stationCol           = "Station",
                                  speciesCol           = "Species",
                                  recordDateTimeCol    = "DateTimeOriginal",
                                  species              = "VTA",
                                  occasionLength       = 7,
                                  day1                 = "2009-05-01",
                                  datesAsOccasionNames = FALSE,
                                  includeEffort        = FALSE,
                                  timeZone             = "Asia/Kuala_Lumpur"),
                 "4 records (out of 5) were removed because they were taken before day1", fixed = TRUE)
}                             
)


test_that("Error when day1 is after camera retrieval", {
  expect_error(detectionHistory(recordTable          = recordTableSample,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "VTA",
                                occasionLength       = 7,
                                day1                 = "2009-06-01",
                                datesAsOccasionNames = FALSE,
                                includeEffort        = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"),
               "day1 (2009-06-01) is after the last station's retrieval date (2009-05-17)", fixed = TRUE)
}                             
)


test_that("Error when day1 is before camera setup", {
  expect_error(detectionHistory(recordTable          = recordTableSample,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "VTA",
                                occasionLength       = 7,
                                day1                 = "2009-04-01",
                                maxNumberDays        = 10,
                                datesAsOccasionNames = FALSE,
                                includeEffort        = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"),
               "day1 (2009-04-01) is before the first station's setup date (2009-04-02)", fixed = TRUE)
}                             
)


test_that("Warning when timeZone not defined", {
  expect_warning(detectionHistory(recordTable          = recordTableSample,
                                  camOp                = camop_no_problem,
                                  stationCol           = "Station",
                                  speciesCol           = "Species",
                                  recordDateTimeCol    = "DateTimeOriginal",
                                  species              = "VTA",
                                  occasionLength       = 7,
                                  day1                 = "station", 
                                  includeEffort        = FALSE),
                 "timeZone is not specified. Assuming UTC", fixed = TRUE)
}                             
)



test_that("Error when camOp is not a matrix", {
  expect_error(detectionHistory(recordTable          = recordTableSample,
                                camOp                = "camop_no_problem",
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "VTA",
                                occasionLength       = 7,
                                day1                 = "station", 
                                includeEffort        = TRUE,
                                timeZone             = "Asia/Kuala_Lumpur"),
               "camOp must be a matrix", fixed = TRUE)
}                             
)


test_that("Error when NAs/blanks in DateTimeOriginal", {
  recordTableSample_withNA <- recordTableSample_with_blank <- recordTableSample
  recordTableSample_withNA$DateTimeOriginal[1] <- NA
  recordTableSample_with_blank$DateTimeOriginal[1] <- ""
  
  expect_error(detectionHistory(recordTable          = recordTableSample_withNA,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "PBE",
                                occasionLength       = 7,
                                day1                 = "station",
                                datesAsOccasionNames = FALSE,
                                includeEffort        = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"
  ),
               "there are NAs in subset_species[, recordDateTimeCol]", fixed = TRUE)
  
  expect_error(detectionHistory(recordTable          = recordTableSample_with_blank,
                                camOp                = camop_no_problem,
                                stationCol           = "Station",
                                speciesCol           = "Species",
                                recordDateTimeCol    = "DateTimeOriginal",
                                species              = "PBE",
                                occasionLength       = 7,
                                day1                 = "station", 
                                includeEffort        = FALSE,
                                timeZone             = "Asia/Kuala_Lumpur"),
               "there are blank values in subset_species[, recordDateTimeCol]", fixed = TRUE)
}                             
)

test_that("Error when DateTimeOriginal can't be interpreted (format wrong)", {
  # suppressWarnings is because parse_date_time() gives a warning too:  All formats failed to parse. No formats found. 
  expect_error(suppressWarnings(detectionHistory(recordTable  = recordTableSample,
                                                 camOp                = camop_no_problem,
                                                 stationCol           = "Station",
                                                 speciesCol           = "Species",
                                                 recordDateTimeCol    = "DateTimeOriginal",
                                                 recordDateTimeFormat = "mdy HMS",  # wrong format
                                                 species              = "PBE",
                                                 occasionLength       = 7,
                                                 day1                 = "station", 
                                                 includeEffort        = FALSE,
                                                 timeZone             = "Asia/Kuala_Lumpur")),
               "Cannot read datetime format in subset_species[, recordDateTimeCol]. Output is all NA.\nexpected:  mdy HMS\nactual:    2009-04-21 00:40:00", fixed = TRUE)
}                             
)





# PART 2: camera operation matrix with fraction days!

camtraps_hrs <- camtraps

camtraps_hrs$Setup_date <- paste(camtraps_hrs$Setup_date, "12")
camtraps_hrs$Retrieval_date <- paste(camtraps_hrs$Retrieval_date, "18")
camtraps_hrs$Problem1_from  <- paste(camtraps_hrs$Problem1_from, "12")
camtraps_hrs$Problem1_to  <- paste(camtraps_hrs$Problem1_to, "18")


# create camera operation matrix
camop_no_problem_frac <- cameraOperation(CTtable      = camtraps_hrs,
                                         stationCol   = "Station",
                                         setupCol     = "Setup_date",
                                         retrievalCol = "Retrieval_date",
                                         hasProblems  = FALSE,
                                         dateFormat   = "dmy H"
)

# add some records on incomplete days
recordTableSample_extra <- recordTableSample_extra2 <- recordTableSample[5,]
recordTableSample_extra$DateTimeOriginal  <- "2009-04-02 05:07:00"  # on first day / StationA
recordTableSample_extra2$DateTimeOriginal <- "2009-05-14 05:07:00"  # on last day / StationA


recordTableSample2 <- rbind(recordTableSample_extra, recordTableSample, recordTableSample_extra2)

test_that("it works when camera operation matrix uses fraction of days & there's records on those days", {

DetHist2_lub_frac <- detectionHistory(recordTable          = recordTableSample2,
                                      camOp                = camop_no_problem_frac,
                                      stationCol           = "Station",
                                      speciesCol           = "Species",
                                      recordDateTimeCol    = "DateTimeOriginal",
                                      recordDateTimeFormat = "ymd HMS",
                                      species              = "VTA",
                                      occasionLength       = 7,
                                      day1                 = "station",
                                      datesAsOccasionNames = FALSE,
                                      includeEffort        = TRUE,
                                      scaleEffort          = FALSE,
                                      timeZone             = "Asia/Kuala_Lumpur"
)
expect_type(DetHist2_lub_frac, type = "list")                                      
})
