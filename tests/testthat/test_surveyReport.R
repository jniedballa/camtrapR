context("recordTable")
library(camtrapR)

data(camtraps)
data(recordTableSample)

camtraps_setup_blank    <- camtraps
camtraps_retrieval_blank <- camtraps
camtraps_setup_blank$Setup_date[1] <- ""
camtraps_retrieval_blank$Retrieval_date[1] <- ""

recordTableSample_datetime_blank <- recordTableSample
recordTableSample_datetime_blank$DateTimeOriginal[1] <- ""

camtraps_setup_NA <- camtraps_setup_blank
camtraps_retrieval_NA <- camtraps_retrieval_blank
recordTableSample_datetime_NA <- recordTableSample_datetime_blank

camtraps_setup_NA$Setup_date[1] <- NA
camtraps_retrieval_NA$Retrieval_date[1] <- NA
recordTableSample_datetime_NA$DateTimeOriginal[1] <- NA

# Test section

test_that("errors about blank values in Date/Time are correct", {
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_setup_blank,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are blank values in CTtable[, setupCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_retrieval_blank,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are blank values in CTtable[, retrievalCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample_datetime_blank,
                             CTtable              = camtraps,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are blank values in recordTable[, recordDateTimeCol]", fixed = TRUE)
})



test_that("errors about NAs in Date/Time are correct", {
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_setup_NA,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are NAs in CTtable[, setupCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_retrieval_NA,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are NAs in CTtable[, retrievalCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample_datetime_NA,
                             CTtable              = camtraps,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are NAs in recordTable[, recordDateTimeCol]", fixed = TRUE)
})

