context("surveyReport")
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

# camera operation matrices
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    writecsv     = FALSE,
                                    hasProblems  = FALSE,
                                    dateFormat   = "%d/%m/%Y"
)

# with problems/malfunction
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "%d/%m/%Y"
)

# Test section

test_that("errors when camOp missing", {
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_setup_blank,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "'camOp' is not defined", fixed = TRUE)
})


test_that("errors about blank values in Date/Time are correct", {
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_setup_blank,
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are blank values in CTtable[, setupCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_retrieval_blank,
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are blank values in CTtable[, retrievalCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample_datetime_blank,
                             CTtable              = camtraps,
                             camOp                = camop_no_problem,
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
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are NAs in CTtable[, setupCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps_retrieval_NA,
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are NAs in CTtable[, retrievalCol]", fixed = TRUE)
  expect_error(surveyReport (recordTable          = recordTableSample_datetime_NA,
                             CTtable              = camtraps,
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y"), 
               "there are NAs in recordTable[, recordDateTimeCol]", fixed = TRUE)
})



test_that("errors about all NAs in Date/Time are correct", {
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps,
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d-%m-%Y"),            # this is intentionally wrong
               "Cannot read date format in CTtable[, setupCol]. Output is all NA.
expected:  %d-%m-%Y
actual:    02/04/2009", fixed = TRUE)
  
  expect_error(surveyReport (recordTable          = recordTableSample,
                             CTtable              = camtraps,
                             camOp                = camop_no_problem,
                             speciesCol           = "Species",
                             stationCol           = "Station",
                             setupCol             = "Setup_date",
                             retrievalCol         = "Retrieval_date",
                             CTDateFormat         = "%d/%m/%Y",
                             recordDateTimeFormat = "%Y/%m/%d %H:%M:%S"),      # this is intentionally wrong
               "Cannot read datetime format in recordTable[, recordDateTimeCol]. Output is all NA.
expected:  %Y/%m/%d %H:%M:%S
actual:    2009-04-21 00:40:00", fixed = TRUE)

})

