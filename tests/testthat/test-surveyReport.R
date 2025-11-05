
context("surveyReport")


library(testthat)
library(withr)

# --- Test Setup ---
# Create shared data and camera operation matrices once.
data(camtraps)
data(recordTableSample)

# Camera operation matrix WITHOUT problems
camop_no_problem <- cameraOperation(
  CTtable      = camtraps,
  stationCol   = "Station",
  setupCol     = "Setup_date",
  retrievalCol = "Retrieval_date",
  hasProblems  = FALSE,
  dateFormat   = "dmy"
)

# Camera operation matrix WITH problems
camop_problem <- cameraOperation(
  CTtable      = camtraps,
  stationCol   = "Station",
  setupCol     = "Setup_date",
  retrievalCol = "Retrieval_date",
  hasProblems  = TRUE,
  dateFormat   = "dmy"
)

# camtrapR:::camopPlot(camop_problem)

# --- Test Suite ---

testthat::describe("Core Functionality and Return Value", {
  
  test_that("it returns a list of 5 data frames with correct structure", {
    report <- surveyReport(
      recordTable  = recordTableSample,
      CTtable      = camtraps,
      camOp        = camop_no_problem,
      setupCol     = "Setup_date",
      retrievalCol = "Retrieval_date",
      CTDateFormat = "dmy"
    )
    
    # 1. Check the main output structure
    expect_true(class(report) == "list")
    expect_length(report, 5)
    expected_names <- c("survey_dates", "species_by_station", "events_by_species",
                        "events_by_station", "events_by_station2")
    expect_named(report, expected_names)
    
    # 2. Check that each element is a data frame
    expect_s3_class(report$survey_dates, "data.frame")
    expect_s3_class(report$species_by_station, "data.frame")
    expect_s3_class(report$events_by_species, "data.frame")
    expect_s3_class(report$events_by_station, "data.frame")
    expect_s3_class(report$events_by_station2, "data.frame")
  })
  
  test_that("calculations are correct for a survey without problems", {
    report <- surveyReport(
      recordTable  = recordTableSample,
      CTtable      = camtraps,
      setupCol     = "Setup_date",
      retrievalCol = "Retrieval_date",
      camOp        = camop_no_problem,
      CTDateFormat = "dmy"
    )
    
    # Check key calculations in the survey_dates table
    survey_dates <- report$survey_dates
    stationA_dates <- survey_dates[survey_dates$Station == "StationA", ]
    
    # Setup: 2009-04-02, Retrieval: 2009-05-14. Total duration: 43 calendar days
    expect_equal(stationA_dates$n_calendar_days_total, 43)
    # No problems, so active days should be the same
    expect_equal(stationA_dates$n_calendar_days_active, 43)
    expect_equal(stationA_dates$n_calendar_days_inactive, 0)
    # Trap nights = active days * number of cameras (1)
    expect_equal(stationA_dates$n_trap_nights_active, 42)
    
    # Check events_by_species
    events_species <- report$events_by_species
    pbe_events <- events_species[events_species$species == "PBE", ]
    expect_equal(as.numeric(pbe_events$n_events), 18) # 18 records for PBE
    expect_equal(as.numeric(pbe_events$n_stations), 3) # Detected at all three stations
  })
  
  test_that("calculations are correct for a survey WITH problems", {
    report_problem <- surveyReport(
      recordTable  = recordTableSample,
      CTtable      = camtraps,
      setupCol     = "Setup_date",
      retrievalCol = "Retrieval_date",
      camOp        = camop_problem,
      CTDateFormat = "dmy"
    )
    
    survey_dates_prob <- report_problem$survey_dates
    stationC_dates_prob <- survey_dates_prob[survey_dates_prob$Station == "StationC", ]
    
    # Station1 had a problem from May 12 to May 17 (6 days)
    # Total days = 44
    # Inactive days = 6
    # Active days = 38
    expect_equal(stationC_dates_prob$n_calendar_days_total, 44)
    expect_equal(stationC_dates_prob$n_calendar_days_active, 38)
    expect_equal(stationC_dates_prob$n_calendar_days_inactive, 6)
    expect_equal(stationC_dates_prob$n_trap_nights_active, 37.5)
  })
  
})


testthat::describe("File Writing and Side Effects", {
  
  # with_tempdir({
  
  tmpdir <- tempdir()
  
    test_that("sinkpath argument creates a text report", {
      report <- surveyReport(
        recordTable  = recordTableSample,
        CTtable      = camtraps,
        setupCol     = "Setup_date",
        retrievalCol = "Retrieval_date",
        camOp        = camop_no_problem,
        CTDateFormat = "dmy",
        sinkpath     = tmpdir #"." # Write to the temp directory
      )
      
      # Check for the report file
      report_files <- list.files(tmpdir, pattern = paste0("survey_report_", Sys.Date(), ".txt"), full.names = T)
      expect_equal(length(report_files), 1)
      
      
      # Check that the file has content
      report_content <- readLines(report_files[1])
      expect_gt(length(report_content), 10)
      expect_true(any(grepl("Total number of stations", report_content)))
    })
    
    
    
    test_that("makezip = TRUE creates a valid zip file with expected contents", {


      report <- surveyReport(
        recordTable  = recordTableSample,
        CTtable      = camtraps,
        setupCol     = "Setup_date",
        retrievalCol = "Retrieval_date",
        camOp        = camop_no_problem,
        CTDateFormat = "dmy",
        Xcol         = "utm_x",
        Ycol         = "utm_y",
        sinkpath     = tmpdir, #".",
        makezip      = TRUE
      )


    # 1. Check if the zip file was created
    zip_files <- list.files(tmpdir, pattern = paste0("surveyReport_", Sys.Date(), ".zip"), full.names = T)
    expect_equal(length(zip_files), 1)

    # 2. Check the contents of the zip file
    zip_contents <- utils::unzip(zip_files[1], list = TRUE)

    # Expected files/folders
    expect_true("camtrapR_scripts.R" %in% basename(zip_contents$Name))

    # Check for specific files
    expect_true("recordTable.csv" %in% basename(zip_contents$Name))
    n_species <- length(unique(recordTableSample$Species))
    expect_equal(sum(grepl("activity_", zip_contents$Name)), n_species)
    expect_equal(sum(grepl("Presence", zip_contents$Name)), n_species)
  })
})
# })



testthat::describe("Date/time column checks", {
  
  
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
})
