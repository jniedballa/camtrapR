context("spatialDetectionHistory")


library(testthat)
library(secr)

# --- Test Setup ---

# 1. Load single-session data
data(recordTableIndividualSample)
data(camtraps)

# 2. Create a standard single-session camera operation matrix
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 hasProblems  = TRUE,
                                 dateFormat   = "dmy")

# 3. Load and prepare multi-session data
data(camtrapsMultiSeason)
# The function expects session IDs to be numeric (1, 2, ...)
camtrapsMultiSeason$session[camtrapsMultiSeason$session == 2009] <- 1
camtrapsMultiSeason$session[camtrapsMultiSeason$session == 2010] <- 2
data(recordTableIndividualSampleMultiSeason)

# 4. Create a multi-session camera operation matrix
camop_session <- cameraOperation(CTtable      = camtrapsMultiSeason,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 sessionCol   = "session",
                                 retrievalCol = "Retrieval_date",
                                 hasProblems  = TRUE,
                                 dateFormat   = "dmy")


# --- Test Suite ---

describe("Core Functionality (Single Session)", {
  
  # Skip all tests in this file if secr is not installed
  skip_if_not_installed("secr")
  
  test_that("it creates a valid single-session capthist object", {
    sdh <- spatialDetectionHistory(
      recordTableIndividual = recordTableIndividualSample,
      species               = "LeopardCat",
      camOp                 = camop_problem,
      CTtable               = camtraps,
      output                = "binary",
      stationCol            = "Station",
      Xcol                  = "utm_x",
      Ycol                  = "utm_y",
      individualCol         = "Individual",
      recordDateTimeCol     = "DateTimeOriginal",
      recordDateTimeFormat  = "ymd HMS",
      occasionLength        = 10,
      day1                  = "survey",
      includeEffort         = TRUE,
      timeZone              = "Asia/Kuala_Lumpur"
    )
    
    # 1. Check main object class and structure
    expect_s3_class(sdh, "capthist")
    expect_false(is.list(sdh))  # Single-session is not a list
    
    # 2. Check dimensions and content
    expect_equal(dim(sdh), c(3,5,3))
    
    # 3. Check traps attribute
    traps_obj <- secr::traps(sdh)
    expect_s3_class(traps_obj, "traps")
    expect_equal(nrow(traps_obj), nrow(camtraps))
    
    # 4. Check usage (effort) attribute
    expect_true(!is.null(secr::usage(traps_obj)))
    expect_equal(dim(secr::usage(traps_obj)), c(nrow(camtraps), 5)) # stations x occasions
  })
})


describe("Multi-Session Functionality", {
  
  skip_if_not_installed("secr")
  
  test_that("it creates a valid multi-session capthist object", {
    sdh_multi <- spatialDetectionHistory(
      recordTableIndividual = recordTableIndividualSampleMultiSeason,
      species               = "LeopardCat",
      camOp                 = camop_session,
      CTtable               = camtrapsMultiSeason,
      sessionCol            = "session",
      output                = "binary",
      stationCol            = "Station",
      Xcol                  = "utm_x",
      Ycol                  = "utm_y",
      individualCol         = "Individual",
      occasionLength        = 10,
      timeZone              = "Asia/Kuala_Lumpur",
      day1                  = "station"
    )
    
    # 1. Check main object class and structure
    expect_s3_class(sdh_multi, "capthist")
    expect_true(is.list(sdh_multi))  # Multi-session IS a list
    expect_length(sdh_multi, 2)      # Two sessions in the data
    
    # 2. Check that list elements are valid capthist objects
    expect_s3_class(sdh_multi[[1]], "capthist")
    expect_s3_class(sdh_multi[[2]], "capthist")
    
  })
})


describe("Covariate Handling", {
  
  skip_if_not_installed("secr")
  
  test_that("it correctly attaches station and individual covariates", {
    sdh_cov <- spatialDetectionHistory(
      recordTableIndividual = recordTableIndividualSampleMultiSeason,
      species               = "LeopardCat",
      camOp                 = camop_session,
      CTtable               = camtrapsMultiSeason,
      sessionCol            = "session",
      Xcol                  = "utm_x",
      Ycol                  = "utm_y",
      individualCol         = "Individual",
      occasionLength        = 10,
      timeZone              = "Asia/Kuala_Lumpur",
      stationCovariateCols  = c("utm_x", "utm_y"), # Use a column from CTtable as a station covariate
      individualCovariateCols = c("Individual", "Species"), # Use columns as individual covariates
      day1                  = "survey"
    )
    
    # 1. Check for station covariates
    # In multi-session, check one of the sessions' traps
    station_covs <- secr::covariates(secr::traps(sdh_cov[[1]]))
    expect_s3_class(station_covs, "data.frame")
    expect_true(all(names(station_covs) %in% c("utm_y", "utm_x")))
    
    # For single covariate, column name is "value". Fix!
    
    
    # 2. Check for individual covariates
    ind_covs <- secr::covariates(sdh_cov)
    expect_s3_class(ind_covs[[1]], "data.frame")
    expect_true(all(c("Individual", "Species") %in% names(ind_covs[[1]])))
  })
})


describe("Output Modes and Other Options", {
  
  skip_if_not_installed("secr")
  
  test_that("output = 'count' works correctly", {
    sdh_count <- spatialDetectionHistory(
      recordTableIndividual = recordTableIndividualSample,
      species               = "LeopardCat",
      camOp                 = camop_problem,
      CTtable               = camtraps,
      output                = "count", # Key argument
      occasionLength        = 10,
      Xcol = "utm_x", Ycol = "utm_y", 
      individualCol = "Individual",
      timeZone = "Asia/Kuala_Lumpur",
      day1 = "station"
    )
    
    # The detector attribute should be 'count'
    expect_equal(secr::detector(secr::traps(sdh_count)), "count")
    
    # The sample data has an individual detected twice in one occasion,
    # so at least one cell value should be > 1.
    expect_true(any(sdh_count > 1))
  })
  
  test_that("makeRMarkInput = TRUE produces a data.frame", {
    rmark_df <- spatialDetectionHistory(
      recordTableIndividual = recordTableIndividualSample,
      species               = "LeopardCat",
      camOp                 = camop_problem,
      CTtable               = camtraps,
      makeRMarkInput        = TRUE, # Key argument
      occasionLength        = 10,
      Xcol                  = "utm_x", 
      Ycol                  = "utm_y", 
      individualCol         = "Individual",
      timeZone              = "Asia/Kuala_Lumpur",
      day1                  = "2009-04-05" 
    )
    
    # 1. Check output class
    expect_s3_class(rmark_df, "data.frame")
    expect_false(inherits(rmark_df, "capthist"))
    
    # 2. Check structure
    expect_true("ch" %in% names(rmark_df)) # Must have a capture history column
    expect_equal(nrow(rmark_df), 3) # One row per individual
    
    # 3. Check capture history string format
    expect_true(all(nchar(rmark_df$ch) == 5))
    expect_true(is.character(rmark_df$ch))
  })
})


describe("Input Validation", {
  
  test_that("it stops for missing or invalid inputs", {
    # Missing camOp
    expect_error(spatialDetectionHistory(recordTableIndividual = recordTableIndividualSample,
                                         species = "LeopardCat",
                                         CTtable = camtraps,
                                         occasionLength = 10,
                                         Xcol = "utm_x", 
                                         Ycol = "utm_y",
                                         individualCol = "Individual"),
                 "'camOp' is not defined")
    
    # Wrong column name
    expect_error(spatialDetectionHistory(recordTableIndividual = recordTableIndividualSample,
                                         species = "LeopardCat",
                                         camOp = camop_problem,
                                         CTtable = camtraps,
                                         occasionLength = 10,
                                         Xcol = "WRONG_X_COL", Ycol = "utm_y",
                                         individualCol = "Individual"),
                 "is not a column name in CTtable")
  })
})