context("filterRecordTable")

# Load necessary library
library(testthat)

# --- Test Setup ---
# This setup code prepares the data needed for the tests.
# It will be run inside the test blocks to ensure independence.

# Define the path to the sample images
wd_images_ID_species <- system.file("pictures/sample_images_species_dir",
                                    package = "camtrapR")

# --- Test Suite ---

describe("Core Filtering Logic", {
  
  # Skip all tests in this describe block if ExifTool is not available
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  # Create the initial, unfiltered record table once for this block
  rec_table <- recordTable(inDir       = wd_images_ID_species,
                           IDfrom      = "directory",
                           minDeltaTime= 0,
                           exclude     = "UNID",
                           timeZone    = "Asia/Kuala_Lumpur")
  
  test_that("filtering with 'lastIndependentRecord' matches the example", {
    # 1. Verify the initial state matches the example
    expect_equal(nrow(rec_table), 55)
    
    # 2. Apply the filtering from the example
    rec_table_filt <- filterRecordTable(recordTable         = rec_table,
                                        minDeltaTime        = 60,
                                        deltaTimeComparedTo = "lastIndependentRecord")
    
    # 3. Verify the final state matches the example
    expect_equal(nrow(rec_table_filt), 39)
    
    # 4. Perform additional checks on the output
    expect_s3_class(rec_table_filt, "data.frame")
    expect_true(all(names(rec_table_filt) %in% names(rec_table))) # Columns should be identical
  })
  
  test_that("filtering with 'lastRecord' is stricter", {
    # 'lastRecord' compares against every previous record, not just independent ones.
    # This should result in fewer (or equal) records than 'lastIndependentRecord'.
    rec_table_filt_last <- filterRecordTable(recordTable         = rec_table,
                                             minDeltaTime        = 60,
                                             deltaTimeComparedTo = "lastRecord")
    
    # For this specific dataset, the result is also 39
    expect_equal(nrow(rec_table_filt_last), 39)
    
    # In general, it must be less than or equal to the other method
    rec_table_filt_independent <- filterRecordTable(recordTable = rec_table,
                                                    minDeltaTime = 60,
                                                    deltaTimeComparedTo = "lastIndependentRecord")
    expect_lte(nrow(rec_table_filt_last), nrow(rec_table_filt_independent))
  })
})


describe("Edge Cases", {
  
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  rec_table <- recordTable(inDir = wd_images_ID_species, IDfrom = "directory", minDeltaTime = 0)
  
  test_that("minDeltaTime = 0 returns all records", {
    # A delta time of 0 means no filtering should occur.
    rec_table_filt_zero <- filterRecordTable(recordTable  = rec_table,
                                             minDeltaTime = 0)
    
    expect_equal(nrow(rec_table_filt_zero), nrow(rec_table))
  })
  
  test_that("a very large minDeltaTime returns one record per station/species", {
    # A huge time gap should result in only the very first record for each
    # combination of Station and Species being kept.
    rec_table_filt_large <- filterRecordTable(recordTable  = rec_table,
                                              minDeltaTime = 999999,
                                              deltaTimeComparedTo = "lastRecord")
    
    # Calculate the expected number of rows: one for each unique combo
    expected_rows <- nrow(unique(rec_table[, c("Station", "Species")]))
    
    expect_equal(nrow(rec_table_filt_large), expected_rows)
  })
})


describe("Input Validation", {
  
  # This describe block does NOT need ExifTool, as it tests inputs, not logic.
  # We create a minimal dummy data frame for these tests.
  dummy_rec_table <- data.frame(
    Station = "S1",
    Species = "A",
    DateTimeOriginal = "2023-01-01 12:00:00"
  )
  
  test_that("it stops for invalid inputs", {
    
    
    # Wrong class for recordTable
    expect_error(
      filterRecordTable(recordTable = as.matrix(dummy_rec_table),
                        minDeltaTime = 60,
                        deltaTimeComparedTo = "lastRecord"),
      regexp = "recordTable must be a data frame"
    )
    
    # Negative minDeltaTime
    expect_error(
      filterRecordTable(recordTable = dummy_rec_table, 
                        minDeltaTime = -10,
                        deltaTimeComparedTo = "lastRecord", 
                        timeZone = "UTC"),
      regexp = "'minDeltaTime' must be a positive number"
    )
    
    # Invalid stationCol
    expect_error(
      filterRecordTable(recordTable = dummy_rec_table, 
                        minDeltaTime = 60, 
                        deltaTimeComparedTo = "lastRecord", 
                        stationCol = "WRONG_COL"),
      regexp = "stationCol is not a column name in recordTable"
    )
    
    # Invalid deltaTimeComparedTo
    expect_error(
      filterRecordTable(recordTable = dummy_rec_table, 
                        minDeltaTime = 60, 
                        deltaTimeComparedTo = "invalid_option",
                        timeZone = "UTC"),
      regexp = "'arg' should be one of"
    )
  })
})