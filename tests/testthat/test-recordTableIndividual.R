context("recordTableIndividual")

# Load necessary libraries
library(testthat)
library(withr)

# --- Test Setup ---
# Define paths to sample data once to be used in multiple tests.
wd_images_ID_individual <- system.file("pictures/sample_images_indiv_tag/LeopardCat",
                                       package = "camtrapR")

# --- Test Suite ---

testthat::describe("Core Functionality: ID from Metadata", {
  
  # All tests in this block require ExifTool
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  test_that("it replicates the main example correctly", {
    rec_table_pbe <- recordTableIndividual(
      inDir                  = wd_images_ID_individual,
      minDeltaTime           = 60,
      deltaTimeComparedTo    = "lastRecord",
      hasStationFolders      = FALSE,
      IDfrom                 = "metadata",
      metadataIDTag          = "individual",
      additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
      timeZone               = "Asia/Kuala_Lumpur"
    )
    
    # 1. Check object structure and dimensions
    expect_s3_class(rec_table_pbe, "data.frame")
    # Based on the sample data and a 60-min filter, 18 independent records are expected
    expect_equal(nrow(rec_table_pbe), 18)
    
    # 2. Check for expected columns
    expected_cols <- c("Station", "Species", "Individual", "DateTimeOriginal", "Date", "Time", "EXIF.Model", "EXIF.Make")
    expect_true(all(expected_cols %in% names(rec_table_pbe)))
    
    # 3. Check content
    expect_equal(unique(rec_table_pbe$Species), "LeopardCat")
    expect_equal(sort(unique(rec_table_pbe$Individual)), c("1", "2", "3"))
    # Check that station was correctly parsed from filename
    expect_true(all(grepl("StationA|StationB|StationC", rec_table_pbe$Station)))
  })
  
  test_that("eventSummary arguments work correctly", {
    rec_table_pbe2 <- recordTableIndividual(
      inDir                = wd_images_ID_individual,
      minDeltaTime         = 60,
      deltaTimeComparedTo  = "lastRecord",
      hasStationFolders    = FALSE,
      IDfrom               = "metadata",
      metadataIDTag        = "individual",
      timeZone             = "Asia/Kuala_Lumpur",
      eventSummaryColumn   = "metadata_individual",
      eventSummaryFunction = "max"
    )
    
    # 1. Check for the new summary column with the correct name convention
    expect_true("metadata_individual_max" %in% names(rec_table_pbe2))

  })
})


testthat::describe("Core Functionality: ID from Directory", {
  
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  test_that("it correctly identifies individuals from directory structure", {
    
    # We need to create a temporary directory structure and copy the sample files into it
    with_tempdir({
      # 1. Create the required directory structure
      species_dir <- "MyLeopardCat"
      station_dir <- file.path(species_dir, "StationX")
      indiv1_dir <- file.path(station_dir, "Cat1")
      indiv2_dir <- file.path(station_dir, "Cat2")
      dir.create(indiv1_dir, recursive = TRUE)
      dir.create(indiv2_dir, recursive = TRUE)
      
      # 2. Copy some sample images into these new directories
      # Get a list of the original sample images
      original_files <- list.files(wd_images_ID_individual, full.names = TRUE, pattern = "\\.JPG$")
      # Copy 2 images for Leo1 and 1 for Leo2
      file.copy(original_files[1:2], indiv1_dir)
      file.copy(original_files[3], indiv2_dir)
      
      # 3. Call the function on our temporary directory structure
      rec_table_dir <- recordTableIndividual(
        inDir             = species_dir,
        hasStationFolders = TRUE,
        IDfrom            = "directory", # Key argument
        minDeltaTime      = 0, # Get all records
        timeZone          = "Asia/Kuala_Lumpur"
      )
      
      # 4. Assert the output is correct
      expect_equal(nrow(rec_table_dir), 3) # We copied 3 files
      expect_equal(unique(rec_table_dir$Station), "StationX")
      expect_equal(sort(unique(rec_table_dir$Individual)), c("Cat1", "Cat2"))
      expect_equal(sum(rec_table_dir$Individual == "Cat1"), 2)
      expect_equal(sum(rec_table_dir$Individual == "Cat2"), 1)
    })
  })
})


testthat::describe("Argument Handling and Side Effects", {
  
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  test_that("cameraID and camerasIndependent arguments work", {
    # The sample filenames do not contain camera IDs, so this test is conceptual.
    # To test properly, you'd need files named e.g. "StationA__...__C1.JPG"
    
    # We test that the function doesn't error and that the camera column is/isn't created.
    rec_table_no_cam <- recordTableIndividual(inDir = wd_images_ID_individual, 
                                              IDfrom = "metadata", 
                                              metadataIDTag = "individual", 
                                              minDeltaTime = 0,
                                              hasStationFolders = F)
    expect_false("Camera" %in% names(rec_table_no_cam))
    
    # When cameraID is specified, expect a 'Camera' column, even if it's all NA
    # because the filenames don't match the required pattern.
    rec_table_with_cam <- recordTableIndividual(inDir = wd_images_ID_individual, 
                                                IDfrom = "metadata", 
                                                metadataIDTag = "individual", 
                                                minDeltaTime = 0,
                                                cameraID = "filename", 
                                                camerasIndependent = FALSE,
                                                hasStationFolders = F)
    expect_true("Camera" %in% names(rec_table_with_cam))
    expect_true(is.character(rec_table_with_cam$Camera))
  })
  
  test_that("writecsv = TRUE creates a CSV file", {
    with_tempdir({
      rec_table_to_write <- recordTableIndividual(
        inDir = wd_images_ID_individual,
        IDfrom = "metadata",
        metadataIDTag = "individual",
        minDeltaTime = 0,
        writecsv = TRUE,
        outDir = ".",  # Write to the temp directory
        hasStationFolders = F
      )
      
      # Check if a CSV file was created
      csv_files <- list.files(pattern = "\\.csv$")
      expect_equal(length(csv_files), 1)
      expect_true(grepl("record_table_individuals", csv_files[1]))
      
      # Check that the written file can be read and has the right number of rows
      written_data <- read.csv(csv_files[1])
      expect_equal(nrow(written_data), nrow(rec_table_to_write))
    })
  })
})


testthat::describe("Input Validation", {
  
  # These tests do not require ExifTool as they check arguments before it is called.
  test_that("it stops for invalid or missing arguments", {
    expect_error(recordTableIndividual(inDir = "non_existent_dir"), "Could not find inDir")
    
    expect_error(recordTableIndividual(inDir = wd_images_ID_individual, IDfrom = "metadata"),
                 regexp = '"metadataIDTag" must be defined if IDfrom = "metadata"')
    
    expect_error(recordTableIndividual(inDir = wd_images_ID_individual, IDfrom = "directory",
                                       minDeltaTime = 60),
                 regexp = "argument \"deltaTimeComparedTo\" is missing, with no default")

  })
  
  test_that("it warns when deltaTimeComparedTo is given with minDeltaTime = 0", {
    expect_warning(
      recordTableIndividual(inDir = wd_images_ID_individual, IDfrom = "metadata", metadataIDTag = "individual",
                            minDeltaTime = 0, deltaTimeComparedTo = "lastRecord", hasStationFolders = F),
      regexp = "minDeltaTime is 0. deltaTimeComparedTo = 'lastRecord' will have no effect"
    )
  })
})