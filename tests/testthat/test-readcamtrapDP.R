
context("readcamtrapDP")

# Load necessary libraries for testing
library(testthat)
library(withr)
library(jsonlite)

# --- Mock Data Creation ---
# For most tests, we'll create data programmatically to ensure the tests
# are self-contained and predictable.

# Mock deployments.csv data
mock_deployments <- data.frame(
  deploymentID = c("dep1", "dep2", "dep3"),
  locationID = c("StationA", "StationA", "StationB"),
  locationName = c("Forest Edge", "Forest Edge", "River Bank"),
  deploymentStart = c("2022-01-01T12:00:00Z", "2022-01-15T12:00:00Z", "2022-01-05T10:00:00Z"),
  deploymentEnd = c("2022-01-10T12:00:00Z", "2022-01-25T12:00:00Z", "2022-01-20T10:00:00Z"),
  deploymentTags = c("habitat:forest|research_project:X", "research_project:X", NA),
  stringsAsFactors = FALSE
)
# The gap between dep1 and dep2 is 5 days (120 hours), which should trigger the problem logic.

# Mock observations.csv data
mock_observations <- data.frame(
  observationID = 1:4,
  deploymentID = c("dep1", "dep1", "dep2", "dep3"),
  observationType = c("animal", "human", "animal", "unidentified"),
  scientificName = c("Panthera pardus", NA, "Sus scrofa", NA),
  count = c(1, 2, 1, 1),
  eventStart = c("2022-01-02T14:00:00Z", "2022-01-03T10:00:00Z", "2022-01-16T22:00:00Z", "2022-01-08T05:00:00Z"),
  bboxX = c(10, NA, 30, NA),
  stringsAsFactors = FALSE
)

# Mock media.csv data
mock_media <- data.frame(
  mediaID = 1:4,
  deploymentID = c("dep1", "dep1", "dep2", "dep3"),
  filePath = c("imgs/dep1/img1.jpg", "imgs/dep1/img2.jpg", "imgs/dep2/img3.jpg", "imgs/dep3/img4.jpg"),
  stringsAsFactors = FALSE
)
# Note: In a real CamtrapDP, mediaIDs should link to observationIDs. We simplify here for the test.

# Mock datapackage.json content
mock_datapackage <- toJSON(list(
  name = "mock-project",
  taxonomic = list(
    scientificName = c("Panthera pardus", "Sus scrofa"),
    taxonRank = c("species", "species"),
    vernacularNames = list(
      eng = c("Leopard", "Wild Boar"),
      fra = c("Léopard", "Sanglier")
    )
  )
), auto_unbox = TRUE, pretty = TRUE)


# --- Test Suite ---

testthat::describe("Core Functionality: Reading Data", {
  
  test_that("it reads correctly from individual CSV files in a temp directory", {
    with_tempdir({
      # Create mock files in the temporary directory
      write.csv(mock_deployments, "deps.csv", row.names = FALSE)
      write.csv(mock_observations, "obs.csv", row.names = FALSE)
      write.csv(mock_media, "med.csv", row.names = FALSE)
      writeLines(mock_datapackage, "dp.json")
      
      # Call the function with explicit file paths
      result <- readcamtrapDP(
        deployments_file = "deps.csv",
        observations_file = "obs.csv",
        media_file = "med.csv", # needed for add_file_path later
        datapackage_file = "dp.json"
      )
      
      # 1. Check output structure
      expect_equal(class(result), "list")
      expect_named(result, c("CTtable", "recordTable", "metadata"))
      
      # 2. Check CTtable
      ct_table <- result$CTtable
      expect_s3_class(ct_table, "data.frame")
      expect_equal(nrow(ct_table), 2) # StationA and StationB
      
      # 3. Check recordTable
      rec_table <- result$recordTable
      expect_s3_class(rec_table, "data.frame")
      expect_equal(nrow(rec_table), 4) # 4 observations
      
      # 4. Check metadata
      expect_true("taxonomic" %in% names(result$metadata))
    })
  })
  
  test_that("it reads correctly when only a directory is set (default filenames)", {
    with_tempdir({
      # Create files with default names
      write.csv(mock_deployments, "deployments.csv", row.names = FALSE)
      write.csv(mock_observations, "observations.csv", row.names = FALSE)
      writeLines(mock_datapackage, "datapackage.json")
      
      # Temporarily change working directory to the temp dir
      with_dir(".", {
        # Call with no arguments
        result <- readcamtrapDP()
      })
      
      # Check that the output is valid
      expect_equal(nrow(result$CTtable), 2)
      expect_equal(nrow(result$recordTable), 4)
    })
  })
  
  test_that("it reads correctly from the package's fixture data (real files)", {
    # This test uses the unzipped data you placed in tests/testthat/fixtures/
    fixture_path <- test_path("fixtures", "sample_camtrap_dp_data")
    
    # Skip if the directory doesn't exist, so the test is robust
    skip_if_not(dir.exists(fixture_path), "Sample fixture data not found.")
    
    # Use with_dir to temporarily set the working directory
    with_dir(fixture_path, {
      result <- readcamtrapDP()
    })
    
    # Perform some basic checks on the real data
    expect_equal(class(result), "list")
    expect_gt(nrow(result$CTtable), 0)
    expect_gt(nrow(result$recordTable), 0)
  })
})


testthat::describe("Data Processing Logic", {
  
  test_that("it correctly detects and records deployment gaps as problems", {
    with_tempdir({
      write.csv(mock_deployments, "deployments.csv", row.names = FALSE)
      write.csv(mock_observations, "observations.csv", row.names = FALSE)
      writeLines(mock_datapackage, "datapackage.json")
      
      result <- readcamtrapDP(deployments_file = "deployments.csv", 
                              observations_file = "observations.csv",
                              datapackage_file = "datapackage.json",
                              min_gap_hours = 24) # Default
      
      ct_table <- result$CTtable
      station_a_data <- ct_table[ct_table$Station == "StationA", ]
      
      # StationA had a 5-day gap, so Problem columns should exist
      expect_true("Problem1_from" %in% names(station_a_data))
      expect_true("Problem1_to" %in% names(station_a_data))
      expect_equal(station_a_data$Problem1_from, "2022-01-10 12:00:00")
      expect_equal(station_a_data$Problem1_to, "2022-01-15 12:00:00")
    })
  })
  
  test_that("it correctly parses tags and adds taxonomic info", {
      
    fixture_path <- test_path("fixtures", "sample_camtrap_dp_data")
    
      result <- readcamtrapDP(deployments_file = file.path(fixture_path, "deployments.csv"), 
                              observations_file = file.path(fixture_path, "observations.csv"),
                              # media_file = file.path(fixture_path, "media.csv"),
                              datapackage_file = file.path(fixture_path, "datapackage.json"),)
      

      # 1. Check taxonomic info
      rec_table <- result$recordTable
      expect_true("vernacularName_eng" %in% names(rec_table))
      species_test_row <- rec_table[rec_table$scientificName == "Anas strepera", ]
      expect_equal(species_test_row$vernacularName_eng[1], "gadwall")
      expect_equal(species_test_row$vernacularName_nld[1], "krakeend")
    # })
  })
})


testthat::testthat::describe("Argument Handling", {
  
  test_that("filter_observations correctly subsets the recordTable", {
    with_tempdir({
      write.csv(mock_deployments, "deployments.csv", row.names = FALSE)
      write.csv(mock_observations, "observations.csv", row.names = FALSE)
      writeLines(mock_datapackage, "datapackage.json")
      
      # Filter to keep only "animal"
      result_animal <- readcamtrapDP(deployments_file = "deployments.csv", 
                                     observations_file = "observations.csv",
                                     datapackage_file = "datapackage.json",
                                     filter_observations = TRUE)
      expect_equal(nrow(result_animal$recordTable), 2)
      expect_true(all(result_animal$recordTable$observationType == "animal"))
      
      # Filter to keep specific types
      result_specific <- readcamtrapDP(deployments_file = "deployments.csv", 
                                       observations_file = "observations.csv",
                                       datapackage_file = "datapackage.json",
                                       filter_observations = c("human", "unidentified"))
      expect_equal(nrow(result_specific$recordTable), 2)
    })
  })
  
  test_that("add_file_path and remove_bbox work correctly", {

    fixture_path <- test_path("fixtures", "sample_camtrap_dp_data")
    
      # Test with file paths added and bbox removed (defaults)
      result1 <- readcamtrapDP(deployments_file = file.path(fixture_path, "deployments.csv"), 
                               observations_file = file.path(fixture_path, "observations.csv"),
                               media_file = file.path(fixture_path, "media.csv"),
                               datapackage_file = file.path(fixture_path, "datapackage.json"),
                               add_file_path = TRUE,
                               remove_bbox = TRUE)
      expect_true("filePath" %in% names(result1$recordTable))
      expect_false("bboxX" %in% names(result1$recordTable))
      
      # Test with file paths removed and bbox kept
      result2 <- readcamtrapDP(deployments_file = file.path(fixture_path, "deployments.csv"), 
                               observations_file = file.path(fixture_path, "observations.csv"),
                               media_file = file.path(fixture_path, "media.csv"),
                               datapackage_file = file.path(fixture_path, "datapackage.json"),
                               add_file_path = FALSE,
                               remove_bbox = FALSE)
      expect_false("filePath" %in% names(result2$recordTable))
      expect_true("bboxX" %in% names(result2$recordTable))
  })
})