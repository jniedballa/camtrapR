context("readCamtrapDP")

# Load necessary libraries for testing
library(testthat)
library(withr)

# --- Setup Fixture Paths ---
fixture_path <- normalizePath(
  test_path("fixtures", "sample_camtrap_dp_data"),
  mustWork = TRUE
)

dp_file <- file.path(fixture_path, "datapackage.json")

# ---------------------------------------------------------
# Test Suite 1: Core Functionality & Legacy Arguments
# ---------------------------------------------------------

testthat::describe("Core Functionality: Reading Data", {
  
  # Skip all tests if camtrapdp or fixture is missing
  skip_if_not_installed("camtrapdp")
  skip_if_not(dir.exists(fixture_path), "Sample fixture data not found.")
  
  test_that("it reads a valid datapackage correctly", {
    result <- readCamtrapDP(file = dp_file)
    
    # 1. Check output structure
    expect_type(result, "list")
    expect_named(result, c("CTtable", "recordTable", "metadata"))
    
    # 2. Check CTtable
    expect_s3_class(result$CTtable, "data.frame")
    expect_gt(nrow(result$CTtable), 0)
    expect_true("Station" %in% colnames(result$CTtable))
    
    # 3. Check recordTable
    expect_s3_class(result$recordTable, "data.frame")
    expect_gt(nrow(result$recordTable), 0)
    
    # 4. Check metadata
    expect_type(result$metadata, "list")
  })
  
  test_that("it handles legacy CSV arguments by issuing a warning but parsing successfully", {
    # Simulate someone providing the old individual file paths
    expect_warning(
      result <- readCamtrapDP(
        file = dp_file,
        deployments_file = file.path(fixture_path, "deployments.csv"),
        media_file = file.path(fixture_path, "media.csv")
      ),
      "ignored"
    )
    
    expect_type(result, "list")
    expect_s3_class(result$CTtable, "data.frame")
    expect_s3_class(result$recordTable, "data.frame")
  })
})
# 
# ---------------------------------------------------------
# Test Suite 2: Conditional Logic & Fallbacks
# ---------------------------------------------------------

testthat::describe("Conditional Logic & Optional Column Fallbacks", {
  
  skip_if_not_installed("camtrapdp")
  skip_if_not(dir.exists(fixture_path), "Sample fixture data not found.")
  
  test_that("it falls back to locationName when locationID is missing", {
    with_tempdir({
      # Copy fixture to temp directory
      file.copy(list.files(fixture_path, full.names = TRUE), ".", recursive = TRUE)
      
      # Corrupt locationID (It is optional in the DP schema, so camtrapdp handles this fine)
      deps <- read.csv("deployments.csv", stringsAsFactors = FALSE)
      deps$locationID <- NA
      write.csv(deps, "deployments.csv", row.names = FALSE, na = "")
      
      res <- readCamtrapDP("datapackage.json")
      
      # Station should now equal locationName
      expect_setequal(res$CTtable$Station, unique(deps$locationName))
    })
  })
  
  test_that("it falls back to deploymentID when both locationID and locationName are missing", {
    with_tempdir({
      file.copy(list.files(fixture_path, full.names = TRUE), ".", recursive = TRUE)
      
      deps <- read.csv("deployments.csv", stringsAsFactors = FALSE)
      deps$locationID <- NA
      deps$locationName <- NA
      write.csv(deps, "deployments.csv", row.names = FALSE, na = "")
      
      res <- readCamtrapDP("datapackage.json")
      
      # Station should now equal deploymentID
      expect_setequal(res$CTtable$Station, unique(deps$deploymentID))
    })
  })
  
  test_that("it assigns 'unknown_camera' when cameraID is missing", {
    with_tempdir({
      file.copy(list.files(fixture_path, full.names = TRUE), ".", recursive = TRUE)
      
      deps <- read.csv("deployments.csv", stringsAsFactors = FALSE)
      deps$cameraID <- NA
      write.csv(deps, "deployments.csv", row.names = FALSE, na = "")
      
      res <- readCamtrapDP("datapackage.json")
      
      expect_true(all(res$CTtable$cameraID == "unknown_camera"))
      expect_true(all(res$recordTable$cameraID == "unknown_camera"))
    })
  })
  
  test_that("it handles taxonomy fallbacks for non-animal observations (e.g. blanks)", {
    with_tempdir({
      file.copy(list.files(fixture_path, full.names = TRUE), ".", recursive = TRUE)
      
      # Read observations and artificially insert a new "blank" row
      obs <- read.csv("observations.csv", stringsAsFactors = FALSE)
      fake_obs <- obs[1, ]
      fake_obs$observationID <- "fake_blank_1"
      fake_obs$observationType <- "blank"
      fake_obs$scientificName <- NA 
      
      obs <- rbind(obs, fake_obs)
      write.csv(obs, "observations.csv", row.names = FALSE, na = "")
      
      res <- readCamtrapDP("datapackage.json")
      
      # Isolate the fake row we just created
      test_row <- res$recordTable[res$recordTable$observationID == "fake_blank_1", ]
      
      # Confirm the row was captured and check its vernacular columns
      vern_cols <- grep("^vernacularName", colnames(test_row), value = TRUE)
      expect_gt(length(vern_cols), 0)
      
      # Because scientificName was NA, the taxonomy join yielded NA. 
      # Our fallback logic should have populated these NAs with the string "blank".
      for (vc in vern_cols) {
        expect_equal(test_row[[vc]][1], "blank")
      }
    })
  })
})

# ---------------------------------------------------------
# Test Suite 3: Gap Analysis & Data Processing
# ---------------------------------------------------------

testthat::describe("Gap Analysis and Data Processing", {
  
  skip_if_not_installed("camtrapdp")
  skip_if_not(dir.exists(fixture_path), "Sample fixture data not found.")
  
  test_that("it correctly detects and records deployment gaps as problems", {
    with_tempdir({
      file.copy(list.files(fixture_path, full.names = TRUE), ".", recursive = TRUE)
      
      deps <- read.csv("deployments.csv", stringsAsFactors = FALSE)
      
      # Force a gap of 5 days (> 24 hours) for the same station
      deps$locationID[1:2] <- "GapStation"
      deps$deploymentStart[1] <- "2020-01-01T12:00:00Z"
      deps$deploymentEnd[1]   <- "2020-01-10T12:00:00Z"
      
      deps$deploymentStart[2] <- "2020-01-15T12:00:00Z"
      deps$deploymentEnd[2]   <- "2020-01-20T12:00:00Z"
      write.csv(deps, "deployments.csv", row.names = FALSE, na = "")
      
      res <- readCamtrapDP("datapackage.json", min_gap_hours = 24)
      
      gap_station <- res$CTtable[res$CTtable$Station == "GapStation", ]
      
      expect_true("Problem1_from" %in% colnames(gap_station))
      expect_true("Problem1_to" %in% colnames(gap_station))
      expect_equal(gap_station$Problem1_from, "2020-01-10 12:00:00")
      expect_equal(gap_station$Problem1_to, "2020-01-15 12:00:00")
    })
  })
})

# ---------------------------------------------------------
# Test Suite 4: Argument Flags
# ---------------------------------------------------------

testthat::describe("Argument Flags Handling", {
  
  skip_if_not_installed("camtrapdp")
  skip_if_not(dir.exists(fixture_path), "Sample fixture data not found.")
  
  test_that("filter_observations subsets recordTable correctly", {
    # Test Boolean TRUE (Keep only animals)
    res_animal <- readCamtrapDP(file = dp_file, filter_observations = TRUE)
    expect_true(all(res_animal$recordTable$observationType == "animal"))
    
    # Test specific string vector targeting
    with_tempdir({
      file.copy(list.files(fixture_path, full.names = TRUE), ".", recursive = TRUE)
      obs <- read.csv("observations.csv", stringsAsFactors = FALSE)
      
      # Ensure we have instances of human and blank
      obs$observationType[1:2] <- c("human", "blank")
      write.csv(obs, "observations.csv", row.names = FALSE, na = "")
      
      res_custom <- readCamtrapDP("datapackage.json", filter_observations = c("human", "blank"))
      expect_true(all(res_custom$recordTable$observationType %in% c("human", "blank")))
    })
  })
  
  test_that("add_file_path joins file paths from media table", {
    # Default is FALSE
    res_default <- readCamtrapDP(file = dp_file, add_file_path = FALSE)
    expect_false("filePath" %in% colnames(res_default$recordTable))
    
    # Turn ON
    res_added <- readCamtrapDP(file = dp_file, add_file_path = TRUE)
    # Ensure it joined securely
    if ("mediaID" %in% colnames(res_added$recordTable)) {
      expect_true("filePath" %in% colnames(res_added$recordTable))
    }
  })
  
  test_that("remove_bbox strips bounding box coordinates", {
    # Default is TRUE
    res_default <- readCamtrapDP(file = dp_file, remove_bbox = TRUE)
    expect_false(any(grepl("bbox", colnames(res_default$recordTable))))
    
    # Turn OFF
    res_kept <- readCamtrapDP(file = dp_file, remove_bbox = FALSE)
    raw_obs <- read.csv(file.path(fixture_path, "observations.csv"), stringsAsFactors = FALSE)
    if ("bboxX" %in% colnames(raw_obs)) {
      expect_true("bboxX" %in% colnames(res_kept$recordTable))
    }
  })
  
})
