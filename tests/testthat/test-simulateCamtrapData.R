library(testthat)

test_that("Default parameters return correctly structured list of two data frames", {
  res <- simulateCamtrapData()
  
  # Check overall structure
  expect_type(res, "list")
  expect_named(res, c("camtraps", "recordTable"))
  expect_s3_class(res$camtraps, "data.frame")
  expect_s3_class(res$recordTable, "data.frame")
  
  # Check default dimensions
  expect_equal(nrow(res$camtraps), 10)      # 10 stations * 1 camera * 1 season
  expect_equal(nrow(res$recordTable), 300)  # 300 records
  
  # Default should NOT have Camera or Season columns
  expect_false("Camera" %in% names(res$camtraps))
  expect_false("Season" %in% names(res$camtraps))
})

test_that("Multiple cameras and seasons correctly expand camtraps and add columns", {
  res <- simulateCamtrapData(nStations = 5, camerasPerStation = 2, nSeasons = 3, nRecords = 50)
  
  # 5 stations * 2 cameras * 3 seasons = 30 deployments
  expect_equal(nrow(res$camtraps), 30)
  
  # Check that columns were added to BOTH tables
  expect_true(all(c("Camera", "Season") %in% names(res$camtraps)))
  expect_true(all(c("Camera", "Season") %in% names(res$recordTable)))
  
  # Check specific values
  expect_setequal(res$camtraps$Camera, c("CamA", "CamB"))
  expect_setequal(res$camtraps$Season, c(1, 2, 3))
})

test_that("Date formatting translator works for different lubridate strings", {
  # Test 1: dmy HMS
  res_dmy <- simulateCamtrapData(nStations = 2, dateFormat = "dmy HMS", nRecords = 5)
  # Regex check: DD-MM-YYYY HH:MM:SS
  expect_match(res_dmy$camtraps$Setup_date[1], "^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}:\\d{2}$")
  
  # Test 2: mdy
  res_mdy <- simulateCamtrapData(nStations = 2, dateFormat = "mdy", probProblem = 1, nRecords = 5)
  # Regex check: MM-DD-YYYY
  expect_match(res_mdy$camtraps$Setup_date[1], "^\\d{2}-\\d{2}-\\d{4}$")
  # Ensure the problem dates also respected the stripped format
  expect_match(res_mdy$camtraps$Problem1_from[1], "^\\d{2}-\\d{2}-\\d{4}$")
  
  # Record Table should ALWAYS remain standard ISO8601 regardless of input format
  expect_match(res_mdy$recordTable$DateTimeOriginal[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
  expect_match(res_mdy$recordTable$Date[1], "^\\d{4}-\\d{2}-\\d{2}$")
  expect_match(res_mdy$recordTable$Time[1], "^\\d{2}:\\d{2}:\\d{2}$")
})

test_that("Bounding box strictly constraints spatial coordinates", {
  custom_bbox <- c(xmin = 10.0, xmax = 11.0, ymin = 20.0, ymax = 21.0)
  res <- simulateCamtrapData(nStations = 50, bbox = custom_bbox, nRecords = 10)
  
  # Check longitudes
  expect_true(all(res$camtraps$longitude >= 10.0 & res$camtraps$longitude <= 11.0))
  # Check latitudes
  expect_true(all(res$camtraps$latitude >= 20.0 & res$camtraps$latitude <= 21.0))
})

test_that("Covariates are accurately simulated and assigned", {
  res <- simulateCamtrapData(
    nStations = 5,
    nRecords = 10,
    covariates = list(
      continuous = list(temp = c(15, 2)), 
      categorical = list(forest_type = 2)
    )
  )
  
  # Continuous covariate
  expect_true("temp" %in% names(res$camtraps))
  expect_type(res$camtraps$temp, "double")
  
  # Categorical covariate
  expect_true("forest_type" %in% names(res$camtraps))
  expect_type(res$camtraps$forest_type, "character")
  expect_true(all(res$camtraps$forest_type %in% c("forest_type_1", "forest_type_2")))
  
  # Verify default covariates (elev, habitat) are NOT there since we overwrote the list
  expect_false("elev" %in% names(res$camtraps))
})

test_that("Problem periods respect probProblem parameter", {
  # 0% problem probability
  res_no_prob <- simulateCamtrapData(nStations = 10, probProblem = 0, nRecords = 5)
  expect_true(all(res_no_prob$camtraps$Problem1_from == ""))
  expect_true(all(res_no_prob$camtraps$Problem1_to == ""))
  
  # 100% problem probability
  res_all_prob <- simulateCamtrapData(nStations = 10, probProblem = 1, nRecords = 5)
  expect_false(any(res_all_prob$camtraps$Problem1_from == ""))
  expect_false(any(res_all_prob$camtraps$Problem1_to == ""))
})

test_that("Species limits are respected", {
  res <- simulateCamtrapData(nStations = 5, nSpecies = 3, nRecords = 100)
  
  unique_species <- unique(res$recordTable$Species)
  # Due to randomness, we might not get exactly 3, but we should NEVER get a 4th
  expect_true(all(unique_species %in% c("Sp_01", "Sp_02", "Sp_03")))
})
