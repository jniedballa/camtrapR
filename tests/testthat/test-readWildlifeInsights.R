library(testthat)
library(lubridate)

# --- Helper: Create Mock Data ---
create_mock_wi_data <- function(type = "csv", 
                                bad_dates = FALSE, 
                                dup_ids = FALSE, 
                                bad_range = FALSE,
                                identical_dates = FALSE,
                                orphan_data = FALSE) { # Added flag to control orphans
  
  # Create a temporary directory
  temp_dir <- file.path(tempdir(), paste0("wi_test_", as.integer(Sys.time())))
  dir.create(temp_dir, showWarnings = FALSE)
  
  # 1. Mock Deployments Data
  deps <- data.frame(
    project_id = 123,
    deployment_id = c("Dep1", "Dep2", "Dep3"),
    placename = c("SiteA", "SiteB", "SiteA"), 
    camera_id = c("Cam1", "Cam2", "Cam3"),
    start_date = c("2023-01-01 12:00:00", "2023-01-02 12:00:00", "2023-01-03 10:00:00"),
    end_date = c("2023-02-01 12:00:00", "2023-02-02 12:00:00", "2023-02-03 10:00:00"),
    latitude = c(10, 20, 30),
    longitude = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  # Inject Errors based on flags
  if (dup_ids) {
    deps$deployment_id[2] <- "Dep1"
  }
  if (bad_dates) {
    deps$start_date[1] <- "Not a date"
  }
  if (bad_range) {
    deps$end_date[1] <- "2022-01-01 12:00:00" 
  }
  if (identical_dates) {
    deps <- rbind(deps, data.frame(
      project_id = 123,
      deployment_id = "Dep_BadTime",
      placename = "SiteC",
      camera_id = "Cam4",
      start_date = "2023-03-01 12:00:00",
      end_date = "2023-03-01 12:00:00",
      latitude = 40, longitude = 40
    ))
  }
  
  # 2. Mock Images Data
  imgs <- data.frame(
    project_id = 123,
    deployment_id = c("Dep1", "Dep1", "Dep2"), # No orphan by default
    image_id = c("Img1", "Img2", "Img3"),
    timestamp = c("2023-01-15 12:00:00", "2023-01-15 12:05:00", "2023-01-15 13:00:00"),
    common_name = c("Deer", "Deer", "Boar"),
    species = c("species1", "species1", "species2"),
    bounding_boxes = c("box1", "box2", "box3"), 
    stringsAsFactors = FALSE
  )
  
  # Inject Orphan Image if requested
  if (orphan_data) {
    imgs <- rbind(imgs, data.frame(
      project_id = 123,
      deployment_id = "Dep_Unknown",
      image_id = "Img4",
      timestamp = "2023-01-15 14:00:00",
      common_name = "Bird",
      species = "species3",
      bounding_boxes = "box4"
    ))
  }
  
  if (identical_dates) {
    imgs <- rbind(imgs, data.frame(
      project_id = 123,
      deployment_id = "Dep_BadTime",
      image_id = "Img5",
      timestamp = "2023-03-01 12:00:00",
      common_name = "Ghost", species = "Ghost", bounding_boxes = NA
    ))
  }
  
  dep_path <- file.path(temp_dir, "deployments.csv")
  img_path <- file.path(temp_dir, "images.csv")
  
  write.csv(deps, dep_path, row.names = FALSE)
  write.csv(imgs, img_path, row.names = FALSE)
  
  if (type == "zip") {
    zip_path <- file.path(temp_dir, "export.zip")
    old_wd <- getwd()
    setwd(temp_dir)
    zip(zipfile = "export.zip", files = c("deployments.csv", "images.csv"))
    setwd(old_wd)
    return(list(zip = zip_path, dir = temp_dir))
  }
  
  return(list(dep = dep_path, img = img_path, dir = temp_dir))
}

# --- UNIT TESTS ---

test_that("Input parameter validation works", {
  paths <- create_mock_wi_data()
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  expect_error(readWildlifeInsights(), 
               "Please provide either 'directory', 'zipfile', or both")
  
  expect_error(readWildlifeInsights(directory = paths$dir, zipfile = "fake.zip"), 
               "Please provide either 'directory', 'zipfile', or both")
  
  expect_error(readWildlifeInsights(deployment_file = "missing.csv", image_file = paths$img), 
               "specified deployment file does not exist")
})

test_that("Reading from individual CSV files works (Happy Path)", {
  # Default orphan_data=FALSE, so no warnings expected
  paths <- create_mock_wi_data(type = "csv", orphan_data = FALSE) 
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  result <- readWildlifeInsights(deployment_file = paths$dep, image_file = paths$img)
  
  expect_type(result, "list")
  expect_named(result, c("CTtable", "CTtable_aggregated", "recordTable"))
  expect_false("bounding_boxes" %in% colnames(result$recordTable))
  expect_equal(nrow(result$CTtable_aggregated), 2) 
})

test_that("Reading from Directory works", {
  # Default orphan_data=FALSE
  paths <- create_mock_wi_data(type = "csv", orphan_data = FALSE)
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  result <- readWildlifeInsights(directory = paths$dir)
  
  expect_equal(nrow(result$CTtable), 3)
  expect_true("timestamp" %in% colnames(result$recordTable))
})

test_that("Reading from Zip file works", {
  skip_if_not(nzchar(Sys.which("zip")))
  
  # Default orphan_data=FALSE
  paths <- create_mock_wi_data(type = "zip", orphan_data = FALSE)
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  result <- readWildlifeInsights(zipfile = paths$zip)
  
  expect_equal(nrow(result$CTtable), 3)
  expect_equal(nrow(result$recordTable), 3) 
})

test_that("Data Integrity: Duplicate Deployment IDs triggers error", {
  paths <- create_mock_wi_data(dup_ids = TRUE)
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  expect_error(readWildlifeInsights(directory = paths$dir), 
               "Duplicate deployment_id found")
})

test_that("Data Integrity: Unparseable Dates triggers error", {
  paths <- create_mock_wi_data(bad_dates = TRUE)
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  # lubridate warns about parsing failures before the function errors out.
  # We suppress warnings to cleanly catch the error we defined.
  expect_error(suppressWarnings(readWildlifeInsights(directory = paths$dir)), 
               "Failed to parse.*start date")
})

test_that("Data Integrity: End date before Start date triggers error", {
  paths <- create_mock_wi_data(bad_range = TRUE)
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  expect_error(readWildlifeInsights(directory = paths$dir), 
               "end date is not after start date")
})

test_that("Logic: Deployments with identical start/end dates are removed", {
  paths <- create_mock_wi_data(identical_dates = TRUE)
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  # Expect warning about removed deployments
  expect_warning(
    result <- readWildlifeInsights(directory = paths$dir),
    "deployments with identical start and end date removed"
  )
  
  expect_false("Dep_BadTime" %in% result$CTtable$deployment_id)
  expect_false("Img5" %in% result$recordTable$image_id)
})



test_that("Logic: Orphan images (no matching deployment) warnings", {
  # Explicitly request orphan data for this test
  paths <- create_mock_wi_data(orphan_data = TRUE) 
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  # 1. Capture all warnings to verify specific messages match without nesting issues
  warn_msgs <- capture_warnings(readWildlifeInsights(directory = paths$dir))
  
  # Verify "Found X deployment_id" warning
  expect_match(warn_msgs, "Found 1 deployment_id", all = FALSE)
  
  # Verify "Merging reduced" warning
  expect_match(warn_msgs, "Merging reduced", all = FALSE)
  
  # 2. Get result for data validation (suppress warnings to prevent console clutter)
  result <- suppressWarnings(readWildlifeInsights(directory = paths$dir))
  
  # Ensure the merged table only has records where deployment existed
  # 4 images created in mock data, 1 was orphan -> 3 remain
  expect_equal(nrow(result$recordTable), 3) 
})

test_that("Logic: Required columns check", {
  paths <- create_mock_wi_data()
  on.exit(unlink(paths$dir, recursive = TRUE))
  
  dep_data <- read.csv(paths$dep)
  dep_data$camera_id <- NULL 
  write.csv(dep_data, paths$dep, row.names = FALSE)
  
  expect_error(readWildlifeInsights(directory = paths$dir),
               "Missing required columns in deployments data")
})
