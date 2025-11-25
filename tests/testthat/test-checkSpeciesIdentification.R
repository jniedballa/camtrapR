library(testthat)
library(mockery)

test_that("Input argument validation works", {
  # FIX: Add cycle = TRUE because the function is called 3 times in this block
  m_sys_which <- mock("/usr/bin/exiftool", cycle = TRUE)
  stub(checkSpeciesIdentification, "Sys.which", m_sys_which)
  
  # Create a dummy directory to pass dir.exists check
  temp_dir <- tempdir()
  
  # 1. Invalid IDfrom
  expect_error(checkSpeciesIdentification(inDir = temp_dir, 
                                          IDfrom = "wrong_option", 
                                          hasCameraFolders = FALSE, 
                                          maxDeltaTime = 120),
               "'IDfrom' must be 'metadata' or 'directory'")
  
  # 2. Missing metadataSpeciesTag when IDfrom = "metadata"
  expect_error(checkSpeciesIdentification(inDir = temp_dir, 
                                          IDfrom = "metadata", 
                                          hasCameraFolders = FALSE, 
                                          maxDeltaTime = 120),
               "'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")
  
  # 3. Invalid types
  expect_error(checkSpeciesIdentification(inDir = temp_dir, 
                                          IDfrom = "directory", 
                                          hasCameraFolders = FALSE, 
                                          maxDeltaTime = "Not a number"),
               "is.numeric")
})

test_that("Logic: ID from Directory - Temporal Independence Check", {
  # --- Setup ---
  temp_dir <- file.path(tempdir(), "test_station_dir")
  dir.create(temp_dir, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "StationA"), showWarnings = FALSE)
  # Create dummy species folders so list.dirs works
  dir.create(file.path(temp_dir, "StationA", "SpeciesA"), showWarnings = FALSE)
  dir.create(file.path(temp_dir, "StationA", "SpeciesB"), showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # --- Mocks ---
  m_sys_which <- mock("found")
  
  # Mock runExiftool return data
  mock_exif_data <- data.frame(
    Directory = c(file.path(temp_dir, "StationA", "SpeciesA"), 
                  file.path(temp_dir, "StationA", "SpeciesB")),
    FileName = c("img1.jpg", "img2.jpg"),
    DateTimeOriginal = c("2023:01:01 12:00:00", "2023:01:01 12:00:10"),
    HierarchicalSubject = c("", ""),
    stringsAsFactors = FALSE
  )
  m_runExiftool <- mock(mock_exif_data)
  
  # Mock assignSpeciesID
  fake_assign_species <- function(intable, ...) {
    intable$species <- basename(intable$Directory)
    return(intable)
  }
  
  # --- Stubbing ---
  stub(checkSpeciesIdentification, "Sys.which", m_sys_which)
  stub(checkSpeciesIdentification, "runExiftool", m_runExiftool)
  stub(checkSpeciesIdentification, "assignSpeciesID", fake_assign_species)
  
  # --- Execution ---
  result <- checkSpeciesIdentification(
    inDir = temp_dir,
    IDfrom = "directory",
    hasCameraFolders = FALSE,
    maxDeltaTime = 60 
  )
  
  # --- Assertions ---
  expect_type(result, "list")
  expect_named(result, c("temporalIndependenceCheck", "IDconflictCheck"))
  
  check_table <- result$temporalIndependenceCheck
  expect_equal(nrow(check_table), 2)
  expect_equal(check_table$species, c("SpeciesA", "SpeciesB"))
  expect_equal(check_table$station, c("StationA", "StationA"))
})

test_that("Logic: ID from Metadata - Double Observer Conflict Check", {
  # --- Setup ---
  temp_dir <- file.path(tempdir(), "test_station_meta")
  dir.create(temp_dir, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "StationB"), showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # --- Mocks ---
  m_sys_which <- mock("found")
  m_progressbar <- mock(NULL)
  
  mock_exif_data <- data.frame(
    Directory = file.path(temp_dir, "StationB"),
    FileName = "img_conflict.jpg",
    DateTimeOriginal = "2023:01:01 12:00:00",
    HierarchicalSubject = "Species|Fox_&_Compare|Wolf", 
    stringsAsFactors = FALSE
  )
  m_runExiftool <- mock(mock_exif_data)
  
  fake_add_meta <- function(intable, ...) {
    intable$metadata_SpeciesTag <- "Fox"
    intable$metadata_CompareTag <- "Wolf"
    return(intable)
  }
  
  fake_assign_species <- function(intable, ...) {
    intable$species <- intable$metadata_SpeciesTag
    return(intable)
  }
  
  # --- Stubbing ---
  stub(checkSpeciesIdentification, "Sys.which", m_sys_which)
  stub(checkSpeciesIdentification, "runExiftool", m_runExiftool)
  stub(checkSpeciesIdentification, "makeProgressbar", m_progressbar)
  stub(checkSpeciesIdentification, "addMetadataAsColumns", fake_add_meta)
  stub(checkSpeciesIdentification, "assignSpeciesID", fake_assign_species)
  
  # --- Execution ---
  result <- checkSpeciesIdentification(
    inDir = temp_dir,
    IDfrom = "metadata",
    hasCameraFolders = FALSE,
    metadataSpeciesTag = "SpeciesTag",
    metadataSpeciesTagToCompare = "CompareTag",
    maxDeltaTime = 60
  )
  
  # --- Assertions ---
  conflict_table <- result$IDconflictCheck
  expect_equal(nrow(conflict_table), 1)
  expect_true("metadata_SpeciesTag" %in% colnames(conflict_table))
  expect_equal(conflict_table$metadata_SpeciesTag, "Fox")
  expect_equal(conflict_table$metadata_CompareTag, "Wolf")
  expect_equal(nrow(result$temporalIndependenceCheck), 0)
})

test_that("Logic: Exclude Species functionality", {
  # --- Setup ---
  temp_dir <- file.path(tempdir(), "test_exclude")
  dir.create(temp_dir, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "StationC"), showWarnings = FALSE)
  dir.create(file.path(temp_dir, "StationC", "Human"), showWarnings = FALSE) 
  dir.create(file.path(temp_dir, "StationC", "Deer"), showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  # --- Mocks ---
  m_sys_which <- mock("found")
  
  mock_exif_data <- data.frame(
    Directory = file.path(temp_dir, "StationC", "Deer"),
    FileName = "deer.jpg",
    DateTimeOriginal = "2023:01:01 12:00:00",
    HierarchicalSubject = "",
    stringsAsFactors = FALSE
  )
  m_runExiftool <- mock(mock_exif_data)
  
  fake_assign_species <- function(intable, ...) {
    intable$species <- "Deer"
    return(intable)
  }
  
  stub(checkSpeciesIdentification, "Sys.which", m_sys_which)
  stub(checkSpeciesIdentification, "runExiftool", m_runExiftool)
  stub(checkSpeciesIdentification, "assignSpeciesID", fake_assign_species)
  
  # --- Execution ---
  result <- checkSpeciesIdentification(
    inDir = temp_dir,
    IDfrom = "directory",
    hasCameraFolders = FALSE,
    maxDeltaTime = 60,
    excludeSpecies = "Human"
  )
  
  # --- Assertions ---
  expect_equal(nrow(result$temporalIndependenceCheck), 0)
  
  # Verify runExiftool command string excluded Human
  args_passed <- mock_args(m_runExiftool)[[1]]
  command_str <- args_passed$command.tmp
  expect_false(grepl("Human", command_str))
  expect_true(grepl("Deer", command_str))
})

test_that("Logic: hasCameraFolders = TRUE logic", {
  # --- Setup ---
  temp_dir <- file.path(tempdir(), "test_cam_folders")
  dir.create(temp_dir, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "StationD"), showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  
  m_sys_which <- mock("found")
  
  mock_exif_data <- data.frame(
    Directory = c(file.path(temp_dir, "StationD", "Cam1", "Fox"),
                  file.path(temp_dir, "StationD", "Cam1", "Deer"),
                  file.path(temp_dir, "StationD", "Cam2", "Deer")),
    FileName = c("fox1.jpg", "deer1.jpg", "deer2.jpg"),
    DateTimeOriginal = c("2023:01:01 12:00:00", 
                         "2023:01:01 12:00:05", 
                         "2023:01:01 12:00:05"),
    HierarchicalSubject = "",
    stringsAsFactors = FALSE
  )
  m_runExiftool <- mock(mock_exif_data)
  
  fake_assign_species <- function(intable, ...) {
    intable$species <- basename(intable$Directory)
    return(intable)
  }
  
  stub(checkSpeciesIdentification, "Sys.which", m_sys_which)
  stub(checkSpeciesIdentification, "runExiftool", m_runExiftool)
  stub(checkSpeciesIdentification, "assignSpeciesID", fake_assign_species)
  
  # --- Execution ---
  result <- checkSpeciesIdentification(
    inDir = temp_dir,
    IDfrom = "directory",
    hasCameraFolders = TRUE,
    maxDeltaTime = 60
  )
  
  # --- Assertions ---
  check_table <- result$temporalIndependenceCheck
  expect_true("camera" %in% colnames(check_table))
  expect_equal(nrow(check_table), 2)
  expect_true(all(check_table$camera == "Cam1"))
  expect_false("Cam2" %in% check_table$camera)
})