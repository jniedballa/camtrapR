context("Image Management Workflow")

# Load necessary libraries for testing
library(testthat)
library(withr)

# --- Test Suite ---

# =========================================================================
# Tests for createStationFolders()
# =========================================================================
testthat::describe("createStationFolders()", {
  
  test_that("it creates station directories correctly", {
    with_tempdir({ # All file operations happen in a temporary, self-cleaning directory
      
      stations <- c("StationA", "StationB", "StationC")
      
      # Call the function
      result <- createStationFolders(inDir = ".", stations = stations)
      
      # 1. Check the returned data frame
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 3)
      expect_named(result, c("station", "directory", "created", "exists"))
      expect_true(all(result$created))
      expect_true(all(result$exists))
      
      # 2. Check the actual file system
      created_dirs <- list.dirs(recursive = FALSE)
      expect_equal(sort(basename(created_dirs)), sort(stations))
    })
  })
  
  test_that("it creates nested station/camera directories correctly", {
    with_tempdir({
      stations <- c("StationA", "StationA", "StationB")
      cameras <- c("Cam1", "Cam2", "Cam1")
      
      result <- createStationFolders(inDir = ".", stations = stations, cameras = cameras)
      
      # 1. Check the returned data frame
      expect_true("camera" %in% names(result))
      expect_true(all(result$created))
      
      # 2. Check the actual file system
      expect_true(dir.exists("StationA/Cam1"))
      expect_true(dir.exists("StationA/Cam2"))
      expect_true(dir.exists("StationB/Cam1"))
      expect_false(dir.exists("StationB/Cam2")) # Ensure it didn't create extra dirs
    })
  })
  
  test_that("it handles existing directories and createinDir correctly", {
    with_tempdir({
      # 1. Test createinDir = TRUE
      new_root <- "new_root_dir"
      expect_false(dir.exists(new_root))
      createStationFolders(inDir = new_root, stations = "StationA", createinDir = TRUE)
      expect_true(dir.exists(new_root))
      
      # 2. Test idempotency (running twice)
      dir.create("existing_station")
      result_rerun <- createStationFolders(inDir = ".", stations = "existing_station")
      expect_false(result_rerun$created) # Should report it was not created this time
      expect_true(result_rerun$exists)   # But that it exists
    })
  })
})


# =========================================================================
# Tests for imageRename()
# =========================================================================
testthat::describe("imageRename()", {
  
  # All tests in this block require ExifTool
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  # Path to the raw sample images included with the package
  wd_images_raw <- system.file("pictures/raw_images", package = "camtrapR")
  
  test_that("it renames correctly with hasCameraFolders = FALSE", {
    with_tempdir({
      # Set up input and output directories
      in_dir <- "raw_data"
      out_dir <- "renamed_data"
      dir.create(in_dir)
      
      # Copy the sample data into our temporary input directory
      file.copy(list.files(wd_images_raw, full.names = TRUE), in_dir, recursive = TRUE)
      
      # Run the function
      renaming_table <- imageRename(
        inDir            = in_dir,
        outDir           = out_dir,
        hasCameraFolders = FALSE,
        copyImages       = TRUE
      )
      
      # 1. Check the returned table
      expect_s3_class(renaming_table, "data.frame")
      expect_true(all(renaming_table$CopyStatus))
      
      # 2. Check the output files
      renamed_files <- list.files(out_dir, recursive = TRUE)
      expect_gt(length(renamed_files), 0)
      
      # 3. Check the filename pattern (Station__Date__Time(n).JPG)
      # Example: "StationA__2009-04-21__15-49-26(1).JPG"
      expect_true(all(grepl("^Station[A-C]__\\d{4}-\\d{2}-\\d{2}__\\d{2}-\\d{2}-\\d{2}\\(\\d+\\)\\.JPG$", basename(renamed_files))))
    })
  })
  
  test_that("copyImages = FALSE produces a table but no files", {
    with_tempdir({
      in_dir <- "raw_data"
      out_dir <- "renamed_data" # This directory will not be created
      dir.create(in_dir)
      file.copy(list.files(wd_images_raw, full.names = TRUE), in_dir, recursive = TRUE)
      
      renaming_table <- imageRename(
        inDir            = in_dir,
        outDir           = out_dir,
        hasCameraFolders = FALSE,
        copyImages       = FALSE # Key argument
      )
      
      # 1. Check returned table
      expect_true(all(!renaming_table$CopyStatus))
      expect_s3_class(renaming_table, "data.frame")
      
      # 2. Assert that NO output directory or files were created
      expect_false(dir.exists(out_dir))
    })
  })
})


# =========================================================================
# Tests for appendSpeciesNames()
# =========================================================================
testthat::describe("appendSpeciesNames()", {
  
  test_that("it can append and then remove species names (IDfrom = directory)", {
    with_tempdir({
      # 1. Setup a directory structure that mimics the state *after* species ID
      species_id_dir <- "species_identified"
      dir.create(file.path(species_id_dir, "StationA", "PBE"), recursive = TRUE)
      dir.create(file.path(species_id_dir, "StationB", "VTA"), recursive = TRUE)
      
      # Create some dummy files with the post-imageRename format
      file.create(file.path(species_id_dir, "StationA", "PBE", "StationA__2023-01-01__12-00-00(1).JPG"))
      file.create(file.path(species_id_dir, "StationB", "VTA", "StationB__2023-01-02__14-00-00(1).JPG"))
      
      # 2. Append the names
      append_table <- appendSpeciesNames(
        inDir            = species_id_dir,
        IDfrom           = "directory",
        hasCameraFolders = FALSE,
        removeNames      = FALSE
      )
      
      # 3. Verify the appended files exist
      files_after_append <- list.files(species_id_dir, recursive = TRUE)
      expect_true(any(grepl("StationA__2023-01-01__12-00-00\\(1\\)__PBE\\.JPG", files_after_append)))
      expect_true(any(grepl("StationB__2023-01-02__14-00-00\\(1\\)__VTA\\.JPG", files_after_append)))
      
      # 4. Remove the names
      remove_table <- appendSpeciesNames(
        inDir            = species_id_dir,
        IDfrom           = "directory", # Needs to know the species to remove the name
        hasCameraFolders = FALSE,
        removeNames      = TRUE
      )
      
      # 5. Verify the files are back to their original names
      files_after_remove <- list.files(species_id_dir, recursive = TRUE)
      expect_false(any(grepl("__PBE\\.JPG", files_after_remove)))
      expect_true(any(grepl("StationA__2023-01-01__12-00-00\\(1\\)\\.JPG", files_after_remove)))
    })
  })
})


# =========================================================================
# Integrated Workflow Test
# =========================================================================
testthat::describe("Full Image Management Workflow", {
  
  skip_if(Sys.which("exiftool") == "", message = "ExifTool not found, skipping tests.")
  
  test_that("the full workflow from creation to renaming works as expected", {
    with_tempdir({
      # --- STAGE 1: createStationFolders ---
      raw_image_root <- "1_raw_images"
      stations <- c("StationA", "StationB")
      createStationFolders(inDir = raw_image_root, stations = stations, createinDir = TRUE)
      
      # Manually copy sample images into the newly created structure
      raw_images_path <- system.file("pictures/raw_images", package = "camtrapR")
      # Put the first 2 sample station dirs into our new structure
      file.copy(file.path(raw_images_path, "StationA"), raw_image_root, recursive = TRUE)
      file.copy(file.path(raw_images_path, "StationB"), raw_image_root, recursive = TRUE)
      
      # --- STAGE 2: imageRename ---
      renamed_image_root <- "2_renamed_images"
      imageRename(
        inDir            = raw_image_root,
        outDir           = renamed_image_root,
        hasCameraFolders = FALSE,
        copyImages       = TRUE
      )
      
      # Verify that renamed files exist
      renamed_files <- list.files(renamed_image_root, recursive = TRUE)
      expect_gt(length(renamed_files), 0)
      
      # --- STAGE 3: Simulate Manual Species ID ---
      # Create species subdirectories and move the renamed files into them
      species_id_root <- "3_species_id_images"
      # We know from sample data that StationA has PBE, VTA, and SUS. We'll just ID them all as PBE for simplicity.
      dir.create(file.path(species_id_root, "StationA", "PBE"), recursive = TRUE)
      # And StationB has VTA
      dir.create(file.path(species_id_root, "StationB", "VTA"), recursive = TRUE)
      
      # Move files
      files_to_move_A <- list.files(file.path(renamed_image_root, "StationA"), full.names = TRUE)
      file.copy(files_to_move_A, file.path(species_id_root, "StationA", "PBE"))
      
      files_to_move_B <- list.files(file.path(renamed_image_root, "StationB"), full.names = TRUE)
      file.copy(files_to_move_B, file.path(species_id_root, "StationB", "VTA"))
      
      # --- STAGE 4: appendSpeciesNames ---
      appendSpeciesNames(inDir = species_id_root, IDfrom = "directory", hasCameraFolders = FALSE)
      
      # Final verification
      final_files <- list.files(species_id_root, recursive = TRUE)
      expect_true(all(grepl("__PBE\\.JPG$|__VTA\\.JPG$", final_files)))
    })
  })
})
