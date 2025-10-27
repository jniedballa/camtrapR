context("createCovariates")

library(testthat)
library(sf)
library(terra)
library(withr)

# --- Test Setup ---

# 1. Create a mock sf object from the 'camtraps' dataset
data(camtraps)
camtraps_sf <- st_as_sf(camtraps,
                        coords = c("utm_x", "utm_y"),
                        crs = 32650) # UTM Zone 50N

# 2. Create mock SpatRaster objects for reproducible testing
# Raster 1: Same CRS as camtraps_sf, predictable values
r1 <- rast(crs = "EPSG:32650", extent = ext(c(522500 , 526300 , 603700 , 607300)), resolution = 100)
values(r1) <- 1:ncell(r1)
names(r1) <- "cov_utm"

# Raster 2: Different CRS (WGS84), tests reprojection
r2 <- rast(crs = "EPSG:4326", extent = ext(c(117.2, 117.24, 5.46, 5.5)), resolution = 0.01)
values(r2) <- (1:ncell(r2)) * 10
names(r2) <- "cov_wgs"


# --- Test Suite ---

testthat::describe("Input Validation and Error Handling", {
  
  test_that("it stops for invalid CTtable inputs", {
    expect_error(createCovariates(camtraps), "CTtable is not an sf object.")
    
    
    expect_error({
      camtraps_sf_no_crs <- st_as_sf(camtraps, coords = c("utm_x", "utm_y"))
      createCovariates(camtraps_sf_no_crs)
    }, "Coordinate reference system of CTtable is not defined.")
    
    expect_error(createCovariates(camtraps_sf[0,]), "The 'CTtable' is empty.")
  })
  
  test_that("it stops when no data source is provided", {
    expect_error(createCovariates(camtraps_sf), 
                 "Please provide one of 'directory', 'filenames', 'rasters', or set 'download_elevation = TRUE'.")
  })
  
  test_that("it stops when multiple data sources are provided", {
    expect_error(createCovariates(camtraps_sf, directory = "a", rasters = r1),
                 "Provide only one of 'directory', 'filenames', or 'rasters'.")
  })
  
  test_that("it stops for invalid buffer or resolution values", {
    expect_error(createCovariates(camtraps_sf, rasters = r1, buffer_ct = -10),
                 "Buffer size for 'buffer_ct' must be numeric and positive.")
    expect_error(createCovariates(camtraps_sf, rasters = r1, resolution = -50),
                 "Invalid spatial resolution. Please provide a positive value.")
  })
})


testthat::describe("Core Functionality from SpatRaster Objects", {
  # Core Functionality from SpatRaster Objects
  
  test_that("it extracts values correctly from a list of SpatRasters", {
    raster_list <- list(my_utm_ras = r1, my_wgs_ras = r2)
    result <- createCovariates(camtraps_sf, rasters = raster_list, resolution = 200)
    
    # Check output structure
    expect_s3_class(result$CTtable, "sf")
    expect_s4_class(result$predictionRaster, "SpatRaster")
    expect_named(result, c("CTtable", "predictionRaster", "originalRaster"))
    
    # Check that new columns were added
    expect_true(all(c("my_utm_ras", "my_wgs_ras") %in% names(result$CTtable)))
    expect_equal(ncol(result$CTtable), ncol(camtraps_sf) + 2)
    
    # Check that prediction raster has the right layers and CRS
    expect_equal(names(result$predictionRaster), c("my_utm_ras", "my_wgs_ras"))
    expect_equal(crs(result$predictionRaster, proj = TRUE), crs(camtraps_sf, proj = TRUE))
  })
  
  test_that("buffering and interpolation options work", {
    # Test with a 200m buffer
    result_buffer <- createCovariates(camtraps_sf, rasters = r1, buffer_ct = 200, resolution = 200)
    # Test without buffer (simple extraction)
    result_simple <- createCovariates(camtraps_sf, rasters = r1, buffer_ct = 0, bilinear = FALSE, resolution = 200)
    
    # Values should be different due to averaging in the buffer
    expect_false(all(result_buffer$CTtable$cov_utm == result_simple$CTtable$cov_utm))
  })
  
  test_that("append = FALSE works correctly", {
    result <- createCovariates(camtraps_sf, rasters = r1, append = FALSE, resolution = 200)
    
    # Output should be a data.frame, not an sf object
    expect_s3_class(result$CTtable, "data.frame")
    expect_false(inherits(result$CTtable, "sf"))
    expect_equal(ncol(result$CTtable), 1) # Only the one covariate column
    expect_named(result$CTtable, "cov_utm")
  })
})


testthat::describe("Core Functionality from Files", {
  
  # Use a temporary directory for file-based tests
  with_tempdir({
    
    # Save our mock rasters to the temp directory
    writeRaster(r1, "cov_utm.tif")
    writeRaster(r2, "cov_wgs.tif")
    dir.create("subdir")
    writeRaster(r1 * 2, "subdir/cov_sub.tif", names = "cov_sub")
    
    test_that("it extracts values from a directory", {
      result <- createCovariates(camtraps_sf, directory = ".", resolution = 200)
      
      expect_true(all(c("cov_utm", "cov_wgs") %in% names(result$CTtable)))
      expect_equal(nlyr(result$predictionRaster), 2)
    })
    
    test_that("it extracts values recursively from a directory", {
      result <- createCovariates(camtraps_sf, directory = ".", recursive = TRUE, resolution = 200)
      
      # Should find all 3 rasters
      expect_true(all(c("cov_utm", "cov_wgs", "cov_sub") %in% names(result$CTtable)))
      expect_equal(nlyr(result$predictionRaster), 3)
    })
    
    test_that("it extracts values from a named character vector of filenames", {
      file_paths <- c(ras1 = "cov_utm.tif", ras2 = "cov_wgs.tif")
      result <- createCovariates(camtraps_sf, filenames = file_paths, resolution = 200)
      
      # Check that the provided names ("ras1", "ras2") are used
      expect_true(all(c("ras1", "ras2") %in% names(result$CTtable)))
      expect_equal(names(result$predictionRaster), c("ras1", "ras2"))
    })
  })
})


testthat::describe("Elevation Download and Terrain Measures", {
  
  # skip_if_not_installed("mockery")
  skip_if_not_installed("elevatr")
  
  test_that("it downloads elevation and calculates terrain measures", {
    # Create a mock elevation raster to be returned by the mocked function
    mock_elev_rast <- rast(crs = "EPSG:4326", extent = ext(c(117.2, 117.24, 5.46, 5.5)), resolution = 0.01)
    values(mock_elev_rast) <- 1000 + (1:ncell(mock_elev_rast)) / 10
    names(mock_elev_rast) <- "mock_elevation"
    
    # Mock the elevatr::get_elev_raster function to avoid actual download.
    # It will now return our predictable mock raster instead.
    m <- mockery::mock(mock_elev_rast)
    mockery::stub(where = createCovariates, what = "elevatr::get_elev_raster", how = m)
    
    # Run the function
    result <- expect_warning(
      createCovariates(camtraps_sf,
                       download_elevation = TRUE,
                       terrain_measures = c("slope", "TRI"),
                       resolution = 200),
      regexp = "NAs found in extracted covariates"
    )
    
    # Check that the mock function was called exactly once
    mockery::expect_called(m, 1)
    
    # Check for correct columns and layers
    expected_names <- c("elevation", "slope", "TRI")
    expect_true(all(expected_names %in% names(result$CTtable)))
    expect_true(all(expected_names %in% names(result$predictionRaster)))
  })
})


testthat::describe("Scaling and Output Structure", {
  
  test_that("scale_covariates = TRUE produces correct output structure and values", {
    # Use a list of rasters for this test
    raster_list <- list(elevation = r1, other_metric = r2)
    
    result <- createCovariates(camtraps_sf, 
                               rasters = raster_list, 
                               resolution = 200,
                               scale_covariates = TRUE)
    
    # 1. Check the output list structure
    expected_names <- c("CTtable", "CTtable_scaled", "predictionRaster", 
                        "predictionRaster_scaled", "originalRaster", "scaling_params")
    expect_named(result, expected_names)
    
    # 2. Check the scaled table
    ct_scaled <- result$CTtable_scaled
    expect_s3_class(ct_scaled, "sf")
    
    # Scaled numeric columns should have mean ~ 0 and sd ~ 1
    expect_equal(mean(ct_scaled$elevation), 0, tolerance = 1e-9)
    expect_equal(sd(ct_scaled$elevation), 1, tolerance = 1e-9)
    expect_equal(mean(ct_scaled$other_metric), 0, tolerance = 1e-9)
    expect_equal(sd(ct_scaled$other_metric), 1, tolerance = 1e-9)
    
    # 3. Check the scaled raster
    ras_scaled <- result$predictionRaster_scaled
    expect_s4_class(ras_scaled, "SpatRaster")
    
    # The global mean of the scaled raster layer should be close to 0
    global_mean <- global(ras_scaled, "mean", na.rm = TRUE)$mean
    names(global_mean) <- names(ras_scaled)
    expect_lt(abs(global_mean["elevation"]), 0.2)
    
    # 4. Check scaling parameters
    params <- result$scaling_params
    expect_named(params, c("center", "scale"))
    expect_named(params$center, c("elevation", "other_metric"))
    expect_true(is.numeric(params$scale["elevation"]))
  })
})
