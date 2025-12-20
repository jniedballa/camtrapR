# test-detectionMaps.R

context("detectionMaps")

# Load necessary libraries for testing
library(testthat)
library(sf)
library(withr)

# --- Test Setup ---
# This block creates shared, self-contained data for all tests.

# 1. Load sample data from the package
data(camtraps)
data(recordTableSample)

# 2. Create the sample Area of Interest (AOI) polygon from the example
aoi_poly <- st_polygon(list(cbind(c(521500, 526500, 527000, 521500, 521500),
                                  c(607500, 608000, 603500, 603500, 607500))))
aoi <- st_sf(name = "My AOI", geometry = st_sfc(aoi_poly), crs = 32650)


# --- Test Suite ---

testthat::describe("Core Functionality & Return Value", {
  
  test_that("it returns a correct data.frame with expected calculations", {
    # Run with plotting and file writing disabled to test only the data output
    maps_df <- detectionMaps(
      CTtable     = camtraps,
      recordTable = recordTableSample,
      Xcol        = "utm_x",
      Ycol        = "utm_y",
      stationCol  = "Station",
      speciesCol  = "Species",
      plotR       = FALSE, # Disable plotting for this test
      writePNG    = FALSE,
      writeShapefile = FALSE
    )
    
    # 1. Check the output type and dimensions
    expect_s3_class(maps_df, "data.frame")
    expect_equal(nrow(maps_df), nrow(camtraps)) # Should return a row for every station
    
    # 2. Check for expected columns (station, coords, species, richness)
    expected_species <- sort(unique(recordTableSample$Species))
    expected_cols <- c("Station", "utm_x", "utm_y", expected_species, "n_species")
    expect_named(maps_df, expected_cols)
    
    # 3. Verify specific calculated values based on sample data
    stationA_data <- maps_df[maps_df$Station == "StationA", ]
    expect_equal(stationA_data$PBE, 4)
    expect_equal(stationA_data$VTA, 2)
    expect_equal(stationA_data$n_species, 2) # PBE, VTA were detected here
    
    stationC_data <- maps_df[maps_df$Station == "StationC", ]
    expect_equal(stationC_data$EGY, 6)
    expect_equal(stationC_data$PBE, 6)
    expect_equal(stationC_data$MNE, 0)
    expect_equal(stationC_data$n_species, 4)
  })
})


testthat::describe("Filtering and Subset Options", {
  
  plot(1)  # avoid warning: calling par(new=TRUE) with no plot
  
  test_that("speciesToShow argument correctly filters the output", {
    species_subset <- c("PBE", "VTA")
    maps_df_filtered <- detectionMaps(
      CTtable       = camtraps,
      recordTable   = recordTableSample,
      speciesToShow = species_subset,
      Xcol = "utm_x", Ycol = "utm_y", plotR = FALSE
    )
    
    # 1. Check that only the specified species columns exist
    expected_cols <- c("Station", "utm_x", "utm_y", sort(species_subset), "n_species")
    expect_named(maps_df_filtered, expected_cols)
    
    # 2. Check that richness is recalculated based on the subset
    stationB_data <- maps_df_filtered[maps_df_filtered$Station == "StationB", ]
    # StationB had MNE, PBE VTA. After filtering, it should only count 2 species.
    expect_equal(stationB_data$n_species, 2)
  })
})


testthat::describe("File Generation (Side Effects)", {
  
  # Use a temporary directory to avoid creating files in the project
  with_tempdir({
    
    test_that("writePNG = TRUE creates the expected number of PNG files", {
      # Suppress plotting to the R device to keep the test console clean
      pdf(NULL)
      detectionMaps(
        CTtable       = camtraps,
        recordTable   = recordTableSample,
        Xcol = "utm_x", Ycol = "utm_y",
        writePNG      = TRUE,
        plotR         = FALSE, # Don't plot to R device
        plotDirectory = ".",   # Write to the temp directory
        createPlotDir = FALSE
      )
      dev.off() # Close the null PDF device
      
      png_files <- list.files(pattern = "\\.png$")
      
      # Expecting 7 PNGs: 1 for richness + 6 for the unique species in the sample data
      n_species <- length(unique(recordTableSample$Species))
      expect_equal(length(png_files), 1 + n_species) 
      expect_true(any(grepl("n_Species_", png_files)))
      expect_true(any(grepl("Presence_PBE", png_files)))
    })
    
    test_that("writeShapefile = TRUE creates a valid shapefile", {
      skip_if_not_installed("sf")
      
      # Generate a unique name to avoid conflicts
      shapefile_name <- "test_detections"
      
      maps_sf_obj <- detectionMaps(
        CTtable            = camtraps,
        recordTable        = recordTableSample,
        Xcol = "utm_x", Ycol = "utm_y", plotR = FALSE,
        writeShapefile     = TRUE,
        shapefileName      = shapefile_name,
        shapefileDirectory = ".", # Write to the temp directory
        shapefileProjection= 4326 # Use a known CRS (WGS84)
      )
      
      # 1. Check that the shapefile components were created
      expect_true(file.exists(paste0(shapefile_name, ".shp")))
      expect_true(file.exists(paste0(shapefile_name, ".dbf")))
      
      # 2. Check that the returned object is an sf object
      expect_s3_class(maps_sf_obj, "sf")
      
      # 3. Read the shapefile back in and verify its properties
      sf_read <- st_read(dsn = ".", layer = shapefile_name, quiet = TRUE)
      expect_s3_class(sf_read, "sf")
      expect_equal(nrow(sf_read), nrow(camtraps))
      expect_true(st_crs(sf_read) == st_crs(4326)) # Verify projection was set
    })
  })
})


testthat::describe("Spatial Features and Plotting Options", {
  
  test_that("backgroundPolygon argument runs without error", {
    # The main test is that the function doesn't crash when trying to plot the polygon.
    # We can't easily test the visual output, so we just check for successful execution.
    pdf(NULL) # Suppress plot output
    expect_no_error(
      detectionMaps(
        CTtable           = camtraps,
        recordTable       = recordTableSample,
        backgroundPolygon = aoi,
        Xcol = "utm_x", Ycol = "utm_y",
        plotR             = TRUE,
        writePNG          = FALSE
      )
    )
    dev.off()
  })
})


testthat::describe("Input Validation", {
  
  plot(1)  # avoid warning: calling par(new=TRUE) with no plot
  
  test_that("it stops for missing or invalid column names", {
    expect_error(
      detectionMaps(CTtable = camtraps, recordTable = recordTableSample, Xcol = "WRONG_X", Ycol = "utm_y"),
      regexp = "Xcol = \"WRONG_X\" is not a column name in CTtable"
    )
    
    # Mismatched station IDs
    recordTable_bad_station <- recordTableSample
    recordTable_bad_station$Station[1] <- "UNKNOWN_STATION"
    expect_error(
      detectionMaps(CTtable = camtraps, recordTable = recordTable_bad_station, Xcol = "utm_x", Ycol = "utm_y"),
      regexp = "items of stationCol in recordTable are not matched"
    )
  })
  
  test_that("it stops if writeShapefile = TRUE and directory is missing", {
    expect_error(
      detectionMaps(CTtable = camtraps, recordTable = recordTableSample, Xcol = "utm_x", Ycol = "utm_y",
                    writeShapefile = TRUE, plotR = FALSE),
      fixed = "hasArg(shapefileDirectory) is not TRUE"
    )
  })
})
