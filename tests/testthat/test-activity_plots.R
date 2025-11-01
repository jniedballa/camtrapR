
data(recordTableSample)

# --- Tests ---
context("Activity Plotting Functions")

#############################################
# Tests for activityRadial()
#############################################
testthat::describe("activityRadial()", {
  
  test_that("Input validation works correctly", {
    expect_error(activityRadial(recordTableSample, species = "PBE", speciesCol = "WRONG_COL"),
                 "is not a column name in recordTable")
    expect_error(activityRadial(recordTableSample, species = "PBE", recordDateTimeCol = "WRONG_COL"),
                 "is not a column name in recordTable")
    expect_error(activityRadial(recordTableSample, species = "UNKNOWN_SPECIES"),
                 "species %in% recordTable\\[, speciesCol\\] is not TRUE")
    expect_error(activityRadial(recordTableSample, species = "PBE", writePNG = TRUE), # missing plotDirectory
                 "writePNG is TRUE. Please set plotDirectory")
  })
  
  test_that("Output for a single species is correct", {
    output <- activityRadial(recordTableSample, species = "PBE", plotR = FALSE)
    expect_s3_class(output, "data.frame")
    expect_equal(nrow(output), 24)
    expect_named(output, c("hour", "n", "perc", "radial.pos", "length4plot"))
    expect_equal(sum(output$n), 18) # 18 records for PBE
    expect_equal(sum(output$perc), 1)
    
    # Check byNumber = FALSE (default): length4plot should be proportions
    expect_equal(output$length4plot, output$perc)
    
    # Check byNumber = TRUE: length4plot should be counts
    output_by_number <- activityRadial(recordTableSample, species = "PBE", byNumber = TRUE, plotR = FALSE)
    expect_equal(output_by_number$length4plot, output_by_number$n)
  })
  
  test_that("Output for all species is correct", {
    output_list <- activityRadial(recordTableSample, allSpecies = TRUE, plotR = FALSE)
    species_in_data <- unique(recordTableSample$Species)
    
    expect_type(output_list, "list")
    expect_named(output_list, species_in_data)
    expect_length(output_list, length(species_in_data))
    
    # Check structure and counts of an element in the list
    expect_s3_class(output_list$VTA, "data.frame")
    expect_equal(nrow(output_list$VTA), 24)
    expect_equal(sum(output_list$VTA$n), 5) # 12 records for VTA
  })
  
  test_that("File writing works as expected", {
    # Using a temporary directory for safe file operations
    withr::with_tempdir({
      # Test 1: createDir = TRUE successfully creates directory
      plot_dir <- "radial_plots"
      expect_false(dir.exists(plot_dir))
      activityRadial(recordTableSample, allSpecies = TRUE, writePNG = TRUE, plotDirectory = plot_dir, createDir = TRUE, plotR = FALSE)
      expect_true(dir.exists(plot_dir))
      expect_equal(length(list.files(plot_dir, pattern = "\\.png$")), 5) # One plot per species, plut richness
      
      # Test 2: Errors when directory doesn't exist and createDir = FALSE
      plot_dir_2 <- "non_existent_dir"
      expect_error(
        activityRadial(recordTableSample, species = "PBE", writePNG = TRUE, plotDirectory = plot_dir_2, createDir = FALSE),
        "plotDirectory does not exist"
      )
    })
  })
})


#############################################
# Tests for activityHistogram()
#############################################
testthat::describe("activityHistogram()", {
  
  test_that("Input validation works correctly", {
    expect_error(activityHistogram(recordTableSample, species = "VTA", speciesCol = "WRONG_COL"),
                 "is not a column name in recordTable")
    expect_error(activityHistogram(recordTableSample, species = "VTA", recordDateTimeCol = "WRONG_COL"),
                 "is not a column name in recordTable")
    expect_error(activityHistogram(recordTableSample, species = "UNKNOWN_SPECIES"),
                 "species %in% recordTable\\[, speciesCol\\] is not TRUE")
    expect_error(activityHistogram(recordTableSample, species = "VTA", writePNG = TRUE), # missing plotDirectory
                 "writePNG is TRUE. Please set plotDirectory")
  })
  
  test_that("Output for a single species is correct", {
    output <- activityHistogram(recordTableSample, species = "VTA", plotR = FALSE)
    expect_s3_class(output, "POSIXct")
    expect_length(output, 5) # 12 records for VTA
  })
  
  test_that("Output for all species is correct", {
    output_list <- activityHistogram(recordTableSample, allSpecies = TRUE, plotR = FALSE)
    species_in_data <- unique(recordTableSample$Species)
    
    expect_type(output_list, "list")
    expect_named(output_list, species_in_data)
    expect_length(output_list, length(species_in_data))
    
    # Check structure and length of one element
    expect_s3_class(output_list$PBE, "POSIXct")
    expect_length(output_list$PBE, 18)
  })
})


#############################################
# Tests for activityDensity()
#############################################
testthat::describe("activityDensity()", {
  
  skip_if_not_installed("overlap")
  
  test_that("Input validation works correctly", {
    expect_error(activityDensity(recordTableSample, species = "PBE", speciesCol = "WRONG_COL"),
                 "is not a column name in recordTable")
    expect_error(activityDensity(recordTableSample, species = "UNKNOWN_SPECIES"),
                 "species %in% recordTable\\[, speciesCol\\] is not TRUE")
    # Should fail for species with only 1 record
    expect_error(activityDensity(recordTableSample[1,], species = "PBE", plotR = FALSE),
                 "had only 1 record. Cannot estimate density.")
    expect_error(activityDensity(recordTableSample, species = "PBE", writePNG = TRUE), # missing plotDirectory
                 "writePNG is TRUE. Please set plotDirectory")
  })
  
  test_that("Output for a single species is correct (times in radians)", {
    output <- activityDensity(recordTableSample, species = "PBE", plotR = FALSE)
    expect_type(output, "double")
    expect_length(output, 18)
    expect_true(all(output >= 0 & output <= 2 * pi))
  })
  
  test_that("Output for all species is correct (with warnings for skipped species)", {
    # It should warn about the species with 1 record and skip it
    expect_warning(
      output_list <- activityDensity(recordTableSample[-which(recordTableSample$Species == "MNE")[2],], allSpecies = TRUE, plotR = FALSE),
      "MNE: It had only 1 record. Cannot estimate density.   - SKIPPED"
    )
    
    # The returned list contains NULL for the skipped species, so we filter it out
    output_list_clean <- output_list[!sapply(output_list, is.null)]
    species_with_enough_data <- c("PBE", "VTA", "EGY", "TRA")
    
    expect_type(output_list, "list")
    expect_named(output_list_clean, species_with_enough_data)
    expect_length(output_list_clean, 4)
    
    # Check structure of one element
    expect_type(output_list_clean$VTA, "double")
    expect_length(output_list_clean$VTA, 5)
  })
})


#############################################
# Tests for activityOverlap()
#############################################
testthat::describe("activityOverlap()", {
  
  skip_if_not_installed("overlap")
  
  test_that("Input validation works correctly", {
    expect_error(activityOverlap(recordTableSample, speciesA = "PBE", speciesB = "VTA", speciesCol = "WRONG_COL"),
                 "is not a column name in recordTable")
    expect_error(activityOverlap(recordTableSample, speciesA = "UNKNOWN", speciesB = "VTA"),
                 "all\\(c\\(speciesA, speciesB\\) %in% recordTable\\[, speciesCol\\]\\) is not TRUE")
    expect_error(activityOverlap(recordTableSample, speciesA = "PBE", speciesB = "SUS"),
                 "all(c(speciesA, speciesB) %in% recordTable[, speciesCol]) is not TRUE")
    expect_error(activityOverlap(recordTableSample[-which(recordTableSample$Species == "MNE")[2],], speciesA = "PBE", speciesB = "MNE"),
                 "speciesB has only 1 record. Cannot estimate density.")
    
    # Check for invalid overlap estimator
    expect_error(activityOverlap(recordTableSample, speciesA = "PBE", speciesB = "VTA", overlapEstimator = "Dhat_WRONG"),
                 "'arg' should be one of")
  })
  
  test_that("Output is a data.frame with correct structure when plotR = TRUE", {
    # Set plotR = TRUE to ensure overlapPlot is called and returns its data
    plot_data <- activityOverlap(recordTableSample, speciesA = "PBE", speciesB = "VTA", plotR = TRUE)
    
    expect_s3_class(plot_data, "data.frame")
    expect_named(plot_data, c("x", "densityA", "densityB"))
    expect_gt(nrow(plot_data), 0)
  })
  
  test_that("Function returns NULL when plotR is FALSE and writePNG is FALSE", {
    # The return value is inside an `if(plotR)` block, so it should be NULL otherwise.
    output <- activityOverlap(recordTableSample, speciesA = "PBE", speciesB = "VTA", plotR = FALSE, writePNG = FALSE)
    expect_null(output)
  })
  
  test_that("File writing works as expected", {
    withr::with_tempdir({
      plot_dir <- "overlap_plots"
      activityOverlap(recordTableSample, 
                      speciesA = "PBE", 
                      speciesB = "VTA", 
                      writePNG = TRUE, 
                      plotDirectory = plot_dir, 
                      createDir = TRUE, 
                      plotR = FALSE) # Set plotR to FALSE as we only test writing
      
      expect_true(dir.exists(plot_dir))
      files <- list.files(plot_dir, pattern = "\\.png$")
      expect_equal(length(files), 1)
      expect_true(grepl("PBE-VTA", files[1]))
    })
  })
})