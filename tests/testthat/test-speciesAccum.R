

context("speciesAccum")

# Load necessary libraries
library(lubridate)
library(dplyr)

# --- Test Setup ---
# Create small, predictable mock data frames for testing.

# Mock CTtable
mock_cams <- data.frame(
  Station = c("A", "B", "C"),
  Setup_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-05"))
)

# Mock recordTable with different species, stations, and dates
mock_recs <- data.frame(
  Station = c("A", "A", "B", "B", "C", "C", "A"),
  Species = c("Fox", "Badger", "Fox", "Deer", "Badger", "Deer", "Fox"),
  DateTime = c(
    "2023-01-01 12:00:00", # Fox at A on Day 1 (station day 1)
    "2023-01-02 10:00:00", # Badger at A on Day 2 (station day 2)
    "2023-01-02 14:00:00", # Fox at B on Day 2 (station day 1)
    "2023-01-04 08:00:00", # Deer at B on Day 4 (station day 3)
    "2023-01-05 18:00:00", # Badger at C on Day 5 (station day 1)
    "2023-01-06 20:00:00", # Deer at C on Day 6 (station day 2)
    "2023-01-03 16:00:00"  # Fox at A on Day 3 (station day 3) - a second visit
  ),
  Season = c("Winter", "Winter", "Winter", "Winter", "Spring", "Spring", "Winter")
)

# --- Test Suite ---

testthat::describe("Core Functionality", {
  
  # Skip all tests if iNEXT is not installed
  skip_if_not_installed("iNEXT")
  
  # test_that("it produces a valid iNEXT object with x_unit = 'station'", {
  #   result <- speciesAccum(
  #     CTtable = mock_cams,
  #     recordTable = mock_recs,
  #     speciesCol = "Species",
  #     recordDateTimeCol = "DateTime",
  #     setupCol = "Setup_date",
  #     stationCol = "Station",
  #     x_unit = "station",
  #     nboot = 10 # Use a small nboot for speed
  #   )
  #   
  #   # 1. Check output class and structure
  #   expect_s3_class(result, "iNEXT")
  #   expect_named(result, c("DataInfo", "iNextEst", "AsyEst"))
  #   
  #   # 2. Check DataInfo for correct dimensions
  #   # 3 stations, 3 species, 5 total incidences
  #   expect_equal(result$DataInfo$T, 3) 
  #   expect_equal(result$DataInfo$S.obs, 3)
  #   expect_equal(result$DataInfo$n, 5)
  })
  
  test_that("it produces a valid iNEXT object with x_unit = 'survey_day'", {
    
    skip_if_not_installed("iNEXT")
    
    result <- speciesAccum(
      CTtable = mock_cams,
      recordTable = mock_recs,
      speciesCol = "Species",
      recordDateTimeCol = "DateTime",
      setupCol = "Setup_date",
      stationCol = "Station",
      x_unit = "survey_day", # Key argument
      nboot = 10
    )
    
    # 1. Check output class
    expect_s3_class(result, "iNEXT")
    
    # 2. Check DataInfo for correct dimensions
    # The survey runs for 6 unique days
    expect_equal(result$DataInfo$T, 6)
    expect_equal(result$DataInfo$S.obs, 3)
  })
  
#   test_that("it produces a valid iNEXT object with x_unit = 'station_day'", {
#     result <- speciesAccum(
#       CTtable = mock_cams,
#       recordTable = mock_recs,
#       speciesCol = "Species",
#       recordDateTimeCol = "DateTime",
#       setupCol = "Setup_date",
#       stationCol = "Station",
#       x_unit = "station_day", # Key argument
#       nboot = 10
#     )
#     
#     # 1. Check output class
#     expect_s3_class(result, "iNEXT")
#     
#     # 2. Check DataInfo for correct dimensions
#     # The maximum number of station_days is 3
#     expect_equal(result$DataInfo$T, 3)
#     expect_equal(result$DataInfo$S.obs, 3)
#   })
# })


# testthat::describe("Assemblage Grouping", {
#   
#   skip_if_not_installed("iNEXT")
#   
#   test_that("assemblageCol correctly splits data into a multi-assemblage iNEXT object", {
#     result_assem <- speciesAccum(
#       CTtable = mock_cams,
#       recordTable = mock_recs,
#       speciesCol = "Species",
#       recordDateTimeCol = "DateTime",
#       setupCol = "Setup_date",
#       stationCol = "Station",
#       assemblageCol = "Season", # Key argument
#       x_unit = "station",
#       nboot = 10
#     )
#     
#     # 1. Check output class and structure
#     expect_s3_class(result_assem, "iNEXT")
#     expect_true(is.list(result_assem$iNextEst)) # iNextEst is a list for multi-assemblage
#     
#     # 2. Check DataInfo for correct number of assemblages
#     expect_equal(nrow(result_assem$DataInfo), 2)
#     expect_equal(sort(result_assem$DataInfo$Assemblage), c("Spring", "Winter"))
#     
#     # 3. Check data for one of the assemblages
#     winter_data_info <- result_assem$DataInfo[result_assem$DataInfo$Assemblage == "Winter", ]
#     # Winter has 2 stations (A, B) and 3 species (Fox, Badger, Deer)
#     expect_equal(winter_data_info$T, 2)
#     expect_equal(winter_data_info$S.obs, 3)
#   })
# })


testthat::describe("Incidence Matrix Creation (Internal Logic)", {
  
  test_that("create_incidence_matrix builds a correct 'station' matrix", {
    
    # Call the internal helper directly to test its output
    mat <- create_incidence_matrix(
      records_subset = mock_recs,
      stations_subset = mock_cams,
      species_list = unique(mock_recs$Species),
      temporal = FALSE, # for x_unit = "station"
      stationCol = "Station", setupCol = "Setup_date", speciesCol = "Species",
      recordDateTimeCol = "DateTime", recordDateTimeFormat = "ymd HMS", dateFormat = "ymd"
    )
    
    # 1. Check dimensions: 3 species x 3 stations
    expect_equal(dim(mat), c(3, 3))
    expect_equal(sort(rownames(mat)), c("Badger", "Deer", "Fox"))
    expect_equal(sort(colnames(mat)), c("A", "B", "C"))
    
    # 2. Check specific cell values
    expect_equal(mat["Fox", "A"], 1)    # Fox at Station A
    expect_equal(mat["Badger", "B"], 0) # Badger NOT at Station B
    expect_equal(mat["Deer", "C"], 1)   # Deer at Station C
    expect_equal(sum(mat), 6)          # 6 total incidences
  })
  
  test_that("create_incidence_matrix builds a correct 'survey_day' matrix", {
    mat <- create_incidence_matrix(
      records_subset = mock_recs,
      stations_subset = mock_cams,
      species_list = unique(mock_recs$Species),
      temporal = TRUE, # for day-based units
      by_station = FALSE, # for "survey_day"
      stationCol = "Station", setupCol = "Setup_date", speciesCol = "Species",
      recordDateTimeCol = "DateTime", recordDateTimeFormat = "ymd HMS", dateFormat = "ymd"
    )
    
    # 1. Check dimensions: 3 species x 6 survey days
    expect_equal(dim(mat), c(3, 6))
    
    # 2. Check specific cell values
    expect_equal(mat["Fox", "1"], 1)     # Fox on Day 1
    expect_equal(mat["Badger", "2"], 1)  # Badger on Day 2
    expect_equal(mat["Deer", "2"], 0)    # Deer NOT on Day 2
    expect_equal(mat["Deer", "4"], 1)    # Deer on Day 4
  })
})


testthat::describe("Input Validation", {
  
  test_that("it stops for missing or invalid columns", {
    
    skip_if_not_installed("iNEXT")
    
    expect_error(
      speciesAccum(CTtable = mock_cams, recordTable = mock_recs, speciesCol = "WRONG_COL",
                   recordDateTimeCol = "DateTime", setupCol = "Setup_date", stationCol = "Station"),
      regexp = "Missing required columns in recordTable"
    )
    
    expect_error(
      speciesAccum(CTtable = mock_cams, recordTable = mock_recs, speciesCol = "Species",
                   recordDateTimeCol = "DateTime", setupCol = "Setup_date", stationCol = "Station",
                   assemblageCol = "WRONG_ASSEMBLAGE"),
      regexp = "assemblageCol not found in recordTable"
    )
  })
})
