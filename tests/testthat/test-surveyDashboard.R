context("surveyDashboard")

library(camtrapR)
library(shiny)


# Basic usage with minimal parameters

data("camtraps")
data("recordTableSample")



test_that("surveyDashboard can be launched without parameters without error", {
  
  testthat::skip_if_not_installed("mapview")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("corrplot")
  testthat::skip_if_not_installed("unmarked")
  testthat::skip_if_not_installed("ubms")
  
  testServer(app = surveyDashboard(), {
    
    # If code inside this block is reached means the server initialized successfully.
    expect_true(TRUE) 
  })
})


test_that("surveyDashboard server logic initializes correctly with sample data", {
  
  testthat::skip_if_not_installed("mapview")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("corrplot")
  testthat::skip_if_not_installed("unmarked")
  testthat::skip_if_not_installed("ubms")
  
  # testServer runs the server function in the current R process,
  # making its internal state (like reactives) accessible for testing.
  
  testServer(
    
    app = surveyDashboard(
      CTtable = camtraps,
      recordTable = recordTableSample,
      xcol = "utm_x",
      ycol = "utm_y",
      crs = "epsg:32650",      # = UTM50N
      stationCol = "Station",
      setupCol = "Setup_date",
      retrievalCol = "Retrieval_date",
      CTdateFormat = "dmy"
    ),
    {
      # Inside this block, you can access server-side objects like
      # input, output, session, and reactive expressions.
      
      expect_equal(num_stations(), 3)
      expect_equal(date_range_min(), as.Date("2009-04-02"))
      expect_equal(date_range_max(), as.Date("2009-05-17"))
      expect_equal(num_species(), 5)
      expect_equal(num_images(), 39)
      expect_equal(avg_records_per_station(), 13)
      expect_equal(dim(data$CTtable), c(3,7))
      expect_equal(dim(data$recordTable), c(39,11))
      expect_equal(dim(df_n_records()), c(5, 2))
      
      # objects not available without user interaction (still NULL)
      # expect_equal(dim(data$aggregated_CTtable), c(3,7))
      # expect_equal(trap_nights(), 128)
      # expect_equal(dim(camop()), c(3,46))
      # expect_equal(dim(data$CTtable_sf), c(3,8))
      # expect_true(inherits(data$CTtable_sf, "sf"))
    }
  )
})

test_that("surveyDashboard server logic initializes correctly with camtrap DP data", {
  
  testthat::skip_if_not_installed("mapview")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("corrplot")
  testthat::skip_if_not_installed("unmarked")
  testthat::skip_if_not_installed("ubms")
  
  # testServer runs the server function in the current R process,
  # making its internal state (like reactives) accessible for testing.
  
  path_camtrapdp <- system.file("sample_data/tdwg_camtrap-dp_1.0.2_example", 
                                package = "camtrapR")
  camtrapdp <- readcamtrapDP(file = file.path(path_camtrapdp, "datapackage.json")) 
  
  testServer(
    
    app = surveyDashboard(CTtable = camtrapdp$CTtable,
                          recordTable = camtrapdp$recordTable, 
                          xcol = "longitude", 
                          ycol = "latitude", 
                          crs = 4326, 
                          stationCol = "locationName", 
                          setupCol = "Setup_date", 
                          retrievalCol = "Retrieval_date", 
                          CTdateFormat = "ymd HMS", 
                          speciesCol = "vernacularName_eng"),
    {
      # Inside this block, you can access server-side objects like
      # input, output, session, and reactive expressions.
      
      expect_equal(num_stations(), 4)
      expect_equal(date_range_min(), as.Date("2020-05-30"))
      expect_equal(date_range_max(), as.Date("2021-04-18"))
      expect_equal(num_species(), 14)
      expect_equal(num_images(), 549)
      expect_equal(round(avg_records_per_station()), 137)
      expect_equal(dim(data$CTtable), c(4,26))
      expect_equal(dim(data$recordTable), c(549,34))
      expect_equal(dim(df_n_records()), c(14, 2))
      
    }
  )
})






