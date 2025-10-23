context("surveyDashboard")

library(camtrapR)
library(shiny)


# Basic usage with minimal parameters

data("camtraps")
data("recordTableSample")



test_that("surveyDashboard can be launched without parameters without error", {
  
  testServer(app = surveyDashboard(), {
    
    # If code inside this block is reached means the server initialized successfully.
    expect_true(TRUE) 
  })
})


test_that("surveyDashboard server logic initializes correctly with data", {
  
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
    }
  )
})






