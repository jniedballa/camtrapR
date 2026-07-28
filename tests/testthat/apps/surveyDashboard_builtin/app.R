# (Note: Behind the scenes, shinytest2 intelligently intercepts this 
# and runs pkgload::load_all() so it tests the current dev code!)
library(camtrapR)

# load test data
data("camtraps")
data("recordTableSample")

# Call shiny app function
surveyDashboard(
  CTtable = camtraps,
  recordTable = recordTableSample,
  xcol = "utm_x",
  ycol = "utm_y",
  crs = "epsg:32650",      # = UTM50N
  stationCol = "Station",
  setupCol = "Setup_date",
  retrievalCol = "Retrieval_date",
  CTdateFormat = "dmy"
)