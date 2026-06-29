# Using reactlog to visualize dependencies in dashboard


# install.packages("reactlog")
library(reactlog)
library(camtrapR)


data("camtraps")
data("recordTableSample")

# Method 1 ----
# enable logging
reactlog::reactlog_enable()


# in app, do one thing only (to isolate bug)

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


# open reactlog in browser
reactlogShow(time = TRUE)

# reset
reactlogReset()

# Method 2 ----

options(shiny.reactlog = F)


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
reactlogShow()
