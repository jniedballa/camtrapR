context("cameraOperation")
library(camtrapR)
library(lubridate)


# load station information
data(camtraps)

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "%d/%m/%Y"
)

# with problems/malfunction
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "%d/%m/%Y"
)

# table with duplicate rows
camtraps_duplicate <- camtraps
camtraps_duplicate[4,]    <- camtraps_duplicate[1,]

camtraps_duplicate$camera <- "cam1"
camtraps_duplicate[c(5,6),] <- camtraps_duplicate[3,]
camtraps_duplicate$session <- 1


# convert setup / retrieval / problem to Date class
camtraps_date <- camtraps
camtraps_date$Setup_date      <- dmy(camtraps$Setup_date)
camtraps_date$Retrieval_date  <- dmy(camtraps$Retrieval_date )
camtraps_date$Problem1_from   <- dmy(camtraps$Problem1_from )
camtraps_date$Problem1_to     <- dmy(camtraps$Problem1_to )


camop_dates <- cameraOperation(CTtable      = camtraps_date,
                               stationCol   = "Station",
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               #writecsv     = FALSE,
                               hasProblems  = TRUE,
                               dateFormat   = "ymd"
)

# add time of day to setup / retrieval / problem
camtraps_date_time <- camtraps
camtraps_date_time$Setup_date      <- dmy_h(paste(camtraps$Setup_date, "12"))
camtraps_date_time$Retrieval_date  <- dmy_h(paste(camtraps$Retrieval_date, "15" ))
camtraps_date_time$Problem1_from   <- dmy_h(paste(camtraps$Problem1_from, "06" ), quiet = TRUE)
camtraps_date_time$Problem1_to     <- dmy_h(paste(camtraps$Problem1_to, "15" ), quiet = TRUE)


camop_dates_time <- cameraOperation(CTtable      = camtraps_date_time,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    #writecsv     = FALSE,
                                    hasProblems  = TRUE,
                                    dateFormat   = "ymd HMS"
)

# Test section

test_that("duplicate row error messages work", {
  expect_error(cameraOperation(CTtable      = camtraps_duplicate,
                               stationCol   = "Station",
                               #sessionCol = "session",
                               #cameraCol = "camera",
                               #byCamera = TRUE,
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               writecsv     = FALSE,
                               hasProblems  = FALSE,
                               dateFormat   = "%d/%m/%Y"
  ),
  "2 stations have more than 1 item in CTtable. Please specify 'cameraCol' or 'sessionCol'
StationA: 2
StationC: 3", fixed = TRUE)
  
  expect_error(cameraOperation(CTtable      = camtraps_duplicate,
                               stationCol   = "Station",
                               #sessionCol = "session",
                               cameraCol = "camera",
                               byCamera = TRUE,
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               writecsv     = FALSE,
                               hasProblems  = FALSE,
                               dateFormat   = "%d/%m/%Y"
  ), 
  "2 station/camera combinations have more than 1 item in CTtable. Consider specifying 'sessionCol' if you have multiple sessions / seasons
StationA - camera cam1: 2
StationC - camera cam1: 3")
  
  expect_error(cameraOperation(CTtable      = camtraps_duplicate,
                               stationCol   = "Station",
                               sessionCol = "session",
                               #cameraCol = "camera",
                               #byCamera = TRUE,
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               writecsv     = FALSE,
                               hasProblems  = FALSE,
                               dateFormat   = "%d/%m/%Y"
  ), 
  "2 station/session combinations have more than 1 item in CTtable. Consider specifying 'cameraCol' if you have multiple cameras per station
StationA - session 1: 2
StationC - session 1: 3")
  
  expect_error(cameraOperation(CTtable      = camtraps_duplicate,
                               stationCol   = "Station",
                               sessionCol = "session",
                               cameraCol = "camera",
                               byCamera = TRUE,
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               writecsv     = FALSE,
                               hasProblems  = FALSE,
                               dateFormat   = "%d/%m/%Y"
  )
  , 
  "2 station/camera/session combination have more than 1 item in CTtable.
StationA - camera cam1 - session 1: 2
StationC - camera cam1 - session 1: 3")
  
}
)


test_that("Input columns in class 'Date' work", {
  
  expect_identical(camop_dates, camop_problem)
}
)


test_that("Input columns with hours (POSIXct) work", {

  expect_true(inherits(camop_dates_time, "matrix"))
  expect_true(!identical(camop_dates_time, camop_dates))
}
)

test_that("Error when Prolem ends after retrieval", {
  
  # Date-time as POSIX
  camtraps_date_time$Problem1_to[3] <- camtraps_date_time$Problem1_to[3] + 1
  expect_error(cameraOperation(CTtable  = camtraps_date_time,
                                      stationCol   = "Station",
                                      setupCol     = "Setup_date",
                                      retrievalCol = "Retrieval_date",
                                      hasProblems  = TRUE,
                                      dateFormat   = "ymd HMS"),
               "StationC : Problem ends after retrieval"
  )
  
  # Dates as character
  camtraps$Problem1_to[3] <- "18/05/2009"
  expect_error(cameraOperation(CTtable      = camtraps,
                                   stationCol   = "Station",
                                   setupCol     = "Setup_date",
                                   retrievalCol = "Retrieval_date",
                                   writecsv     = FALSE,
                                   hasProblems  = TRUE,
                                   dateFormat   = "%d/%m/%Y"),
               "StationC : Problem ends after retrieval"
  )
}
)
