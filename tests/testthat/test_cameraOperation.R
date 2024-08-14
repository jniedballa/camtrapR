context("cameraOperation")
library(camtrapR)
library(lubridate)


# load station information
data(camtraps)

# create camera operation matrix
# dates are specified as character
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


# dates are specified as Date class
camop_dates <- cameraOperation(CTtable      = camtraps_date,
                               stationCol   = "Station",
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               #writecsv     = FALSE,
                               hasProblems  = TRUE,
                               dateFormat   = "ymd"
)



# Date-time 

# add time of day to setup / retrieval / problem
# date-time is provided as POXIXct

camtraps_date_time <- camtraps
camtraps_date_time$Setup_date      <- dmy_h(paste(camtraps$Setup_date, "12"))
camtraps_date_time$Retrieval_date  <- dmy_h(paste(camtraps$Retrieval_date, "15" ))
camtraps_date_time$Problem1_from   <- dmy_h(paste(camtraps$Problem1_from, "06" ), quiet = TRUE)
camtraps_date_time$Problem1_to     <- dmy_h(paste(camtraps$Problem1_to, "15" ), quiet = TRUE)

camop_dates_time <- cameraOperation(CTtable      = camtraps_date_time,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = TRUE,
                                    dateFormat   = "ymd HMS"
)


# date-time is provided as character
camtraps_date_time_char <- camtraps_date_time
camtraps_date_time_char$Setup_date      <- as.character(camtraps_date_time$Setup_date)
camtraps_date_time_char$Retrieval_date  <- as.character(camtraps_date_time$Retrieval_date)
camtraps_date_time_char$Problem1_from   <- as.character(camtraps_date_time$Problem1_from)
camtraps_date_time_char$Problem1_to     <- as.character(camtraps_date_time$Problem1_to)

camop_dates_time_char <- cameraOperation(CTtable      = camtraps_date_time_char,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = TRUE,
                                    dateFormat   = "ymd HMS"
)


# set one time to midnight. It should still work.
# issue is that as.character(POSIXct) removes the time if it's exactly midnight
# input is character
camtraps_date_time_char_midnight <- camtraps_date_time_char
camtraps_date_time_char_midnight$Setup_date[1] <- "2009-04-02 00:00:00"

camop_dates_time_char_midnight <- cameraOperation(CTtable      = camtraps_date_time_char_midnight,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = TRUE,
                                    dateFormat   = "ymd HMS"
)

# input is POSIXct
# camtraps_date_time_char <- camtraps_date_time
# camtraps_date_time_char$Setup_date <- format(camtraps_date_time_char$Setup_date, 
#                                         format = "%Y-%m-%d %H:%M:%S")

camtraps_date_time_midnight <- camtraps_date_time_char_midnight
camtraps_date_time_midnight$Setup_date <- ymd_hms(camtraps_date_time_midnight$Setup_date)
camtraps_date_time_midnight$Retrieval_date <- ymd_hms(camtraps_date_time_midnight$Retrieval_date)
camtraps_date_time_midnight$Problem1_from <- ymd_hms(camtraps_date_time_midnight$Problem1_from)
camtraps_date_time_midnight$Problem1_to <- ymd_hms(camtraps_date_time_midnight$Problem1_to)



camop_dates_time_midnight <- cameraOperation(CTtable      = camtraps_date_time_midnight,
                                     stationCol   = "Station",
                                     setupCol     = "Setup_date",
                                     retrievalCol = "Retrieval_date",
                                     hasProblems  = TRUE,
                                     dateFormat   = "ymd HMS"
)


# change datetime format
camtraps_date_time_format <- camtraps_date_time
stupid_date_format <- "%H:%M:%S %d/%m/%Y"    #"%d/%m/%Y %H:%M:%S"
camtraps_date_time_format$Setup_date <- format(camtraps_date_time_format$Setup_date, 
                                        format = stupid_date_format)
camtraps_date_time_format$Retrieval_date <- format(camtraps_date_time_format$Retrieval_date, 
                                        format = stupid_date_format)
camtraps_date_time_format$Problem1_from <- format(camtraps_date_time_format$Problem1_from, 
                                        format = stupid_date_format)
camtraps_date_time_format$Problem1_to <- format(camtraps_date_time_format$Problem1_to, 
                                            format = stupid_date_format)

camop_dates_time_format <- cameraOperation(CTtable      = camtraps_date_time_format,
                                     stationCol   = "Station",
                                     setupCol     = "Setup_date",
                                     retrievalCol = "Retrieval_date",
                                     hasProblems  = TRUE,
                                     dateFormat   = "HMS dmy"
)



# add empty Problem columns
camtraps2 <- camtraps
camtraps2$Problem2_from <- NA
camtraps2$Problem2_to <- NA



# Test section
test_that("POSIX and character give same output", {
  expect_equal(camop_dates_time, camop_dates_time_char)
  expect_equal(camop_dates_time_char_midnight, camop_dates_time_midnight)
})

test_that("Different date formats work", {
  expect_equal(camop_dates_time_format, camop_dates_time)
})

camop_dates_time_format


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

test_that("Error when Problem ends after retrieval", {
  
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

test_that("setting time to 00:00:00 works", {
  
  expect_true(camop_dates_time_midnight[1,1] == 1)
  expect_true(camop_dates_time_char_midnight[1,1] == 1)
  expect_true(!identical(camop_dates_time_midnight, camop_dates))
}
)


test_that("Error when all NA in problem columns", {
  
  expect_error(cameraOperation(CTtable      = camtraps2,
                               stationCol   = "Station",
                               setupCol     = "Setup_date",
                               retrievalCol = "Retrieval_date",
                               writecsv     = FALSE,
                               hasProblems  = TRUE,
                               dateFormat   = "dmy"
  ),
  "Problem2 columns are both NA"
  )
}
)
