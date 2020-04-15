context("cameraOperation")
library(camtrapR)


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


camtraps_duplicate <- camtraps
camtraps_duplicate[4,]    <- camtraps_duplicate[1,]

camtraps_duplicate$camera <- "cam1"
camtraps_duplicate[c(5,6),] <- camtraps_duplicate[3,]
camtraps_duplicate$session <- 1


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

