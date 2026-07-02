seed     <- 100
nStation <- 10

simcam1 <- simulateCamtrapData(nStation = nStation,
                               seed = seed, 
                               n_cam)

surveyDashboard(CTtable = simcam1$camtraps,
                recordTable = simcam1$recordTable,
                camerasPerStation = 2,
                stationCol = "Station",
                speciesCol = "Species",
                xcol = "longitude",
                ycol = "latitude",
                setupCol = "Setup_date",
                retrievalCol = "Retrieval_date",
                crs = 4326)
