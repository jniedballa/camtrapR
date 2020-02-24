library(camtrapR)
library(dplyr)

data(camtraps)
data(recordTableSample)

camtraps_tibble <- as_tibble(camtraps)
recordTableSample_tibble <- as_tibble(recordTableSample)

str(recordTableSample_tibble)
str(camtraps_tibble)


camopPlot <- function(camOp, 
                      palette = "Red-Yellow"){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  values_tmp <- na.omit(unique(c(camOp)))

  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = hcl.colors(n = length(values_tmp), palette = palette, rev = TRUE))
  
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}




covariates_one_seasons_w_cams <- rbind(covariates_season1, covariates_season1)
covariates_one_seasons_w_cams$camera <- c(paste(covariates_season1[, stationCol], "Cam1", sep = "_"),
                                                  paste(covariates_season1[, stationCol], "Cam2", sep = "_"))



covariates_all_seasons_w_cams_seasons <- rbind(covariates_all_seasons, covariates_all_seasons)
covariates_all_seasons_w_cams_seasons$camera <- c(paste(covariates_all_seasons[, stationCol], "Cam1", sep = "_"),
                                                  paste(covariates_all_seasons[, stationCol], "Cam2", sep = "_"))

# CTtable <- covariates_all_seasons_w_cams
# cameraCol <- "camera"


camop_season_test <- cameraOperation(CTtable = covariates_season1,
                                     stationCol   = stationCol,
                                     setupCol     = "date_setup", 
                                     retrievalCol = "date_retrieval"
                                     )

camop_season_test2 <- cameraOperation(CTtable = covariates_all_seasons,
                                     stationCol   = stationCol,
                                     sessionCol   = sessionCol,
                                     setupCol     = "date_setup", 
                                     retrievalCol = "date_retrieval")


camop_season_test3 <- cameraOperation(CTtable = covariates_one_seasons_w_cams, 
                                      stationCol   = stationCol,
                                      cameraCol    = cameraCol,
                                      setupCol     = "date_setup",
                                      byCamera     = TRUE,
                                      retrievalCol = "date_retrieval")

camop_season_test4 <- cameraOperation(CTtable = covariates_all_seasons_w_cams_seasons, 
                                      stationCol   = stationCol,
                                      cameraCol    = cameraCol,
                                      sessionCol    = sessionCol,
                                      setupCol     = "date_setup",
                                      byCamera     = TRUE,
                                      retrievalCol = "date_retrieval")

image(camop_season_test)
image(camop_season_test2)
image(camop_season_test3)
image(camop_season_test4)



camOp_empty <- camop_season_test[[1]]
camop.attribute.df <- camop_season_test[[2]]


CTtable_St <- data.frame(Station = c(paste("St", 1:5, sep = "_")),
                         x = c(100, 100, 200, 200, 300),
                         y = c(100, 200, 100, 200, 200),
                           date_setup = as.Date(seq(0,4), origin = "2010-01-01"),
                           date_retrieval = as.Date(seq(0,4), origin = "2010-02-01"),
                           Problem1_from = c(as.Date(10, origin = "2010-01-01"), rep(NA, 4)),
                           Problem1_to = c(as.Date(11, origin = "2010-01-01"), rep(NA, 4)))

# CT table with station Info
CTtable_St_Se <- rbind(CTtable_St, CTtable_St)
CTtable_St_Se <- cbind(CTtable_St_Se, session = rep(c(2010, 2011), each = nrow(CTtable_St)))
CTtable_St_Se[CTtable_St_Se$session == 2011, setupCol] <- CTtable_St_Se[CTtable_St_Se$session == 2011, setupCol] + c(rep(365, times = 4), 380)
CTtable_St_Se[CTtable_St_Se$session == 2011, retrievalCol] <- CTtable_St_Se[CTtable_St_Se$session == 2011, retrievalCol] + 375
CTtable_St_Se[CTtable_St_Se$session == 2011, "Problem1_from"] <- CTtable_St_Se[CTtable_St_Se$session == 2011, "Problem1_from"] + 365
CTtable_St_Se[CTtable_St_Se$session == 2011, "Problem1_to"] <- CTtable_St_Se[CTtable_St_Se$session == 2011, "Problem1_to"] + 370

# one station missing in season 2
CTtable_St_Se <- CTtable_St_Se[-9,]

# CT table with station and camera Info
CTtable_St_Se_Cam <- CTtable_St_Se[rep(seq(1, nrow(CTtable_St_Se)), each = 2),]
CTtable_St_Se_Cam$camera <- rep(c("Cam1", "Cam2"), times = nrow(CTtable_St_Se))
CTtable_St_Se_Cam[5, setupCol] <- CTtable_St_Se_Cam[2, setupCol] + 10

CTtable_St_Se_Cam[13,  "Problem1_from"] <- "2011-02-01"
CTtable_St_Se_Cam[13,  "Problem1_to"] <- "2011-02-12"

CTtable_St_Se_Cam[14,  "Problem1_from"] <- "2011-02-05"
CTtable_St_Se_Cam[14,  "Problem1_to"] <- "2011-02-12"


# CT table with camera Info
CTtable_St_Cam <- CTtable_St_Se_Cam[CTtable_St_Se_Cam$session == 2010,]


hasProblems <- TRUE

{
camop_test1 <- cameraOperation(CTtable = CTtable_St,
                                     setupCol     = setupCol, 
                                     retrievalCol = retrievalCol,
                               hasProblems = hasProblems
)

camop_test2 <- cameraOperation(CTtable = CTtable_St_Se,
                               sessionCol = "session",
                               setupCol     = setupCol, 
                               retrievalCol = retrievalCol,
                               hasProblems = hasProblems
)

camop_test3a1 <- cameraOperation(CTtable = CTtable_St_Cam,
                               cameraCol = "camera",
                               setupCol     = setupCol, 
                               retrievalCol = retrievalCol,
                               byCamera = FALSE,
                               allCamsOn = FALSE,
                               camerasIndependent = TRUE,
                               hasProblems = hasProblems
                               )

camop_test3a2 <- cameraOperation(CTtable = CTtable_St_Cam,
                                cameraCol = "camera",
                                setupCol     = setupCol, 
                                retrievalCol = retrievalCol,
                                byCamera = FALSE,
                                allCamsOn = FALSE,
                                camerasIndependent = FALSE,
                                hasProblems = hasProblems
)
camop_test3a3 <- cameraOperation(CTtable = CTtable_St_Cam,
                                 cameraCol = "camera",
                                 setupCol     = setupCol, 
                                 retrievalCol = retrievalCol,
                                 byCamera = FALSE,
                                 allCamsOn = TRUE,
                                 #camerasIndependent = FALSE,
                                 hasProblems = hasProblems
)

camop_test3b <- cameraOperation(CTtable = CTtable_St_Cam,
                                cameraCol = "camera",
                                setupCol     = setupCol, 
                                retrievalCol = retrievalCol,
                                byCamera = TRUE,
                                hasProblems = hasProblems
)

camop_test4a1 <- cameraOperation(CTtable = CTtable_St_Se_Cam,
                               sessionCol = "session",
                               cameraCol = "camera",
                               setupCol     = setupCol, 
                               retrievalCol = retrievalCol,
                               byCamera = FALSE,
                               allCamsOn = FALSE,
                               camerasIndependent = TRUE,
                               hasProblems = hasProblems
)

camop_test4a2 <- cameraOperation(CTtable = CTtable_St_Se_Cam,
                                sessionCol = "session",
                                cameraCol = "camera",
                                setupCol     = setupCol, 
                                retrievalCol = retrievalCol,
                                byCamera = FALSE,
                                allCamsOn = FALSE,
                                camerasIndependent = FALSE,
                                hasProblems = hasProblems
)

camop_test4a3 <- cameraOperation(CTtable = CTtable_St_Se_Cam,
                                sessionCol = "session",
                                cameraCol = "camera",
                                setupCol     = setupCol, 
                                retrievalCol = retrievalCol,
                                byCamera = FALSE,
                                allCamsOn = TRUE,
                                #camerasIndependent = TRUE,
                                hasProblems = hasProblems
)

camop_test4b <- cameraOperation(CTtable = CTtable_St_Se_Cam,
                               sessionCol = "session",
                               cameraCol = "camera",
                               setupCol     = setupCol, 
                               retrievalCol = retrievalCol,
                               byCamera = TRUE,
                               hasProblems = hasProblems
                               )
}


par(oma = c(0, 5,0,0))

camopPlot(camop_test1)
camopPlot(camop_test2)
camopPlot(camop_test3a1)
camopPlot(camop_test3a2)
camopPlot(camop_test3a3)
camopPlot(camop_test3b)
camopPlot(camop_test4a1)
camopPlot(camop_test4a2)
camopPlot(camop_test4a3)

par(oma = c(0, 9,0,0))
camopPlot(camop_test4b)


deparseCamOpRownames(camop_test1)
deparseCamOpRownames(camop_test2)
deparseCamOpRownames(camop_test3a1)
deparseCamOpRownames(camop_test3a2)
deparseCamOpRownames(camop_test3a3)
deparseCamOpRownames(camop_test3b)
deparseCamOpRownames(camop_test4a1)
deparseCamOpRownames(camop_test4a2)
deparseCamOpRownames(camop_test4a3)
deparseCamOpRownames(camop_test4b)

