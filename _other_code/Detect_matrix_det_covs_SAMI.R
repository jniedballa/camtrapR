#
#   DFR herpetofauna: Camtrap R trials 
#

setwd("D:/Research/Deramakot amphibians/Project_Data/Data_trials") # Munt-Porc directory
outDir <- "D:/Research/Deramakot amphibians/Project_Data/Data_trials"  # this is where you want the results to go

#install.packages("overlap")
#library(overlap)
#install.packages("camtrapR")
#library(camtrapR)

dat<-read.csv("P1_Trans_Summary.csv",sep = ",", stringsAsFactors  = FALSE, header = TRUE)
spec<-read.csv("P1_Trans_Species.csv",sep = ",", stringsAsFactors  = FALSE, header = TRUE)
DetCov<-read.csv("P1_Trans_DetectCovs.csv",sep = ",", stringsAsFactors  = FALSE, header = TRUE)

table(unique(spec$Speices))

DetCov$Date <- as.Date(DetCov$Date, format = "%d/%m/%Y")

dat$Date <- as.Date(dat$Date, format = "%d/%m/%Y")
dat <- dat[order(dat$Location, dat$Date),]
dat$occasion <- unlist(tapply(dat$Date, INDEX = dat$Location, FUN = order))

nrow_tmp <- length(unique(dat$Location))
ncol_tmp <- max(tapply(dat$Date, INDEX = dat$Location, FUN = length))

det_mat <- matrix(NA, 
                  ncol = ncol_tmp,
                  nrow = nrow_tmp)
rownames(det_mat) <- unique(dat$Location)

spec2 <- spec[,c("Date", "Location", "Speices")]

spec2$Date <- as.Date(spec2$Date, format = "%d/%m/%Y")


##############
# detection matrix for 1 species

speciesOfInterest <- "Leptobrachella_parva_LEPA"

speciesOfInterest <- "Pulchrana_picturata_PUPI"
speciesOfInterest <- "Meristogenys_orphnocnemis_MEOR"
speciesOfInterest <- "Limnonectes_kuhlii_LIKU"


spec2_subset <- spec2[spec2$Speices == speciesOfInterest,]

for(j in 1:nrow(det_mat)){
  spec_tmp <- spec2_subset[spec2_subset$Location == rownames(det_mat)[j],]
  spec_tmp  <- spec_tmp[order(spec_tmp$Date),]
  
  
  det_mat[j,] <- c(rep(0, times = length(dat[dat$Location == rownames(det_mat)[j],]$Date)),
                   rep(NA, times = ncol(det_mat) - length(dat[dat$Location == rownames(det_mat)[j],]$Date)))

  if(nrow(spec_tmp) != 0){
    det_mat[j, match(unique(spec_tmp$Date), dat[dat$Location == rownames(det_mat)[j],]$Date)] <- 1
  
  }
}

det_mat

write.csv(det_mat,
          file = file.path(outDir, paste("detection_history_", 
                                         speciesOfInterest, 
                                         ".csv", sep = ""))
)

##########################################################################################

# detection histories for all species
det_mat_list <- list()


for(xyz in 1:length(unique(spec$Speices))){
spec2_subset <- spec2[spec2$Speices == unique(spec$Speices)[xyz],]

for(j in 1:nrow(det_mat)){
  spec_tmp <- spec2_subset[spec2_subset$Location == rownames(det_mat)[j],]
  spec_tmp  <- spec_tmp[order(spec_tmp$Date),]
  
  
  det_mat[j,] <- c(rep(0, times = length(dat[dat$Location == rownames(det_mat)[j],]$Date)),
                   rep(NA, times = ncol(det_mat) - length(dat[dat$Location == rownames(det_mat)[j],]$Date)))
  
  if(nrow(spec_tmp) != 0){
    det_mat[j, match(unique(spec_tmp$Date), dat[dat$Location == rownames(det_mat)[j],]$Date)] <- 1
    
  }
  det_mat_list[[xyz]] <- det_mat
}
}
names(det_mat_list) <- unique(spec$Speices)
det_mat_list




###########################################################################################

# observation covariates

mean_Sd <- aggregate(DetCov$Stream_depth, 
                       by = list(Date = DetCov$Date, 
                                 Location = DetCov$Location),
                       FUN = mean, 
                       na.rm = TRUE)

#mean_AllStream <- aggregate(DetCov[,c("Stream_width", "Stream_depth", "Stream_turbidity", "Stream_speed")], 
#                       by = list(Date = DetCov$Date, 
#                                 Location = DetCov$Location),
#                       FUN = mean, 
#                       na.rm = TRUE)
#colnames(median_SW)[colnames(median_SW) == "x"] <- "stream_width_median"
# do this for other covariates too
# combine them with cbind() or data.frame()

covariateOfInterest <- "Stream_depth"

colnames(mean_Sd)[colnames(mean_Sd) == "x"] <- covariateOfInterest

cov_mat <- matrix(NA, 
                  ncol = ncol_tmp,
                  nrow = nrow_tmp)
rownames(cov_mat) <- unique(dat$Location)

for(j in 1:nrow(cov_mat)){
  
  # subset to that station j
  mean_Sd_tmp <- mean_Sd[mean_Sd$Location == rownames(cov_mat)[j],]
  dat_tmp <- dat[dat$Location == rownames(cov_mat)[j],]
  #det_mat[j,] <- c(rep(0, times = length(dat[dat$Location == rownames(det_mat)[j],]$Date)),
  #                 rep(NA, times = ncol(det_mat) - length(dat[dat$Location == rownames(det_mat)[j],]$Date)))
  cov_mat[j, match(unique(mean_Sd_tmp$Date), 
                   dat_tmp$Date)] <- mean_Sd_tmp[, covariateOfInterest]
  rm(mean_Sd_tmp, dat_tmp) 
  
}

cov_mat

write.csv(cov_mat,
          file = file.path(outDir, paste("Obs_cov_Mean_", 
                                         covariateOfInterest, 
                                         ".csv", sep = ""))
)

#########   Trying solo! Pull rainfall data, frame as matrix   ##################################


covariateOfInterest <- "Rain_within_24hrs"


rain <- dat[,c("Date", "Location", "Rain_within_24hrs")]
### 

rain_mat <- matrix(NA, 
                  ncol = ncol_tmp,
                  nrow = nrow_tmp)
rownames(rain_mat) <- unique(dat$Location)

for(j in 1:nrow(rain_mat)){
  
  # subset to that station j
  rain_tmp <- rain[rain$Location == rownames(rain_mat)[j],]
  dat_tmp <- dat[dat$Location == rownames(rain_mat)[j],]
  #det_mat[j,] <- c(rep(0, times = length(dat[dat$Location == rownames(det_mat)[j],]$Date)),
  #                 rep(NA, times = ncol(det_mat) - length(dat[dat$Location == rownames(det_mat)[j],]$Date)))
  
  
  rain_mat[j, match(unique(rain_tmp$Date), 
                   dat_tmp$Date)] <- rain_tmp[, covariateOfInterest]
  rm(rain_tmp, dat_tmp) 
  
}

rain_mat  # Sucess! I think?!

write.csv(rain_mat,
          file = file.path(outDir, paste("Obs_cov_", 
                                         covariateOfInterest, 
                                         ".csv", sep = ""))
)

######################################################################################
mean_SS <- aggregate(DetCov$Stream_speed.mpS., 
                     by = list(Date = DetCov$Date, 
                               Location = DetCov$Location),
                     FUN = mean, 
                     na.rm = TRUE)

