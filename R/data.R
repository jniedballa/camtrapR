#' Sample camera trap station information
#' 
#' Example camera trap station information table
#' 
#' This is a general example of how information about camera trap stations are
#' arranged in camtrapR. It contains setup and retrieval dates and coordinates.
#' If more than 1 camera was set up at a station (e.g. 2 cameras facing each
#' other), a camera ID column must be added, with camera-specific information
#' instead of station-specific information. If cameras malfunctioned
#' repeatedly, additional pairs of problem columns can be added, e.g.
#' "Problem2_from" and "Problem2_to" etc..
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{utm_y} \tab y coordinate of station (northing) \cr
#' \code{utm_x} \tab x coordinate of station (easting) \cr
#' \code{Setup_date} \tab camera trap setup date \cr
#' \code{Retrieval_date} \tab camera trap retrieval date \cr
#' \code{Problem1_from} \tab first day of camera malfunction \cr
#' \code{Problem1_to} \tab last day of camera malfunction \cr
#' }
#' 
#' @name camtraps
#' @docType data
#' @format A data frame with 3 rows and 7 variables
#' @note The coordinates can be in the units of any coordinate system. UTM was
#' chosen as an example, but it could be latlong or anything else, too.
#' \code{\link[secr]{capthist}} objects (as created by
#' \code{\link{spatialDetectionHistory}} for spatial capture-recapture
#' analyses) expect the unit to be meters.
#' @keywords datasets
NULL


#' Sample multi-season camera trap station information
#' 
#' Example multi-season camera trap station information table
#' 
#' This is a general example of how information about camera trap stations from
#' multiple seasons are arranged in camtrapR. It contains setup and retrieval
#' dates, coordinates and a season identifier. If more than 1 camera was set up
#' at a station (e.g. 2 cameras facing each other), a camera ID column must be
#' added, with camera-specific information instead of station-specific
#' information. If cameras malfunctioned repeatedly, additional pairs of
#' problem columns can be added, e.g. "Problem2_from" and "Problem2_to" etc..
#' 
#' Note that season 2010 has an additional station (StationD). This is to
#' simulate a situation where a station was not set up during an entire season.
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{utm_y} \tab y coordinate of station (northing) \cr
#' \code{utm_x} \tab x coordinate of station (easting) \cr
#' \code{Setup_date} \tab camera trap setup date \cr
#' \code{Retrieval_date} \tab camera trap retrieval date \cr
#' \code{Problem1_from} \tab first day of camera malfunction \cr
#' \code{Problem1_to} \tab last day of camera malfunction \cr
#' \code{session} \tab Identifier for trapping session / season \cr
#' }
#' 
#' @name camtrapsMultiSeason
#' @docType data
#' @format A data frame with 7 rows and 8 variables
#' @note The coordinates can be in the units of any coordinate system. UTM was
#' chosen as an example, but it could be latlong or anything else, too.
#' \code{\link[secr]{capthist}} objects (as created by
#' \code{\link{spatialDetectionHistory}} for spatial capture-recapture
#' analyses) expect the unit to be meters. \code{\link[secr]{capthist}} alse
#' require session information as integer numbers starting with 1.
#' 
#' "Season" and "session" are used synonymously here. \pkg{secr} nomenclature
#' is "session", in \pkg{unmarked} it is "season".
#' @keywords datasets
#' @examples
#' 
#' # data were created with the following code:
#' data(camtraps)
#' 	
#' camtraps_season2 <- camtraps
#' 
#' # change 2009 to 2010
#' camtraps_season2[, "Setup_date"]     <- gsub("2009", "2010", camtraps_season2[, "Setup_date"])
#' camtraps_season2[, "Retrieval_date"] <- gsub("2009", "2010", camtraps_season2[, "Retrieval_date"])
#' camtraps_season2[, "Problem1_from"]  <- gsub("2009", "2010", camtraps_season2[, "Problem1_from"])
#' camtraps_season2[, "Problem1_to"]    <- gsub("2009", "2010", camtraps_season2[, "Problem1_to"])
#' 
#' # add an extra station with different dates in session 2010
#' camtraps_season2 <- rbind(camtraps_season2, NA)
#' camtraps_season2$Station[4] <- "StationD"
#' camtraps_season2$utm_y[4]  <- 607050
#' camtraps_season2$utm_x[4]  <- 525000
#' camtraps_season2$Setup_date[4]      <- "04/04/2010"
#' camtraps_season2$Retrieval_date[4]  <- "17/06/2010"
#' camtraps_season2$Problem1_from[4]   <- "20/05/2010"
#' camtraps_season2$Problem1_to[4]     <- "30/05/2010"
#' 
#' # add season column
#' camtraps$session         <- 2009
#' camtraps_season2$session <- 2010
#' 
#' # combine the tables for 2 seasons
#' camtrapsMultiSeason <- rbind(camtraps, camtraps_season2)
#' 
#' 
NULL


#' Sample single-species record table with custom metadata from camera trap
#' images
#' 
#' Sample single-species record table with individual IDs from the tagged
#' sample images in the package. Generated with function
#' \code{\link{recordTableIndividual}}.
#' 
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{Species} \tab Species ID \cr
#' \code{Individual} \tab Individual ID \cr
#' \code{DateTimeOriginal} \tab Date and time as extracted from image \cr
#' \code{Date} \tab record date \cr
#' \code{Time} \tab record time of day \cr
#' \code{delta.time.secs} \tab time difference to first species record at a station (seconds) \cr
#' \code{delta.time.mins} \tab time difference to first species record at a station (minutes) \cr
#' \code{delta.time.hours} \tab time difference to first species record at a station (hours) \cr
#' \code{delta.time.days} \tab time difference to first species record at a station (days) \cr
#' \code{Directory} \tab Image directory \cr
#' \code{FileName} \tab image filename \cr
#' \code{HierarchicalSubject} \tab content of the HierarchicalSubject image metadata tag \cr
#' \code{Model} \tab camera model extracted from image metadata \cr
#' \code{Make} \tab camera make extracted from image metadata \cr
#' \code{metadata_Species} \tab content of custom image metadata tag "Species" (see HierarchicalSubject) \cr
#' \code{metadata_individual} \tab content of custom image metadata tag "individual" (see HierarchicalSubject) \cr
#' }
#' 
#' @name recordTableIndividualSample
#' @docType data
#' @format A data frame with 21 rows and 17 variables
#' @keywords datasets
NULL


#' Sample single-species multi-season record table with custom metadata from
#' camera trap images
#' 
#' Sample single-species multi-season record table with individual IDs from the
#' tagged sample images in the package. Generated with function
#' \code{\link{recordTableIndividual}}, then duplicated to simulate a second
#' year.
#' 
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{Species} \tab Species ID \cr
#' \code{Individual} \tab Individual ID \cr
#' \code{DateTimeOriginal} \tab Date and time as extracted from image \cr
#' \code{Date} \tab record date \cr
#' \code{Time} \tab record time of day \cr
#' \code{delta.time.secs} \tab time difference to first species record at a station (seconds) \cr
#' \code{delta.time.mins} \tab time difference to first species record at a station (minutes) \cr
#' \code{delta.time.hours} \tab time difference to first species record at a station (hours) \cr
#' \code{delta.time.days} \tab time difference to first species record at a station (days) \cr
#' \code{Directory} \tab Image directory \cr
#' \code{FileName} \tab image filename \cr
#' \code{HierarchicalSubject} \tab content of the HierarchicalSubject image metadata tag \cr
#' \code{Model} \tab camera model extracted from image metadata \cr
#' \code{Make} \tab camera make extracted from image metadata \cr
#' \code{metadata_Species} \tab content of custom image metadata tag "Species" (see HierarchicalSubject) \cr
#' \code{metadata_individual} \tab content of custom image metadata tag "individual" (see HierarchicalSubject) \cr
#' }
#' 
#' @name recordTableIndividualSampleMultiSeason
#' @docType data
#' @format A data frame with 31 rows and 17 variables
#' @keywords datasets
#' @examples
#' 
#' # example data were created as follows:
#' data(recordTableIndividualSample)
#' 
#' recordTableIndividualSample_season2 <- recordTableIndividualSample[1:10,]
#' recordTableIndividualSample_season2$DateTimeOriginal <- gsub("2009", "2010", 
#'       recordTableIndividualSample_season2$DateTimeOriginal)
#' recordTableIndividualSampleMultiSeason <- rbind(recordTableIndividualSample, 
#'       recordTableIndividualSample_season2)
#' 
NULL


#' Sample species record table from camera trap images
#' 
#' Sample species record table from camera trap images generated from the
#' sample images in the package with the function \code{\link{recordTable}} .
#' 
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{Species} \tab Species ID \cr
#' \code{DateTimeOriginal} \tab Date and time as extracted from image \cr
#' \code{Date} \tab record date \cr
#' \code{Time} \tab record time of day \cr
#' \code{delta.time.secs} \tab time difference to first species record at a station (seconds) \cr
#' \code{delta.time.mins} \tab time difference to first species record at a station (minutes) \cr
#' \code{delta.time.hours} \tab time difference to first species record at a station (hours) \cr
#' \code{delta.time.days} \tab time difference to first species record at a station (days) \cr
#' \code{Directory} \tab Image directory \cr
#' \code{FileName} \tab image filename \cr
#' \code{n_images} \tab Number of images \cr
#' }
#' 
#' @name recordTableSample
#' @docType data
#' @format A data frame with 39 rows and 12 variables
#' @keywords datasets
NULL


#' Sample multi-season species record table from camera trap images
#' 
#' Sample multi-season species record table from camera trap images generated
#' from the sample images in the package with the function
#' \code{\link{recordTable}}. Season 2009 is the same as
#' \code{\link{recordTableSample}}, season 2010 was simulated by adding 1 year
#' to these records.
#' 
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{Species} \tab Species ID \cr
#' \code{DateTimeOriginal} \tab Date and time as extracted from image \cr
#' \code{Date} \tab record date \cr
#' \code{Time} \tab record time of day \cr
#' \code{delta.time.secs} \tab time difference to first species record at a station (seconds) \cr
#' \code{delta.time.mins} \tab time difference to first species record at a station (minutes) \cr
#' \code{delta.time.hours} \tab time difference to first species record at a station (hours) \cr
#' \code{delta.time.days} \tab time difference to first species record at a station (days) \cr
#' \code{Directory} \tab Image directory \cr
#' \code{FileName} \tab image filename \cr
#' }
#' 
#' @name recordTableSampleMultiSeason
#' @docType data
#' @format A data frame with 78 rows and 11 variables
#' @keywords datasets
#' @examples
#' 
#' 	# data were created with the following code:
#' 
#' 	data(recordTableSample)
#' 	recordTableSample_season2 <- recordTableSample
#' 	
#' 	# substitute 2009 with 2010
#' 	recordTableSample_season2$DateTimeOriginal <- gsub("2009", "2010", 
#' 	     recordTableSample_season2$DateTimeOriginal) 
#'   # combine with season 2009
#' 	recordTableSampleMultiSeason <- rbind(recordTableSample, recordTableSample_season2)    
#' 
NULL


#' Sample camera trap time shift table
#' 
#' Sample camera trap time shift table
#' 
#' If image Exif metadata timestamps are wrong systematically (e.g. because
#' camera system time was not set after changing batteries), it can be
#' corrected using a \code{data.frame} in the following format using function
#' \code{\link{timeShiftImages}}. For details on data format, please see
#' \code{\link{timeShiftImages}}.
#' 
#' The variables are as follows:
#' 
#' \tabular{ll}{
#' \code{Station} \tab Camera trap station ID \cr
#' \code{camera} \tab Camera trap ID (optional) \cr
#' \code{timeshift} \tab time shift amount to be applied \cr
#' \code{sign} \tab direction of time shift \cr
#' }
#' 
#' @name timeShiftTable
#' @docType data
#' @format A data frame with 2 rows and 4 variables
#' @keywords datasets
NULL