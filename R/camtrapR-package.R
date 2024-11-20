#' Overview of the functions in the camtrapR package
#' 
#' This package provides a streamlined workflow for processing data generated
#' in camera trap-based wildlife studies and prepares input for further
#' analyses, particularly in occupancy and spatial capture-recapture
#' frameworks. It suggests a simple data structure and provides functions for
#' managing digital camera trap photographs (and videos), generating record
#' tables, maps of species richness and species detections and species activity
#' diagrams. It further helps prepare subsequent analyses by creating
#' detection/non-detection matrices for occupancy analyses, e.g. in the
#' \pkg{unmarked} or \pkg{ubms} packages, and \code{capthist} objects for
#' spatial capture-recapture analyses in the \pkg{secr} package. In addition,
#' basic survey statistics are computed. The functions build on one another in
#' a logical sequence. The only manual input needed is species (and individual)
#' identification, which is achieved by moving images into species directories
#' or by tagging images in image management software. Besides, a table holding
#' basic information about camera trap station IDs, locations and trapping
#' periods must be created in spreadsheet software.
#' 
#' Image metadata (such as date and time or user-assigned tags) are extracted
#' from the images using Phil Harvey's ExifTool (available from
#' \url{https://exiftool.org/}) and the information is stored in a record
#' table. An adjustable criterion for temporal independence of records can be
#' applied. Maps of species presence and species richness can be generated.
#' Several functions are available for plotting single- and two-species
#' activity patterns. Information about the camera-specific trapping periods
#' (and periods of malfunction) are summarized into a matrix about camera trap
#' operability. These, together with the record table, are used to generate
#' species detection histories for occupancy and spatial capture-recapture
#' analyses. The user has considerable freedom in generating the detection
#' histories; sampling occasion length, beginning date and and occasion start
#' times are adjustable. In addition, trapping effort (i.e. active trap nights
#' per station and occasion) can be computed for use as a covariate / offset on
#' detection probability.
#' 
#' @name camtrapR-package
#' @aliases camtrapR-package camtrapR
#' @docType package
#' 
#' @section User support: The camtrapR Google group is an online support and
#' help forum for camtrapR users. You can find it here:
#' \url{https://groups.google.com/forum/#!forum/camtrapr}.
#' 
#' @author Juergen Niedballa
#' 
#' Maintainer:Juergen Niedballa <camtrapr@@gmail.com>
#' 
#' @seealso \pkg{overlap} \pkg{unmarked} \pkg{ubms} \pkg{secr} \pkg{wiqid}
#' 
#' @references
#' 
#' Niedballa, J., Sollmann, R., Courtiol, A., Wilting, A. (2016): camtrapR: an
#' R package for efficient camera trap data management. Methods in Ecology and
#' Evolution, 7(12).
#' \url{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12600}
#' \cr
#' 
#' camtrapR Google Group \url{https://groups.google.com/forum/#!forum/camtrapr}
#' \cr
#' 
#' Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News,
#' 6(4): 8-12. \cr
#' 
#' Mike Meredith and Martin Ridout (2018). overlap: Estimates of coefficient of
#' overlapping for animal activity patterns. R package version 0.3.2.
#' \url{https://CRAN.R-project.org/package=overlap} \cr
#' 
#' Phil Harvey's ExifTool \url{https://exiftool.org/} \cr
#' @keywords internal
"_PACKAGE"





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
#' \itemize{ \item Station.  Camera trap station ID \item utm_y.  y coordinate
#' of station (northing) \item utm_x.  x coordinate of station (easting) \item
#' Setup_date.  camera trap setup date \item Retrieval_date.  camera trap
#' retrieval date \item Problem1_from.  first day of camera malfunction \item
#' Problem1_to.  last day of camera malfunction }
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
#' \itemize{ \item Station.  Camera trap station ID \item utm_y.  y coordinate
#' of station (northing) \item utm_x.  x coordinate of station (easting) \item
#' Setup_date.  camera trap setup date \item Retrieval_date.  camera trap
#' retrieval date \item Problem1_from.  first day of camera malfunction \item
#' Problem1_to.  last day of camera malfunction \item session.  Identified for
#' trapping session / season }
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
#' \itemize{ \item Station.  Camera trap station ID \item Species.  Species ID
#' \item Individual.  Individual ID \item DateTimeOriginal.  Date and time as
#' extracted from image \item Date.  record date \item Time.  record time of
#' day \item delta.time.secs.  time difference to first species record at a
#' station (seconds) \item delta.time.mins.  time difference to first species
#' record at a station (minutes) \item delta.time.hours.  time difference to
#' first species record at a station (hours) \item delta.time.days.  time
#' difference to first species record at a station (days) \item Directory.
#' Image directory \item FileName.  image filename \item HierarchicalSubject.
#' content of the HierarchicalSubject image metadata tag \item Model.  camera
#' model extracted from image metadata \item Make.  camera make extracted from
#' image metadata \item metadata_Species.  content of custom image metadata tag
#' "Species" (see HierarchicalSubject) \item metadata_individual.  content of
#' custom image metadata tag "individual" (see HierarchicalSubject) }
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
#' \itemize{ \item Station.  Camera trap station ID \item Species.  Species ID
#' \item Individual.  Individual ID \item DateTimeOriginal.  Date and time as
#' extracted from image \item Date.  record date \item Time.  record time of
#' day \item delta.time.secs.  time difference to first species record at a
#' station (seconds) \item delta.time.mins.  time difference to first species
#' record at a station (minutes) \item delta.time.hours.  time difference to
#' first species record at a station (hours) \item delta.time.days.  time
#' difference to first species record at a station (days) \item Directory.
#' Image directory \item FileName.  image filename \item HierarchicalSubject.
#' content of the HierarchicalSubject image metadata tag \item Model.  camera
#' model extracted from image metadata \item Make.  camera make extracted from
#' image metadata \item metadata_Species.  content of custom image metadata tag
#' "Species" (see HierarchicalSubject) \item metadata_individual.  content of
#' custom image metadata tag "individual" (see HierarchicalSubject) }
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
#' \itemize{ \item Station.  Camera trap station ID \item Species.  Species ID
#' \item DateTimeOriginal.  Date and time as extracted from image \item Date.
#' record date \item Time.  record time of day \item delta.time.secs.  time
#' difference to first species record at a station (seconds) \item
#' delta.time.mins.  time difference to first species record at a station
#' (minutes) \item delta.time.hours.  time difference to first species record
#' at a station (hours) \item delta.time.days.  time difference to first
#' species record at a station (days) \item Directory.  Image directory \item
#' FileName.  image filename }
#' 
#' @name recordTableSample
#' @docType data
#' @format A data frame with 39 rows and 11 variables
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
#' \itemize{ \item Station.  Camera trap station ID \item Species.  Species ID
#' \item DateTimeOriginal.  Date and time as extracted from image \item Date.
#' record date \item Time.  record time of day \item delta.time.secs.  time
#' difference to first species record at a station (seconds) \item
#' delta.time.mins.  time difference to first species record at a station
#' (minutes) \item delta.time.hours.  time difference to first species record
#' at a station (hours) \item delta.time.days.  time difference to first
#' species record at a station (days) \item Directory.  Image directory \item
#' FileName.  image filename }
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
#' \itemize{ \item Station.  Camera trap station ID \item camera.  Camera trap
#' ID (optional) \item timeshift.  time shift amount to be applied \item sign.
#' direction of time shift
#' 
#' }
#' 
#' @name timeShiftTable
#' @docType data
#' @format A data frame with 2 rows and 4 variables
#' @keywords datasets
NULL



