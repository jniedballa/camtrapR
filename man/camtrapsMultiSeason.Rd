\docType{data}
\name{camtrapsMultiSeason}
\alias{camtrapsMultiSeason}
\title{Sample multi-season camera trap station information}
\format{A data frame with 7 rows and 8 variables}
\usage{
data(camtrapsMultiSeason)
}
\description{
Example multi-season camera trap station information table
}
\details{
This is a general example of how information about camera trap stations from multiple seasons are arranged in camtrapR. It contains setup and retrieval dates, coordinates and a season identifier. If more than 1 camera was set up at a station (e.g. 2 cameras facing each other), a camera ID column must be added, with camera-specific information instead of station-specific information.
If cameras malfunctioned repeatedly, additional pairs of problem columns can be added, e.g. "Problem2_from" and "Problem2_to" etc..

Note that season 2010 has an additional station (StationD). This is to simulate a situation where a station was not set up during an entire season.

The variables are as follows:

\tabular{ll}{
\code{Station} \tab Camera trap station ID \cr
\code{utm_y} \tab y coordinate of station (northing) \cr
\code{utm_x} \tab x coordinate of station (easting) \cr
\code{Setup_date} \tab camera trap setup date \cr
\code{Retrieval_date} \tab camera trap retrieval date \cr
\code{Problem1_from} \tab first day of camera malfunction \cr
\code{Problem1_to} \tab  last day of camera malfunction \cr
\code{session} \tab  Identified for trapping session / season \cr
}

}

\note{
The coordinates can be in the units of any coordinate system. UTM was chosen as an example, but it could be latlong or anything else, too. \code{\link[secr]{capthist}} objects (as created by \code{\link{spatialDetectionHistory}} for spatial capture-recapture analyses) expect the unit to be meters. \code{\link[secr]{capthist}} alse require session information as integer numbers starting with 1.

"Season" and "session" are used synonymously here. \pkg{secr} nomenclature is "session", in \pkg{unmarked} it is "season". 
}

\examples{
# data were created with the following code:
data(camtraps)

camtraps_season2 <- camtraps

# change 2009 to 2010
camtraps_season2[, "Setup_date"] <- gsub("2009", "2010", camtraps_season2[,
    "Setup_date"])
camtraps_season2[, "Retrieval_date"] <- gsub("2009", "2010", camtraps_season2[,
    "Retrieval_date"])
camtraps_season2[, "Problem1_from"] <- gsub("2009", "2010", camtraps_season2[,
    "Problem1_from"])
camtraps_season2[, "Problem1_to"] <- gsub("2009", "2010", camtraps_season2[,
    "Problem1_to"])

# add an extra station with different dates in session 2010
camtraps_season2 <- rbind(camtraps_season2, NA)
camtraps_season2$Station[4] <- "StationD"
camtraps_season2$utm_y[4] <- 607050
camtraps_season2$utm_x[4] <- 525000
camtraps_season2$Setup_date[4] <- "04/04/2010"
camtraps_season2$Retrieval_date[4] <- "17/06/2010"
camtraps_season2$Problem1_from[4] <- "20/05/2010"
camtraps_season2$Problem1_to[4] <- "30/05/2010"

# add season column
camtraps$session <- 2009
camtraps_season2$session <- 2010

# combine the tables for 2 seasons
camtrapsMultiSeason <- rbind(camtraps, camtraps_season2)
}
\keyword{datasets}
