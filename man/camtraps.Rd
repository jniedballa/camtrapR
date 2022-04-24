\docType{data}
\name{camtraps}
\alias{camtraps}
\title{Sample camera trap station information}
\format{A data frame with 3 rows and 7 variables}
\usage{
data(camtraps)
}
\description{
Example camera trap station information table
}
\details{
This is a general example of how information about camera trap stations are arranged in camtrapR. It contains setup and retrieval dates and coordinates. If more than 1 camera was set up at a station (e.g. 2 cameras facing each other), a camera ID column must be added, with camera-specific information instead of station-specific information.
If cameras malfunctioned repeatedly, additional pairs of problem columns can be added, e.g. "Problem2_from" and "Problem2_to" etc..

The variables are as follows:

 
\tabular{ll}{
\code{Station} \tab Camera trap station ID \cr
\code{utm_y} \tab y coordinate of station (northing) \cr
\code{utm_x} \tab x coordinate of station (easting) \cr
\code{Setup_date} \tab camera trap setup date \cr
\code{Retrieval_date} \tab camera trap retrieval date \cr
\code{Problem1_from} \tab first day of camera malfunction \cr
\code{Problem1_to} \tab  last day of camera malfunction \cr
}
 
}

\note{
The coordinates can be in the units of any coordinate system. UTM was chosen as an example, but it could be latlong or anything else, too. \code{\link[secr]{capthist}} objects (as created by \code{\link{spatialDetectionHistory}} for spatial capture-recapture analyses) expect the unit to be meters.
}
\keyword{datasets}
