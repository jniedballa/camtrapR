\docType{data}
\name{recordTableSample}
\alias{recordTableSample}
\title{Sample species record table from camera trap images}
\format{A data frame with 39 rows and 11 variables}
\usage{
data(recordTableSample)
}
\description{
Sample species record table from camera trap images generated from the sample images in the package with the function \code{\link{recordTable}} .
}
\details{


The variables are as follows:

\itemize{
 \item Station.   Camera trap station ID
 \item Species.   Species ID
 \item DateTimeOriginal.   Date and time as extracted from image
 \item Date.   record date
 \item Time.   record time of day
 \item delta.time.secs.   time difference to first species record at a station (seconds)
 \item delta.time.mins.   time difference to first species record at a station (minutes)
 \item delta.time.hours.   time difference to first species record at a station (hours)
 \item delta.time.days.   time difference to first species record at a station (days)
 \item Directory.   Image directory
 \item FileName.   image filename
 }
}
\keyword{datasets}

