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

\tabular{ll}{
\code{Station} \tab Camera trap station ID \cr
\code{Species} \tab Species ID \cr
\code{DateTimeOriginal} \tab Date and time as extracted from image \cr
\code{Date} \tab record date \cr
\code{Time} \tab record time of day \cr
\code{delta.time.secs} \tab time difference to first species record at a station (seconds) \cr
\code{delta.time.mins} \tab time difference to first species record at a station (minutes) \cr
\code{delta.time.hours} \tab time difference to first species record at a station (hours) \cr
\code{delta.time.days} \tab time difference to first species record at a station (days) \cr
\code{Directory} \tab Image directory \cr
\code{FileName} \tab image filename \cr
}
 
 
}
\keyword{datasets}
