\docType{data}
\name{recordTableSampleMultiSeason}
\alias{recordTableSampleMultiSeason}
\title{Sample multi-season species record table from camera trap images}
\format{A data frame with 78 rows and 11 variables}
\usage{
data(recordTableSampleMultiSeason)
}
\description{
Sample multi-season species record table from camera trap images generated from the sample images in the package with the function \code{\link{recordTable}}. Season 2009 is the same as \code{\link{recordTableSample}}, season 2010 was simulated by adding 1 year to these records.
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

\examples{
# data were created with the following code:

data(recordTableSample)
recordTableSample_season2 <- recordTableSample

# substitute 2009 with 2010
recordTableSample_season2$DateTimeOriginal <- gsub("2009", "2010",
    recordTableSample_season2$DateTimeOriginal)
# combine with season 2009
recordTableSampleMultiSeason <- rbind(recordTableSample, recordTableSample_season2)
}
