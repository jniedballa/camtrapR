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