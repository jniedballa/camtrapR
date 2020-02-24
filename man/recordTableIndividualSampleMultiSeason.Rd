\docType{data}
\name{recordTableIndividualSampleMultiSeason}
\alias{recordTableIndividualSampleMultiSeason}
\title{Sample single-species multi-season record table with custom metadata from camera trap images}
\format{A data frame with 31 rows and 17 variables}
\usage{
data(recordTableIndividualSampleMultiSeason)
}
\description{
Sample single-species multi-season record table with individual IDs from the tagged sample images in the package. Generated with function \code{\link{recordTableIndividual}}, then duplicated to simulate a second year.
}
\details{


The variables are as follows:

\itemize{
 \item Station.   Camera trap station ID
 \item Species.   Species ID
 \item Individual.   Individual ID
 \item DateTimeOriginal.   Date and time as extracted from image
 \item Date.   record date
 \item Time.   record time of day
 \item delta.time.secs.   time difference to first species record at a station (seconds)
 \item delta.time.mins.   time difference to first species record at a station (minutes)
 \item delta.time.hours.   time difference to first species record at a station (hours)
 \item delta.time.days.   time difference to first species record at a station (days)
 \item Directory.   Image directory
 \item FileName.   image filename
 \item HierarchicalSubject.   content of the HierarchicalSubject image metadata tag
 \item Model.   camera model extracted from image metadata
 \item Make.   camera make extracted from image metadata
 \item metadata_Species.   content of custom image metadata tag "Species" (see HierarchicalSubject)
 \item metadata_individual.   content of custom image metadata tag "individual" (see HierarchicalSubject)
 }
}

\examples{
# example data were created as follows:
data(recordTableIndividualSample)

recordTableIndividualSample_season2 <- recordTableIndividualSample[1:10,]
recordTableIndividualSample_season2$DateTimeOriginal <- gsub("2009", "2010", 
      recordTableIndividualSample_season2$DateTimeOriginal)
recordTableIndividualSampleMultiSeason <- rbind(recordTableIndividualSample, 
      recordTableIndividualSample_season2)
}
\keyword{datasets}

