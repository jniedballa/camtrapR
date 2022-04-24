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

\tabular{ll}{
\code{Station} \tab Camera trap station ID \cr
\code{Species} \tab Species ID \cr
\code{Individual} \tab Individual ID \cr
\code{DateTimeOriginal} \tab Date and time as extracted from image \cr
\code{Date} \tab record date \cr
\code{Time} \tab record time of day \cr
\code{delta.time.secs} \tab time difference to first species record at a station (seconds) \cr
\code{delta.time.mins} \tab time difference to first species record at a station (minutes) \cr
\code{delta.time.hours} \tab time difference to first species record at a station (hours) \cr
\code{delta.time.days} \tab time difference to first species record at a station (days) \cr
\code{Directory} \tab Image directory \cr
\code{FileName} \tab image filename \cr
\code{HierarchicalSubject} \tab content of the HierarchicalSubject image metadata tag \cr
\code{Model} \tab camera model extracted from image metadata \cr
\code{Make} \tab camera make extracted from image metadata \cr
\code{metadata_Species} \tab content of custom image metadata tag "Species" (see HierarchicalSubject) \cr
\code{metadata_individual} \tab content of custom image metadata tag "individual" (see HierarchicalSubject) \cr
}

}

\examples{
# example data were created as follows:
data(recordTableIndividualSample)

recordTableIndividualSample_season2 <- recordTableIndividualSample[1:10, ]
recordTableIndividualSample_season2$DateTimeOriginal <- gsub("2009", "2010",
    recordTableIndividualSample_season2$DateTimeOriginal)
recordTableIndividualSampleMultiSeason <- rbind(recordTableIndividualSample,
    recordTableIndividualSample_season2)
}
\keyword{datasets}
