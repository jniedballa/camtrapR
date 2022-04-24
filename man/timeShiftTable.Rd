\docType{data}
\name{timeShiftTable}
\alias{timeShiftTable}
\title{Sample camera trap time shift table}
\format{A data frame with 2 rows and 4 variables}
\usage{
data(timeShiftTable)
}
\description{
Sample camera trap time shift table
}
\details{
If image Exif metadata timestamps are wrong systematically (e.g. because camera system time was not set after changing batteries), it can be corrected using a \code{data.frame} in the following format using function \code{\link{timeShiftImages}}. For details on data format, please see \code{\link{timeShiftImages}}.

The variables are as follows:

\tabular{ll}{
\code{Station} \tab Camera trap station ID \cr
\code{camera} \tab Camera trap ID (optional) \cr
\code{timeshift} \tab time shift amount to be applied \cr
\code{sign} \tab direction of time shift \cr
}
}
\keyword{datasets}
