\name{activityHistogram}
\alias{activityHistogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot histogram of single-species activity
}
\description{
The function generates a histogram of species diel activity in 1-hour intervals.
}
\usage{
activityHistogram(recordTable, 
	species, 
	allSpecies = FALSE, 
	speciesCol = "Species", 
	recordDateTimeCol = "DateTimeOriginal", 
	recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S", 
	plotR = TRUE, 
	writePNG = FALSE, 
	plotDirectory, 
	createDir = FALSE, 
	pngMaxPix = 1000,
	...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
	data.frame. the record table created by \code{\link{recordTable}}
}
  \item{species}{
Name of the single species for which to create a histogram of activity
}
  \item{allSpecies}{
logical. Create plots for all species in \code{speciesCol} of \code{recordTable}? Overrides argument \code{species}
}
  \item{speciesCol}{
	character. name of the column specifying species names in \code{recordTable}
}
  \item{recordDateTimeCol}{
	character. name of the column specifying date and time in \code{recordTable}
}
  \item{recordDateTimeFormat}{
	character. format of column \code{recordDateTimeCol} in \code{recordTable}
}
  \item{plotR}{
logical. Show plots in R graphics device?
}
 \item{writePNG}{
	logical. Create pngs of the plots?
}
  \item{plotDirectory}{
	character. Directory in which to create png plots if \code{writePNG = TRUE}
}
  \item{createDir}{
	logical. Create \code{plotDirectory}?
}
  \item{pngMaxPix}{
	integer. image size of png (pixels along x-axis)
}
  \item{\dots}{
	additional arguments to be passed to function \code{\link[graphics]{hist}}
}
}
\details{
Activity is calculated from the time of day of records. The date is ignored.
}
 \value{
  It returns invisibly a vector of species record date and time in \code{POSIXlt} format.
If \code{allSpecies == TRUE}, all species' vectors are returned in an invisible named list.
 }

\note{ 
If you have a sufficiently large number of records you may wish to consider using \code{\link{activityDensity}} instead. Please be aware that this function (like the other activity... function of this package) use clock time. If your survey was long enough to see changes in sunrise and sunset times, this may result in biased representations of species activity.
}

\author{
Juergen Niedballa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{activityDensity}}, \code{\link{activityRadial}}, \code{\link{activityOverlap}}
}
\examples{

# load record table
data(recordTableSample)

# generate activity histogram
species4activity <- "VTA"    # = Viverra tangalunga, Malay Civet

activityHistogram (recordTable = recordTableSample,
                   species     = species4activity,
                   allSpecies = FALSE)
}
