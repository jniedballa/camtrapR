\name{activityRadial}
\alias{activityRadial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Radial plots of single-species activity
}
\description{
The function generates a radial plot of species diel activity using an adapted version of function \code{\link[plotrix]{radial.plot}} from package \pkg{plotrix} (without the need to install the package). Records are aggregated by hour. The number of independent events is used as input, which in turn is based on the argument \code{minDeltaTime} in \code{\link{recordTable}}.
}
\usage{
  activityRadial(recordTable, 
  species,
  allSpecies = FALSE,
  speciesCol = "Species",
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  byNumber = FALSE,
  plotR = TRUE, 
  writePNG = FALSE, 
  plotDirectory, 
  createDir = FALSE, 
  pngMaxPix = 1000,
  ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordTable}}
}
  \item{species}{
  Name of the species for which to create an kernel density plot of activity
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
  \item{byNumber}{
  logical. If FALSE, plot proportion of records. If TRUE, plot number of records
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
  additional arguments to be passed to function \code{\link[plotrix]{radial.plot}}
}
}
\details{

\code{radial.plot} was adjusted to show a clockwise 24-hour clock face. It is recommended to set argument \code{lwd} to a value >= 2. You may also wish to add argument \code{rp.type="p"} to show a polygon instead of bars.
}
%\note{
%These plots can be considered radial histograms.
%}
\value{
Returns invisibly a data.frame containing all information needed to create the plot: radial position, lengths, hour (for labels).
If \code{allSpecies == TRUE}, all species' data frames are returned in an invisible named list.
}
\references{
Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12. \cr
\url{https://CRAN.R-project.org/package=plotrix }
}

\author{
Juergen Niedballa
}

\seealso{
\code{\link{activityDensity}}, \code{\link{activityHistogram}}, \code{\link{activityOverlap}}
}
\examples{

# load record table
data(recordTableSample)

species4activity <- "PBE"    # = Prionailurus bengalensis, Leopard Cat

activityRadial(recordTable       = recordTableSample,
               species           = species4activity,
               allSpecies        = FALSE,
               speciesCol        = "Species",
               recordDateTimeCol = "DateTimeOriginal",
               plotR             = TRUE,
               writePNG          = FALSE,
               lwd               = 5
)

# plot type = polygon

activityRadial(recordTable       = recordTableSample,
               species           = species4activity,
               allSpecies        = FALSE,
               speciesCol        = "Species",
               recordDateTimeCol = "DateTimeOriginal",
               plotR             = TRUE,
               writePNG          = FALSE,
               lwd               = 5,
               rp.type           = "p"      
)

}
