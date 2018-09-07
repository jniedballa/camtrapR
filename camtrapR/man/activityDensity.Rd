\name{activityDensity}
\alias{activityDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot kernel density estimation of single-species activity
}
\description{
The function plots a kernel density estimation of species diel activity using function \code{\link[overlap]{densityPlot}} from package \pkg{overlap}.
}
\usage{
activityDensity(recordTable, 
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
  add.rug = TRUE,
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
  logical. Create \code{plotDirectory} if \code{writePNG = TRUE}?
}
  \item{pngMaxPix}{
  integer. image size of png (pixels along x-axis)
}
  \item{add.rug}{
  logical. add a rug to the plot?
}
  \item{\dots}{
  additional arguments to be passed to function \code{densityPlot}
}
}
\details{
  \code{species} must be in the \code{speciesCol} of \code{recordTable}.
}
\value{
  Returns invisibly a vector of species record observation times in radians, i.e. scaled to  [\eqn{0, 2\pi}].
If \code{allSpecies == TRUE}, all species' vectors are returned in an invisible named list.
}
\references{
Martin Ridout and Matthew Linkie (2009). Estimating overlap of daily activity patterns from camera trap data. Journal of Agricultural, Biological and Environmental Statistics, 14(3), 322-337 \cr
Mike Meredith and Martin Ridout (2014). overlap: Estimates of coefficient of overlapping for animal activity patterns. R package version 0.2.4. \url{https://CRAN.R-project.org/package=overlap}

}
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{activityHistogram}}, \code{\link{activityRadial}}, \code{\link{activityOverlap}}
\url{http://www.kent.ac.uk/smsas/personal/msr/overlap.html}
}
\examples{
# load record table
data(recordTableSample)

species4activity <- "VTA"    # = Viverra tangalunga, Malay Civet

activityDensity(recordTable = recordTableSample,
                species     = species4activity)


# all species at once

activityDensity(recordTable = recordTableSample,
                allSpecies  = TRUE,
                writePNG    = FALSE,
                plotR       = TRUE,
                add.rug     = TRUE)
}
