\name{activityOverlap}
\alias{activityOverlap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot overlapping kernel densities of two-species activities
}
\description{
This function plots kernel density estimates of two species' diel activity data by calling the function \code{\link[overlap]{overlapPlot}} from package \pkg{overlap}. It further computes the overlap coefficient Dhat1 by calling \code{\link[overlap]{overlapEst}}.
}
\usage{
  activityOverlap(recordTable, 
  speciesA,
  speciesB,
  speciesCol = "Species",
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  plotR = TRUE, 
  writePNG = FALSE, 
  addLegend = TRUE,
  legendPosition = "topleft",
  plotDirectory, 
  createDir = FALSE, 
  pngMaxPix = 1000,
  add.rug = TRUE,
  ...
)
}

\arguments{
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordTable}}
}
  \item{speciesA}{
  Name of species 1
}
  \item{speciesB}{
  Name of species 2
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
 \item{addLegend}{
  logical. Add a legend to the plots?
}
 \item{legendPosition}{
  character. Position of the legend (keyword)
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
  \item{add.rug}{
  logical. add a rug to the plot?
}
  \item{\dots}{
  additional arguments to be passed to function \code{\link[overlap]{overlapPlot}}
}
}
\details{
\code{...} can be graphical parameters passed on to function \code{\link[overlap]{overlapPlot}}, e.g. \code{linetype}, \code{linewidth}, \code{linecol}.
}
\value{
Returns invisibly the \code{data.frame} with plot coordinates returned by \code{\link[overlap]{overlapPlot}}.
}
\references{
Mike Meredith and Martin Ridout (2014). overlap: Estimates of coefficient of overlapping for animal activity patterns. R package version 0.2.4. \url{https://CRAN.R-project.org/package=overlap} \cr
Ridout, M.S. and Linkie, M. (2009) Estimating overlap of daily activity patterns from camera trap data. Journal of Agricultural, Biological and Environmental Statistics, 14, 322-337.
}
\author{
Juergen Niedballa
}
\note{ 
Please be aware that the function (like the other activity... function of this package) use clock time, not solar time. If your survey was long enough to see changes in sunrise and sunset times, this may result in biased representations of species activity. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{activityDensity}} \cr
\url{http://www.kent.ac.uk/smsas/personal/msr/overlap.html}
}
\examples{
# load record table
data(recordTableSample)

# define species of interest
speciesA_for_activity <- "VTA"    # = Viverra tangalunga, Malay Civet
speciesB_for_activity <- "PBE"    # = Prionailurus bengalensis, Leopard Cat

# create activity overlap plot
activityOverlap (recordTable = recordTableSample,
                 speciesA    = speciesA_for_activity,
                 speciesB    = speciesB_for_activity,
                 writePNG    = FALSE,
                 plotR       = TRUE,
                 createDir   = FALSE,
                 pngMaxPix   = 1000,
                 linecol     = c("red", "blue"),
                 linewidth   = c(3,3),
                 add.rug     = TRUE
)


}

