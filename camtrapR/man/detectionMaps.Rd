\name{detectionMaps}
\alias{detectionMaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate maps of observed species richness and species presences by station
}
\description{
Generates maps of observed species richness and species presence by species and station. Output can be R graphics, PNG graphics or a shapefile for use in GIS software.
}
\usage{
detectionMaps(CTtable,
  recordTable,
  Xcol,
  Ycol,
  backgroundPolygon,
  stationCol = "Station",
  speciesCol = "Species",
  speciesToShow,
  richnessPlot = TRUE,
  speciesPlots = TRUE,
  addLegend = TRUE,
  printLabels = FALSE,
  smallPoints,
  plotR = TRUE,
  writePNG = FALSE,
  plotDirectory,
  createPlotDir = FALSE,
  pngMaxPix = 1000,
  writeShapefile = FALSE,
  shapefileName,
  shapefileDirectory,
  shapefileProjection
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CTtable}{
  data.frame. contains station IDs and coordinates
}
  \item{Xcol}{
  character. name of the column specifying x coordinates in \code{CTtable}
}
  \item{Ycol}{
  character. name of the column specifying y coordinates in \code{CTtable}
}
  \item{backgroundPolygon}{
  SpatialPolygons or SpatialPolygonsDataFrame. Polygon to be plotted in the background of the map (e.g. project area boundary)
}
  \item{stationCol}{
  character. name of the column specifying station ID in \code{CTtable} and \code{recordTable}
}
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordTable}}
}
  \item{speciesCol}{
  character. name of the column specifying species in \code{recordTable}
}
\item{speciesToShow}{
  character. Species to include in the maps. If missing, all species in \code{recordTable} will be included.
}
  \item{writePNG}{
  logical. Create PNGs of the plots?
}
  \item{plotR}{
  logical. Create plots in R graphics device?
}
  \item{plotDirectory}{
  character. Directory in which to save the PNGs
}
  \item{createPlotDir}{
  logical. Create \code{plotDirectory}?
}
  \item{richnessPlot}{
  logical. Generate a species richness plot?
}
  \item{speciesPlots}{
  logical. Generate plots of all species number of independent events?
}
  \item{printLabels}{
  logical. Add station labels to the plots?
}
  \item{smallPoints}{
  numeric. Number by which to decrease point sizes in plots (optional).
}
  \item{addLegend}{
  logical. Add legends to the plots?
}
  \item{pngMaxPix}{
  integer. number of pixels in pngs on the longer side
}
  \item{writeShapefile}{
  logical. Create a shapefile from the output?
}
  \item{shapefileName}{
  character. Name of the shapefile to be saved. If empty, a name will be generated automatically.
}
  \item{shapefileDirectory}{
  character. Directory in which to save the shapefile.
}
  \item{shapefileProjection}{
  character. A character string of projection arguments to use in the shapefile.
}
}
 \details{
The column name \code{stationCol} must be identical in \code{CTtable} and \code{recordTable} and station IDs must match.

Shapefile creation depends on the packages \pkg{sp} and \pkg{rgdal}. \code{shapefileProjection} must be a valid argument of \code{\link[sp]{CRS}}. If \code{shapefileProjection} is undefined, the resulting shapefile will lack a coordinate reference system.
 }
\value{
An invisible \code{data.frame} with station coordinates, numbers of events by species at each station and total species number by station. In addition and optionally, R graphics or png image files.
}

\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{

# load station information
data(camtraps)

# load record table
data(recordTableSample)


# create maps
Mapstest <- detectionMaps(CTtable           = camtraps,
                          recordTable       = recordTableSample,
                          Xcol              = "utm_x",
                          Ycol              = "utm_y",
                          stationCol        = "Station",
                          speciesCol        = "Species",
                          writePNG          = FALSE,
                          plotR             = TRUE,
                          printLabels       = TRUE,
                          richnessPlot      = TRUE,
                          addLegend         = TRUE
)


\dontrun{

# with a polygon in the background

# make a dummy polygon for the background
poly1 <- Polygon(cbind(c(523500,524500,525000),c(606500, 605000, 606000)))
poly2 <- Polygons(list(poly1), "s1")
poly3 <- SpatialPolygons(list(poly2))

Mapstest2 <- detectionMaps(CTtable           = camtraps,
                           recordTable       = recordTableSample,
                           Xcol              = "utm_x",
                           Ycol              = "utm_y",
                           backgroundPolygon = poly3,         # this was added
                           stationCol        = "Station",
                           speciesCol        = "Species",
                           writePNG          = FALSE,
                           plotR             = TRUE,
                           printLabels       = TRUE,
                           richnessPlot      = TRUE,
                           addLegend         = TRUE
)

}
}

\references{
A great resource for \code{\link[sp]{CRS}} arguments is \url{http://spatialreference.org/}. Use the Proj4 string as \code{shapefileProjection} argument.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
