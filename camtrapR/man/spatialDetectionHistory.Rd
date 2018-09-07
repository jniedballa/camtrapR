\name{spatialDetectionHistory}
\alias{spatialDetectionHistory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a \code{capthist} object for spatial capture-recapture analyses from camera-trapping data
}
\description{
This function generates spatial detection histories of individuals of a species for spatial capture-recapture analyses with package \code{\link[secr]{secr}}. Data are stored in a \code{\link[secr]{capthist}} object. The \code{\link[secr]{capthist}} object contains detection histories, camera-trap station location and possibly individual and station-level covariates. Detection histories can have adjustable occasion length and occasion start time (as in the function \code{\link{detectionHistory}}).
}
\usage{
spatialDetectionHistory(recordTableIndividual,
  species,
  camOp,
  CTtable,
  output,
  stationCol = "Station",
  speciesCol = "Species",
  sessionCol,
  Xcol,
  Ycol,
  stationCovariateCols,
  individualCol,
  individualCovariateCols,
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  occasionLength,
  minActiveDaysPerOccasion,
  occasionStartTime = 0,
  maxNumberDays,
  day1,
  buffer,
  includeEffort = TRUE,
  scaleEffort = FALSE,
  binaryEffort,
  timeZone,
  makeRMarkInput
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTableIndividual}{
  data.frame. the record table with individual IDs created by \code{\link{recordTableIndividual}}
}
  \item{species}{
  character. the species for which to compute the detection history
}
  \item{camOp}{
  The camera operability matrix as created by \code{\link{cameraOperation}}
}
  \item{CTtable}{
  data.frame. contains station IDs and coordinates. Same as used in \code{\link{cameraOperation}}.
}
  \item{output}{
  character. Return individual counts ("count") or binary observations ("binary")?
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{recordTableIndividual} and \code{CTtable}
}
  \item{speciesCol}{
  character. name of the column specifying species in \code{recordTableIndividual}
}
  \item{sessionCol}{
  character. name of the column specifying session IDs, either in \code{recordTableIndividual} or in \code{CTtable}. See 'Details' for more information. Session ID values must be a sequence of integer numbers beginning with 1 (i.e., 1,2,3,...).
}
  \item{Xcol}{
  character. name of the column specifying x coordinates in \code{CTtable}
}
  \item{Ycol}{
  character. name of the column specifying y coordinates in \code{CTtable}
}
  \item{stationCovariateCols}{
  character. name of the column(s) specifying station-level covariates in \code{CTtable}
}
  \item{individualCol}{
  character. name of the column specifying individual IDs in \code{recordTableIndividual}
}
  \item{individualCovariateCols}{
  character. name of the column(s) specifying individual covariates in \code{recordTableIndividual}
}
 \item{recordDateTimeCol}{
  character. name of the column specifying date and time in \code{recordTableIndividual}
}
  \item{recordDateTimeFormat}{
  format of column \code{recordDateTimeCol} in \code{recordTableIndividual}
}
  \item{occasionLength}{
  integer. occasion length in days
}
  \item{minActiveDaysPerOccasion}{
  integer. minimum number of active trap days for occasions to be included (optional)
}
  \item{occasionStartTime}{
  integer. time of day (the full hour) at which to begin occasions.
}
  \item{maxNumberDays}{
  integer. maximum number of trap days per station (optional)
}
  \item{day1}{
  character. When should occasions begin: station setup date ("station"), first day of survey ("survey"), a specific date (e.g. "2015-12-31")?
}
 \item{buffer}{
  integer. Makes the first occasion begin a number of days after station setup. (optional)
}
  \item{includeEffort}{
  logical. Include trapping effort (number of active camera trap days per station and occasion) as usage in \code{\link[secr]{capthist}}  object?
}
  \item{scaleEffort}{
  logical. scale and center effort matrix to mean = 0 and sd = 1? Currently not used. Must be FALSE.
}
  \item{binaryEffort}{
  logical. Should effort be binary (1 if >1 active day per occasion, 0 otherwise)?
}
  \item{timeZone}{
  character. must be an argument of \code{\link[base]{OlsonNames}}
}
 \item{makeRMarkInput}{
  logical. If \code{FALSE}, output will be a data frame for RMark. If \code{FALSE} or not specified, a secr \code{\link[secr]{capthist}} object
}

}
\details{
The function creates a \code{\link[secr]{capthist}} object by combining three different objects: 1) a record table of identified individuals of a species, 2) a camera trap station table with station coordinates and 3) a camera operation matrix computed with \code{\link{cameraOperation}}. The record table must contain a column with individual IDs and optionally individual covariates. The camera trap station table must contain station coordinates and optionally station-level covariates. The camera operation matrix provides the dates stations were active or not and the number of active stations.

\code{day1} defines if each stations detection history will begin on that station's setup day (\code{day1 = "station"}) or if all station's detection histories have a common origin (the day the first station was set up if \code{day1 = "survey"} or a fixed date if, e.g. \code{day1 = "2015-12-31"}).

\code{includeEffort} controls whether an effort matrix is computed or not. If TRUE, effort will be used for object \code{\link[secr]{usage}} information in a \code{\link[secr]{traps}}. \code{binaryEffort} makes the effort information binary. \code{scaleEffort} is currently not used and must be set to FALSE. The reason is that \code{\link[secr]{usage}} can only be either binary, or nonnegative real values, whereas scaling effort would return negative values.

The number of days that are aggregated is controlled by \code{occasionLength}. \code{occasionStartTime} can be used to make occasions begin another hour than midnight (the default). This may be relevant for nocturnal animals, in which 1 whole night would be considered an occasion.
Output can be returned as individual counts per occasion (\code{output = "count"}) or as binary observation (\code{output = "binary"}).

Argument \code{sessionCol} can be used to a create multi-session \code{\link[secr]{capthist}} object. There are two different ways in which the argument is interpreted. It depends on whether a column named \code{sessionCol} exists in \code{recordTableIndividual} or in \code{CTtable}. If \code{sessionCol} is found in \code{recordTableIndividual}, the records will be assigned to the specified sessions, and it will be assumed that all camera trap station were used in all sessions. Alternatively, if \code{sessionCol} is found in\code{CTtable}, it will be assumed that only a subset of stations was used in each session, and the records will be assigned automatically (using the station IDs to identify which session they belong into). In both cases, session information must be provided as a column in \code{recordTableIndividual} as a sequence of integer numbers beginnign with 1, i.e., you provide the session number directly in \code{sessionCol}. See \code{\link[secr]{session}} for more information about sessions in \pkg{secr}.

\code{\link[secr]{capthist}} objects (as created by \code{\link{spatialDetectionHistory}} for spatial capture-recapture analyses) expect the units of coordinates (\code{Xcol} and \code{col} in \code{CTtable}) to be meters. Therefore, please use a suitable coordinate system (e.g. UTM).
}

\section{Warning }{
Please note the section about defining argument \code{timeZone} in the vignette on data extraction (accessible via \code{vignette("DataExtraction")} or online (\url{https://cran.r-project.org/package=camtrapR/vignettes/DataExtraction.html})).
}



\value{
Output depends on argument \code{makeRMarkInput}:

\item{\code{makeRMarkInput = FALSE}}{A \code{\link[secr]{capthist}} object}
\item{\code{makeRMarkInput = TRUE}}{A data frame for use in RMark}

}
% \references{
% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

 \seealso{
\pkg{secr}
\pkg{RMark}
 }
\examples{

data(recordTableIndividualSample)
data(camtraps)

# create camera operation matrix (with problems/malfunction)
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "\%d/\%m/\%Y"
)

sdh <- spatialDetectionHistory(recordTableIndividual = recordTableIndividualSample,
                               species               = "LeopardCat",
                               camOp                 = camop_problem,
                               CTtable               = camtraps,
                               output                = "binary",
                               stationCol            = "Station",
                               speciesCol            = "Species",
                               Xcol                  = "utm_x",
                               Ycol                  = "utm_y",
                               individualCol         = "Individual",
                               recordDateTimeCol     = "DateTimeOriginal",
                               recordDateTimeFormat  = "\%Y-\%m-\%d \%H:\%M:\%S",
                               occasionLength        = 10,
                               day1                  = "survey",
                               includeEffort         = TRUE,
                               timeZone              = "Asia/Kuala_Lumpur"
  )

# missing space in species = "LeopardCat" was introduced by recordTableIndividual
# (because of CRAN package policies.
# In your data you can have spaces in your directory names)

  summary(sdh)
  plot(sdh, tracks = TRUE)
}