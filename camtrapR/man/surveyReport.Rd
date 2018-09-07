\name{surveyReport}
\alias{surveyReport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a report about a camera trapping survey and species detections
}
\description{
This function creates a report about a camera trapping survey and species records. It uses a camera trap station information table and a record table (generated with \code{\link{recordTable}}) as input. Output tables can be saved and a zip file for simple data sharing can be created easily.
}
\usage{
surveyReport (recordTable,
  CTtable,
  speciesCol = "Species",
  stationCol = "Station",
  cameraCol, 
  setupCol,
  retrievalCol,
  CTDateFormat = "\%Y-\%m-\%d", 
  CTHasProblems = FALSE,
  recordDateTimeCol = "DateTimeOriginal",
  recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
  Xcol,
  Ycol,
  sinkpath,
  makezip
)
}

\arguments{
  \item{recordTable}{
   data.frame containing a species record table as given by \code{\link{recordTable}}
}
  \item{CTtable}{
   data.frame containing information about location and trapping period of camera trap stations (equivalent to \code{\link{camtraps}}
}
  \item{speciesCol}{
  character. name of the column specifying Species ID in \code{recordTable}
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{CTtable} and \code{recordTable}
}
  \item{cameraCol}{
  character. name of the column specifying Camera ID in \code{CTtable} and \code{recordTable}
}
  \item{setupCol}{
  character. name of the column containing camera setup dates in \code{CTtable}
}
  \item{retrievalCol}{
  character. name of the column containing camera retrieval dates in \code{CTtable}
}
  \item{CTDateFormat}{
  character. The date format in columns \code{setupCol} and \code{retrievalCol} of \code{CTtable}. 
}
\item{CTHasProblems}{
  logical. Are there periods of camera malfunction specified in \code{CTtable}?
}
  \item{recordDateTimeCol}{
  character. The name of the column containing date and time of records in \code{recordTable}
}
  \item{recordDateTimeFormat}{
  character. The date/time format of column \code{recordDateTimeCol} in \code{recordTable}. 
}
  \item{Xcol}{
  character. name of the column specifying x coordinates in \code{CTtable}. Used to create detection maps if \code{makezip} is TRUE. (optional)
}
  \item{Ycol}{
  character. name of the column specifying y coordinates in \code{CTtable}. Used to create detection maps if \code{makezip} is TRUE. (optional)
}
  \item{sinkpath}{
  character. The directory into which the survey report is saved (optional)
}
  \item{makezip}{
  logical. Create a zip file containing tables, plots and maps in \code{sinkpath}?
}
}
\details{

The value of \code{CTDateFormat} should be interpretable by \code{\link[base]{as.Date}}. \code{CTDateFormat} defaults to "YYYY-MM-DD", e.g. "2014-10-31". See \code{\link[base]{strptime}} for how to format date and time strings in R.
If \code{CTHasProblems} is set to TRUE, the function tries to find columns \code{ProblemX_from} and \code{ProblemX_to} in \code{CTtable} (\code{X} designates numbers from 1 to n in which a camera or station was not operational). If there are no such columns all stations are assumed to have been operational uninterruptedly from setup to retrieval.
}
\value{
 An invisible list containing 5 \code{data.frames}. 

  \item{survey_dates}{station and image date ranges, number of total and active trap nights, number of cameras per station}
  \item{species_by_station}{species numbers by station}
  \item{events_by_species}{number of events and stations by species}
  \item{events_by_station}{number of events for every species by station (only species that were recorded)}
  \item{events_by_station2}{number of events for all species at all stations (including species that were not recorded)}
  
    
The output will be saved to a .txt file if \code{sinkpath} is defined. 

If \code{makezip} is TRUE, a zip file will be created in \code{sinkpath}. It contains single-species activity plots, detection maps (if \code{Xcol} and \code{Ycol} are defined), the survey report tables, the record table and the camera trap station table, and an example R script.
}

\author{
Juergen Niedballa
}
% \note{
%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{recordTable}}
}
\examples{
data(camtraps)
data(recordTableSample)

reportTest <- surveyReport (recordTable          = recordTableSample,
                            CTtable              = camtraps,
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            setupCol             = "Setup_date",
                            retrievalCol         = "Retrieval_date",
                            CTDateFormat         = "\%d/\%m/\%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S")

class(reportTest)  # a list with
length(reportTest) # 5 elements

reportTest[[1]]    # camera trap operation times and image date ranges
reportTest[[2]]    # number of species by station
reportTest[[3]]    # number of events and number of stations by species
reportTest[[4]]    # number of species events by station
reportTest[[5]]    # number of species events by station including 0s (non-observed species)

# with camera problems

reportTest_problem <- surveyReport (recordTable          = recordTableSample,
                                    CTtable              = camtraps,
                                    speciesCol           = "Species",
                                    stationCol           = "Station",
                                    setupCol             = "Setup_date",
                                    retrievalCol         = "Retrieval_date",
                                    CTDateFormat         = "\%d/\%m/\%Y", 
                                    recordDateTimeCol    = "DateTimeOriginal",
                                    recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
                                    CTHasProblems        = TRUE)

reportTest_problem$survey_dates

\dontrun{
# run again with sinkpath defined
reportTest <- surveyReport (recordTable          = recordTableSample,
                            CTtable              = camtraps,
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            setupCol             = "Setup_date",
                            retrievalCol         = "Retrieval_date",
                            CTDateFormat         = "\%d/\%m/\%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
                            sinkpath             = getwd())

# have a look at the text file
readLines(list.files(getwd(), pattern = paste("survey_report_", Sys.Date(), ".txt", sep = ""), 
					 full.names = TRUE))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
