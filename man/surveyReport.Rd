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
  camOp,
  speciesCol = "Species",
  stationCol = "Station",
  cameraCol, 
  setupCol,
  retrievalCol,
  CTDateFormat = "\%Y-\%m-\%d", 
  CTHasProblems = "deprecated",
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
  \item{camOp}{
   camera operation matrix created with \code{\link{cameraOperation}}
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
  character. The format of columns \code{setupCol} and \code{retrievalCol} (and potential problem columns) in \code{CTtable}. Must be interpretable by either \code{as.Date} or the "orders" argument \code{\link[lubridate]{parse_date_time}} in \pkg{lubridate}.
}
\item{CTHasProblems}{
  deprecated (since version 2.1)
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

\code{dateFormat} defaults to "YYYY-MM-DD", e.g. "2014-10-31". It can be specified either in the format required by \code{\link[base]{strptime}} or the 'orders' argument in \code{\link[lubridate]{parse_date_time}} in \pkg{lubridate}. In the example above, "YYYY-MM-DD" would be specified as "\%Y-\%m-\%d" or "ymd". 

\code{recordDateTimeFormat} defaults to the "YYYY-MM-DD HH:MM:SS" convention, e.g. "2014-09-30 22:59:59". \code{recordDateTimeFormat} can be interpreted either by base-R via \code{\link[base]{strptime}} or in \pkg{lubridate} via \code{\link[lubridate]{parse_date_time}} (argument "orders"). \pkg{lubridate} will be used if there are no "\%" characters in \code{recordDateTimeFormat}. 

For "YYYY-MM-DD HH:MM:SS", \code{recordDateTimeFormat} would be either "\%Y-\%m-\%d \%H:\%M:\%S" or "ymd HMS". For details on how to specify date and time formats in R see \code{\link[base]{strptime}} or \code{\link[lubridate]{parse_date_time}}.

Note: as of version 2.1, argument \code{CTHasProblems} is deprecated and defunct. Please use \code{camOp} instead to provide information about periods of camera activity and malfunction. 
 
}
\value{
 An invisible list containing 5 \code{data.frames}. 

  \item{survey_dates}{station and image date ranges, number of total and active trap days (calendar days and taking into account independent effort of multiple cameras, if applicable), number of cameras per station}
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

# since version 2.1, camera operation matrix is required as input

camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    writecsv     = FALSE,
                                    hasProblems  = FALSE,
                                    dateFormat   = "\%d/\%m/\%Y"
)

reportTest <- surveyReport (recordTable          = recordTableSample,
                            CTtable              = camtraps,
                            camOp                = camop_no_problem,
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

camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = "\%d/\%m/\%Y"
)

reportTest_problem <- surveyReport (recordTable          = recordTableSample,
                                    CTtable              = camtraps,
                                    camOp                = camop_problem,
                                    speciesCol           = "Species",
                                    stationCol           = "Station",
                                    setupCol             = "Setup_date",
                                    retrievalCol         = "Retrieval_date",
                                    CTDateFormat         = "\%d/\%m/\%Y", 
                                    recordDateTimeCol    = "DateTimeOriginal",
                                    recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S")

reportTest_problem$survey_dates



## if camOp is missing, the legacy version (from 2.0.3) will be used:

reportTest_problem_old <- surveyReport (recordTable          = recordTableSample,
                                    CTtable              = camtraps,
                                   # camOp                = camop_problem,
                                    speciesCol           = "Species",
                                    stationCol           = "Station",
                                    setupCol             = "Setup_date",
                                    retrievalCol         = "Retrieval_date",
                                    CTDateFormat         = "\%d/\%m/\%Y", 
                                    recordDateTimeCol    = "DateTimeOriginal",
                                    recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S")

\dontrun{
# run again with sinkpath defined
reportTest <- surveyReport (recordTable          = recordTableSample,
                            CTtable              = camtraps,
                            camOp                = camop_no_problem,
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
