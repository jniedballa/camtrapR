\name{detectionHistory}
\alias{detectionHistory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species detection histories for occupancy analyses
}
\description{
This function generates species detection histories that can be used in occupancy analyses, e.g. with package \code{\link[unmarked]{unmarked}}. It generates detection histories in different formats, with adjustable occasion length and occasion start time.
}
\usage{
detectionHistory(recordTable,
 species,
 camOp,
 stationCol = "Station",
 speciesCol = "Species",
 recordDateTimeCol = "DateTimeOriginal",
 recordDateTimeFormat = "\%Y-\%m-\%d \%H:\%M:\%S",
 occasionLength,
 minActiveDaysPerOccasion,
 maxNumberDays,
 day1,
 buffer,
 includeEffort = TRUE,
 scaleEffort = FALSE,
 occasionStartTime = 0,
 datesAsOccasionNames = FALSE,
 timeZone,
 writecsv = FALSE,
 outDir
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordTable}{
  data.frame. the record table created by \code{\link{recordTable}}
}
  \item{species}{
  character. the species for which to compute the detection history
}
  \item{camOp}{
  The camera operability matrix as created by \code{\link{cameraOperation}}
}
  \item{stationCol}{
  character. name of the column specifying Station ID in \code{recordTable}
}
  \item{speciesCol}{
  character. name of the column specifying species in \code{recordTable}
}
 \item{recordDateTimeCol}{
  character. name of the column specifying date and time in \code{recordTable}
}
  \item{recordDateTimeFormat}{
  format of column \code{recordDateTimeCol} in \code{recordTable}
}
  \item{occasionLength}{
  integer. occasion length in days
}
  \item{minActiveDaysPerOccasion}{
  integer. minimum number of active trap days for occasions to be included (optional)
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
  logical. Compute trapping effort (number of active camera trap days per station and occasion)?
}
  \item{scaleEffort}{
  logical. scale and center effort matrix to mean = 0 and sd = 1?
}
  \item{occasionStartTime}{
  integer. time of day (the full hour) at which to begin occasions.
}
  \item{datesAsOccasionNames}{
  If \code{day1 = "survey"}, occasion names in the detection history will be composed of first and last day of that occasion.
}
  \item{timeZone}{
  character. must be an argument of \code{\link[base]{OlsonNames}}
}
  \item{writecsv}{
  logical. Should the detection history be saved as a .csv?
}
  \item{outDir}{
  character. Directory into which detection history .csv file is saved
}

}
\details{
The function computes a species detection matrix, either as a detection-by-date or a detection-by-occasion matrix. \code{day1} defines if each stations detection history will begin on that station's setup day (\code{day1 = "station"}) or if all station's detection histories have a common origin (the day the first station was set up if \code{day1 = "survey"} or a fixed date if, e.g. \code{day1 = "2015-12-31"}). If \code{day1} is a date, \code{\link[base]{as.Date}} must be able to understand it. The most suitable format is "\%Y-\%m-\%d", e.g. "2015-12-31".

\code{includeEffort} controls whether an additional effort matrix is computed or not. This also affects the detection matrices. If \code{includeEffort = FALSE}, all occasions in which a station was not set up or malfunctioning (NA or 0 in \code{camOp}) will result in NAs in the detection history. If \code{includeEffort = TRUE}, the record history will only contain 0 and 1, and no NAs. The effort matrix can then be included in occupancy models as a (continuous) observation covariate to estimate the effect of effort on detection probability.

The number of days that are aggregated is controlled by \code{occasionLength}. \code{occasionStartTime} can be used to make occasions begin another hour than midnight (the default). This may be relevant for nocturnal animals, in which 1 whole night would be considered an occasion.
The values of \code{stationCol} in \code{recordTable} must be matched by the row names of \code{camOp} (case-insensitive), otherwise an error is raised.
\code{DateTimeFormat} defaults to "\%Y-\%m-\%d \%H:\%M:\%S", e.g. "2014-09-30 22:59:59". For details on how to specify date and time formats in R see \code{\link[base]{strptime}}.

}
\value{
Depending on the value of \code{includeEffort} and \code{scaleEffort}, a list with either 1, 2 or 3 elements. The first element is the species detection history. The second is the optional effort matrix and the third contains the effort scaling parameters.
  \item{detection_history}{A species detection matrix}
  \item{effort}{A matrix giving the number of active camera trap days per station and occasion (= camera trapping effort). It is only returned if \code{includeEffort = TRUE}}
  \item{effort_scaling_parameters}{Scaling parameters of the effort matrix. It is only returned if \code{includeEffort} and \code{scaleEffort} are \code{TRUE}}
}

 \section{Warning}{
 Please note the section about defining argument \code{timeZone} in the vignette on data extraction (accessible via \code{vignette("DataExtraction")} or online (\url{https://cran.r-project.org/package=camtrapR/vignettes/DataExtraction.html})).
 }


\author{
Juergen Niedballa
}

\examples{

# define image directory
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# load station information
data(camtraps)

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "\%d/\%m/\%Y"
)


if (Sys.which("exiftool") != ""){        # only run this function if ExifTool is available
recordTableSample <- recordTable(inDir               = wd_images_ID,
                                 IDfrom              = "directory",
                                 minDeltaTime        = 60,
                                 deltaTimeComparedTo = "lastRecord",
                                 exclude             = "NO_ID",
                                 timeZone            = "Asia/Kuala_Lumpur"
)
} else {
data(recordTableSample)
}

# compute detection history for a species

# without trapping effort
DetHist1 <- detectionHistory(recordTable         = recordTableSample,
                            camOp                = camop_no_problem,
                            stationCol           = "Station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "VTA",
                            occasionLength       = 7,
                            day1                 = "station",
                            datesAsOccasionNames = FALSE,
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

DetHist1                     # this is a list with 1 element
DetHist1$detection_history   # this is the contained detection/non-detection matrix


# with effort
DetHist2 <- detectionHistory(recordTable          = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             occasionLength       = 7,
                             day1                 = "station",
                             datesAsOccasionNames = FALSE,
                             includeEffort        = TRUE,
                             scaleEffort          = FALSE,
                             timeZone             = "Asia/Kuala_Lumpur"
)

DetHist2$detection_history  # detection history  (alternatively, use: DetHist2[[1]])
DetHist2$effort             # effort (alternatively, use: DetHist2[[2]])

}
