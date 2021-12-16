#' Plot overlapping kernel densities of two-species activities
#' 
#' This function plots kernel density estimates of two species' diel activity
#' data by calling the function \code{\link[overlap]{overlapPlot}} from package
#' \pkg{overlap}. It further computes the overlap coefficient Dhat1 by calling
#' \code{\link[overlap]{overlapEst}}.
#' 
#' \code{...} can be graphical parameters passed on to function
#' \code{\link[overlap]{overlapPlot}}, e.g. \code{linetype}, \code{linewidth},
#' \code{linecol} (see example below).
#' 
#' \code{recordDateTimeFormat} defaults to the "YYYY-MM-DD HH:MM:SS"
#' convention, e.g. "2014-09-30 22:59:59". \code{recordDateTimeFormat} can be
#' interpreted either by base-R via \code{\link[base]{strptime}} or in
#' \pkg{lubridate} via \code{\link[lubridate]{parse_date_time}} (argument
#' "orders"). \pkg{lubridate} will be used if there are no "\%" characters in
#' \code{recordDateTimeFormat}.
#' 
#' For "YYYY-MM-DD HH:MM:SS", \code{recordDateTimeFormat} would be either
#' "\%Y-\%m-\%d \%H:\%M:\%S" or "ymd HMS". For details on how to specify date
#' and time formats in R see \code{\link[base]{strptime}} or
#' \code{\link[lubridate]{parse_date_time}}.
#' 
#' @param recordTable data.frame. the record table created by
#' \code{\link{recordTable}}
#' @param speciesA Name of species 1 (as found in \code{speciesCol} of
#' recordTable)
#' @param speciesB Name of species 2 (as found in \code{speciesCol} of
#' recordTable)
#' @param speciesCol character. name of the column specifying species names in
#' \code{recordTable}
#' @param recordDateTimeCol character. name of the column specifying date and
#' time in \code{recordTable}
#' @param recordDateTimeFormat character. format of column
#' \code{recordDateTimeCol} in \code{recordTable}
#' @param plotR logical. Show plots in R graphics device?
#' @param writePNG logical. Create pngs of the plots?
#' @param addLegend logical. Add a legend to the plots?
#' @param legendPosition character. Position of the legend (keyword)
#' @param plotDirectory character. Directory in which to create png plots if
#' \code{writePNG = TRUE}
#' @param createDir logical. Create \code{plotDirectory}?
#' @param pngMaxPix integer. image size of png (pixels along x-axis)
#' @param add.rug logical. add a rug to the plot?
#' @param overlapEstimator character. Which overlap estimator to return (passed
#' on to argument \code{type} in \code{\link[overlap]{overlapEst}})
#' @param \dots additional arguments to be passed to function
#' \code{\link[overlap]{overlapPlot}}
#' 
#' @return Returns invisibly the \code{data.frame} with plot coordinates
#' returned by \code{\link[overlap]{overlapPlot}}.
#' 
#' @note Please be aware that the function (like the other activity... function
#' of this package) use clock time, not solar time. If your survey was long
#' enough to see changes in sunrise and sunset times, this may result in biased
#' representations of species activity.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{activityDensity}} \cr
#' \url{https://www.kent.ac.uk/smsas/personal/msr/overlap.html}
#' 
#' @references Mike Meredith and Martin Ridout (2018). overlap: Estimates of
#' coefficient of overlapping for animal activity patterns. R package version
#' 0.3.2. \url{https://CRAN.R-project.org/package=overlap} \cr Ridout, M.S. and
#' Linkie, M. (2009) Estimating overlap of daily activity patterns from camera
#' trap data. Journal of Agricultural, Biological and Environmental Statistics,
#' 14, 322-337.
#' 
#' @examples
#' 
#' # load record table
#' data(recordTableSample)
#' 
#' # define species of interest
#' speciesA_for_activity <- "VTA"    # = Viverra tangalunga, Malay Civet
#' speciesB_for_activity <- "PBE"    # = Prionailurus bengalensis, Leopard Cat
#' 
#' # create activity overlap plot (basic)
#' activityOverlap (recordTable = recordTableSample,
#'                  speciesA    = "VTA",    # = Viverra tangalunga, Malay Civet
#'                  speciesB    = "PBE",    # = Prionailurus bengalensis, Leopard Cat
#'                  writePNG    = FALSE,
#'                  plotR       = TRUE
#' )
#' 
#' 
#'                                      
#' # create activity overlap plot (prettier and with some overlapPlot arguments set)
#' 
#' activityOverlap (recordTable = recordTableSample,
#'                  speciesA    = speciesA_for_activity,
#'                  speciesB    = speciesB_for_activity,
#'                  writePNG    = FALSE,
#'                  plotR       = TRUE,
#'                  createDir   = FALSE,
#'                  pngMaxPix   = 1000,
#'                  linecol     = c("black", "blue"),
#'                  linewidth   = c(5,3),
#'                  linetype    = c(1, 2),
#'                  olapcol     = "darkgrey",
#'                  add.rug     = TRUE,
#'                  extend      = "lightgrey",
#'                  ylim        = c(0, 0.25),
#'                  main        = paste("Activity overlap between ", 
#'                                      speciesA_for_activity, "and", 
#'                                      speciesB_for_activity)
#' )
#' 
#' @importFrom overlap overlapEst getBandWidth densityFit overlapPlot densityPlot
#' @export activityOverlap
#' 
activityOverlap <- function(recordTable,
                            speciesA,
                            speciesB,
                            speciesCol = "Species",
                            recordDateTimeCol = "DateTimeOriginal",
                            recordDateTimeFormat = "ymd HMS",
                            plotR = TRUE,
                            writePNG = FALSE,
                            addLegend = TRUE,
                            legendPosition = "topleft",
                            plotDirectory,
                            createDir = FALSE,
                            pngMaxPix = 1000,
                            add.rug = TRUE,
                            overlapEstimator = c("Dhat1", "Dhat4", "Dhat5"),
                            ...){


  wd0  <- getwd()
  mar0 <- par()$mar
  on.exit(setwd(wd0))
  on.exit(par(mar = mar0), add = TRUE)

  overlapEstimator <- match.arg(overlapEstimator)
  
  checkForSpacesInColumnNames(speciesCol = speciesCol, recordDateTimeCol = recordDateTimeCol)
  recordTable <- dataFrameTibbleCheck(df = recordTable)
  
  if(!speciesCol %in% colnames(recordTable))        stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste('recordDateTimeCol = "', recordDateTimeCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  stopifnot(is.logical(c(writePNG, plotR, createDir)))
  stopifnot(hasArg(speciesA))
  stopifnot(hasArg(speciesB))
  stopifnot(all(c(speciesA, speciesB) %in% recordTable[,speciesCol]))
  
  timeZone <- "UTC"

  recordTable$DateTime2 <- parseDateTimeObject(inputColumn = recordTable[,recordDateTimeCol],
                                               dateTimeFormat = recordDateTimeFormat,
                                               timeZone = timeZone)

  recordTable$Time2 <-   format(recordTable$DateTime2, format = "%H:%M:%S", usetz = FALSE)

  # convert time to radians
  recordTable$Time.rad <- (as.numeric(as.POSIXct(strptime(recordTable$Time2, format = "%H:%M:%S", tz = timeZone))) -
                             as.numeric(as.POSIXct(strptime("0", format = "%S", tz = timeZone)))) / 3600 * (pi/12)

  subset_speciesA <- subset(recordTable, recordTable[,speciesCol] == speciesA)
  subset_speciesB <- subset(recordTable, recordTable[,speciesCol] == speciesB)
  
  if(nrow(subset_speciesA) == 1) stop("speciesA has only 1 record. Cannot estimate density.")
  if(nrow(subset_speciesB) == 1) stop("speciesB has only 1 record. Cannot estimate density.")
  
  if(any(is.na( c(subset_speciesA$Time.rad, subset_speciesB$Time.rad))))stop("NAs produced in converting to radial time. Sorry, that's a bug. Please report it.")

  n_record_string <- paste("number of records:", paste(nrow(subset_speciesA), nrow(subset_speciesB), sep = " / "))
  
   
  # set graphics  parameters and out directory
  overlapEst.tmp <- overlap::overlapEst(A = subset_speciesA$Time.rad, B = subset_speciesB$Time.rad, 
                                        type = overlapEstimator)

  dhat.tmp <- paste(names(overlapEst.tmp),
                    round(overlapEst.tmp, digits = 2), sep = "=")
  cex.sub      <- 0.75
  pngWidth     <- pngMaxPix
  pngHeight    <- round(pngMaxPix * 0.8)
  mar.tmp      <-  c(7.1, 4.1, 4.1, 2.1)
  y_usr_factor <- 0.22

  # extract line type, color and width for use in legend
  dots <- list(...)
  if(!is.null(dots[['linetype']])){ lty.tmp <- dots[['linetype']]}  else {lty.tmp <- c(1, 2)}
  if(!is.null(dots[['linewidth']])){lwd.tmp <- dots[['linewidth']]} else {lwd.tmp <- c(1, 1)}
  if(!is.null(dots[['linecol']])){  col.tmp <- dots[['linecol']]}   else {col.tmp <- c("black", "blue")}
  if(!is.null(dots[['main']])){     main.tmp <- dots[['main']]}   else {main.tmp <- paste("Activity overlap: ", 
                                                                                          substitute(speciesA), "and", 
                                                                                          substitute(speciesB))}

# check that plot directory exists, create it if needed and desired, and set working directory
  if(isTRUE(writePNG)){
    if(is.null(plotDirectory) == FALSE){
      if(isTRUE(createDir)){
        dir.create(plotDirectory, recursive = TRUE, showWarnings = FALSE)
        setwd(plotDirectory)
      } else {
        stopifnot(file.exists(plotDirectory))
        setwd(plotDirectory)
      }
    } else {stop("writePNG is TRUE. Please set plotDirectory")}
  }

  if(isTRUE(writePNG)){

    png(filename = paste("activity_overlap_", speciesA, "-", speciesB, "_", Sys.Date(), ".png", sep = ""),
        width    = pngWidth, 
        height   = pngHeight, 
        units    = "px", 
        res      = 96, 
        type     = "cairo")

    par(mar = mar.tmp)
    overlapPlot(A    = subset_speciesA$Time.rad, 
                B    = subset_speciesB$Time.rad,
                rug  = add.rug,
                main = main.tmp,
                ...)
    
    legend( x      = "top",
            legend = dhat.tmp,
            bty    = "n",
            cex    = cex.sub)

    if(isTRUE(addLegend)){
      legend(x      = legendPosition,
             legend = c(speciesA, speciesB),
             lty    = lty.tmp,
             lwd    = lwd.tmp,
             col    = col.tmp,
             bg     = "white"
      )
    }
    mtext(n_record_string, side = 3, line = 0)            
    dev.off()
  }

  if(isTRUE(plotR)){

    par(mar = mar.tmp)
    plot.values <- overlapPlot(A = subset_speciesA$Time.rad, 
                               B = subset_speciesB$Time.rad,
                               rug = add.rug,
                               main = main.tmp,
                               ...)

    legend(x      = "top",
           legend = dhat.tmp,
           bty    = "n",
           cex    = cex.sub)

    if(isTRUE(addLegend)){
      legend(x      = legendPosition,
             legend = c(speciesA, speciesB),
             lty    = lty.tmp,
             lwd    = lwd.tmp,
             col    = col.tmp,
             bg     = "white"
      )
    }
    mtext(n_record_string, side = 3, line = 0)            
    
    return(invisible(plot.values))
  }
}
