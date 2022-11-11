#' Plot histogram of single-species activity
#' 
#' The function generates a histogram of species diel activity in 1-hour
#' intervals.
#' 
#' Activity is calculated from the time of day of records. The date is ignored.
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
#' @param species Name of the single species for which to create a histogram of
#' activity
#' @param allSpecies logical. Create plots for all species in \code{speciesCol}
#' of \code{recordTable}? Overrides argument \code{species}
#' @param speciesCol character. name of the column specifying species names in
#' \code{recordTable}
#' @param recordDateTimeCol character. name of the column specifying date and
#' time in \code{recordTable}
#' @param recordDateTimeFormat character. format of column
#' \code{recordDateTimeCol} in \code{recordTable}
#' @param plotR logical. Show plots in R graphics device?
#' @param writePNG logical. Create pngs of the plots?
#' @param plotDirectory character. Directory in which to create png plots if
#' \code{writePNG = TRUE}
#' @param createDir logical. Create \code{plotDirectory}?
#' @param pngMaxPix integer. image size of png (pixels along x-axis)
#' @param \dots additional arguments to be passed to function
#' \code{\link[graphics]{hist}}
#' @return It returns invisibly a vector of species record date and time in
#' \code{POSIXlt} format. If \code{allSpecies == TRUE}, all species' vectors
#' are returned in an invisible named list.
#' 
#' @note If you have a sufficiently large number of records you may wish to
#' consider using \code{\link{activityDensity}} instead. Please be aware that
#' this function (like the other activity... function of this package) use
#' clock time. If your survey was long enough to see changes in sunrise and
#' sunset times, this may result in biased representations of species activity.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{activityDensity}}, \code{\link{activityRadial}},
#' \code{\link{activityOverlap}}
#' 
#' @examples
#' 
#' 
#' # load record table
#' data(recordTableSample)
#' 
#' # generate activity histogram
#' species4activity <- "VTA"    # = Viverra tangalunga, Malay Civet
#' 
#' activityHistogram (recordTable = recordTableSample,
#'                    species     = species4activity,
#'                    allSpecies = FALSE)
#' 
#' @export activityHistogram
#' 
activityHistogram <- function(recordTable,
                              species,
                              allSpecies = FALSE,
                              speciesCol = "Species",
                              recordDateTimeCol = "DateTimeOriginal",
                              recordDateTimeFormat = "ymd HMS",
                              plotR = TRUE,
                              writePNG = FALSE,
                              plotDirectory,
                              createDir = FALSE,
                              pngMaxPix = 1000,
                              ...){

  wd0 <- getwd()
  mar0 <- par()$mar
  on.exit(setwd(wd0))
  on.exit(par(mar = mar0), add = TRUE)

  checkForSpacesInColumnNames(speciesCol = speciesCol, recordDateTimeCol = recordDateTimeCol)
  recordTable <- dataFrameTibbleCheck(df = recordTable)
  
  if(!speciesCol %in% colnames(recordTable))           stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste('recordDateTimeCol = "', recordDateTimeCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  stopifnot(is.logical(c(allSpecies, writePNG, plotR, createDir)))
  if(allSpecies == FALSE){
    stopifnot(hasArg(species))
    stopifnot(species %in% recordTable[,speciesCol])
  }

  timeZone <- "UTC"
  
  recordTable$DateTime2 <- parseDateTimeObject(inputColumn = recordTable[,recordDateTimeCol],
                                               dateTimeFormat = recordDateTimeFormat,
                                               timeZone = timeZone)

  recordTable$Hour <- as.POSIXlt(recordTable$DateTime2)$hour

  # set graphics  parameters and out directory
  col_bars <- "gray"
  hist_breaks <-  seq(0,24,1)  # 24
  xlab.tmp = "Time of Day [h]"

  pngWidth <- pngMaxPix
  pngHeight <- round(pngMaxPix * 0.8)

  if(isTRUE(writePNG)){
    if(hasArg(plotDirectory)){
      if(isTRUE(createDir)){
        dir.create(plotDirectory, recursive = TRUE, showWarnings = FALSE)
        setwd(plotDirectory)
      } else {
        stopifnot(file.exists(plotDirectory))
        setwd(plotDirectory)
      }
    } else {stop("writePNG is TRUE. Please set plotDirectory")}
  }

  if(allSpecies == FALSE){
    subset_species <- recordTable[recordTable[,speciesCol] == species,]
    
    subset_species$Hour <- subset_species$Hour + 0.1   # otherwise both 0 and 1 will be in histogram class 0

    if(isTRUE(writePNG)){
      png(filename = paste("activity_histogram_", species, "_", Sys.Date(), ".png", sep = ""),
          width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
      hist(subset_species$Hour, breaks = hist_breaks,
           col  = col_bars,
           main = paste("Activity of", species),
           xlab = xlab.tmp,
           axes = FALSE,
           ...)
      axis(1, at = seq(0,24, by = 3))
      axis(2)
      box()
      mtext(paste("number of records:", length(subset_species$Hour)), side = 3, line = 0)
      dev.off()
    }

    if(isTRUE(plotR)){
      hist(subset_species$Hour,
           breaks = hist_breaks,
           col = col_bars,
           freq = TRUE,
           main = paste("Activity of", species),
           xlab = xlab.tmp,
           axes = FALSE,
           ...)
      axis(1, at = seq(0,24, by = 3))
      axis(2)
      box()
      mtext(paste("number of records:", length(subset_species$Hour)), side = 3, line = 0)
    }


  } else {

    subset_species_list <- list()

    for(i in 1:length(unique(recordTable[,speciesCol]))){

      spec.tmp <- unique(recordTable[,speciesCol])[i]
      subset_species <- subset(recordTable, recordTable[,speciesCol] == spec.tmp)

      if(isTRUE(writePNG)){
        png(filename = paste("activity_histogram_", spec.tmp, "_", Sys.Date(), ".png", sep = ""),
            width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
        hist(subset_species$Hour,
             breaks = hist_breaks,
             col  = col_bars,
             main = paste("Activity of", spec.tmp),
             xlab = xlab.tmp,
             axes = FALSE,
             ...)
        axis(1, at = seq(0,24, by = 3))
        axis(2)
        box()
        mtext(paste("number of records:", length(subset_species$Hour)), side = 3, line = 0)
        dev.off()
      }

      if(isTRUE(plotR)){
        hist(subset_species$Hour,
             breaks = hist_breaks,
             col  = col_bars,
             main = paste("Activity of", spec.tmp),
             xlab = xlab.tmp,
             axes = FALSE,
             ...)
        axis(1, at = seq(0,24, by = 3))
        axis(2)
        box()
        mtext(paste("number of records:", length(subset_species$Hour)), side = 3, line = 0)
      }
      subset_species_list[[i]] <- subset_species$DateTime2
      names(subset_species_list)[i] <- spec.tmp
    }
  }

  if(allSpecies == FALSE){
    return(invisible(subset_species$DateTime2))
  } else {
    return(invisible(subset_species_list))
  }
}
