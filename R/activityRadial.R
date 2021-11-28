#' Radial plots of single-species activity
#' 
#' The function generates a radial plot of species diel activity using an
#' adapted version of function \code{\link[plotrix]{radial.plot}} from package
#' \pkg{plotrix} (without the need to install the package). Records are
#' aggregated by hour. The number of independent events is used as input, which
#' in turn is based on the argument \code{minDeltaTime} in
#' \code{\link{recordTable}}.
#' 
#' 
#' \code{radial.plot} was adjusted to show a clockwise 24-hour clock face. It
#' is recommended to set argument \code{lwd} to a value >= 2. You may also wish
#' to add argument \code{rp.type="p"} to show a polygon instead of bars.
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
#' @param species Name of the species for which to create an kernel density
#' plot of activity
#' @param allSpecies logical. Create plots for all species in \code{speciesCol}
#' of \code{recordTable}? Overrides argument \code{species}
#' @param speciesCol character. name of the column specifying species names in
#' \code{recordTable}
#' @param recordDateTimeCol character. name of the column specifying date and
#' time in \code{recordTable}
#' @param recordDateTimeFormat character. format of column
#' \code{recordDateTimeCol} in \code{recordTable}
#' @param byNumber logical. If FALSE, plot proportion of records. If TRUE, plot
#' number of records
#' @param plotR logical. Show plots in R graphics device?
#' @param writePNG logical. Create pngs of the plots?
#' @param plotDirectory character. Directory in which to create png plots if
#' \code{writePNG = TRUE}
#' @param createDir logical. Create \code{plotDirectory}?
#' @param pngMaxPix integer. image size of png (pixels along x-axis)
#' @param \dots additional arguments to be passed to function
#' \code{\link[plotrix]{radial.plot}}
#' 
#' @return Returns invisibly a data.frame containing all information needed to
#' create the plot: radial position, lengths, hour (for labels). If
#' \code{allSpecies == TRUE}, all species' data frames are returned in an
#' invisible named list.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{activityDensity}}, \code{\link{activityHistogram}},
#' \code{\link{activityOverlap}}
#' 
#' @references Lemon, J. (2006) Plotrix: a package in the red light district of
#' R. R-News, 6(4): 8-12. \cr \url{https://CRAN.R-project.org/package=plotrix }
#' 
#' @examples
#' 
#' 
#' # load record table
#' data(recordTableSample)
#' 
#' species4activity <- "PBE"    # = Prionailurus bengalensis, Leopard Cat
#' 
#' activityRadial(recordTable       = recordTableSample,
#'                species           = species4activity,
#'                allSpecies        = FALSE,
#'                speciesCol        = "Species",
#'                recordDateTimeCol = "DateTimeOriginal",
#'                plotR             = TRUE,
#'                writePNG          = FALSE,
#'                lwd               = 5
#' )
#' 
#' # plot type = polygon
#' 
#' activityRadial(recordTable       = recordTableSample,
#'                species           = species4activity,
#'                allSpecies        = FALSE,
#'                speciesCol        = "Species",
#'                recordDateTimeCol = "DateTimeOriginal",
#'                plotR             = TRUE,
#'                writePNG          = FALSE,
#'                lwd               = 5,
#'                rp.type           = "p"      
#' )
#' 
#' 
#' @export activityRadial
#' 
activityRadial <- function(recordTable,
                           species,
                           allSpecies = FALSE,
                           speciesCol = "Species",
                           recordDateTimeCol = "DateTimeOriginal",
                           recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                           byNumber = FALSE,
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

  stopifnot(is.logical(c(allSpecies, writePNG, plotR, createDir, byNumber)))
  if(allSpecies == FALSE) {
    stopifnot(hasArg(species))
    stopifnot(species %in% recordTable[,speciesCol])
  }
  
  timeZone <- "UTC"

  recordTable$DateTime2 <- parseDateTimeObject(inputColumn = recordTable[,recordDateTimeCol],
                                               dateTimeFormat = recordDateTimeFormat,
                                               timeZone = timeZone)

  recordTable$Time2 <- as.POSIXlt(recordTable$DateTime2)$hour
  
  recordTable[,speciesCol] <- as.character(recordTable[,speciesCol])



  if(isTRUE(writePNG)){
    if(hasArg(plotDirectory)){
      if(isTRUE(createDir)){
        dir.create(plotDirectory, recursive = TRUE, showWarnings = FALSE)
        setwd(plotDirectory)
      } else {
        if(file.exists(plotDirectory) == FALSE) stop("plotDirectory does not exist.")
        setwd(plotDirectory)
      }
    } else {stop("writePNG is TRUE. Please set plotDirectory")}
  }

  pngWidth <- pngMaxPix
  pngHeight <- pngMaxPix


  if(allSpecies == FALSE){

    subset_species <- subset(recordTable, recordTable[,speciesCol] == species)
    lengths.tmp <- table(subset_species$Time2)

    seq.tmp <- data.frame(hour = seq(0,23, length.out = 24),
                          n = 0)
    seq.tmp$n[match(as.numeric(names(lengths.tmp)), seq.tmp$hour)] <- lengths.tmp
    seq.tmp$perc <- seq.tmp$n / sum(seq.tmp$n)
    seq.tmp$radial.pos <- seq.tmp$hour/(24/(2*pi))
    if(isTRUE(byNumber)){
      seq.tmp$length4plot <- seq.tmp$n
    } else {
      seq.tmp$length4plot <- seq.tmp$perc
    }

    if(isTRUE(writePNG)){
      png(filename = paste("activity_radial_", species, "_", Sys.Date(), ".png", sep = ""),
          width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
      .radial.plot(lengths = seq.tmp$length4plot, radial.pos = seq.tmp$radial.pos,
                   clockwise = TRUE,
                   start = (pi/2),
                   labels = paste(formatC(seq.tmp$hour, width = 2,  flag = 0), "00", sep = ""),
                   main = paste("Activity of", species),
                   boxed.radial = FALSE,
                   ...)
      #title(main = paste("Activity of", species), line = 3)
      #mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
      dev.off()
    }
    if(isTRUE(plotR)){
      .radial.plot(lengths = seq.tmp$length4plot, radial.pos = seq.tmp$radial.pos,
                   clockwise = TRUE,
                   start = (pi/2),
                   labels = paste(formatC(seq.tmp$hour, width = 2,  flag = 0), "00", sep = ""),
                   main = paste("Activity of", species),
                   boxed.radial = FALSE,
                   ...)
      #title(main = paste("Activity of", species), line = 3)
      #mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
    }

  } else {

    subset_species_list <- list()

    for(i in 1:length(unique(recordTable[,speciesCol]))){

      spec.tmp <- unique(recordTable[,speciesCol])[i]
      subset_species <- subset(recordTable, recordTable[,speciesCol] == spec.tmp)

      lengths.tmp <- table(subset_species$Time2)

      seq.tmp <- data.frame(hour = seq(0,23, length.out = 24),
                            n = 0)
      seq.tmp$n[match(as.numeric(names(lengths.tmp)), seq.tmp$hour)] <- lengths.tmp
      seq.tmp$perc <- seq.tmp$n / sum(seq.tmp$n)
      seq.tmp$radial.pos <- seq.tmp$hour/(24/(2*pi))
      if(isTRUE(byNumber)){
        seq.tmp$length4plot <- seq.tmp$n
      } else {
        seq.tmp$length4plot <- seq.tmp$perc
      }

      if(isTRUE(writePNG)){
        png(filename = paste("activity_radial_", spec.tmp, "_", Sys.Date(), ".png", sep = ""),
            width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
        .radial.plot(lengths = seq.tmp$length4plot, radial.pos = seq.tmp$radial.pos,
                     clockwise = TRUE,
                     start = (pi/2),
                     labels = paste(formatC(seq.tmp$hour, width = 2,  flag = 0), "00", sep = ""),
                     main = paste("Activity of", spec.tmp),
                     boxed.radial = FALSE,
                     ...)
        #title(main = paste("Activity of", spec.tmp), line = 3)             
        #mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
        dev.off()
      }

      if(isTRUE(plotR)){
        .radial.plot(lengths = seq.tmp$length4plot, radial.pos = seq.tmp$radial.pos,
                     clockwise = TRUE,
                     start = (pi/2),
                     labels = paste(formatC(seq.tmp$hour, width = 2,  flag = 0), "00", sep = ""),
                     main = paste("Activity of", spec.tmp),
                     boxed.radial = FALSE,
                     ...)
        #title(main = paste("Activity of", spec.tmp), line = 3)             
        #mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
      }
      subset_species_list[[i]] <- seq.tmp
      names(subset_species_list)[i] <- spec.tmp
    }
  }
  if(allSpecies == FALSE){
    return(invisible(seq.tmp))
  } else {
    return(invisible(subset_species_list))
  }
}
