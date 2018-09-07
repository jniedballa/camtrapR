activityHistogram <- function(recordTable,
                              species,
                              allSpecies = FALSE,
                              speciesCol = "Species",
                              recordDateTimeCol = "DateTimeOriginal",
                              recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
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
  if(!is.data.frame(recordTable)) stop("recordTable must be a data frame", call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))           stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste('recordDateTimeCol = "', recordDateTimeCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  stopifnot(is.logical(c(allSpecies, writePNG, plotR, createDir)))
  if(allSpecies == FALSE){
    stopifnot(hasArg(species))
    stopifnot(species %in% recordTable[,speciesCol])
  }
  recordTable$DateTime2 <- strptime(as.character(recordTable[,recordDateTimeCol]), format = recordDateTimeFormat, tz = "UTC")
  if("POSIXlt" %in% class(recordTable$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(recordTable$DateTime2))) stop(paste("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat. row",
                                                   paste(which(is.na(recordTable$DateTime2)), collapse = ", ")))

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
    subset_species <- subset(recordTable, recordTable[,speciesCol] == species)
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