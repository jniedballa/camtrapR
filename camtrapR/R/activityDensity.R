activityDensity <- function(recordTable,
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
                            add.rug = TRUE,
                            ...
){
  
  
  wd0 <- getwd()
  mar0 <- par()$mar
  on.exit(setwd(wd0))
  on.exit(par(mar = mar0), add = TRUE)
  
  tz <- "UTC"
  
  checkForSpacesInColumnNames(speciesCol = speciesCol, recordDateTimeCol = recordDateTimeCol)
  if(!is.data.frame(recordTable)) stop("recordTable must be a data frame", call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))           stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste('recordDateTimeCol = "', recordDateTimeCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  stopifnot(is.logical(c(allSpecies, writePNG, plotR, createDir)))
  if(allSpecies == FALSE) {
    stopifnot(species %in% recordTable[,speciesCol])
    stopifnot(hasArg(species))
  }
  
  recordTable$DateTime2 <- strptime(as.character(recordTable[,recordDateTimeCol]), format = recordDateTimeFormat, tz = tz)
  if("POSIXlt" %in% class(recordTable$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(recordTable$DateTime2))) stop(paste("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat. row",
                                                   paste(which(is.na(recordTable$DateTime2)), collapse = ", ")))
  recordTable$Time2 <- format(recordTable$DateTime2, format = "%H:%M:%S", usetz = FALSE)
  
  
  # radians time
  recordTable$Time.rad <- (as.numeric(as.POSIXct(strptime(recordTable$Time2, format = "%H:%M:%S", tz = tz))) -
                             as.numeric(as.POSIXct(strptime("0", format = "%S", tz = tz)))) / 3600 * (pi/12)
  
  if(isTRUE(writePNG)){
    if(hasArg(plotDirectory)){
      if(isTRUE(createDir)){
        dir.create(plotDirectory, recursive = TRUE, showWarnings = FALSE)
        setwd(plotDirectory)
      } else {
        stopifnot(file.exists(plotDirectory))
        setwd(plotDirectory)
      }
    } else {
      stop("writePNG is TRUE. Please set plotDirectory", call. = FALSE)}
  }
  
  pngWidth <- pngMaxPix
  pngHeight <- round(pngMaxPix * 0.8)
  
  if(allSpecies == FALSE){
    
    subset_species <- subset(recordTable, recordTable[,speciesCol] == species)
    
    if(nrow(subset_species) == 1)  stop(paste(species, "had only 1 record. Cannot estimate density."), call. = FALSE)
    
    try_error_tmp <- try({
      if(isTRUE(writePNG))  png(filename = paste("activity_density_", species, "_", Sys.Date(), ".png", sep = ""),
                                width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
      if(isTRUE(writePNG) | isTRUE(plotR)){ densityPlot(subset_species$Time.rad,
                                                        main = paste("Activity of", species),
                                                        rug  = add.rug,
                                                        ...)
        mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)}
      if(isTRUE(writePNG))  dev.off()
    }, silent = TRUE)
    if(class(try_error_tmp) == "try-error") warning(paste(toupper(spec.tmp), ": ", try_error_tmp[1], "    - SKIPPED", sep = ""), call. = FALSE)
  
    # if(isTRUE(writePNG)){
    #   png(filename = paste("activity_density_", species, "_", Sys.Date(), ".png", sep = ""),
    #       width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
    #   densityPlot(subset_species$Time.rad,
    #               main = paste("Activity of", species),
    #               rug = add.rug,
    #               ...)
    #   mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
    #   dev.off()
    # }
    # if(isTRUE(plotR)){
    #   densityPlot(subset_species$Time.rad,
    #               main = paste("Activity of", species),
    #               rug = add.rug,
    #               ...)
    #   mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
    # }
    
  } else {
    
    subset_species_list <- list()
    
    for(i in 1:length(unique(recordTable[,speciesCol]))){
      
      spec.tmp <- unique(recordTable[,speciesCol])[i]
      subset_species <- subset(recordTable, recordTable[,speciesCol] == spec.tmp)
      plot_main_title <- paste("Activity of", spec.tmp)
      
      if(nrow(subset_species) == 1){
        warning(paste(toupper(spec.tmp), ": It had only 1 record. Cannot estimate density.   - SKIPPED", sep = ""), call. = FALSE)
        next
      } else {
        
        try_error_tmp <- try({
          if(isTRUE(writePNG))  png(filename = paste("activity_density_", spec.tmp, "_", Sys.Date(), ".png", sep = ""),
                                    width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
          if(isTRUE(writePNG) | isTRUE(plotR)){ densityPlot(subset_species$Time.rad,
                                                            main = plot_main_title,
                                                            rug  = add.rug,
                                                            ...)
            mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
            }
          if(isTRUE(writePNG))  dev.off()
        }, silent = TRUE)
        if(class(try_error_tmp) == "try-error") warning(paste(toupper(spec.tmp), ": ", try_error_tmp[1], "    - SKIPPED", sep = ""), call. = FALSE)
      }
      
      #if(isTRUE(plotR) & class(try_error_tmp) != "try-error"){
      #  densityPlot(subset_species$Time.rad,
      #              main = plot_main_title,
      #              rug = add.rug,
      #              ...)
      #  mtext(paste("number of records:", nrow(subset_species)), side = 3, line = 0)
      #}
      #suppressWarnings(rm(try_error_tmp))
    }
    subset_species_list[[i]] <- subset_species$Time.rad
    names(subset_species_list)[i] <- spec.tmp
  }
  #}
  if(allSpecies == FALSE){
    return(invisible(subset_species$Time.rad))
  } else {
    return(invisible(subset_species_list))
  }
}