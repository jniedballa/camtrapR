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
  if(!is.data.frame(recordTable)) stop("recordTable must be a data frame", call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))           stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste('recordDateTimeCol = "', recordDateTimeCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)

  stopifnot(is.logical(c(allSpecies, writePNG, plotR, createDir, byNumber)))
  if(allSpecies == FALSE) {
    stopifnot(hasArg(species))
    stopifnot(species %in% recordTable[,speciesCol])
  }
  
  tz <- "UTC"

  recordTable$DateTime2 <- strptime(as.character(recordTable[,recordDateTimeCol]), format = recordDateTimeFormat, tz = tz)
  if("POSIXlt" %in% class(recordTable$DateTime2) == FALSE) stop("couldn't interpret recordDateTimeCol of recordTable using specified recordDateTimeFormat")
  if(any(is.na(recordTable$DateTime2))) stop(paste("at least 1 entry in recordDateTimeCol of recordTable could not be interpreted using recordDateTimeFormat. row",
                                                   paste(which(is.na(recordTable$DateTime2)), collapse = ", ")))
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