detectionMaps <- function(CTtable,
                          recordTable,
                          Xcol,
                          Ycol,
                          backgroundPolygon,
                          stationCol = "Station",
                          speciesCol = "Species",
                          speciesToShow,
                          richnessPlot = TRUE,
                          speciesPlots = TRUE,
                          addLegend = TRUE,
                          printLabels = FALSE,
                          smallPoints,
                          plotR = TRUE,
                          writePNG = FALSE,
                          plotDirectory,
                          createPlotDir = FALSE,
                          pngMaxPix = 1000,
                          writeShapefile = FALSE,
                          shapefileName,
                          shapefileDirectory,
                          shapefileProjection){

  wd0 <- getwd()
  opar <- par(no.readonly = TRUE)
  on.exit(setwd(wd0))
  on.exit(par(opar), add = TRUE)

  # check column names
  checkForSpacesInColumnNames(stationCol = stationCol, speciesCol = speciesCol, Xcol = Xcol, Ycol = Ycol)
  if(class(CTtable) != "data.frame") stop("CTtable must be a data.frame", call. = FALSE)
  if(class(recordTable) != "data.frame") stop("recordTable must be a data.frame", call. = FALSE)

  if(!stationCol %in% colnames(CTtable))      stop(paste('stationCol = "',   stationCol,     '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!stationCol %in% colnames(recordTable))  stop(paste('stationCol = "',   stationCol,     '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))  stop(paste('speciesCol = "', speciesCol,       '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!Xcol %in% colnames(CTtable))            stop(paste('Xcol = "',   Xcol, '" is not a column name in CTtable', sep = ''), call. = FALSE)
  if(!Ycol %in% colnames(CTtable))            stop(paste('Ycol = "',   Ycol, '" is not a column name in CTtable', sep = ''), call. = FALSE)


  CTtable[,stationCol] <- as.character(CTtable[,stationCol])
  recordTable[,stationCol] <- as.character(recordTable[,stationCol])

  # check all stations in recordTable are matched in CTtable
  if(all(recordTable[,stationCol] %in% CTtable[,stationCol]) == FALSE) {
    stop(paste("items of stationCol in recordTable are not matched in stationCol of CTtable: ", paste(recordTable[-which(recordTable[,stationCol] %in% CTtable[,stationCol]),stationCol], collapse = ", ")))
  }

  recordTable[,speciesCol] <- as.character(recordTable[,speciesCol])

  if(any(is.na(CTtable[,Xcol])))stop("there are NAs in Xcol")
  if(any(is.na(CTtable[,Ycol])))stop("there are NAs in Ycol")
  if(any(is.na(CTtable[,stationCol])))stop("there are NAs in stationCol")

  if(writeShapefile == TRUE) {
    
    if (!requireNamespace("rgdal", quietly = TRUE)) {
      stop("the package 'rgdal' is needed for this function,  you can try to install via: install.packages('rgdal')")
    } 
    
    stopifnot(hasArg(shapefileDirectory))
    stopifnot(file.exists(shapefileDirectory))
  }

  if(hasArg(backgroundPolygon)) stopifnot(class(backgroundPolygon) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))

  if(hasArg(speciesToShow)){
    stopifnot(is.character(speciesToShow))
    if(any(!speciesToShow %in% recordTable[,speciesCol])) stop(" these speciesToShow are not in speciesCol of recordTable:   ", paste(speciesToShow[!speciesToShow %in% recordTable[,speciesCol]], collapse = ", "), call. = FALSE)
    recordTable <- recordTable[recordTable[,speciesCol] %in% speciesToShow,]
  }

  # data preparation
  dat2 <- aggregate(CTtable[, c(Ycol, Xcol)], by = list(CTtable[,stationCol]), FUN = mean)    # get coordinates
  colnames(dat2)[1] <- stationCol

  t1 <- aggregate(recordTable[, speciesCol], by = list(recordTable[,stationCol]), FUN = table, simplify = FALSE)   # number of species records by station
  t2 <- data.frame(as.character(t1[,1]), matrix(unlist(t1$x), nrow = length(unique(recordTable[,stationCol])), byrow = TRUE))
  colnames(t2) <- c(stationCol, names(t1[,2][[1]]))

  t3 <- t2[match(toupper(as.character(dat2[,stationCol])), toupper(as.character(t2[,stationCol]))),]    # matching IDs between recordTable and CTtable
  cex.t3 <- data.frame(t3[,1], apply(data.frame(t3[,-1]), MARGIN = 2, FUN = function(x){x/max(x, na.rm = TRUE)}))
  colnames(cex.t3)[1] <- stationCol

  t4 <- data.frame(t3[,1], n_species = apply(as.data.frame(ifelse(t3[,-1] >=1, 1, 0)), 1, FUN = sum))    # number of species by station
  cex.t4 <- data.frame(t4[,1], n_species_scaled = t4$n_species / max(t4$n_species, na.rm = TRUE))
  colnames(t4)[1] <- colnames(cex.t4)[1] <- stationCol

  rm(t1, t2)

  # set graphics  parameters and out directory
  if(hasArg(smallPoints) == FALSE) {smallPoints <- 0} else {
    if(smallPoints >  3 | smallPoints < 0 | !is.numeric(smallPoints)) stop("smallPoints must be a number between 0 and 3")
  }
  cex_pt1 <- 4.5 - smallPoints
  cex_pt2.max <- cex_pt1 - 1.5 + smallPoints/3
  cex_pt1_gg <- 9
  col_pt1_fill <- rgb(1,1,1, 0)
  col_pt1_border <- rgb(0,0,0, 0.5)
  col_pt2 <- "black"
  cex.labels <- 1.1
  cex.legend <- 1.5
  x_intersp <- 2
  y_intersp <- 1.3
  grconvertX.val <- 0.87
  grconvertY.val <- 0.9
  par.mar.tmp <- c(5.1, 4.1, 4.1, 8.1)
  pch1 <- 21
  pch2 <- 16
  col_polygon_border <- rgb(0.5,0.5,0.5,1)
  lwd_polygon_border <- 1.5
  lty_polygon_border <- 1

  range.expand.factor <- 0.04

  if(hasArg(backgroundPolygon)){
    x.range <- extendrange(r = range(c(coordinates(backgroundPolygon@bbox)[1,], dat2[,Xcol])), f = range.expand.factor)
    y.range <- extendrange(r = range(c(coordinates(backgroundPolygon@bbox)[2,], dat2[,Ycol])), f = range.expand.factor)

    X.tmp <- pngMaxPix / diff(range(c(coordinates(backgroundPolygon@bbox)[1,], dat2[,Xcol])))
    Y.tmp <- pngMaxPix / diff(range(c(coordinates(backgroundPolygon@bbox)[1,], dat2[,Ycol])))

  } else {

    x.range <- extendrange(r = range(range(dat2[,Xcol])), f = range.expand.factor)
    y.range <- extendrange(r = range(range(dat2[,Ycol])), f = range.expand.factor)

    X.tmp <- pngMaxPix / diff(range(dat2[Xcol]))
    Y.tmp <- pngMaxPix / diff(range(dat2[Ycol]))}

  par(mar = par.mar.tmp,
      xpd = TRUE,
      xaxs = "i")



  if(X.tmp > Y.tmp){
    pngWidth <-  pngMaxPix
    pngHeight <- round(pngMaxPix / (X.tmp /  Y.tmp))
  } else {
    pngWidth <-  round(pngMaxPix / (X.tmp /  Y.tmp))
    pngHeight <-  pngMaxPix
  }
  # if any of these is NA or NaN, set to pngMaxPix (may happen if only 1 station and x or y range is 0, bcause of division by 0)
  if(is.na(pngWidth))  pngWidth <- pngMaxPix
  if(is.na(pngHeight))  pngHeight <- pngMaxPix

  if(isTRUE(writePNG)){
    if(hasArg(plotDirectory)){
      if(isTRUE(createPlotDir)){
        dir.create(plotDirectory, recursive = TRUE, showWarnings = FALSE)
        setwd(plotDirectory)
      } else {
        stopifnot(file.exists(plotDirectory))
        setwd(plotDirectory)
      }
    } else {stop("please set plotDirectory")}
  }

  # species richness plot
  if(isTRUE(richnessPlot)){

    legend.label.richness <- sort(unique(t4$n_species))
    legend.cex.richness <- sort(unique((cex.t4$n_species_scaled * (cex_pt2.max))))

    if(isTRUE(writePNG)){
      png(filename = paste("n_Species_", Sys.Date(), ".png", sep = ""),
          width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
      par(mar = par.mar.tmp, xpd=TRUE, xaxs = "i")

      plot(x = 0, type = "n", main = "Species Richness", ylab = Ycol, xlab = Xcol,
             xlim = x.range, ylim = y.range,
             asp = 1,
             xaxs = "i", yaxs = "i")

      if(hasArg(backgroundPolygon)){plot(backgroundPolygon, add = TRUE, border = col_polygon_border, lty = lty_polygon_border, lwd = lwd_polygon_border)}

      # station points
      points(y = dat2[, Ycol], x = dat2[, Xcol], pch = pch1,  bg  = col_pt1_fill, col = col_pt1_border,
           cex = cex_pt1)
      # detection points (scaled)
      points(y = dat2[, Ycol], x = dat2[, Xcol],  pch = pch2,
             col = col_pt2,
             cex = cex.t4$n_species_scaled * (cex_pt2.max))

      if(isTRUE(printLabels)){
        text(y = dat2[, Ycol], x = dat2[, Xcol], labels = dat2[,stationCol], cex = cex.labels, pos = 1, col = "red")
      }
      if(isTRUE(addLegend)){
        legend(
          x =  grconvertX(grconvertX.val, from = "ndc", to = "user"),
          y = grconvertY(grconvertY.val, from = "ndc", to = "user"),
          legend = legend.label.richness,
          pch = pch2,
          col = col_pt2,
          cex = cex.legend,
          pt.cex = legend.cex.richness,
          x.intersp = x_intersp,
          y.intersp = y_intersp,
          bty = "n")
      }
      dev.off()
    }

    if(isTRUE(plotR)){

      # this works. migrate everything to ggplot?

      #       ggplot(dat2, aes(dat2[,Xcol], dat2[,Ycol])) +
      #         geom_point(cex =  cex_pt1_gg , fill = "white", colour = "black", pch = 21) +
      #         geom_point(aes(size = factor(t4$n_species)), colour = "black") +
      #         theme_bw() +
      #         theme(legend.position = "right",
      #               legend.box = "vertical",
      #               legend.key = element_blank()) +
      #         labs(x = Xcol,
      #              y = Ycol,
      #              title = "Species Richness")  +
      #         coord_fixed(ratio = 1) +
      #         scale_size_discrete(name = "n species")


      plot(x = 0, type = "n", main = "Species Richness", ylab = Ycol, xlab = Xcol,
           xlim = x.range, ylim = y.range,
           asp = 1,
           xaxs = "i", yaxs = "i")

      if(hasArg(backgroundPolygon)){plot(backgroundPolygon, add = TRUE, border = col_polygon_border, lty = lty_polygon_border, lwd = lwd_polygon_border)}

      # station points
      points(y = dat2[, Ycol], x = dat2[, Xcol], pch = pch1,  bg  = col_pt1_fill, col = col_pt1_border,
             cex = cex_pt1)
      # detection points (scaled)
      points(y = dat2[, Ycol], x = dat2[, Xcol],  pch = pch2,
             col = col_pt2,
             cex = cex.t4$n_species_scaled * (cex_pt2.max))

      if(isTRUE(printLabels)){
        text(y = dat2[, Ycol], x = dat2[, Xcol], labels = dat2[,stationCol], cex = cex.labels, pos = 1, col = "red")
      }
      if(isTRUE(addLegend)){
        legend(
          x =  grconvertX(grconvertX.val, from = "ndc", to = "user"),
          y = grconvertY(grconvertY.val, from = "ndc", to = "user"),
          legend = legend.label.richness,
          pch = pch2,
          col = col_pt2,
          cex = cex.legend,
          pt.cex = legend.cex.richness,
          x.intersp = x_intersp,
          y.intersp = y_intersp,
          bty = "n")
      }
    }
  }

  if(isTRUE(speciesPlots)){
    #  species presence plots
    for(i in 2:ncol(t3)){
      i2 <- gsub(pattern = ".", replacement = "_",
                 x = colnames(t3)[i], fixed = TRUE)
      i2a <- gsub(pattern = ".", replacement = " ",
                  x = colnames(t3)[i], fixed = TRUE)

      legend.label <- sort(unique(t3[which(as.integer(t3[,i]) != 0),i]))
      legend.cex <- sort(unique((cex.t3[,i]^(1/2) * (cex_pt2.max))[which(as.integer(t3[,i]) != 0)]))

      if(isTRUE(writePNG)){
        png(filename = paste("Presence_", i2, "_", Sys.Date(), ".png", sep = ""),
            width = pngWidth, height = pngHeight, units = "px", res = 96, type = "cairo")
        par(mar = par.mar.tmp, xpd=TRUE, xaxs = "i")

        plot(x = 0, type = "n", main = i2a, ylab = Ycol, xlab = Xcol,
             xlim = x.range, ylim = y.range,
             asp = 1,
             xaxs = "i", yaxs = "i")

        if(hasArg(backgroundPolygon)){plot(backgroundPolygon, add = TRUE, border = col_polygon_border, lty = lty_polygon_border, lwd = lwd_polygon_border)}

        # station points
        points(y = dat2[, Ycol], x = dat2[, Xcol], pch = pch1,  bg  = col_pt1_fill, col = col_pt1_border,
               cex = cex_pt1)
        # detection points (scaled)
        points(y = dat2[, Ycol], x = dat2[, Xcol],  pch = pch2,
               col = col_pt2,
               cex = cex.t3[,i]^(1/2) * (cex_pt2.max))




        # plot(y = dat2[, Ycol], x = dat2[, Xcol],  pch = pch1,  bg  = col_pt1_fill, col = col_pt1_border,
        #      cex = cex_pt1, main = i2a, ylab = Ycol, xlab = Xcol,
        #      xlim = x.range, ylim = y.range,
        #      asp = 1,
        #      xaxs = "i", yaxs = "i")
        # points(y = dat2[, Ycol], x = dat2[, Xcol],  pch = pch2,
        #        col = col_pt2,
        #        cex = cex.t3[,i]^(1/2) * (cex_pt2.max))

        if(isTRUE(printLabels)){
          text(y = dat2[, Ycol], x = dat2[, Xcol], labels = dat2[,stationCol], cex = cex.labels, pos = 1, col = "red")
        }
        if(isTRUE(addLegend)){
          legend(
            x =  grconvertX(grconvertX.val, from = "ndc", to = "user"),
            y = grconvertY(grconvertY.val, from = "ndc", to = "user"),
            legend = legend.label,
            pch = pch2,
            col = col_pt2,
            cex = cex.legend,
            pt.cex = legend.cex,
            x.intersp = x_intersp,
            y.intersp = y_intersp,
            bty = "n")
        }
        dev.off()
      }

      if(isTRUE(plotR)){
        plot(x = 0, type = "n", main = i2a, ylab = Ycol, xlab = Xcol,
             xlim = x.range, ylim = y.range,
             asp = 1,
             xaxs = "i", yaxs = "i")

        if(hasArg(backgroundPolygon)){plot(backgroundPolygon, add = TRUE, border = col_polygon_border, lty = lty_polygon_border, lwd = lwd_polygon_border)}

        # station points
        points(y = dat2[, Ycol], x = dat2[, Xcol], pch = pch1,  bg  = col_pt1_fill, col = col_pt1_border,
               cex = cex_pt1)
        # detection points (scaled)
        points(y = dat2[, Ycol], x = dat2[, Xcol],  pch = pch2,
               col = col_pt2,
               cex = cex.t3[,i]^(1/2) * (cex_pt2.max))

        if(isTRUE(printLabels)){
          text(y = dat2[, Ycol], x = dat2[, Xcol], labels = dat2[,stationCol], cex = cex.labels, pos = 1, col = "red")
        }
        if(isTRUE(addLegend)){
          legend(
            x =  grconvertX(grconvertX.val, from = "ndc", to = "user"),
            y = grconvertY(grconvertY.val, from = "ndc", to = "user"),
            legend = legend.label,
            pch = pch2,
            col = col_pt2,
            cex = cex.legend,
            pt.cex = legend.cex,
            x.intersp = x_intersp,
            y.intersp = y_intersp,
            bty = "n")
        }
      }
    }
  }

  outtable <- data.frame(dat2, t3[,-1], n_species = t4[,-1])
  # if only 1 species, add column name 
  if(ncol(t3) == 2 & hasArg(speciesToShow)) colnames(outtable)[ncol(outtable) - 1] <- speciesToShow

  rownames(outtable) <- NULL
  # write Shapefile
  if(writeShapefile == TRUE){
    if(hasArg(shapefileProjection)){proj.tmp <- shapefileProjection } else {proj.tmp <- NA}
    if(hasArg(shapefileName)){layer.tmp <- shapefileName } else {layer.tmp <- paste("species_detection_", Sys.Date(), sep = "")}

    spdf <- SpatialPointsDataFrame(coords = outtable[,c(Xcol, Ycol)],
                                   data = outtable,
                                   proj4string = CRS(as.character(proj.tmp)))

    writeOGR(obj = spdf,
             dsn = shapefileDirectory,
             layer = layer.tmp,
             driver = "ESRI Shapefile")
  }

  return(invisible(outtable))
}