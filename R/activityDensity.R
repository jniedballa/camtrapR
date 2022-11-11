#' Plot kernel density estimation of single-species activity
#' 
#' The function plots a kernel density estimation of species diel activity
#' using function \code{\link[overlap]{densityPlot}} from package
#' \pkg{overlap}.
#' 
#' \code{species} must be in the \code{speciesCol} of \code{recordTable}.
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
#' @param plotR logical. Show plots in R graphics device?
#' @param writePNG logical. Create pngs of the plots?
#' @param plotDirectory character. Directory in which to create png plots if
#' \code{writePNG = TRUE}
#' @param createDir logical. Create \code{plotDirectory} if \code{writePNG =
#' TRUE}?
#' @param pngMaxPix integer. image size of png (pixels along x-axis)
#' @param add.rug logical. add a rug to the plot?
#' @param \dots additional arguments to be passed to function
#' \code{densityPlot}
#' 
#' @return Returns invisibly a vector of species record observation times in
#' radians, i.e. scaled to [\eqn{0, 2\pi}]. If \code{allSpecies == TRUE}, all
#' species' vectors are returned in an invisible named list.
#' 
#' @author Juergen Niedballa
#' 
#' @seealso \code{\link{activityHistogram}}, \code{\link{activityRadial}},
#' \code{\link{activityOverlap}}
#' \url{https://www.kent.ac.uk/smsas/personal/msr/overlap.html}
#' 
#' @references Martin Ridout and Matthew Linkie (2009). Estimating overlap of
#' daily activity patterns from camera trap data. Journal of Agricultural,
#' Biological and Environmental Statistics, 14(3), 322-337 \cr Mike Meredith
#' and Martin Ridout (2018). overlap: Estimates of coefficient of overlapping
#' for animal activity patterns. R package version 0.3.2.
#' \url{https://CRAN.R-project.org/package=overlap}
#' 
#' @examples
#' 
#' # load record table
#' data(recordTableSample)
#' 
#' species4activity <- "VTA"    # = Viverra tangalunga, Malay Civet
#' 
#' activityDensity(recordTable = recordTableSample,
#'                 species     = species4activity)
#' 
#' 
#' # all species at once
#' 
#' activityDensity(recordTable = recordTableSample,
#'                 allSpecies  = TRUE,
#'                 writePNG    = FALSE,
#'                 plotR       = TRUE,
#'                 add.rug     = TRUE)
#' 
#' @export activityDensity
activityDensity <- function(recordTable,
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
                            add.rug = TRUE,
                            ...
){
  
  
  wd0 <- getwd()
  mar0 <- par()$mar
  on.exit(setwd(wd0))
  on.exit(par(mar = mar0), add = TRUE)

  recordTable <- dataFrameTibbleCheck(df = recordTable)
  
  timeZone <- "UTC"
  
  checkForSpacesInColumnNames(speciesCol = speciesCol, recordDateTimeCol = recordDateTimeCol)
  if(!is.data.frame(recordTable)) stop("recordTable must be a data frame", call. = FALSE)
  if(!speciesCol %in% colnames(recordTable))           stop(paste('speciesCol = "', speciesCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  if(!recordDateTimeCol %in% colnames(recordTable)) stop(paste('recordDateTimeCol = "', recordDateTimeCol, '" is not a column name in recordTable', sep = ''), call. = FALSE)
  
  stopifnot(is.logical(c(allSpecies, writePNG, plotR, createDir)))
  if(allSpecies == FALSE) {
    stopifnot(species %in% recordTable[,speciesCol])
    stopifnot(hasArg(species))
  }
  
  
  recordTable$DateTime2 <- parseDateTimeObject(inputColumn = recordTable[,recordDateTimeCol],
                                               dateTimeFormat = recordDateTimeFormat,
                                               timeZone = timeZone)
  recordTable$Time2 <- format(recordTable$DateTime2, format = "%H:%M:%S", usetz = FALSE)
  
  
  # radians time
  recordTable$Time.rad <- (as.numeric(as.POSIXct(strptime(recordTable$Time2, format = "%H:%M:%S", tz = timeZone))) -
                             as.numeric(as.POSIXct(strptime("0", format = "%S", tz = timeZone)))) / 3600 * (pi/12)
  
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
    
    subset_species <- recordTable[recordTable[,speciesCol] == species,]

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
    if("try-error" %in% class(try_error_tmp)) warning(paste(toupper(species), ": ", try_error_tmp[1], "    - SKIPPED", sep = ""), call. = FALSE)
    
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
        if("try-error" %in% class(try_error_tmp)) warning(paste(toupper(spec.tmp), ": ", try_error_tmp[1], "    - SKIPPED", sep = ""), call. = FALSE)
      }
      
      
      subset_species_list[[i]] <- subset_species$Time.rad
      names(subset_species_list)[i] <- spec.tmp
    }
  }
  
  if(allSpecies == FALSE){
    return(invisible(subset_species$Time.rad))
  } else {
    return(invisible(subset_species_list))
  }
}
