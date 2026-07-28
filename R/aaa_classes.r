
## --- S4 registration ------------------------------------------------------
## Register the S3 class chain with S4 so that S4 is()/validity checks
## (e.g. unmarked's unmarkedFrame validity) recognise "cams" objects as
## data frames. The registered chain MUST match class(<cams object>) exactly.
methods::setOldClass(c("cams", "data.frame"))
methods::setOldClass(c("records", "data.frame"))


## --- Constructors ----------------------------------------------------------
## Internal helper. Guarantees a plain base data.frame underneath so that
## (i) the class chain always matches the setOldClass() registration and
## (ii) base-R subsetting semantics apply (tibble/data.table differ).
as_cams <- function(x, stationCol) {
  x <- as.data.frame(x)                # demotes tibble/data.table/anything
  class(x) <- c("cams", "data.frame")
  attr(x, "stationCol") <- stationCol
  x
}

as_cams_dp <- function(x) {
  class(x) <- unique(c("cams_dp", class(x)))
  x
}

as_records <- function(x, stationCol, speciesCol) {
  x <- as.data.frame(x)
  # declare specific class and store attributes
  class(x) <- unique(c("records", class(x)))
  attr(x, "stationCol") <- stationCol
  attr(x, "speciesCol") <- speciesCol
  x
}

as_camop <- function(x) {
  x <- matrix(x)     
  class(x) <- unique(c("camOp", class(x)))
  x
}


as_dethist <- function(list, stationCol, speciesCol, recordDateTimeCol) {
    class(list) <- unique(c("detHist", class(list)))
    attr(list, "stationCol") <- stationCol
    attr(list, "speciesCol") <- speciesCol
    attr(list, "recordDateTimeCol") <- recordDateTimeCol
    list
  }


## --- S3 methods -----------------------------------------------------------

### cams ----


#' Printing method for camera table objects
#' 
#' @export
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @method print cams
#' @keywords internal
print.cams <- function(x, ...) {
  
  station.col <- attr(x, "stationCol")
  
  has.cols <- !is.null(station.col) && station.col %in% names(x)
  
  if (!has.cols) {
    print(tibble::as_tibble(x))
    return(invisible(x))
  }
  
  n.station <- length(unique(x[[station.col]]))
  n.rows <- nrow(x)
  station.wording <- if (n.station > 1) " stations" else " station"
  record.wording  <- if (n.rows > 1) " rows" else " row"
  message(crayon::cyan("Camera table"), " with ",
          crayon::blue(n.station), station.wording, 
          " in ", crayon::blue(n.rows), record.wording, ":")
  # TODO: include information about cameraCol / sessionCol, if available
  
  # TODO: Not ideal that print() shows "# A tibble: ", when the class is "data.frame". Might confuse users since tibble is only applied for printing-
  
  print(tibble::as_tibble(x), ...)
  invisible(x)
}


### cams_dp ----


#' Printing method for Camtrap DP objects
#' 
#' @export
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @method print cams_dp
#' @keywords internal
print.cams_dp <- function(x, ...) {
  
  ## prevent nested pillar advice
  old.opt <- options(pillar.advice = FALSE)
  on.exit(options(old.opt))
  
  message(crayon::cyan("Camtrap DP"), " list with the following elements:")
  
  if ("CTtable" %in% names(x)) {
    cat("$CTtable\n")
    print(x$CTtable)
    cat("\n")
  }
  if ("recordTable" %in% names(x)) {
    cat("$recordTable\n")
    print(x$recordTable)
    cat("\n")
  }
  if ("metadata" %in% names(x)) {
    cat("$metadata\n")
    message(crayon::cyan("Metadata"), " available:")
    print(names(x$metadata))
  }
  invisible(x)
}



### records ----


#' Printing method for record tables
#' 
#' @export
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @method print records
#' @keywords internal
print.records <- function(x, ...) {
  
  species.col <- attr(x, "speciesCol")
  station.col <- attr(x, "stationCol")
  
  has.cols <- !is.null(species.col) && species.col %in% names(x) &&
    !is.null(station.col) && station.col %in% names(x)
  
  if (!has.cols) {
    print(tibble::as_tibble(x))
    return(invisible(x))
  }
  
  n.species <- length(unique(x[[species.col]]))
  n.station <- length(unique(x[[station.col]]))
  n.record <- nrow(x)
  station.wording <- if (n.station > 1) " stations" else " station"
  record.wording  <- if (n.record > 1) " records" else " record"
  message(crayon::cyan("Record table"), " based on ",
          crayon::blue(n.station), station.wording, 
          " and ", crayon::blue(n.record), record.wording, " of a total of ", crayon::blue(n.species), " species:")
  print(tibble::as_tibble(x))
  invisible(x)
}


#' Wrapper to discard class in dplyr
#' 
#' This function is called internally by dplyr. It prevents the propagation of
#' attributes dependent on columns when such columns are filtered out.
#' 
#' @export
#' @keywords internal
#' @inheritParams dplyr::dplyr_reconstruct
#' @importFrom dplyr dplyr_reconstruct
dplyr_reconstruct.records <- function(data, template) {
  species.col  <- attr(template, "speciesCol")
  station.col  <- attr(template, "stationCol")
  
  keeps.class <- !is.null(species.col)  && species.col %in% names(data) &&
    !is.null(station.col) && station.col %in% names(data)
  
  if (keeps.class) {
    attr(data, "speciesCol")  <- species.col
    attr(data, "stationCol")  <- station.col
    class(data) <- class(template)
    data
  } else {
    # drop back to a plain tibble/data.frame — no more "records" class
    if (inherits(data, "tbl_df")) {
      tibble::as_tibble(data)
    } else {
      as.data.frame(data)
    }
  }
}


#' Subsetting method for record tables
#' 
#' Ensures the \code{records} class (and its attributes) is only retained when 
#' the columns they point to are still present after subsetting. This covers 
#' base R subsetting (\code{x[...]}), which does not go through dplyr's 
#' \code{dplyr_reconstruct} machinery.
#' 
#' @export
#' @param x an object used to select a method
#' @param i row index
#' @param j column index
#' @param drop logical, whether to simplify to a vector when selecting a single column
#' @param ... further arguments passed to or from other methods
#' @method [ records
#' @keywords internal
`[.records` <- function(x, i, j, ..., drop) {
  
  species.col <- attr(x, "speciesCol")
  station.col <- attr(x, "stationCol")
  
  out <- NextMethod()   # let [.data.frame do the real subsetting
  
  # drop = TRUE with a single selected column returns an atomic vector,
  # not a data.frame -- nothing to reconstruct
  if (!is.data.frame(out)) return(out)
  
  keeps.class <- !is.null(species.col) && species.col %in% names(out) &&
    !is.null(station.col) && station.col %in% names(out)
  
  if (keeps.class) {
    attr(out, "speciesCol") <- species.col
    attr(out, "stationCol") <- station.col
    class(out) <- class(x)
  } else {
    class(out) <- setdiff(class(out), "records")
    attr(out, "speciesCol") <- NULL
    attr(out, "stationCol") <- NULL
  }
  
  out
}


#' Summary method for record tables
#' 
#' @export
#' @param object an object of class \code{records}
#' @param nSpeciesMax integer for the maximum number of species to show in the
#'   per-species detection table (defaults = 5)
#' @param ... further arguments passed to or from other methods
#' @return the species table (returned invisibly and unfiltered)
#' @method summary records
#' @keywords internal
summary.records <- function(object, nSpeciesMax = 5, ...) {
  
  species.col <- attr(object, "speciesCol")
  station.col <- attr(object, "stationCol")
  
  has.cols <- !is.null(species.col) && species.col %in% names(object) &&
    !is.null(station.col) && station.col %in% names(object)
  
  if (!has.cols) {
    stop("'object' no longer has the species/station columns needed to summarise it as a records table (likely lost through subsetting).", call. = FALSE)
  }
  
  n.species <- length(unique(object[[species.col]]))
  n.station <- length(unique(object[[station.col]]))
  n.record  <- nrow(object)
  
  # TABLE SPECIFICATION section
  cat("=== Record Table Summary ===\n\n")
  cat("TABLE SPECIFICATION\n")
  cat(sprintf("  Species column:  %s\n", species.col))
  cat(sprintf("  Station column:  %s\n", station.col))
  cat("\n")
  
  # DIMENSIONS section
  cat("DIMENSIONS\n")
  cat(sprintf("  Species:  %d\n", n.species))
  cat(sprintf("  Stations: %d\n", n.station))
  cat(sprintf("  Records:  %d\n\n", n.record))
  
  # PERIOD section (if Date is available)
  if ("Date" %in% names(object) && !all(is.na(object$Date))) {
    date.range <- range(object$Date, na.rm = TRUE)
    cat("PERIOD\n")
    cat(sprintf("  From: %s\n", format(date.range[1])))
    cat(sprintf("  To:   %s\n\n", format(date.range[2])))
  }
  
  # SPECIES DETECTIONS section
  cat("SPECIES DETECTIONS\n")
  
  sp.tbl <- as.data.frame(table(object[[species.col]]), stringsAsFactors = FALSE)
  names(sp.tbl) <- c("Species", "Records")
  sp.tbl$Stations <- vapply(sp.tbl$Species, function(sp) {
    length(unique(object[[station.col]][object[[species.col]] == sp]))
  }, integer(1))
  sp.tbl <- sp.tbl[order(-sp.tbl$Records), ]
  rownames(sp.tbl) <- NULL
  
  n.subset  <- min(nrow(sp.tbl), nSpeciesMax)
  sp.shown  <- sp.tbl[seq_len(n.subset), , drop = FALSE]
  
  # column widths (max content length + padding), mirroring print_covariate_table
  sp.width   <- max(nchar("Species"),  nchar(sp.shown$Species)) + 2
  rec.width  <- max(nchar("Records"),  nchar(as.character(sp.shown$Records))) + 2
  stat.width <- max(nchar("Stations"), nchar(as.character(sp.shown$Stations))) + 2
  
  header_fmt <- paste0("    %-", sp.width, "s %-", rec.width, "s %-", stat.width, "s\n")
  
  cat(sprintf(header_fmt, "Species", "Records", "Stations"))
  separator <- paste(rep("-", sp.width + rec.width + stat.width + 2), collapse = "")
  cat(paste0("    ", separator, "\n"))
  
  for (i in seq_len(nrow(sp.shown))) {
    cat(sprintf(header_fmt, sp.shown$Species[i], sp.shown$Records[i], sp.shown$Stations[i]))
  }
  cat("\n")
  
  if (nrow(sp.tbl) > n.subset) {
    cat(sprintf(crayon_grey_0.6("  ... %d more species not shown (use `summary(nSpeciesMax = ...)` to see more)\n\n"),
                nrow(sp.tbl) - n.subset))
  }
  
  invisible(sp.tbl)
}


### camera Operation ----



#' Printing method for camera trap station operation matrix
#' 
#' @export
#' @param x an object used to select a method
#' @param nRows the number of first and last rows to display
#' @param nCols the number of first and last columns to display
#' @param digits the number of digits to use within cells
#' @param ... further arguments passed to or from other methods
#' @method print camOp
#' @keywords internal
print.camOp <- function(x, nRows = 4, nCols = 3, digits = 3, ...) {
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  ## reformat matrix
  fmt <- showMatrixCorner(x, nRows = nRows, nCols = nCols, digits = digits)
  
  ## output
  active.days <- sum(colSums(x, na.rm = TRUE) > 0)
  
  station.wording <- if (nr > 1) " stations" else " station"
  date.wording  <- if (nc > 1) " days" else " day"
  message(crayon::cyan("Camera trap station operation matrix"), " based on\n",
          crayon::blue(nr), station.wording, " monitored from ", crayon::blue(attr(x, "from")),
          " till ", crayon::blue(attr(x, "to")), ",\n",
          "representing ", crayon::blue(nc), date.wording, " (" , crayon::blue(active.days), " active):")
  print(fmt, quote = FALSE, right = TRUE, ...)
  if (nr > 2 * nRows) {
    cat(crayon_grey_0.6("# Use `print(nRows = ...)` to see more rows\n"))
  }
  if (nc > 2 * nCols) {
    cat(crayon_grey_0.6("# Use `print(nCols = ...)` to see more columns\n"))
  }
  invisible(x)
}


#' Subsetting method for camera trap station operation matrices
#' 
#' Ensures that subsetting a \code{camOp} object (e.g. \code{x[1:5, ]},
#' \code{x[, 1:30]}) retains the \code{camOp} class and its provenance
#' attributes (\code{stationCol}, \code{cameraCol}, \code{sessionCol},
#' \code{setupCol}, \code{retrievalCol}). Unlike those, the \code{from}/\code{to}
#' attributes are recomputed from the remaining column names (dates) rather
#' than carried over unchanged, since they describe the date range actually
#' covered by \code{x} and would otherwise go stale after column subsetting.
#' 
#' @export
#' @param x an object of class \code{camOp}
#' @param i row index
#' @param j column index
#' @param drop logical, whether to simplify to a vector when selecting a single row/column
#' @param ... further arguments passed to or from other methods
#' @method [ camOp
#' @keywords internal
`[.camOp` <- function(x, i, j, ..., drop) {
  
  station.col   <- attr(x, "stationCol")
  camera.col    <- attr(x, "cameraCol")
  session.col   <- attr(x, "sessionCol")
  setup.col     <- attr(x, "setupCol")
  retrieval.col <- attr(x, "retrievalCol")
  
  out <- NextMethod()   # let the default matrix `[` do the real subsetting
  
  # drop = TRUE with a single row or column returns a plain vector, not
  # a matrix -- nothing to reconstruct in that case
  if (!is.matrix(out)) return(out)
  
  class(out) <- unique(c("camOp", class(out)))
  attr(out, "stationCol")   <- station.col
  attr(out, "cameraCol")    <- camera.col
  attr(out, "sessionCol")   <- session.col
  attr(out, "setupCol")     <- setup.col
  attr(out, "retrievalCol") <- retrieval.col
  
  # recompute from/to from the columns that actually remain, since column
  # names are dates -- carrying over the original from/to would misrepresent
  # the date range after column subsetting
  cn <- colnames(out)
  parsed <- if (!is.null(cn) && length(cn) > 0) {
    # colnames may carry a "+Xh" suffix when occasionStartTime != 0
    suppressWarnings(as.Date(sub("\\+[0-9]+h$", "", cn)))
  } else {
    NA
  }
  
  if (length(parsed) > 0 && !anyNA(parsed)) {
    attr(out, "from") <- min(parsed)
    attr(out, "to")   <- max(parsed)
  } else {
    # fall back to the original range if column names couldn't be parsed
    # as dates (e.g. after colnames<- was used to rename them)
    attr(out, "from") <- attr(x, "from")
    attr(out, "to")   <- attr(x, "to")
  }
  
  out
}


#' Summary method for camera trap station operation matrices
#' 
#' @export
#' @param object an object of class \code{camOp}
#' @param nStationsMax integer providing the maximum number of rows to show in
#'   the station-level table.
#' @param ... further arguments passed to or from other methods
#' @method summary camOp
#' @keywords internal

summary.camOp <- function(object, nStationsMax = 10, ...) {
  
  x  <- object
  nr <- nrow(x)
  nc <- ncol(x)
  
  # reuse the package's own rowname parser (station/session/camera)
  camop.info  <- deparseCamOpRownames(x)
  has.session <- "session" %in% colnames(camop.info)
  has.camera  <- "camera"  %in% colnames(camop.info)
  
  n.station <- length(unique(camop.info$station))
  n.session <- if (has.session) length(unique(camop.info$session)) else NA
  n.camera  <- if (has.camera)  length(unique(camop.info$camera))  else NA
  
  active.trap.days <- sum(x, na.rm = TRUE)
  active.days      <- sum(colSums(x, na.rm = TRUE) > 0)
  problem.days     <- sum(x == 0, na.rm = TRUE)
  notset.days      <- sum(is.na(x))
  total.cells      <- length(x)
  
  # SURVEY SPECIFICATION section
  cat("=== Camera Trap Station Operation Summary ===\n\n")
  cat("SURVEY SPECIFICATION\n")
  cat(sprintf("  Station column:    %s\n", attr(x, "stationCol")))
  if (has.camera)  cat(sprintf("  Camera column:     %s\n", attr(x, "cameraCol")))
  if (has.session) cat(sprintf("  Session column:    %s\n", attr(x, "sessionCol")))
  cat(sprintf("  Setup column:      %s\n", attr(x, "setupCol")))
  cat(sprintf("  Retrieval column:  %s\n\n", attr(x, "retrievalCol")))
  
  # PERIOD section
  cat("PERIOD\n")
  cat(sprintf("  From: %s\n", format(attr(x, "from"))))
  cat(sprintf("  To:   %s\n\n", format(attr(x, "to"))))
  
  # DIMENSIONS section
  cat("DIMENSIONS\n")
  cat(sprintf("  Stations: %d\n", n.station))
  if (has.camera)  cat(sprintf("  Cameras:  %d\n", n.camera))
  if (has.session) cat(sprintf("  Sessions: %d\n", n.session))
  cat(sprintf("  Days:     %d\n\n", nc))
  
  # EFFORT section
  cat("EFFORT\n")
  cat(sprintf("  Active days:            %d out of %d days (at least 1 station active)\n", active.days, nc))
  cat(sprintf("  Active trap-days:       %s (sum of effort across all stations and days)\n",
              formatC(active.trap.days, format = "f", digits = 1)))
  cat(sprintf("  Problem trap-days:      %d (%.1f%% of %d station-days)\n",
              problem.days, 100 * problem.days / total.cells, total.cells))
  cat(sprintf("  Not set up trap-days:   %d (%.1f%% of %d station-days)\n\n",
              notset.days, 100 * notset.days / total.cells, total.cells))
  
  # STATION-LEVEL SUMMARY table (one row per matrix row, i.e. per
  # station/session/camera combination as applicable)
  cat("STATION-LEVEL SUMMARY\n")
  
  row.tbl <- data.frame(Station = camop.info$station, stringsAsFactors = FALSE)
  if (has.session) row.tbl$Session <- camop.info$session
  if (has.camera)  row.tbl$Camera  <- camop.info$camera
  
  row.tbl$Monitored <- apply(x, 1, function(r) sum(!is.na(r)))
  row.tbl$Active    <- apply(x, 1, function(r) sum(r, na.rm = TRUE))
  row.tbl$Problem   <- apply(x, 1, function(r) sum(r == 0, na.rm = TRUE))
  
  n.subset  <- min(nrow(row.tbl), nStationsMax)
  row.shown <- row.tbl[seq_len(n.subset), , drop = FALSE]
  
  # build final display strings for EVERY column first, then measure widths
  # off those -- not off the raw (pre-formatting) values, since e.g. Active
  # sums of fractional effort can be long floating-point strings
  # (e.g. "19.85975") that would otherwise inflate the column width
  disp <- row.shown
  disp$Active <- formatC(row.shown$Active, format = "f", digits = 1)
  disp[] <- lapply(disp, as.character)
  
  col.widths <- vapply(names(disp), function(cn) {
    as.integer(max(nchar(cn), nchar(disp[[cn]])) + 2)}, integer(1))
  
  header_fmt <- paste0("    ", paste0("%-", col.widths, "s", collapse = " "), "\n")
  
  cat(do.call(sprintf, c(list(header_fmt), as.list(names(disp)))))
  separator <- paste(rep("-", sum(col.widths) + length(col.widths) - 1), collapse = "")
  cat(paste0("    ", separator, "\n"))
  
  for (i in seq_len(nrow(disp))) {
    cat(do.call(sprintf, c(list(header_fmt), as.list(unlist(disp[i, ])))))
  }
  cat("\n")
  
  if (nrow(row.tbl) > n.subset) {
    cat(sprintf(crayon_grey_0.6("  ... %d more row(s) not shown (use `summary(nStationsMax = ...)` to see more)\n\n"),
                nrow(row.tbl) - n.subset))
  }
  
  invisible(row.tbl)
}


#' Plotting method for camera trap station operation matrix
#' 
#' @export
#' @param x an object used to select a method
#' @param camOp alternative to `x` (for backward compatibility)
#' @param palette the color palette to use (default: `"viridis"`)
#' @param lattice whether or not to plot using lattice (default: `FALSE`)
#' @param ... further arguments passed to or from other methods
#' @method plot camOp
#' @keywords internal
plot.camOp <- function(x, camOp = NULL, palette = "viridis", lattice = FALSE, ...) {
  if (!is.null(camOp) & missing(x)) {
    x <- camOp
  }
  camopPlot(camOp = x, palette = palette, lattice = lattice)
}



### Detection history ----


#' Printing method for detection history
#' 
#' @export
#' @param x an object used to select a method
#' @param nRows the number of first and last rows to display
#' @param nCols the number of first and last columns to display
#' @param digits the number of digits to use within cells
#' @param ... further arguments passed to or from other methods
#' @method print detHist
#' @keywords internal
print.detHist <- function(x, nRows = 4, nCols = 3, digits = 3, ...) {
  message(crayon::cyan("Detection history list"))
  print(lapplyLeaf(x, \(m) showMatrixCorner(m, nRows = nRows, nCols = nCols, digits = digits)),
        quote = FALSE, right  = TRUE, ...)
  invisible(x)
} 



#' Subsetting method for detection history
#' 
#' Ensures that subsetting a \code{detHist} object (e.g. \code{x[1]},
#' \code{x["detection_history"]}) retains the \code{detHist} class and its
#' attributes, so the result still prints via \code{\link{print.detHist}}
#' instead of falling back to the default list print.
#' 
#' @export
#' @param x an object of class \code{detHist}
#' @param i index specifying elements to extract
#' @param ... further arguments passed to or from other methods
#' @method [ detHist
#' @keywords internal
`[.detHist` <- function(x, i, ...) {
  
  if (!missing(i)) {
    if (is.character(i)) {
      unmatched <- setdiff(i, names(x))
      if (length(unmatched) > 0) {
        stop("Undefined element(s) selected: ",
             paste(sQuote(unmatched), collapse = ", "),
             ". Available elements: ",
             paste(names(x), collapse = ", "), call. = FALSE)
      }
    } else if (is.numeric(i)) {
      i_pos <- i[i > 0]   # ignore negative (exclusion) indices
      if (length(i_pos) > 0 && any(i_pos > length(x))) {
        stop("Subscript out of bounds: detHist object has ", length(x),
             " element(s), requested index ", max(i_pos), ".", call. = FALSE)
      }
    }
  }
  
  station.col   <- attr(x, "stationCol")
  species.col   <- attr(x, "speciesCol")
  datetime.col  <- attr(x, "recordDateTimeCol")
  
  out <- NextMethod()   # default list subsetting: drops class/attributes
  
  # unlike records' speciesCol/stationCol, these attributes describe
  # provenance (which recordTable columns were used upstream), not live
  # references into x itself -- so they remain valid regardless of which
  # elements of the list were kept
  
  out <- as_dethist(out, 
                    stationCol = station.col,
                    speciesCol = species.col,
                    recordDateTimeCol = datetime.col)
  
  out
}


# TODO: Make print more informative (when including effort especially)
# TODO: Add summary method for dethist
