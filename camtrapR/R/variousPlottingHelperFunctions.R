# for function activityRadial

# the following 3 functions were adapted from package "plotrix" version 3.5-11
#   radial.plot
#   radial.grid
#   boxed.labels

# thanks to Jim Lemon et al.!


.radial.plot <- function (lengths, radial.pos = NULL, labels = NA, label.pos = NULL,
                          radlab = FALSE, start = 0, clockwise = FALSE, rp.type = "r",
                          label.prop = 1.1, main = "", xlab = "", ylab = "", line.col = par("fg"),
                          lty = par("lty"), lwd = par("lwd"), mar = c(2, 2, 3, 2),
                          show.grid = TRUE, show.grid.labels = 4, show.radial.grid = TRUE,
                          rad.col = "gray", grid.col = "gray", grid.bg = "transparent",
                          grid.left = FALSE, grid.unit = NULL, point.symbols = 1, point.col = par("fg"),
                          show.centroid = FALSE, radial.lim = NULL, radial.labels = NULL,
                          boxed.radial = TRUE, poly.col = NA, add = FALSE, ...)
{
  if (is.null(radial.lim))
    radial.lim <- range(lengths)
  length.dim <- dim(lengths)
  if (is.null(length.dim)) {
    npoints <- length(lengths)
    nsets <- 1
    lengths <- matrix(lengths, nrow = 1)
  }
  else {
    npoints <- length.dim[2]
    nsets <- length.dim[1]
    lengths <- as.matrix(lengths)
  }
  lengths <- lengths - radial.lim[1]
  lengths[lengths < 0] <- NA
  if (is.null(radial.pos))
    radial.pos <- seq(0, pi * (2 - 2 * (rp.type != "l")/npoints),
                      length.out = npoints)
  radial.pos.dim <- dim(radial.pos)
  if (is.null(radial.pos.dim))
    radial.pos <- matrix(rep(radial.pos, nsets), nrow = nsets,
                         byrow = TRUE)
  else radial.pos <- as.matrix(radial.pos)
  if (rp.type == "l") {
    clockwise <- TRUE
    start <- pi/2
  }
  if (clockwise)
    radial.pos <- -radial.pos
  if (start)
    radial.pos <- radial.pos + start
  if (show.grid) {
    if (length(radial.lim) < 3)
      grid.pos <- pretty(radial.lim)
    else grid.pos <- radial.lim
    if (grid.pos[1] < radial.lim[1])
      grid.pos <- grid.pos[-1]
    maxlength <- max(grid.pos - radial.lim[1])
    angles <- seq(0, 1.96 * pi, by = 0.04 * pi)
  }
  else {
    grid.pos <- NA
    maxlength <- diff(radial.lim)
  }
  oldpar <- par("xpd", "mar", "pty")
  if (!add) {
    par(mar = mar, pty = "s")
    plot(c(-maxlength, maxlength), c(-maxlength, maxlength),
         type = "n", axes = FALSE, main = main, xlab = xlab,
         ylab = ylab)
    if (is.null(label.pos)) {
      if (is.null(labels))
        nlpos <- ifelse(npoints > 8, 8, npoints)
      else {
        if (is.na(labels[1]))
          nlpos <- ifelse(npoints > 8, 8, npoints)
        else nlpos <- length(labels)
      }
      label.pos <- seq(0, pi * (2 - 2/nlpos), length.out = nlpos)
    }
    if (show.grid) {
      .radial.grid(labels = labels, label.pos = label.pos,
                   radlab = radlab, radial.lim = radial.lim, start = start,
                   clockwise = clockwise, label.prop = label.prop,
                   grid.pos = grid.pos, grid.col = grid.col, grid.bg = grid.bg,
                   show.radial.grid = show.radial.grid)
    }
  }
  par(xpd = TRUE)
  if (length(line.col) < nsets)
    line.col <- 1:nsets
  if (length(rp.type) < nsets)
    rp.type <- rep(rp.type, length.out = nsets)
  if (length(point.symbols) < nsets)
    point.symbols <- rep(point.symbols, length.out = nsets)
  if (length(point.col) < nsets)
    point.col <- rep(point.col, length.out = nsets)
  if (length(poly.col) < nsets)
    poly.col <- rep(poly.col, length.out = nsets)
  if (length(lty) < nsets)
    lty <- rep(lty, length.out = nsets)
  if (length(lwd) < nsets)
    lwd <- rep(lwd, length.out = nsets)
  for (i in 1:nsets) {
    if (nsets > 1) {
      linecol <- line.col[i]
      polycol <- poly.col[i]
      pointcol <- point.col[i]
      pointsymbols <- point.symbols[i]
      ltype <- lty[i]
      lwidth <- lwd[i]
    }
    else {
      linecol <- line.col
      polycol <- poly.col
      pointcol <- point.col
      pointsymbols <- point.symbols
      ltype <- lty
      lwidth <- lwd
    }
    rptype <- unlist(strsplit(rp.type[i], ""))
    if (match("s", rptype, 0)) {
      if (is.null(pointsymbols))
        pointsymbols <- i
      if (is.null(pointcol))
        pointcol <- i
    }
    xpos <- cos(radial.pos[i, ]) * lengths[i, ]
    ypos <- sin(radial.pos[i, ]) * lengths[i, ]
    if (match("r", rptype, 0))
      segments(0, 0, xpos, ypos, col = linecol, lty = ltype,
               lwd = lwidth, ...)
    if (match("p", rptype, 0))
      polygon(xpos, ypos, border = linecol, col = polycol,
              lty = ltype, lwd = lwidth, ...)
    if (match("s", rptype, 0))
      points(xpos, ypos, pch = pointsymbols, col = pointcol,
             ...)
    if (match("l", rptype, 0))
      lines(xpos, ypos, lty = ltype, lwd = lwidth, col = linecol,
            ...)
    if (show.centroid) {
      if (match("p", rptype, 0)) {
        nvertices <- length(xpos)
        polygonarea <- xpos[nvertices] * ypos[1] - xpos[1] *
          ypos[nvertices]
        for (vertex in 1:(nvertices - 1)) polygonarea <- polygonarea +
          xpos[vertex] * ypos[vertex + 1] - xpos[vertex +
                                                   1] * ypos[vertex]
        polygonarea <- polygonarea/2
        centroidx <- (xpos[nvertices] + xpos[1]) * (xpos[nvertices] *
                                                      ypos[1] - xpos[1] * ypos[nvertices])
        centroidy <- (ypos[nvertices] + ypos[1]) * (xpos[nvertices] *
                                                      ypos[1] - xpos[1] * ypos[nvertices])
        for (vertex in 1:(nvertices - 1)) {
          centroidx <- centroidx + (xpos[vertex] + xpos[vertex +
                                                          1]) * (xpos[vertex] * ypos[vertex + 1] -
                                                                   xpos[vertex + 1] * ypos[vertex])
          centroidy <- centroidy + (ypos[vertex] + ypos[vertex +
                                                          1]) * (xpos[vertex] * ypos[vertex + 1] -
                                                                   xpos[vertex + 1] * ypos[vertex])
        }
        points(centroidx/(6 * polygonarea), centroidy/(6 *
                                                         polygonarea), col = point.col[i], pch = point.symbols[i],
               cex = 2, ...)
      }
      else points(mean(xpos), mean(ypos), col = pointcol,
                  pch = pointsymbols, cex = 2, ...)
    }
  }
  if (show.grid.labels && !add) {
    if (show.grid.labels%%2) {
      ypos <- grid.pos - radial.lim[1]
      xpos <- rep(0, length(grid.pos))
      if (show.grid.labels == 1)
        ypos <- -ypos
    }
    else {
      xpos <- grid.pos - radial.lim[1]
      ypos <- rep(0, length(grid.pos))
      if (show.grid.labels == 2)
        xpos <- -xpos
    }
    if (is.null(radial.labels))
      radial.labels <- grid.pos
    if (!is.null(grid.unit))
      radial.labels[length(grid.pos)] <- paste(radial.labels[length(grid.pos)],
                                               grid.unit)
    if (boxed.radial)
      .boxed.labels(xpos, ypos, radial.labels, border = FALSE,
                    cex = par("cex.lab"))
    else text(xpos, ypos, radial.labels, cex = par("cex.lab"))
  }
  invisible(oldpar)
}



.radial.grid <- function (labels = NA, label.pos = NULL, radlab = FALSE, radial.lim = NULL,
                          start = 0, clockwise = FALSE, label.prop = 1.1, grid.pos,
                          grid.col = "gray", grid.bg = "transparent", show.radial.grid = TRUE)
{
  par(xpd = TRUE)
  if (is.null(label.pos))
    label.pos <- seq(0, 1.8 * pi, length = 9)
  if (!is.null(labels)) {
    if (is.na(labels[1]))
      labels <- as.character(round(label.pos, 2))
  }
  if (clockwise)
    label.pos <- -label.pos
  if (start)
    label.pos <- label.pos + start
  angles <- seq(0, 1.96 * pi, by = 0.04 * pi)
  for (i in seq(length(grid.pos), 1, by = -1)) {
    xpos <- cos(angles) * (grid.pos[i] - radial.lim[1])
    ypos <- sin(angles) * (grid.pos[i] - radial.lim[1])
    polygon(xpos, ypos, border = grid.col, col = grid.bg)
  }
  maxlength <- max(grid.pos) - radial.lim[1]
  if (show.radial.grid) {
    xpos <- cos(label.pos) * maxlength
    ypos <- sin(label.pos) * maxlength
    segments(0, 0, xpos, ypos, col = grid.col)
    xpos <- cos(label.pos) * maxlength
    ypos <- sin(label.pos) * maxlength
  }
  if (!is.null(labels)) {
    xpos <- cos(label.pos) * maxlength * label.prop
    ypos <- sin(label.pos) * maxlength * label.prop
    if (radlab) {
      for (label in 1:length(labels)) {
        if (radlab < 0)
          labelsrt <- 180 * label.pos[label]/pi - 90 +
            180 * (label.pos[label] > pi && label.pos[label] <
                     2 * pi)
        else labelsrt <- (180 * label.pos[label]/pi) +
            180 * (label.pos[label] > pi/2 && label.pos[label] <
                     3 * pi/2)
        text(xpos[label], ypos[label], labels[label],
             cex = par("cex.axis"), srt = labelsrt)
      }
    }
    else .boxed.labels(xpos, ypos, labels, ypad = 0.7, border = FALSE,
                       cex = par("cex.axis"))
  }
  par(xpd = FALSE)
}



.boxed.labels <- function (x, y = NA, labels, bg = ifelse(match(par("bg"), "transparent",
                                                                0), "white", par("bg")), border = TRUE, xpad = 1.2, ypad = 1.2,
                           srt = 0, cex = 1, adj = 0.5, xlog = FALSE, ylog = FALSE,
                           ...)
{
  oldpars <- par(c("cex", "xpd"))
  par(cex = cex, xpd = TRUE)
  if (is.na(y) && is.list(x)) {
    y <- unlist(x[[2]])
    x <- unlist(x[[1]])
  }
  box.adj <- adj + (xpad - 1) * cex * (0.5 - adj)
  if (srt == 90 || srt == 270) {
    bheights <- strwidth(labels)
    theights <- bheights * (1 - box.adj)
    bheights <- bheights * box.adj
    lwidths <- rwidths <- strheight(labels) * 0.5
  }
  else {
    lwidths <- strwidth(labels)
    rwidths <- lwidths * (1 - box.adj)
    lwidths <- lwidths * box.adj
    bheights <- theights <- strheight(labels) * 0.5
  }
  args <- list(x = x, y = y, labels = labels, srt = srt, adj = adj,
               col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) <
                              350, "white", "black"))
  args <- modifyList(args, list(...))
  if (xlog) {
    xpad <- xpad * 2
    xr <- exp(log(x) - lwidths * xpad)
    xl <- exp(log(x) + lwidths * xpad)
  }
  else {
    xr <- x - lwidths * xpad
    xl <- x + lwidths * xpad
  }
  if (ylog) {
    ypad <- ypad * 2
    yb <- exp(log(y) - bheights * ypad)
    yt <- exp(log(y) + theights * ypad)
  }
  else {
    yb <- y - bheights * ypad
    yt <- y + theights * ypad
  }
  rect(xr, yb, xl, yt, col = bg, border = border)
  do.call(text, args)
  par(cex = oldpars)
}

