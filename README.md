
<!-- README.md is generated from README.Rmd. Please edit that file -->

# camtrapR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/camtrapR)](https://CRAN.R-project.org/package=camtrapR)
![](https://cranlogs.r-pkg.org/badges/grand-total/camtrapR)
[![](http://cranlogs.r-pkg.org/badges/camtrapR)](https://cran.r-project.org/package=camtrapR)
<!-- [![Build Status](https://travis-ci.org/jniedballa/camtrapR.svg?branch=master)](https://travis-ci.org/jniedballa/camtrapR) -->
<!-- badges: end -->

camtrapR is a package for camera trap data management in R.

## Installation

You can install the release version of camtrapR from CRAN:

``` r
install.packages("camtrapR")
```

or the  GitHub version (containing recent changes and new
features) via:

``` r
# install.packages("remotes")
# install.packages("R.rsp")
remotes::install_github("jniedballa/camtrapR", build_vignettes = TRUE)
```

or from the dev branch for the latest development version (may not always be available).
``` r
remotes::install_github("jniedballa/camtrapR", ref = "dev", build_vignettes = TRUE)
```

For installation from GitHub make sure package `R.rsp` is available.
`build_vignettes = TRUE` is necessary since v 2.1.1. due to the new
static vignettes.

You can also install a previous version (e.g. if a recent update broke
the code you need to run urgently) via:

``` r
remotes::install_version(package = "camtrapR", version = "2.0.3")
```

## Exiftool

Numerous important camtrapR functions read EXIF metadata from JPG images
(and videos). This is done via Exiftool, a free and open-source sofware
tool developed by Phil Harvey and available for Windows, MacOS and
Linux.

To make full use of camtrapR, you will need Exiftool on your system. You
can download it from the [Exiftool homepage](https://exiftool.org/).
Please follow the installation instruction in vignette 1 and the
Exiftool website.

You may not need Exiftool if you do not work with image files, but only
use camtrapR to create input for occupancy or spatial capture-recapture
models from existing record tables.

## Help and Support

### Google Group

There is a [Google
Group](https://groups.google.com/forum/#!forum/camtrapr) for all
questions and problems regarding camtrapR. Please feel free to join the
discussion there.

### GitHub issues

Alternatively, you can report bugs and problems as issues on GitHub
[(here)](https://github.com/jniedballa/camtrapR/issues).

## Further reading

See the [Article in Methods in Ecology and
Evolution](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12600)
for an overview of the package. The five vignettes provide examples for
the entire workflow.

## Citation

To cite camtrapR, please use:

Jürgen Niedballa, Rahel Sollmann, Alexandre Courtiol, Andreas Wilting
(2016). camtrapR: an R package for efficient camera trap data
management. Methods in Ecology and Evolution 7(12), 1457-1462,
<https://doi.org/10.1111/2041-210X.12600>, URL:
<https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12600>

or see `citation("camtrapR")`
