
# camtrapR

camtrapR is a package for camera trap data management in R.

## Installation

You can install the release version of camtrapR from CRAN:

``` r
install.packages("camtrapR")
```


You can also install a previous version (e.g. if a recent update broke
the code you need to run urgently) via:

``` r
remotes::install_version(package = "camtrapR", version = "2.3.1")
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

The original (non-anonymized) version of the package provides multiple ways of 
obtaining help from the package maintainers.

## Further reading

The five vignettes provide examples for the entire workflow.


