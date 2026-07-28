
# camtrapR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-ago/camtrapR)](https://CRAN.R-project.org/package=camtrapR)
![](https://cranlogs.r-pkg.org/badges/grand-total/camtrapR)
[![](http://cranlogs.r-pkg.org/badges/camtrapR)](https://cran.r-project.org/package=camtrapR)
[![Codecov test
coverage](https://codecov.io/gh/jniedballa/camtrapR/graph/badge.svg)](https://app.codecov.io/gh/jniedballa/camtrapR)
[![R-CMD-check](https://github.com/jniedballa/camtrapR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jniedballa/camtrapR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

camtrapR is a package for camera trap data management in R.

## Features

- **Data management** — Build record tables from raw camera trap
  images/videos using EXIF metadata
- **Data import** — Import data from external platforms (Wildlife
  Insights, Camtrap DP, csv files)
- **Survey dashboard** — `surveyDashboard()` provides a code-free
  graphical user interface for data exploration and occupancy modelling
- **Activity patterns** — Kernel density estimation and comparison of
  temporal activity
- **Occupancy models** — Prepare and run single-species and Bayesian
  community occupancy models (via `unmarked`, `ubms`, JAGS or NIMBLE)
- **Spatial capture-recapture** — Generate input for SCR analyses (with
  the `secr` package)
- **Spatial covariates** — Extract environmental covariates and predict
  species occupancy across space
- **Visualisation** — Maps, activity plots, and survey summary figures

## Installation

Install the release version from CRAN:

``` r
install.packages("camtrapR")
```

Install the GitHub master version:

``` r
# install.packages("remotes")
# install.packages("R.rsp")
remotes::install_github("jniedballa/camtrapR", build_vignettes = TRUE)
```

Install the GitHub development version (most recent changes and new
features):

``` r
remotes::install_github("jniedballa/camtrapR", ref = "dev", build_vignettes = TRUE)
```

> **Note:** For installation from GitHub, package `R.rsp` is required.
> `build_vignettes = TRUE` is necessary due to the use of static
> vignettes.

You can also install a specific previous version (e.g., if a recent
update broke code you need):

``` r
remotes::install_version(package = "camtrapR", version = "3.0.1")
```

## Exiftool

Many camtrapR functions read EXIF metadata from JPG images and videos
via [**Exiftool**](https://exiftool.sourceforge.net/), a free and
open-source tool by Phil Harvey (available for Windows, macOS, and
Linux).

To make full use of camtrapR, you will need Exiftool on your system. See
the installation instructions in [vignette
1](https://CRAN.R-project.org/package=camtrapR/vignettes/camtrapr1.pdf)
and the [Exiftool website](https://exiftool.sourceforge.net/).

> If you only use camtrapR to create model input from existing record
> tables (without working with raw image files), Exiftool is not
> required.

## Vignettes

camtrapR includes six vignettes covering the entire workflow:

1.  **Camera trap data management**
2.  **Species and individual identification**
3.  **Record tables and model input**
4.  **Maps, species activity and reports**
5.  **Community Occupancy models**
6.  **Sample data sets and the surveyDashboard()**

Browse them online at <https://cran.r-project.org/package=camtrapR> or
locally after installation with `browseVignettes("camtrapR")`.

## Help and Support

- **Google Group**:
  [camtrapR](https://groups.google.com/forum/#!forum/camtrapr) — for all
  questions and problems around the use of `camtrapR`
- **GitHub Issues**: [Report bugs or request
  features](https://github.com/jniedballa/camtrapR/issues)

## Further reading

Niedballa, J., Sollmann, R., Courtiol, A., Wilting, A. (2016). camtrapR:
an R package for efficient camera trap data management. *Methods in
Ecology and Evolution*, 7(12), 1457–1462.
<https://doi.org/10.1111/2041-210X.12600>

Niedballa, J., Sollmann, R., Wilting, A. (2025) The camtrapR R package:
From data management to interactive ecological analysis of camera trap
data. *bioRxiv* 2025.09.26.678697; doi:
<https://doi.org/10.1101/2025.09.26.678697>

## Citation

To cite camtrapR in publications, please use:

> Niedballa, J., Sollmann, R., Courtiol, A., Wilting, A. (2016).
> camtrapR: an R package for efficient camera trap data management.
> *Methods in Ecology and Evolution* 7(12), 1457–1462.
> <https://doi.org/10.1111/2041-210X.12600>

or run `citation("camtrapR")` in R.

## License

camtrapR is released under the GPL-2 license.
