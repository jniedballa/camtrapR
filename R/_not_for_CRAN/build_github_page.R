library(pkgdown)
require(devtools)


# build_home()
pkgdown::build_site()   # build whole github page

build_news()       # NEWS section
build_article()    # vignettes
build_reference()  # funcion documentation
