library(pkgdown)
require(devtools)


# build_home()
pkgdown::build_site()   # build whole github page

build_news()       # NEWS section
build_articles()   # all vignettes 
build_article(name = "camtrapr1")   # is for individual vignettes
build_reference()  # funcion documentation
