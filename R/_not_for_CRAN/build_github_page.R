library(pkgdown)
require(devtools)


Sys.setlocale("LC_ALL","English")
Sys.setenv(TZ = "UTC")


build_home()
#pkgdown::build_site()   # build whole github page

build_news()       # NEWS section
build_articles()   # all vignettes 
build_article(name = "camtrapr1")   # is for individual vignettes
build_article(name = "camtrapr5")   # is for individual vignettes
build_articles_index()


build_manual()

 
build_reference()  # function documentation
build_reference_index(pkg = ".")


