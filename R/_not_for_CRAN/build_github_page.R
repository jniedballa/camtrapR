library(pkgdown)
require(devtools)


Sys.setlocale("LC_ALL","English")


build_home()
#pkgdown::build_site()   # build whole github page

# NEWS section
build_news()

# all vignettes 
build_articles()   

# for individual vignettes
build_article(name = "camtrapr1") 
build_article(name = "camtrapr2")
build_article(name = "camtrapr3")
build_article(name = "camtrapr4")
build_article(name = "camtrapr5")
build_article(name = "camtrapr6")

build_articles_index()

build_manual()

 
build_reference()  # function documentation
build_reference_index(pkg = ".")


