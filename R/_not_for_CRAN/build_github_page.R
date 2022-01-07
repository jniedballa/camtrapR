library(pkgdown)
require(devtools)


Sys.setlocale("LC_ALL","English")


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



library(rmarkdown)

# building html vignette with code below ensures that toc works correctly
wd <- "C:/Users/niedballa/Documents/GitHub/camtrapR/vignettes"
render(input = file.path(wd, "camtrapr5.Rmd"),
     #output = file.path(wd, "camtrapR5.html")
     output_format = "html_document",
     output_options = list(#self_contained = T,
                           toc = T,
                           toc_depth = 2,
                           toc_float = T))
