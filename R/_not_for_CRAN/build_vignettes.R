
library(rmarkdown)

# building html vignette with code below ensures that toc works correctly
wd <- "C:/Users/Juergen/Documents/GitHub/camtrapR/vignettes"

render(input = file.path(wd, "camtrapr1.Rmd"),
       #output = file.path(wd, "camtrapR5.html")
       output_format = "pdf_document",
       output_options = list(#self_contained = T,
         toc = T,
         toc_depth = 2,
         toc_float = T))

render(input = file.path(wd, "camtrapr2.Rmd"),
       #output = file.path(wd, "camtrapR5.html")
       output_format = "pdf_document",
       output_options = list(#self_contained = T,
         toc = T,
         toc_depth = 2,
         toc_float = T))

render(input = file.path(wd, "camtrapr3.Rmd"),
       #output = file.path(wd, "camtrapR5.html")
       output_format = "pdf_document",
       output_options = list(#self_contained = T,
         toc = T,
         toc_depth = 2,
         toc_float = T))

render(input = file.path(wd, "camtrapr4.Rmd"),
       #output = file.path(wd, "camtrapR5.html")
       output_format = "pdf_document",
       output_options = list(#self_contained = T,
         toc = T,
         toc_depth = 2,
         toc_float = T))



# vignette 5 is html, not pdf

render(input = file.path(wd, "camtrapr5.Rmd"),
       #output = file.path(wd, "camtrapR5.html")
       output_format = "html_document",
       output_options = list(#self_contained = T,
         toc = T,
         toc_depth = 2,
         toc_float = T))
