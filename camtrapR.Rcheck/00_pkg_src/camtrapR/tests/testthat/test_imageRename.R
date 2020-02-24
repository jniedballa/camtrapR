context("imageRename")
library(camtrapR)


wd_images_ID_species <- system.file("pictures/sample_images", package = "camtrapR")


test_that("imageRename throws error if no station directories", {
  
  # expect_error(imageRename(list.dirs(wd_images_ID_species)[2])
  #              "inDir contains no station directories"
  
  
})