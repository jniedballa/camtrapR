context("imageRename")
library(camtrapR)


wd_images_ID_species <- system.file("pictures/sample_images_species_dir", package = "camtrapR")


test_that("imageRename throws error if inDir doesn't exist", {
  expect_error(imageRename(inDir = paste(wd_images_ID_species, "blabla", sep = "")),
               "Could not find inDir")
})


test_that("imageRename throws error if inDir doesn't have subdirectories", {
  expect_error(imageRename(inDir = list.dirs(wd_images_ID_species)[3]),   # pointing to StationA/PBE, which doesn't have subdirectories
               "inDir contains no station directories")
})