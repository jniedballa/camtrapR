context("recordTable")
library(camtrapR)

# run recordTable

wd_images_ID_species <- system.file("pictures/sample_images", package = "camtrapR")

rec_table0 <- recordTable(inDir               = wd_images_ID_species,
                          IDfrom                 = "directory",
                          timeZone = "UTC")


  rec_table1 <- recordTable(inDir               = wd_images_ID_species,
                            IDfrom                 = "directory",
                            minDeltaTime           = 60,
                            deltaTimeComparedTo    = "lastRecord",
                            writecsv               = FALSE,
                            additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
                            timeZone               = "Asia/Kuala_Lumpur"
  )

  
  # with additionalMetadataTags, event Summary and exclude
  
  rec_table2_1 <- recordTable(inDir               = wd_images_ID_species,
                            IDfrom                 = "directory",
                            minDeltaTime           = 60,
                            deltaTimeComparedTo    = "lastRecord",
                            exclude                = "NO_ID",
                            writecsv               = FALSE,
                            timeZone               = "Asia/Kuala_Lumpur",
                            additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
                            eventSummaryColumn     = "EXIF:Make",
                            eventSummaryFunction   = "unique"
  )
  
  # with additionalMetadataTags and multiple event summaries
  
  rec_table2_2 <- recordTable(inDir               = wd_images_ID_species,
                            IDfrom                 = "directory",
                            minDeltaTime           = 60,
                            deltaTimeComparedTo    = "lastRecord",
                            exclude                = "NO_ID",
                            writecsv               = FALSE,
                            timeZone               = "Asia/Kuala_Lumpur",
                            additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
                            eventSummaryColumn     = c("EXIF:Make", "EXIF:Model"),
                            eventSummaryFunction   = c("unique", "unique")
  )
  
  
  # with removeDuplicateRecords
  
   rec_table3a <- recordTable(inDir              = wd_images_ID_species,
                             IDfrom                 = "directory",
                             minDeltaTime           = 0,
                             exclude                = "NO_ID",
                             timeZone               = "Asia/Kuala_Lumpur",
                             removeDuplicateRecords = FALSE
  )
  
  rec_table3b <- recordTable(inDir              = wd_images_ID_species,
                             IDfrom                 = "directory",
                             minDeltaTime           = 0,
                             exclude                = "NO_ID",
                             timeZone               = "Asia/Kuala_Lumpur",
                             removeDuplicateRecords = TRUE
  )
  
  
  
# Test section
  
test_that("recordTable output has correct class", {
  expect_is(rec_table0,   "data.frame")
  expect_is(rec_table1,   "data.frame")
  expect_is(rec_table2_1, "data.frame")
  expect_is(rec_table2_2, "data.frame")
  expect_is(rec_table3a,  "data.frame")
  expect_is(rec_table3b,  "data.frame")
})

test_that("recordTable output has correct dimensions", {
  expect_equal(dim(rec_table0),   c(56,12))
  expect_equal(dim(rec_table1),   c(40,14))
  expect_equal(dim(rec_table2_1), c(39,15))
  expect_equal(dim(rec_table2_2), c(39,16))
  expect_equal(dim(rec_table3a),  c(67,12))
  expect_equal(dim(rec_table3b),  c(55,12))
})

test_that("removeDuplicateRecords works", {
  expect_equal(anyDuplicated(rec_table3a[, c("Station", "Species", "DateTimeOriginal")]), 12)   # 12 duplicates
  expect_equal(anyDuplicated(rec_table3b[, c("Station", "Species", "DateTimeOriginal")]), 0)    # 0 duplicates
})

test_that("errors are correct", {
  expect_error(recordTable(inDir               = wd_images_ID_species,
                           IDfrom                 = "Directory",
                           timeZone               = "Asia/Kuala_Lumpur"), 
               "'arg' should be one of")
  expect_error(recordTable(inDir               = wd_images_ID_species,
                           IDfrom              = "directory",
                           cameraID            = "directory",
                           timeZone               = "Asia/Kuala_Lumpur"), 
               "camerasIndependent is not defined. It must be defined if cameraID is defined")
  expect_error(recordTable(inDir               = wd_images_ID_species,
                           IDfrom              = "directory",
                           cameraID            = "Filename",
                           camerasIndependent  = TRUE,
                           timeZone               = "Asia/Kuala_Lumpur"), 
               "'arg' should be one of")
})


test_that("warnings are correct", {
  
  expect_warning(recordTable(inDir               = wd_images_ID_species,
                             IDfrom                 = "directory"),
                 "timeZone is not specified. Assuming UTC")
  
  expect_warning(recordTable(inDir               = wd_images_ID_species,
                             IDfrom                 = "directory",
                             additionalMetadataTags = c("EXIF:Model", "NonExistingTag"),
                             timeZone               = "Asia/Kuala_Lumpur"),
                 " not found in image metadata:  NonExistingTag")
  
})
