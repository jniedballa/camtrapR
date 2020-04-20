library(testthat)
# test only if exiftool is available
if(Sys.which("exiftool") != ""){
  test_check("camtrapR")
}
