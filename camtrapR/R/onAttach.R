.onAttach <- function(...){
  # adapted from http://thecoatlessprofessor.com/programming/automatically-check-if-r-package-is-the-latest-version-on-package-load/. Thank you!

  # Avoid running if in batch job / user not present
  if (!interactive()) return()
  
  # Obtain the installed package information
  local_version = utils::packageDescription('camtrapR')
  
  # Grab the package information from CRAN
  cran_version = .pkgVersionCRAN("camtrapR")     # function defined in "variousOtherHelperFunctions.R"
  
  # Verify we have package information
  if(!is.null(cran_version) && length(cran_version) != 0L){
    latest_version = utils::compareVersion(cran_version, local_version$Version)
    
    d = if(latest_version == 0){
      'CURRENT'
    }else if(latest_version == 1){
      'OUT OF DATE'
    }else{
      'DEVELOPMENT'
    }
  
  }else{ # Gracefully fail.
   d2 = "cannot connect to CRAN for package version check"
   latest_version = 0
  }
  
  # Use packageStartUpMessages() so that folks can suppress package messages with 
  # suppressPackageStartupMessages(library(pkg))
  
  if(exists("d"))  packageStartupMessage('This is camtrapR version: ', local_version$Version, ' (', d,')')  # built on ', local_version$Date)
  if(exists("d2")) packageStartupMessage('This is camtrapR version: ', local_version$Version, '    (', d2,')')  # built on ', local_version$Date)
  
  
  if(latest_version == 1){
    packageStartupMessage('\n NEW VERSION AVAILABLE ONLINE: ', cran_version , ' !')
    packageStartupMessage('Download it from CRAN via:    install.packages("camtrapR")\n')
  }
}

