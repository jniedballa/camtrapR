library(rhub)
#validate_email()

rhub::check()

check_on_linux() 
check_on_windows()

check_for_cran()

previous_checks <- rhub::list_package_checks(".",
                                             howmany = 4)
previous_checks
