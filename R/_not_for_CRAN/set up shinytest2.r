# set up dashboard tests with shinytest2

library(shinytest2)
library(here)
# 1. create app R files in /tests/testthat/apps
# one possible approach is one folder for each configuration

load_timeout <- 30 * 1000  # 20 seconds

# launch app in test recorder:

# from blank dashboard
record_test(app = here("tests/testthat/apps/surveyDashboard_empty"),
                      load_timeout = load_timeout)

# using built-in dataset
record_test(app = here("tests/testthat/apps/surveyDashboard_builtin"),
                      load_timeout = load_timeout)

# using camtrapDP sample data
record_test(app = here("tests/testthat/apps/surveyDashboard_camtrapdp"),
                      load_timeout = load_timeout)

# using a synthetic dataset (2 cameras per station, single season)
record_test(app = here("tests/testthat/apps/surveyDashboard_simulated_2cam"),
                      load_timeout = load_timeout)

# save test in recorder
# will add named testthat blocks in e.g. tests/testthat/test-app/surveyDashboard_builtin.R