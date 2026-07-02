library(shinytest2)

test_that("{shinytest2} recording: surveyDashboard_camtrapdp_launch", {
  local_app_support(test_path("apps/surveyDashboard_camtrapdp"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_camtrapdp"), name = "surveyDashboard_camtrapdp_launch", 
      height = 993, width = 1619, load_timeout = 30000)
  app$expect_values()
})
