library(shinytest2)



test_that("dashboard_camtrapdp_launch", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_camtrapdp"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_camtrapdp"), name = "app_camtrapdp_start", 
      height = 993, width = 1619, load_timeout = 30000)
  withr::defer(app$stop()) 
  
  app$wait_for_idle()
  app$expect_values()
})




test_that("{shinytest2} recording: surveyDashboard_camtrapdp_maps", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_camtrapdp"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_camtrapdp"), name = "surveyDashboard_camtrapdp_maps", 
      height = 993, width = 1619, load_timeout = 30000)
  
  withr::defer(app$stop()) 
  
  app$set_inputs(sidebarItemExpanded = "Maps")
  app$set_inputs(species_for_map = "beech marten")
  app$wait_for_idle()
  app$set_inputs(no_record_more_transparent = FALSE)
  app$set_inputs(no_record_more_transparent = TRUE)
  app$set_inputs(scale_size = FALSE)
  app$set_inputs(scale_size = TRUE)
  app$expect_values()
})
  