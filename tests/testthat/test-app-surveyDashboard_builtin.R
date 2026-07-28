library(shinytest2)

load_timeout <- 30 * 1000  # 20 seconds




test_that("{shinytest2} recording: surveyDashboard_builtin_launch", {
  skip_on_cran()
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_launch",
      height = 993, width = 1619, load_timeout = load_timeout)
  
  # Ensure the R session closes when the test ends
  withr::defer(app$stop()) 
  
  # if reusing the same app across tests, use 
  # withr::defer(app$stop(), testthat::teardown_env())  once (kills app when test suite ends)
  
  app$expect_values()
})


test_that("{shinytest2} recording: surveyDashboard_builtin_filters", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_filters",
                       height = 993, width = 1619, load_timeout = load_timeout)
  
  withr::defer(app$stop()) 
  
  app$set_inputs(sidebarItemExpanded = "DataFilters")
  app$set_inputs(categoryValues = c("StationB", "StationC"))
  app$click("applyFilter_camtrap_properties")
  app$set_inputs(date_range_filter = c("2009-04-10", "2009-05-17"))
  app$click("apply_date_filter")
  app$click("removeSelectedSpecies")
  app$set_inputs(sidebarItemExpanded = "Tables")
  app$expect_values()
})


test_that("{shinytest2} recording: surveyDashboard_builtin_correlations", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_correlat",
      height = 993, width = 1619, load_timeout = 30000)

  withr::defer(app$stop()) 
  
  app$set_inputs(sidebarItemExpanded = "DataProcessing")
  app$set_inputs(correlationMethod = "spearman")
  app$set_inputs(correlationMethod = "kendall")
  app$set_inputs(corrplotMethod = "circle")
  app$set_inputs(corrplotMethod = "square")
  app$set_inputs(corrplotMethod = "ellipse")
  app$set_inputs(corrplotMethod = "shade")
  app$set_inputs(corrplotMethod = "pie")
  app$set_inputs(corrplotOrder = "original")
  app$set_inputs(corrplotOrder = "FPC")
  app$set_inputs(corrplotOrder = "AOE")
  app$set_inputs(corrplotOrder = "alphabet")
  app$set_inputs(plotType = "pairs")
  app$set_inputs(correlationMethod = "pearson")
  app$set_inputs(correlationMethod = "spearman")
  app$set_inputs(correlationThreshold = 0.4)
  app$expect_values()
})


test_that("{shinytest2} recording: surveyDashboard_builtin_detectionHistory", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_detHist",
      height = 993, width = 1619, load_timeout = 30000)
  
  withr::defer(app$stop()) 
  
  app$set_inputs(sidebarItemExpanded = "Single-speciesOccupancy")
  app$set_inputs(species_dethist = "MNE")
  app$set_inputs(day1_single_species = "station")
  app$set_inputs(occasionLength_single_species = 5)
  app$expect_values()
})



test_that("{shinytest2} recording: surveyDashboard_builtin_occupancy", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_occu",
      height = 993, width = 1619, load_timeout = 30000)
  
  withr::defer(app$stop()) 
  
  
  app$set_inputs(sidebarItemExpanded = "Single-speciesOccupancy")
  app$click("basic_run_model")
  app$click("basic_add_to_modsel")
  app$set_inputs(basic_effort_on_detection = TRUE)
  app$click("basic_run_model")
  app$click("basic_add_to_modsel")
  app$set_inputs(basic_occ_covs = "utm_y")
  app$set_inputs(basic_occ_covs = c("utm_y", "utm_x"))
  app$click("basic_run_model")
  app$click("basic_add_to_modsel")
  app$set_inputs(basic_workflow_tabs = "Model selection")
  app$set_inputs(basic_workflow_tabs = "Response Plots")
  app$set_inputs(basic_plot_type = "Occupancy covariates")
  app$expect_values()
})



test_that("{shinytest2} recording: surveyDashboard_builtin_communiy_UI", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_commun_UI",
      height = 993, width = 1619, load_timeout = 30000)
  
  withr::defer(app$stop()) 
  
  app$set_inputs(detCovFixed = "utm_y")
  app$set_inputs(occuCovRanef = "utm_x")
  app$set_inputs(detCovIndep = "utm_y")
  app$set_inputs(useEffortAsDetCov = TRUE)
  app$set_inputs(occasionLength_community = 15)
  app$click("createCommunityModel")
  app$set_inputs(niter = 100)
  app$expect_values()
})


test_that("{shinytest2} recording: surveyDashboard_builtin_covariate_extraction_dem", {
  skip_on_cran() 
  skip_on_covr()
  skip_on_ci()
  
  local_app_support(test_path("apps/surveyDashboard_builtin"))
  app <- AppDriver$new(test_path("apps/surveyDashboard_builtin"), name = "app_builtin_covs_DEM", 
      height = 993, width = 1619, load_timeout = 30000)
  
  withr::defer(app$stop()) 
  
  app$set_inputs(sidebarItemExpanded = "DataProcessing")
  app$set_inputs(use_elevation = TRUE)
  app$set_inputs(elevationZoom = "9")
  app$set_inputs(terrainMeasures = c("slope", "TRI", "TPI"))
  app$click("run_covariate_extraction", timeout_ = 10000)
  
  app$set_inputs(colorPalette = "Plasma")
  app$set_inputs(rasterBand = "TPI")
  app$set_inputs(predictionRasterBand = "TRI")
  app$wait_for_idle()
  app$expect_values()
})
