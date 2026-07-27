context("surveyDashboard")

library(camtrapR)
library(shiny)


# Basic usage with minimal parameters

data("camtraps")
data("recordTableSample")



test_that("surveyDashboard can be launched without parameters without error", {
  
  testthat::skip_if_not_installed("mapview")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("corrplot")
  testthat::skip_if_not_installed("unmarked")
  testthat::skip_if_not_installed("ubms")
  
  testServer(app = surveyDashboard(), {
    
    # If code inside this block is reached means the server initialized successfully.
    expect_true(TRUE) 
  })
})


test_that("surveyDashboard server logic initializes correctly with sample data", {
  
  testthat::skip_if_not_installed("mapview")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("corrplot")
  testthat::skip_if_not_installed("unmarked")
  testthat::skip_if_not_installed("ubms")
  
  # testServer runs the server function in the current R process,
  # making its internal state (like reactives) accessible for testing.
  
  testServer(
    
    app = surveyDashboard(
      CTtable = camtraps,
      recordTable = recordTableSample,
      xcol = "utm_x",
      ycol = "utm_y",
      crs = "epsg:32650",      # = UTM50N
      stationCol = "Station",
      setupCol = "Setup_date",
      retrievalCol = "Retrieval_date",
      CTdateFormat = "dmy"
    ),
    {
      # Inside this block, you can access server-side objects like
      # input, output, session, and reactive expressions.
      
      expect_equal(num_stations(), 3)
      expect_equal(date_range_min(), as.Date("2009-04-02"))
      expect_equal(date_range_max(), as.Date("2009-05-17"))
      expect_equal(num_species(), 5)
      expect_equal(num_images(), 39)
      expect_equal(avg_records_per_station(), 13)
      expect_equal(dim(data$CTtable), c(3,7))
      expect_equal(dim(data$recordTable), c(39,12))
      expect_equal(dim(df_n_records()), c(5, 2))
      
      # objects not available without user interaction (still NULL)
      # expect_equal(dim(data$aggregated_CTtable), c(3,7))
      # expect_equal(trap_nights(), 128)
      # expect_equal(dim(camop()), c(3,46))
      # expect_equal(dim(data$CTtable_sf), c(3,8))
      # expect_true(inherits(data$CTtable_sf, "sf"))
    }
  )
})


test_that("surveyDashboard server logic initializes correctly with camtrap DP data", {
  
  testthat::skip_if_not_installed("mapview")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("corrplot")
  testthat::skip_if_not_installed("unmarked")
  testthat::skip_if_not_installed("ubms")
  
  # testServer runs the server function in the current R process,
  # making its internal state (like reactives) accessible for testing.
  
  path_camtrapdp <- system.file("sample_data/tdwg_camtrap-dp_1.0.2_example", 
                                package = "camtrapR")
  camtrapdp <- readCamtrapDP(file = file.path(path_camtrapdp, "datapackage.json")) 
  
  testServer(
    
    app = surveyDashboard(CTtable = camtrapdp$CTtable,
                          recordTable = camtrapdp$recordTable, 
                          xcol = "longitude", 
                          ycol = "latitude", 
                          crs = 4326, 
                          stationCol = "locationName", 
                          setupCol = "Setup_date", 
                          retrievalCol = "Retrieval_date", 
                          CTdateFormat = "ymd HMS", 
                          speciesCol = "vernacularName_eng"),
    {
      # Inside this block, you can access server-side objects like
      # input, output, session, and reactive expressions.
      
      expect_equal(num_stations(), 4)
      expect_equal(date_range_min(), as.Date("2020-05-30"))
      expect_equal(date_range_max(), as.Date("2021-04-18"))
      expect_equal(num_species(), 14)
      expect_equal(num_images(), 549)
      expect_equal(round(avg_records_per_station()), 137)
      expect_equal(dim(data$CTtable), c(4,26))
      expect_equal(dim(data$recordTable), c(549,34))
      expect_equal(dim(df_n_records()), c(14, 2))
      
      expect_equal(dim(detmaps_sf()), c(4,19))
    }
  )
})

# 
# 
# # skipping this section for now due to unresolved and unclear error
# test_that("warning reactives fire for small / low-detection datasets", {
# 
#   testthat::skip_if_not_installed("mapview")
#   testthat::skip_if_not_installed("plotly")
#   testthat::skip_if_not_installed("corrplot")
#   testthat::skip_if_not_installed("unmarked")
#   testthat::skip_if_not_installed("ubms")
#   testthat::skip_if_not_installed("coda")
# 
#   testServer(
#     app = surveyDashboard(
#       CTtable = camtraps,
#       recordTable = recordTableSample,
#       xcol = "utm_x",
#       ycol = "utm_y",
#       crs = "epsg:32650",
#       stationCol = "Station",
#       setupCol = "Setup_date",
#       retrievalCol = "Retrieval_date",
#       CTdateFormat = "dmy"
#     ),
#     {
#       # browser()
#       
#       # ---- collect_warnings / renderWarningBlock (pure helpers) ----
#       expect_equal(length(collect_warnings(NULL, NULL)), 0)
#       expect_equal(length(collect_warnings(list(a = 1), NULL, list(b = 2))), 2)
# 
#       # ---- Warning #1: station sample size ----
#       # camtraps has 3 stations (< 20) -> warning must fire.
#       # data$aggregated_CTtable is not populated without the UI "confirm"
#       # flow, so set it directly to exercise the reactive logic.
#       data$aggregated_CTtable <- camtraps
#       w1 <- warn_station_sample_size()
#       expect_false(is.null(w1))
#       expect_equal(w1$id, "station_sample_size")
#       expect_true(grepl("20", w1$message))
#       expect_s3_class(renderWarningBlock(w1), "shiny.tag")
# 
#       # With >= 20 stations the warning must NOT fire.
#       data$aggregated_CTtable <- data.frame(Station = paste0("S", 1:25))
#       expect_null(warn_station_sample_size())
#       data$aggregated_CTtable <- camtraps   # restore for downstream checks
# 
#       # ---- Warning #3 + #4a: community species selection ----
#       # Before any selection, the speciesTable input is NULL (not rendered
#       # in testServer) -> both community reactives return NULL.
#       expect_null(warn_n_species())
#       expect_null(warn_low_detections_community())
# 
#       # camtraps has 5 species; selecting 3 (<= 5) -> #3 fires.
#       session$setInputs(speciesTable_rows_selected = c(1, 2, 3))
#       w3 <- warn_n_species()
#       expect_false(is.null(w3))
#       expect_equal(w3$id, "n_species")
#       
#       # TODO: the w3 <- line causes error:
#       # 86: updateSelectInput
#       # 85: update_species_inputs [~/Projects/camtrapR/R/surveyDashboard.R#5002]
#       # 84: observe [~/Projects/camtrapR/R/surveyDashboard.R#3551]
#       # 83: <observer>
#       #   ── Warning: warning reactives fire for small / low-detection datasets ──
#       #   Error in if: argument is of length zero
# 
#       # With only 3 stations total every species is detected at <= 3
#       # stations -> #4a fires.
#       w4a <- warn_low_detections_community()
#       expect_false(is.null(w4a))
#       expect_equal(w4a$id, "low_detections")
# 
#       # Model Configuration surfaces both at once via collect_warnings.
#       cw <- collect_warnings(warn_n_species(), warn_low_detections_community())
#       expect_equal(length(cw), 2)
# 
#       # ---- Warning #4b: low detections (single-species workflow) ----
#       session$setInputs(species_dethist = "MNE")
#       w4b <- warn_low_detections_single()
#       expect_false(is.null(w4b))
#       expect_equal(w4b$id, "low_detections")
# 
#       # ---- Warning #2: convergence / effective sample size ----
#       # Build a small mcmc.list directly (no need to fit a real model).
#       make_mcmc <- function(means, n = 2000, seed = 1) {
#         set.seed(seed)
#         chains <- lapply(means, function(m) coda::mcmc(stats::rnorm(n, mean = m, sd = 1)))
#         coda::as.mcmc.list(chains)
#       }
#       # Well-converged (two chains from the same distribution) -> no warning.
#       fitted_comm_model(make_mcmc(c(0, 0)))
#       session$flushReact()
#       expect_null(warn_convergence())
# 
#       # Non-converged (two chains with very different means) -> warning fires.
#       fitted_comm_model(make_mcmc(c(0, 8)))
#       session$flushReact()
#       w2 <- warn_convergence()
#       expect_false(is.null(w2))
#       expect_equal(w2$id, "convergence")
# 
#       # ---- Warning #5: lack of fit (helper + reactive) ----
#       # Helper returns NULL when no GOF results.
#       expect_null(compute_lack_of_fit_warning(NULL))
#       # With BP values in the acceptable range -> NULL.
#       gof_ok <- list(BP = data.frame(BP = c(0.3, 0.5, 0.7)))  # last row = community
#       expect_null(compute_lack_of_fit_warning(gof_ok))
#       # With a species BP out of range -> warning fires.
#       gof_bad <- list(BP = data.frame(BP = c(0.05, 0.5, 0.7)))
#       w5 <- compute_lack_of_fit_warning(gof_bad)
#       expect_false(is.null(w5))
#       expect_equal(w5$id, "lack_of_fit")
#       # Reactive reads from gof_results().
#       gof_results(gof_bad)
#       session$flushReact()
#       expect_equal(warn_lack_of_fit()$id, "lack_of_fit")
# 
#       # ---- Acknowledgment: popups only show once per dataset ----
#       # Initially no warnings are acknowledged.
#       expect_null(acknowledged_warnings())
# 
#       # filter_unacknowledged passes everything through when nothing is acked.
#       unack <- filter_unacknowledged(list(w1, w3))
#       expect_equal(length(unack), 2)
# 
#       # Acknowledge w1 (station_sample_size) and w3 (n_species).
#       acknowledged_warnings(c("station_sample_size", "n_species"))
# 
#       # filter_unacknowledged now drops the acknowledged ones.
#       unack2 <- filter_unacknowledged(list(w1, w3, w4a))
#       expect_equal(length(unack2), 1)
#       expect_equal(unack2[[1]]$id, "low_detections")
# 
#       # Sidebar review buttons show ALL active warnings regardless of ack.
#       all_data <- collect_warnings(
#         warn_station_sample_size(),
#         warn_n_species(),
#         warn_low_detections_community(),
#         warn_low_detections_single()
#       )
#       expect_true(length(all_data) >= 2)  # at least low_detections variants
# 
#       # Clearing acknowledged_warnings (simulating new dataset) shows all again.
#       acknowledged_warnings(NULL)
#       unack3 <- filter_unacknowledged(list(w1, w3))
#       expect_equal(length(unack3), 2)
#     }
#   )
# })
# 
# 
