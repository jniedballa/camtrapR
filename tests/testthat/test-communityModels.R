context("communityModels")
library(camtrapR)

# Use local() to create a self-contained environment for all related tests.

local({
  
  # --- SETUP: Define all shared data objects ONCE ---
  
  data("camtraps")
  data("recordTableSample")
  
  # create camera operation matrix
  camop_no_problem <- cameraOperation(
    CTtable      = camtraps,
    stationCol   = "Station",
    setupCol     = "Setup_date",
    retrievalCol = "Retrieval_date",
    hasProblems  = FALSE,
    dateFormat   = "dmy"
  )
  
  # make list of detection histories
  species_to_include <- unique(recordTableSample$Species)
  DetHist_list <- detectionHistory(
    recordTable         = recordTableSample,
    camOp               = camop_no_problem,
    stationCol          = "Station",
    speciesCol          = "Species",
    recordDateTimeCol   = "DateTimeOriginal",
    species             = species_to_include,
    occasionLength      = 7,
    day1                = "station",
    datesAsOccasionNames= FALSE,
    includeEffort       = TRUE,
    scaleEffort         = TRUE,
    timeZone            = "Asia/Kuala_Lumpur"
  )
  
  # create covariates
  sitecovs <- camtraps[, c(1:3)]
  sitecovs$elevation <- c(300, 500, 600)
  sitecovs[, c(2:4)] <- scale(sitecovs[, -1])
  
  # bundle input data
  data_list <- list(
    ylist = DetHist_list$detection_history,
    siteCovs = sitecovs,
    obsCovs = list(effort = DetHist_list$effort)
  )
  
  # create community model
  modelfile1 <- tempfile(fileext = ".txt")
  mod.jags <- communityModel(
    data_list,
    occuCovs = list(fixed = "utm_y", ranef = "elevation"),
    detCovsObservation = list(fixed = "effort"),
    intercepts = list(det = "ranef", occu = "ranef"),
    modelFile = modelfile1
  )
  
  # --- TESTS: Now run the tests from within this environment ---
  
  test_that("communityModel output has correct structure for JAGS occupancy model", {
    # Objects like mod.jags are available from the parent local() environment
    expect_s4_class(mod.jags, "commOccu")
    expect_equal(slotNames(mod.jags), c("modelText", "params", "inits_fun", "data", "input", "nimble", "modelFile",
                                        "covariate_info", "model"))
    expect_false(mod.jags@nimble)
    expect_true(file.exists(mod.jags@modelFile))
    expect_equal(mod.jags@model, "Occupancy")
  })
  
  
  test_that("JAGS workflow (fit, plot, predict) runs correctly", {
    
    # Skips first
    skip_if_not_installed("rjags")
    skip_if(Sys.which("jags") == "", message = "JAGS executable not found. Skipping test.")
    library(rjags)
    
    # 1. Fit the model
    fit.jags <- fit(mod.jags, n.iter = 100, n.burnin = 50, chains = 3)
    
    # Check fit object
    expect_s3_class(fit.jags, "mcmc.list")
    expect_length(fit.jags, 3)
    
    # 2. Test plotting
    plot_eff_state <- plot_effects(mod.jags, fit.jags, submodel = "state")
    plot_coef_state_comb <- plot_coef(mod.jags, fit.jags, submodel = "state", combine = TRUE, ordered = FALSE)
    
    expect_length(plot_eff_state, 2)
    expect_s3_class(plot_coef_state_comb, "ggplot")
    
    # 3. Test prediction and PPC
    draws <- 20
    p <- predict(object = mod.jags, mcmc.list = fit.jags, type = "p_array", draws = draws)
    psi <- predict(object = mod.jags, mcmc.list = fit.jags, type = "psi_array", draws = draws)
    
    ppc_comm <- PPC.community(p = p, psi = psi, y = mod.jags@input$ylist, model = "Occupancy", type = "FT")
    
    expect_length(ppc_comm, 2)
    expect_named(ppc_comm, c("BP", "residuals"))
    expect_equal(dim(ppc_comm$BP), c(6, 2))
    expect_named(ppc_comm$residuals, species_to_include)
  })
  
}) # End of the local() block