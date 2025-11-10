context("communityModels")
library(camtrapR)
library(terra)

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
  sitecovs$some_factor <- factor(c("A", "A", "B"))
  
  # bundle input data
  data_list <- list(
    ylist = DetHist_list$detection_history,
    siteCovs = sitecovs,
    obsCovs = list(effort = DetHist_list$effort)
  )
  
  # create community model
  modelfile1   <- tempfile(fileext = ".txt")
  modelfile1_RN <- tempfile(fileext = ".txt")
  
  
  mod.jags <- communityModel(
    data_list,
    occuCovs = list(fixed = c("some_factor", "utm_y"), ranef = "elevation"),
    detCovsObservation = list(fixed = "effort"),
    intercepts = list(det = "fixed", occu = "ranef"),
    modelFile = modelfile1
  )
  
  mod.jags_RN <- communityModel(
    data_list,
    model = "RN",
    occuCovs = list(fixed = c("utm_y"), ranef = "elevation"),
    detCovsObservation = list(fixed = "effort"),
    intercepts = list(det = "fixed", occu = "ranef"),
    modelFile = modelfile1_RN
  )
  
  
  # create prediction raster
  
  # 1. Create the base raster template
  template <- rast(nrows = 10, ncols = 10)
  
  # 2. Create the 'utm_y' layer
  layer_utm_y <- setValues(template, rnorm(ncell(template)))
  names(layer_utm_y) <- "utm_y"
  
  # 3. Create the 'elevation' layer
  layer_elevation <- setValues(template, rnorm(ncell(template)))
  names(layer_elevation) <- "elevation"
  
  # 4. Create the 'some_factor' (categorical) layer
  factor_values <- sample(c(1, 2), ncell(template), replace = TRUE)
  layer_factor <- setValues(template, factor_values) 
  names(layer_factor) <- "some_factor"
  
  # 5. Combine the individual layers into a single SpatRaster
  prediction_raster <- c(layer_utm_y, layer_elevation, layer_factor)
  
  
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
  
  test_that("communityModel Royle-Nichols output has correct structure", {
    # Objects like mod.jags are available from the parent local() environment
    expect_s4_class(mod.jags_RN, "commOccu")
    expect_equal(slotNames(mod.jags_RN), c("modelText", "params", "inits_fun", "data", "input", "nimble", "modelFile",
                                        "covariate_info", "model"))
    expect_false(mod.jags_RN@nimble)
    expect_true(file.exists(mod.jags_RN@modelFile))
    expect_equal(mod.jags_RN@model, "RN")
  })
  
  test_that("summary works", {
    expect_output(summary(mod.jags), "=== Community Occupancy Model Summary ===")
    expect_output(summary(mod.jags_RN), "=== Community Occupancy Model Summary ===")
  })
  
  test_that("JAGS workflow (fit, plot, predict) runs correctly", {
    
    # Skips first
    skip_if_not_installed("rjags")
    skip_if(Sys.which("jags") == "", message = "JAGS executable not found. Skipping test.")

    
    # 1. Fit the model
    
    fit.jags    <- fit(mod.jags, n.iter = 100, n.burnin = 50, chains = 3)
    fit.jags_RN <- fit(mod.jags_RN, n.iter = 100, n.burnin = 50, chains = 3)
    
    
    
    # Check fit object
    expect_s3_class(fit.jags, "mcmc.list")
    expect_length(fit.jags, 3)
    
    expect_s3_class(fit.jags_RN, "mcmc.list")
    expect_length(fit.jags_RN, 3)
    
    # 2. Test plotting
    plot_eff_state       <- plot_effects(mod.jags, fit.jags, submodel = "state")
    plot_coef_state_comb <- plot_coef(mod.jags, fit.jags, submodel = "state", combine = TRUE, ordered = FALSE)
    
    expect_length(plot_eff_state, 3)
    expect_s3_class(plot_coef_state_comb, "ggplot")
    
    
    plot_eff_state_RN       <- plot_effects(mod.jags_RN, fit.jags_RN, submodel = "state")
    plot_coef_state_RN_comb <- plot_coef(mod.jags_RN, fit.jags_RN, submodel = "state", combine = TRUE, ordered = FALSE)
    
    expect_length(plot_eff_state_RN, 2)
    expect_s3_class(plot_coef_state_RN_comb, "ggplot")
    
    
    # 3. Test predictions
    draws <- 20
    
    
    p_array <- predict(object = mod.jags, mcmc.list = fit.jags, type = "p_array", draws = draws)
    psi_array <- predict(object = mod.jags, mcmc.list = fit.jags, type = "psi_array", draws = draws)
    
    rich <- predict(object = mod.jags, mcmc.list = fit.jags, type = "richness", draws = draws)
    psi <- predict(object = mod.jags, mcmc.list = fit.jags, type = "psi", draws = draws)
    pao <- predict(object = mod.jags, mcmc.list = fit.jags, type = "pao", draws = draws)
    
    rich_batch <- predict(object = mod.jags, mcmc.list = fit.jags, type = "richness", draws = draws,
                          batch = T)
    
    psi_array_rast <- predict(object = mod.jags, mcmc.list = fit.jags, type = "psi_array", draws = draws)
    
    rich_rast <- predict(object = mod.jags, mcmc.list = fit.jags, type = "richness", draws = draws, 
                         x = prediction_raster,
                         interval = "confidence")
    psi_rast <- predict(object = mod.jags, mcmc.list = fit.jags, type = "psi", draws = draws,
                   x = prediction_raster)
    pao <- predict(object = mod.jags, mcmc.list = fit.jags, type = "pao", draws = draws,
                   x = prediction_raster,)
    
    expect_equal(dim(rich), c(3, 2))
    expect_equal(dim(psi), c(15, 4))
    
    expect_length(pao, 3)
    expect_named(pao, c("pao_summary", "pao_matrix",  "pao_df"))
    expect_equal(dim(pao$pao_summary), c(5,8))
    expect_equal(dim(pao$pao_df), c(100,3))
    expect_equal(dim(pao$pao_matrix), c(5,20))
    
    expect_true(class(rich_rast) == "SpatRaster")
    expect_equal(dim(rich_rast), c(10, 10, 4))
    expect_named(rich_rast, c("mean", "sd", "lower", "upper"))
    
    
    # 4. Test Posterior predictive checks
    ppc_comm <- PPC.community(p = p_array, psi = psi_array, y = mod.jags@input$ylist, model = "Occupancy", type = "FT")
    
    
    expect_length(ppc_comm, 2)
    expect_named(ppc_comm, c("BP", "residuals"))
    expect_equal(dim(ppc_comm$BP), c(6, 2))
    expect_named(ppc_comm$residuals, species_to_include)
    
    ppc_comm2 <- PPC.community(p = p_array, psi = psi_array, y = mod.jags@input$ylist, model = "Occupancy", type = "PearChi2")
    
    
    # 5. Test predictions (Royle-Nichols model)
    
    p_array <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "p_array", draws = draws)
    psi_array <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "psi_array", draws = draws)
    
    rich <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "richness", draws = draws)
    rich_conf <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "richness", draws = draws, 
                         interval = "confidence")
    psi <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "psi", draws = draws)
    abundance <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "abundance", draws = draws)
    abundance_array <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "lambda_array", draws = draws)
    pao <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "pao", draws = draws)
    
    abundance_rast <- predict(object = mod.jags_RN, mcmc.list = fit.jags_RN, type = "abundance", draws = draws,
                              x = prediction_raster)
    
    expect_equal(dim(rich), c(3, 2))
    expect_equal(dim(rich_conf), c(3, 4))
    expect_equal(dim(psi), c(15, 4))
    expect_equal(dim(abundance), c(15, 4))
    expect_equal(dim(abundance_array), c(3,5,20))
    
    expect_length(pao, 3)
    expect_named(pao, c("pao_summary", "pao_matrix",  "pao_df"))
    expect_equal(dim(pao$pao_summary), c(5,8))
    expect_equal(dim(pao$pao_df), c(100,3))
    expect_equal(dim(pao$pao_matrix), c(5,20))
    
    
    # 6. Test PPC (RN model)
    
    # returns warning in each call of PPC.residualy
    ppc_comm <- expect_warning(PPC.community(p = p_array, psi = abundance_array, 
                              y = mod.jags_RN@input$ylist, model = "RN", type = "FT"))
    
    
    
    expect_length(ppc_comm, 2)
    expect_named(ppc_comm, c("BP", "residuals"))
    expect_equal(dim(ppc_comm$BP), c(6, 2))
    expect_named(ppc_comm$residuals, species_to_include)
    
    ppc_comm2 <- expect_warning(PPC.community(p = p_array, psi = abundance_array, 
                               y = mod.jags_RN@input$ylist, model = "RN", type = "PearChi2"))
    
    
  })
  
  test_that("Other model specifications work", {
    
    # various effect types
    mod.jags_fixed <- communityModel(
      data_list,
      occuCovs = list(fixed = c("some_factor", "utm_y"), fixed = c("some_factor", "elevation")),
      detCovsObservation = list(fixed = "effort"),
      intercepts = list(det = "fixed", occu = "fixed"),
      modelFile = tempfile(fileext = "txt")
    )
    
    mod.jags_ranef <- communityModel(
      data_list,
      occuCovs = list(fixed = c("some_factor", "elevation")),
      detCovsObservation = list(ranef = "effort"),
      intercepts = list(det = "ranef", occu = "ranef"),
      modelFile = tempfile(fileext = "txt")
    )
    
    mod.jags_indep <- communityModel(
      data_list,
      occuCovs = list(independent  = c("utm_y")),
      intercepts = list(det = "independent", occu = "independent"),
      modelFile = tempfile(fileext = "txt")
    )
    
    mod.jags_fixed_specsiteranef <- communityModel(
      data_list,
      speciesSiteRandomEffect = list(det = T, occu = F),
      modelFile = tempfile(fileext = "txt")
    )
     
    
    # data augmentation
    
    
    mod.jags_aug1 <- communityModel(
      data_list,
      modelFile = tempfile(fileext = "txt"),
      augmentation = c(maxknown = 6) 
    )
    
    mod.jags_aug2 <- communityModel(
      data_list,
      modelFile = tempfile(fileext = "txt"),
      augmentation = c(full = 6) 
    )
    
    # nimble models
    mod.jags_fixed_nimble <- communityModel(
      data_list,
      occuCovs = list(fixed = c("some_factor", "utm_y"), fixed = c("some_factor", "elevation")),
      detCovsObservation = list(fixed = "effort"),
      intercepts = list(det = "fixed", occu = "fixed"),
      modelFile = tempfile(fileext = "txt"),
      nimble = TRUE
    )
    
    mod.jags_ranef_nimble <- communityModel(
      data_list,
      occuCovs = list(fixed = c("some_factor", "elevation")),
      detCovsObservation = list(ranef = "effort"),
      intercepts = list(det = "ranef", occu = "ranef"),
      modelFile = tempfile(fileext = "txt"),
      nimble = TRUE
    )
    
    mod.jags_indep_nimble <- communityModel(
      data_list,
      occuCovs = list(independent  = c("utm_y")),
      intercepts = list(det = "independent", occu = "independent"),
      modelFile = tempfile(fileext = "txt"),
      nimble = TRUE
    )
    
  })
  
  test_that("Other error conditions work", {
    
    expect_error(
      mod.jags_indep <- communityModel(
        data_list,
        occuCovs = list(independent  = c("some_factor", "utm_y")),
        intercepts = list(det = "independent", occu = "independent"),
        modelFile = tempfile(fileext = "txt")
      ),
      "independent effects of categorical site covariates are currently not supported"
    )
    
    expect_error(
    mod.jags_fixed_specsiteranef <- communityModel(
      data_list,
      speciesSiteRandomEffect = list(det = T, occu = T),
      modelFile = tempfile(fileext = "txt")
    ),
    fixed = "speciesSiteRandomEffect$occu must be FALSE"
    )
    
  })
  
})
