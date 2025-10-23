context("communityModels")
library(camtrapR)


data("camtraps")

# create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "dmy"
)

data("recordTableSample")

# make list of detection histories
species_to_include <- unique(recordTableSample$Species)

DetHist_list <- detectionHistory(
  recordTable         = recordTableSample,
  camOp                = camop_no_problem,
  stationCol           = "Station",
  speciesCol           = "Species",
  recordDateTimeCol    = "DateTimeOriginal",
  species              = species_to_include,
  occasionLength       = 7,
  day1                 = "station",
  datesAsOccasionNames = FALSE,
  includeEffort        = TRUE,
  scaleEffort          = TRUE,
  timeZone             = "Asia/Kuala_Lumpur"
)


# create some fake covariates for demonstration
sitecovs <- camtraps[, c(1:3)]
sitecovs$elevation <- c(300, 500, 600)   

# scale numeric covariates
sitecovs[, c(2:4)] <- scale(sitecovs[,-1])


# bundle input data for communityModel
data_list <- list(ylist = DetHist_list$detection_history,
                  siteCovs = sitecovs,
                  obsCovs = list(effort = DetHist_list$effort))


# create community model for JAGS
modelfile1 <- tempfile(fileext = ".txt")
mod.jags <- communityModel(data_list,
                           occuCovs = list(fixed = "utm_y", ranef = "elevation"),
                           detCovsObservation = list(fixed = "effort"),
                           intercepts = list(det = "ranef", occu = "ranef"),
                           modelFile = modelfile1)


test_that("communityModel output has correct structure for JAGS occupancy model", {
  expect_s4_class(mod.jags, "commOccu")
  expect_equal(slotNames(mod.jags), c("modelText", "params", "inits_fun", "data", "input", "nimble", "modelFile",
                                       "covariate_info", "model"))
  
  expect_equal(length(mod.jags@modelText), 48)
  expect_equal(length(mod.jags@params), 13)
  expect_equal(length(mod.jags@inits_fun()), 8)
  expect_equal(length(mod.jags@data), 8)
  expect_equal(names(mod.jags@data), c("y", "effort_binary", "effort", "M", "J", "maxocc", "utm_y", "elevation"  ))
  expect_equal(length(mod.jags@input), 3)
  expect_equal(names(mod.jags@input), c("ylist", "siteCovs", "obsCovs"))
  expect_false(mod.jags@nimble)
  expect_true(file.exists(mod.jags@modelFile))
  expect_s3_class(mod.jags@covariate_info, "data.frame")
  expect_equal(dim(mod.jags@covariate_info), c(5,12))
  expect_equal(mod.jags@model, "Occupancy")
  
})



test_that("JAGS model fits correctly with valid data", {
  
  # 1. Skip if the R wrapper package isn't installed
  skip_if_not_installed("R2jags")
  
  library(rjags)
  
  # 2. Skip if the JAGS software itself is not found on the system
  skip_if(Sys.which("jags") == "", message = "JAGS executable not found. Skipping test.")
  
  
  # fit in JAGS
  fit.jags <- fit(mod.jags,
                  n.iter = 100,
                  n.burnin = 50,
                  chains = 3)   
  
  
  expect_s3_class(fit.jags, "mcmc.list")
  expect_equal(length(fit.jags), 3)
  
  expect_equal(nrow(fit.jags[[1]]), 50)
  expect_equal(length(colnames(fit.jags[[1]])), 27)
  
})
  

test_that("plotting works for community models", {
  skip_if_not(exists("fit.jags"))
  
  # response curves (= marginal effect plots)
  plot_eff_state <- plot_effects(mod.jags, 
               fit.jags, 
               submodel = "state")
  plot_eff_det <- plot_effects(mod.jags, 
               fit.jags, 
               submodel = "det")
  
  # effect sizes plot
  plot_coef_state <- plot_coef(mod.jags, 
            fit.jags, 
            submodel = "state")
  plot_coef_state_comb <- plot_coef(mod.jags, 
                               fit.jags, 
                               submodel = "state", 
                               combine = T,
                               ordered = F)
  plot_coef_det <- plot_coef(mod.jags, 
            fit.jags, 
            submodel = "det")              
  
  
  expect_equal(length(plot_eff_state), 2)
  expect_equal(length(plot_eff_det), 1)
  
  expect_equal(length(plot_coef_state), 2)
  expect_equal(length(plot_coef_det), 1)
  
  expect_s3_class(plot_coef_state_comb, "ggplot")
  
})


test_that("posterior predictive checks for community models work", {
  
  skip_if_not(exists("fit.jags"))
  
  # create predictions for p and psi
  draws <- 20
  
  p <- predict(object = mod.jags,
               mcmc.list = fit.jags,
               type = "p_array",
               draws = draws)
  
  psi <- predict(object = mod.jags,
                 mcmc.list = fit.jags,
                 type = "psi_array",
                 draws = draws)

  
  ppc_comm <- PPC.community(
    p = p,
    psi = psi,
    y = mod.jags@input$ylist, 
    model = "Occupancy",
    type = "FT")
  
  
  expect_equal(length(ppc_comm), 2)
  expect_equal(names(ppc_comm), c("BP", "residuals"))
  expect_equal(dim(ppc_comm$BP), c(6,2))
  expect_equal(class(ppc_comm$residuals), "list")
  expect_equal(names(ppc_comm$residuals), species_to_include)
  
  
})
