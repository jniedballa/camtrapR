
#' Create a community (multi-species) occupancy model for JAGS or Nimble
#' 
#' @description 
#' Flexibly creates complete code and input data for community occupancy models for JAGS amd Nimble (both standard occupancy models and Royle-Nichols occupancy models), and automatically sets initial values and parameters to monitor. 
#' Supports fixed and random effects of covariates on detection and occupancy probabilities, using both continuous and categorical covariates (both site and site-occasion covariates). 
#' 
#' Optionally includes data augmentation (fully open community, or up to known maximum number of species, or no data augmentation). 
#' Allows combination of all these parameters for fast and flexible customization of community occupancy models.
#' 
#' Incidentally, the function can also be used to create model code and input for single-species single-season occupancy models (it is the special case of the community model with only one species). 
#' Such a model will run slower than proper single-species model JAGS code due to the additional species loop, but it is possible.
#' 
#' The function returns several derived quantities, e.g. species richness, Bayesian p-values (overall and by species), Freeman-Tukey residuals for actual and simulated data (by station and total). If doing data augmentation, metacommunity size and number of unseen species are returned also. 
#'
#' @importFrom generics fit
#  @importFrom rjags jags.model coda.samples
# @importFrom coda c coda::mcmc.list mcmc
# @importFrom snowfall snowfall::sfInit snowfall::sfLibrary snowfall::sfExportAll snowfall::sfClusterSetupRNG snowfall::sfLapply snowfall::sfStop
# @importFrom nimble nimble::readBUGSmodel nimble::buildMCMC nimble::compileNimble nimble::runMCMC
#' @importFrom stats rnorm window 
#' @importFrom ggplot2 ggplot ggsave xlim geom_line facet_wrap geom_ribbon theme_bw ggtitle xlab ylab aes
#' @export
#'
#' @param data_list    list. Contains 3 slots: ylist, siteCovs, obsCovs. ylist is a list of detection histories (can be named), e.g. from \code{\link{detectionHistory}}. siteCovs is a data.frame with site covariates (optional). obsCovs is a list of site-occasion level covariates (e.g. site-occasion-specific effort, which is also returned by \code{\link{detectionHistory}}.
#' @param model character. "Occupancy" for standard occupancy model,  or "RN" for the occupancy model of Royle and Nichols (2003), which relates probability of detection of the species to the number of individuals available for detection at each station
#' @param occuCovs  list. Up to 3 items named "fixed", "independent", and/or "ranef". Specifies fixed, independent or random effects of covariates on occupancy probability (continuous or categorical covariates). Independent effects are only supported for continuous covariates.
#' @param detCovs   list. Up to 3 items named "fixed", "independent", and/or "ranef". Specifies fixed, independent or random effects of covariates on detection probability (continuous or categorical covariates). Independent effects are only supported for continuous covariates.
#' @param detCovsObservation   list. Up to 2 items named "fixed" and/or "ranef". Specifies fixed or random effects of observation-level covariates on detection probability  (continuous or categorical covariates - categorical must be coded as character matrix)
#' @param speciesSiteRandomEffect list. Two items named "det" and "occu". If TRUE, adds a random effect of species and station. Only implemented for detection probability. 
#' @param intercepts     list. Two items named "det" and "occu" for detection and occupancy probability intercepts. Values can be "fixed" (= constant across species), "independent" (= independent estimates for each species), or "ranef" (= random effect of species on intercept).
#' @param effortCov     character. Name of list item in \code{data_list$obsCovs} which contains effort. This does not include effort as a covariate on detection probability, but only uses NA / not NA information to create binary effort and ensure detection probabilities p are 0 when there was no effort (p will be 0 whereever \code{effortCov} is NA).
#' @param richnessCategories  character. Name of categorical covariate in \code{data_list$siteCovs} for which to calculate separate richness estimates (optional). Can be useful to obtain separate richness estimates for different areas.
#' @param augmentation     If NULL, no data augmentation (only use species in \code{data_list$ylist}), otherwise named list or vector with total number of (potential) species. Names: "maxknown" or "full". Example: \code{augmentation = c(maxknown = 30)} or \code{augmentation = c(full = 30)}
#' @param modelFile   character. Text file name to save model to
#' @param nimble  logical. If TRUE, model code will be for Nimble (incompatible with JAGS). If FALSE, model code is for JAGS.
#'
#'
#' @details
#' For examples of implementation, see Vignette 5: Multi-species occupancy models.
#' 
#' Fixed effects of covariates are constant across species, whereas random effect covariates differ between species. Independent effect differ between species and are independent (there is no underlying hyperdistribution).
#' Fixed, independent and random effects are allowed for station-level detection and occupancy covariates (a.k.a. site covariates). Fixed and random effects are also allowed for station-occasion level covariates (a.k.a. observation covariates). 
#' Currently independent effects are only supported for continuous site covariates, not categorical site covariates or observation-level covariates.
#' 
#' By default, random effects will be by species. It is however possible to use categorical site covariates for grouping (continuous|categorical).
#' Furthermore, is is possible to use use nested random effects of species and another categorical site covariate (so that there is a random effect of species and an additional random effect of a categorical covariate within each species).
#' 
#' 
#' Derived quantities returned by the model are:
#'   
#'   \tabular{ll}{
#'     \code{Bpvalue} \tab Bayesian p-value (overall) \cr
#'     \code{Bpvalue_species} \tab Bayesian p-value (by species) \cr
#'     
#'     
#'     \code{Nspecies} \tab Species richness (only in JAGS model)\cr
#'     \code{Nspecies_Covariate} \tab Species richness by categorical covariate (when using \code{richnessCategories}, only in JAGS model) \cr
#'     
#'     
#'     \code{R2} \tab sum of Freeman-Tukey residuals of observed data within each species \cr
#'     \code{new.R2} \tab sum of Freeman-Tukey residuals of simulated data within each species \cr
#'     \code{R3} \tab Total sum of Freeman-Tukey residuals of observed data \cr 
#'     \code{new.R3} \tab Total sum of Freeman-Tukey residuals of simulated data \cr
#'     \emph{\code{Ntotal}} \tab Total metacommunity size (= observed species + n0) \cr
#'     \emph{\code{n0}} \tab Number of unseen species in metacommunity \cr
#'     \emph{\code{omega}} \tab Data augmentation parameter \cr
#'     \emph{\code{w}} \tab Metacommunity membership indicator for each species
#'   }
#' 
#' Quantities in \emph{italic} at the bottom are only returned in full data augmentation. \code{Nspecies} and \code{Nspecies_Covariate} are only returned in JAGS models (because Nimble models don't explicitly return latent occupancy status z).
#' 
#' 
#' 
#' @return
#' 
#' \code{commOccu} object. It is an S4 class containing all information required to run the models. See  \code{\link{commOccu-class}} for details.
#'
#' @encoding UTF-8 
#' @references 
#' Kéry, M., and J. A. Royle. "Applied hierarchical modelling in ecology - Modeling distribution, abundance and species richness using R and BUGS." Volume 1: Prelude and Static Models. Elsevier/Academic Press, 2016.
#' 
#' @section Parameter naming convention: 
#' The parameter names are assembled from building blocks. The nomenclature  is as follows:    
#'   \tabular{lll}{
#'     \bold{Name}           \tab \bold{Refers to}  \tab  \bold{Description}  \cr
#'     \bold{\code{alpha}}   \tab Submodel          \tab detection submodel  \cr
#'     \bold{\code{beta}}    \tab Submodel          \tab occupancy submode \cr
#'     \bold{\code{0}}       \tab Intercept         \tab denotes the intercepts (alpha0, beta0)  \cr
#'     \bold{\code{fixed}}   \tab Effect type       \tab fixed effects (constant across species)  \cr
#'     \bold{\code{indep}}   \tab Effect type       \tab independent effects (separate for each species)  \cr
#'     \bold{\code{ranef}}   \tab Effect type       \tab random effects (of species and/or other categorical covariates)  \cr
#'     \bold{\code{cont}}    \tab Covariate type    \tab continuous covariates \cr
#'     \bold{\code{categ}}   \tab Covariate type    \tab categorical covariates \cr
#'     \bold{\code{mean}}    \tab Hyperparameter    \tab mean of random effect \cr
#'     \bold{\code{sigma}}   \tab Hyperparameter    \tab standard deviation of random effect \cr
#'     \bold{\code{tau}}     \tab Hyperparameter    \tab precision of random effect (used internally, not returned)
#'   }
#' 
#' 
#' For example, a fixed intercept of occupancy (constant across species) is \bold{\code{beta0}}, and a fixed intercept of detection probability is \bold{\code{alpha0}}.
#' 
#' 
#' An occupancy probability intercept with a random effect of species is: 
#'   
#' \bold{\code{beta0.mean}} community mean of the occupancy probability intercept
#' 
#' \bold{\code{beta0.sigma}} standard deviation of the community occupancy probability intercept.
#' 
#' \bold{\code{beta0[1]}} occupancy probability intercept of species 1 (likewise for other species). 
#' 
#' 
#' For effects of site covariates, the pattern is:
#'   
#' \code{submodel.effectType.covariateType.CovariateName.hyperparameter}
#' 
#' For example:
#'   
#' \bold{\code{beta.ranef.cont.habitat.mean}} is the mean community effect of the continuous site covariate 'habitat' on occupancy probability.
#' 
#' \bold{\code{beta.ranef.cont.habitat[1]}} is the effect of continuous site covariate 'habitat' on occupancy probability of species 1.
#' 
#' Site-occasion covariates are denoted by ".obs" after the submodel, e.g.: 
#'   
#' \bold{\code{alpha.obs.fixed.cont.effort}} is the fixed effect of the continuous observation-level covariate 'effort' on detection probability
#' 
#' 
#' @author Juergen Niedballa
#'  
#' @examples
#' 
#' \dontrun{
# the example below fits community occupancy models to the sample data in camtrapR
# models are fit both in JAGS and Nimble
# The data set only contains 5 species and 3 stations, so the results will be nonsense. 
# It is only a technical demonstration with the camtrapR workflow
# for more complete examples, see vignette 5
#' 
#' data("camtraps")
#' 
#' # create camera operation matrix
#' camop_no_problem <- cameraOperation(CTtable      = camtraps,
#'                                     stationCol   = "Station",
#'                                     setupCol     = "Setup_date",
#'                                     retrievalCol = "Retrieval_date",
#'                                     hasProblems  = FALSE,
#'                                     dateFormat   = "dmy"
#' )
#' 
#' data("recordTableSample")
#' 
#' # make list of detection histories
#' DetHist_list <- lapply(unique(recordTableSample$Species), FUN = function(x) {
#'   detectionHistory(
#'     recordTable         = recordTableSample,
#'     camOp                = camop_no_problem,
#'     stationCol           = "Station",
#'     speciesCol           = "Species",
#'     recordDateTimeCol    = "DateTimeOriginal",
#'     species              = x,
#'     occasionLength       = 7,
#'     day1                 = "station",
#'     datesAsOccasionNames = FALSE,
#'     includeEffort        = TRUE,
#'     scaleEffort          = TRUE,
#'     timeZone             = "Asia/Kuala_Lumpur"
#'   )}
#' )
#' 
#' # assign species names to list items
#' names(DetHist_list) <- unique(recordTableSample$Species)
#' 
#' # extract detection histories (omit effort matrices)
#' ylist <- lapply(DetHist_list, FUN = function(x) x$detection_history)
#' 
#' # create some fake covariates for demonstration
#' sitecovs <- camtraps[, c(1:3)]
#' sitecovs$elevation <- c(300, 500, 600)   
#' 
#' # scale numeric covariates
#' sitecovs[, c(2:4)] <- scale(sitecovs[,-1])
#' 
#' 
#' # bundle input data for communityModel
#' data_list <- list(ylist = ylist,
#'                   siteCovs = sitecovs,
#'                   obsCovs = list(effort = DetHist_list[[1]]$effort))
#' 
#' 
#' # create community model for JAGS
#' modelfile1 <- tempfile(fileext = ".txt")
#' mod.jags <- communityModel(data_list,
#'                            occuCovs = list(fixed = "utm_y", ranef = "elevation"),
#'                            detCovsObservation = list(fixed = "effort"),
#'                            intercepts = list(det = "ranef", occu = "ranef"),
#'                            modelFile = modelfile1)
#' 
#' summary(mod.jags)
#' 
#' # fit in JAGS
#' fit.jags <- fit(mod.jags,
#'                 n.iter = 1000,
#'                 n.burnin = 500,
#'                 chains = 3)   
#' summary(fit.jags)
#' 
#' # response curves (= marginal effect plots)
#' plot_effects(mod.jags, 
#'              fit.jags, 
#'              submodel = "state")
#' plot_effects(mod.jags, 
#'              fit.jags, 
#'              submodel = "det")
#'              
#' # effect sizes plot
#' plot_coef(mod.jags, 
#'           fit.jags, 
#'           submodel = "state")
#' plot_coef(mod.jags, 
#'           fit.jags, 
#'           submodel = "det")              
#' 
#' # create community model for Nimble
#' modelfile2 <- tempfile(fileext = ".txt")
#' mod.nimble <- communityModel(data_list,
#'                              occuCovs = list(fixed = "utm_x", ranef = "utm_y"),
#'                              detCovsObservation = list(fixed = "effort"),
#'                              intercepts = list(det = "ranef", occu = "ranef"),
#'                              modelFile = modelfile2, 
#'                              nimble = TRUE)      # set nimble = TRUE
#' 
#' # load nimbleEcology package 
#' # currently necessary to do explicitly, to avoid additional package dependencies
#' require(nimbleEcology)
#' 
#' # fit uncompiled model in Nimble
#' fit.nimble.uncomp <- fit(mod.nimble, 
#'                          n.iter = 10, 
#'                          chains = 1)
#' 
#' # fit compiled model in Nimble
#' fit.nimble.comp <- fit(mod.nimble, 
#'                        n.iter = 5000, 
#'                        n.burnin = 2500,
#'                        chains = 3, 
#'                        compile = TRUE)
#' 
#' # parameter summary statistics
#' summary(fit.nimble.comp)
#' 
#' 
#' # response curves (= marginal effect plots)
#' plot_effects(mod.nimble, 
#'              fit.nimble.comp, 
#'              submodel = "state")
#' plot_effects(mod.nimble, 
#'              fit.nimble.comp, 
#'              submodel = "det")
#' 
#' # effect sizes plot
#' plot_coef(mod.nimble, 
#'           fit.nimble.comp, 
#'           submodel = "state")
#' plot_coef(mod.nimble, 
#'           fit.nimble.comp, 
#'           submodel = "det")   
#' 
#' # traceplots
#' plot(fit.nimble.comp)
#' 
#' 
#' }
#' 

communityModel <- function(data_list,
                           model = c("Occupancy", "RN"),
                           occuCovs = list(fixed = NULL, independent = NULL, ranef = NULL),
                           detCovs = list(fixed = NULL, ranef = NULL),
                           detCovsObservation = list(fixed = NULL, ranef = NULL),
                           speciesSiteRandomEffect = list(det = FALSE, occu = FALSE),
                           intercepts = list(det = "fixed", occu = "fixed"),
                           effortCov = "effort",
                           richnessCategories = NULL,
                           augmentation = NULL,
                           modelFile = NULL,
                           nimble = FALSE)
{   
  
  
  model <- match.arg(model, choices = c("Occupancy", "RN"))
  if(model == "RN" & nimble) stop("Royle-Nichols models are currently not supported in Nimble.")
  
  
  speciesIndex <- "i"      # k in AHMbook
  stationIndex <- "j"      # i in AHMbook
  occasionIndex <- "k"     # j in AHMbook
  
  # this function uses [species, station, occasion] indexing. 
  # AHM book uses: [station, occasion, species]
  
  # occupancy parameters = beta 
  # detection parameters = alpha
  # ...0 -> intercept, ...1-n -> covariates
  
  # define prior distributions to be used throughout (maybe make it an argument later)
  prior_list <- list(dnorm = "dnorm(0, 0.05)",
                     dgamma = "dgamma(0.1, 0.1)")
  
  # define inits
  inits_list <- list(
    inits_runif_mean_low = list("runif", list(1, -4.5, -1.5)),   # n_draws, min, max
    inits_runif_mean_0 = list("runif", list(1, -1, 1)), 
    inits_runif_tau  = list("runif", list(1, 0.01, 0.1)),
    inits_runif_omega = list("runif", list(1, 0, 1))
  )
  
  stopifnot(is.list(intercepts))
  stopifnot(length(intercepts) == 2)
  stopifnot(names(intercepts) %in% c("det", "occu"))
  stopifnot(intercepts$det %in% c("fixed", "ranef", "independent"))
  stopifnot(intercepts$occu %in% c("fixed", "ranef", "independent"))
  stopifnot(is.list(speciesSiteRandomEffect))
  stopifnot(is.logical(speciesSiteRandomEffect$det))
  
  if(exists("occu", speciesSiteRandomEffect)) {
    stopifnot(is.logical(speciesSiteRandomEffect$occu))
    if(speciesSiteRandomEffect$occu) stop("speciesSiteRandomEffect$occu must be FALSE")
  }
  
  
  if(!is.list(data_list)) stop("data_list must be a list")
  
  if(exists("ylist", where = data_list)) {
    if(!inherits(data_list$ylist, "list")) stop("data_list$ylist must be a list")
  } else {
    stop("data_list$ylist is missing")
  }
  
  if(exists("obsCovs", where = data_list)) {
    if(!inherits(data_list$obsCovs, "list")) stop("data_list$obsCovs must be a list")
  } else {
    stop("data_list$obsCovs is missing")
  }
  
  # data frame with information about intercepts
  covariate_info <- data.frame(param = c("intercept", "intercept"),
                           submodel = c("det", "state"),
                           covariate_type = NA,
                           covariate = NA,
                           data_type = NA,         # categ / numeric
                           is_quadratic = FALSE,
                           has_quadratic = FALSE,
                           ranef = c(ifelse(intercepts$det == "ranef", T, F), 
                                     ifelse(intercepts$occu == "ranef", T, F)), 
                           ranef_nested = FALSE,
                           ranef_cov = NA, 
                           independent = c(ifelse(intercepts$det == "independent", T, F), 
                                           ifelse(intercepts$occu == "independent", T, F)), 
                           coef = c("alpha0", "beta0"))
                           
  
  
  # keyword for nested random effects
  keyword_nested <- "+Species"
  
  # keyword for quadratic covariate effect
  keyword_quadratic <- "_squared"
  
  # parse covariates ####
  
  ## observation level covariates  ####
  if(!effortCov %in% names(data_list$obsCovs)) stop(paste0("effortCov '", effortCov, "' is not in the list of observation covariates (data_list$obsCovs)."))
  
  
  if(!is.null(unlist(detCovsObservation))) {
    covariate_info <- rbind(covariate_info, get_cov_info (cov = detCovsObservation, 
                                                          keyword_nested, 
                                                          keyword_quadratic, 
                                                          data_list, 
                                                          type = "obs", 
                                                          submodel = "det"))
    }
  
  
  obs_covariates <- data_list$obsCovs
  obs_covariates_numeric <- names(obs_covariates)[sapply(obs_covariates, is.numeric)]
  obs_covariates_categ   <- names(obs_covariates)[sapply(obs_covariates, is.character)]
  
  if(!all(names(detCovsObservation) %in% c("fixed", "ranef"))) stop("detCovsObservation can currentlly only be 'fixed' or 'ranef'")
  
  if(!is.null(detCovsObservation$fixed)) {
    for(i in detCovsObservation$fixed) {
      if(i == effortCov) next
      if(any(is.na(data_list$obsCovs[[i]]))) warning(paste("there are NAs in obsCovs:", i))
    }
  }
  
  if(!is.null(detCovsObservation$ranef)) {
    for(i in detCovsObservation$ranef) {
      if(i == effortCov) next
      if(any(is.na(data_list$obsCovs[[i]]))) warning(paste("there are NAs in obsCovs:", i))
    }
  }
  
  detCovsObservation_categ <- list(fixed = NULL, ranef = NULL)
  
  if(!is.null(detCovsObservation$fixed)) {
    if(any(!detCovsObservation$fixed %in% obs_covariates_numeric)) {
      if(!all(detCovsObservation$fixed [which(!detCovsObservation$fixed %in% obs_covariates_numeric)] %in% obs_covariates_categ)) stop(paste("Detection covariate ", paste(detCovsObservation$fixed [which(!detCovsObservation$fixed %in% obs_covariates_numeric)], collapse = ", "), 
                                                                                                                                                                     "is not a factor"))
      if(nimble == TRUE) warning("Currently categorical observation-level covariates are only supported in JAGS.\nIn Nimble they can only be used in uncompiled models (even that is experimental). Change model structure or set 'nimble = FALSE'", call. = FALSE)
      
      detCovsObservation_categ$fixed <- detCovsObservation$fixed[detCovsObservation$fixed %in% obs_covariates_categ]
      detCovsObservation$fixed       <- detCovsObservation$fixed[detCovsObservation$fixed %in% obs_covariates_numeric]
      if(length(detCovsObservation$fixed) == 0) detCovsObservation <- list(fixed = NULL, ranef = detCovsObservation$ranef)
    }
  }
  
  if(!is.null(detCovsObservation$ranef)) {
    if(any(!detCovsObservation$ranef %in% obs_covariates_numeric)) {
      if(!all(detCovsObservation$ranef [which(!detCovsObservation$ranef %in% obs_covariates_numeric)] %in% obs_covariates_categ)) stop(paste("Detection covariate ", paste(detCovsObservation$ranef [which(!detCovsObservation$ranef %in% obs_covariates_numeric)], collapse = ", "), 
                                                                                                                                                                     "is not a factor"))
      if(nimble == TRUE) warning("Currently categorical observation-level covariates are only supported in JAGS.\nIn Nimble they can only be used in uncompiled models (even that is experimental). Change model structure or set 'nimble = FALSE'", call. = FALSE)
      
      detCovsObservation_categ$ranef <- detCovsObservation$ranef[detCovsObservation$ranef %in% obs_covariates_categ]
      detCovsObservation$ranef       <- detCovsObservation$ranef[detCovsObservation$ranef %in% obs_covariates_numeric]
      if(length(detCovsObservation$ranef) == 0) detCovsObservation <- list(fixed = detCovsObservation$fixed, ranef = NULL)
    }
  }
  
  # convert character matrices to integer
  if(length(obs_covariates_categ) >= 1) {
    for(i in 1:length(obs_covariates_categ)){
      levels_tmp <- levels(as.factor(data_list$obsCovs[[obs_covariates_categ[i]]]))
      matrix_tmp <- match(data_list$obsCovs[[obs_covariates_categ[i]]], levels_tmp)
      data_list$obsCovs <- c(data_list$obsCovs, 
                             xxx = list(matrix(matrix_tmp, 
                                               nrow = dim(data_list$obsCovs[[obs_covariates_categ[i]]])[1],
                                               ncol = dim(data_list$obsCovs[[obs_covariates_categ[i]]])[2])))
      names(data_list$obsCovs)[length(data_list$obsCovs)] <- paste0(obs_covariates_categ[i], "_integer")
      attr(data_list$obsCovs[[length(data_list$obsCovs)]], "levels") <- levels_tmp
    }
  }
  
  ## site covariates ####
  
  if(exists("siteCovs", where = data_list)) {
    if(!inherits(data_list$siteCovs, "data.frame")) stop("data_list$siteCovs must be a data.frame")
  } else {
    warning("data_list$siteCovs does not exist. No site covariates available.\n", call. = FALSE)
  }
  
  
  # get covariate information
  if(!is.null(unlist(detCovs))) {
    if(!all(c("obsCovs", "siteCovs") %in% names(data_list))) {
      stop("detCovs is defined, but data_list does not contain siteCovs or obsCovs.", call. = F)
    } else {
      covariate_info <- rbind(covariate_info, get_cov_info (detCovs, keyword_nested, keyword_quadratic, data_list, type = "site", submodel = "det"))
    }
  }
  
  if(!is.null(unlist(occuCovs))) {
    if(! "siteCovs" %in% names(data_list)) {
      stop("occuCovs is defined, but data_list does not contain siteCovs.", call. = F)
    } else {
      covariate_info <- rbind(covariate_info, get_cov_info (occuCovs, keyword_nested, keyword_quadratic, data_list, type = "site", submodel = "state")) 
    }
  }
  
  covariates_original <- data_list$siteCovs

  # identify required covariates:
  site_covs_tmp <- unlist(c(occuCovs, detCovs, richnessCategories))
  if(length(site_covs_tmp) >= 1){
    covariatenames_needed <- unique(unlist(strsplit(site_covs_tmp, split = "|", fixed = T)))
  } else { 
    covariatenames_needed <- NULL
  }
  
  
  # remove "+Species" which indicates nested random effects
  covariatenames_needed <- gsub(keyword_nested, "", covariatenames_needed, fixed = T)
  
  # subset covariates
  covariates <- covariates_original [, colnames(covariates_original) %in% covariatenames_needed, drop = FALSE]
  
  # make a note of which covariates are in model (for summary method)
  attr(data_list$siteCovs, "in_model") <- colnames(covariates_original) %in% colnames(covariates)
  
  
  
  # identify numeric and categorical covariates
  if(ncol(covariates) != 0) {
    covariates_numeric <- names(covariates)[sapply(covariates, is.numeric)]
    covariates_categ   <- names(covariates)[sapply(covariates, is.factor)]
    covariates_char    <- names(covariates)[sapply(covariates, is.character)]
  } else {
    covariates_numeric <- NULL
    covariates_categ   <- NULL
    covariates_char    <- NULL
  }
  
  # throw error for character covariates
  
  if(length(covariates_char) >= 1) stop(paste("Covariate", covariates_char, "is character. Please convert to factor."), call. = FALSE)
    
  # paste cov1|cov2 (to check for random effects other than species, and nested random effects)
  covariates_numeric_categ <- unlist(sapply(covariates_numeric, paste, covariates_categ, sep = "|", simplify = F), use.names = F)
  covariates_numeric_categ <- c(covariates_numeric_categ, paste0(covariates_numeric_categ, keyword_nested))
  covariates_categ_categ   <- unlist(sapply(covariates_categ, paste, covariates_categ, sep = "|", simplify = F), use.names = F)
  

  if(!is.null(richnessCategories)) {
    if(length(richnessCategories) > 1) stop("richnessCategories can only have length 1, or be NULL")
    if(!richnessCategories %in% covariates_categ) stop(paste("richnessCategories", richnessCategories, "is not a categorical covariate in the covariate data frame"))
    if(nimble) warning("richnessCategories will be ignored because nimble = TRUE")
    occuIntercept_categ <- list(fixed = richnessCategories)
  } else {
    occuIntercept_categ <- list(fixed = NULL)
  }
  

  
  occuCovs_categ <- list(fixed = NULL, ranef = NULL)
  detCovs_categ  <- list(fixed = NULL, ranef = NULL)
  
  ## input checks on covariates  ####
  
  if(!is.null(occuCovs$fixed)) {
    if(any(!occuCovs$fixed %in% colnames(covariates))) {
      stop(paste(occuCovs$fixed [!occuCovs$fixed %in% colnames(covariates)], collapse = ", "), " is not in covariates")
    }
    if(any(!occuCovs$fixed %in% covariates_numeric)) {
      if(!all(occuCovs$fixed [which(!occuCovs$fixed %in% covariates_numeric)] %in% covariates_categ)) stop(paste("Occupancy covariate ", paste(occuCovs$fixed [which(!occuCovs$fixed %in% covariates_numeric)], collapse = ", "), 
                                                                                                                                         "is not a factor"))
      occuCovs_categ$fixed <- occuCovs$fixed[occuCovs$fixed %in% covariates_categ]
      occuCovs$fixed       <- occuCovs$fixed[occuCovs$fixed %in% covariates_numeric]
      if(length(occuCovs$fixed) == 0) occuCovs <- modifyList(occuCovs, list(fixed = NULL))
    }
  }
  
  if(!is.null(occuCovs$independent)) {
    if(any(!occuCovs$independent %in% colnames(covariates))) {
      stop(paste(occuCovs$independent [!occuCovs$independent %in% colnames(covariates)], collapse = ", "), " is not in covariates")
    }
    if(any(!occuCovs$independent %in% covariates_numeric)) {
      if(!all(occuCovs$independent [which(!occuCovs$independent %in% covariates_numeric)] %in% covariates_categ)) stop(paste("Occupancy covariate ", paste(occuCovs$independent [which(!occuCovs$independent %in% covariates_numeric)], collapse = ", "), 
                                                                                                                 "is not a factor"))
      
      # prohibit independent effects of categorical covariates
      if(any(occuCovs$independent %in% covariates_categ)) stop("independent effects of categorical site covariates are currently not supported")
      
      # occuCovs_categ$independent <- occuCovs$independent[occuCovs$independent %in% covariates_categ]
      # occuCovs$independent       <- occuCovs$independent[occuCovs$independent %in% covariates_numeric]
      # if(length(occuCovs$independent) == 0) occuCovs <- modifyList(occuCovs, list(independent = NULL))
    }
  }
  
  
  if(!is.null(occuCovs$ranef)) {
    if(any(occuCovs$ranef %in% covariates_categ_categ)) stop("random effects of categorical site covariates on other categorical site covariates are not supported yet (except for random effect of species)")
    
    if(any(!occuCovs$ranef %in% c(covariates_numeric, covariates_categ, covariates_numeric_categ))) {
      stop(paste(occuCovs$ranef [!occuCovs$ranef %in% c(covariates_numeric, covariates_numeric_categ)], collapse = ", "), " is not in covariates")
    }
    if(any(!occuCovs$ranef %in% covariates_numeric)) {  
      if(!all(occuCovs$ranef [which(!occuCovs$ranef %in% c(covariates_numeric, covariates_numeric_categ))] %in% covariates_categ)) stop(paste("Occupancy covariate ", paste(occuCovs$ranef [which(!occuCovs$ranef %in% covariates_numeric)], collapse = ", "), 
                                                                                                                                                                      "is not a factor"))
      occuCovs_categ$ranef <- c(occuCovs_categ$ranef, 
                                occuCovs$ranef[occuCovs$ranef %in% c(covariates_categ, covariates_categ_categ)])
      if(length(occuCovs_categ$ranef) == 0) occuCovs_categ$ranef <- NULL
      occuCovs$ranef       <- occuCovs$ranef[occuCovs$ranef %in% c(covariates_numeric, covariates_numeric_categ)]
      if(length(occuCovs$ranef) == 0) occuCovs <- modifyList(occuCovs, list(ranef = NULL))
    }
  }
  
  if(!is.null(detCovs$fixed)) {
    if(any(!detCovs$fixed %in% colnames(covariates))) {
      stop(paste(detCovs$fixed [!detCovs$fixed %in% colnames(covariates)], collapse = ", "), " is not in covariates")
    }
    if(any(!detCovs$fixed %in% covariates_numeric)) {
      if(!all(detCovs$fixed [which(!detCovs$fixed %in% covariates_numeric)] %in% covariates_categ)) stop(paste("Detection covariate ", paste(detCovs$fixed [which(!detCovs$fixed %in% covariates_numeric)], collapse = ", "), 
                                                                                                                                       "is not a factor"))
      
      detCovs_categ$fixed <- detCovs$fixed[detCovs$fixed %in% covariates_categ]
      detCovs$fixed       <- detCovs$fixed[detCovs$fixed %in% covariates_numeric]
      if(length(detCovs$fixed) == 0) detCovs <- modifyList(detCovs, list(fixed = NULL))
    }
  }
  
  
  if(!is.null(detCovs$independent)) {
    if(any(!detCovs$independent %in% colnames(covariates))) {
      stop(paste(detCovs$independent [!detCovs$independent %in% colnames(covariates)], collapse = ", "), " is not in covariates")
    }
    if(any(!detCovs$independent %in% covariates_numeric)) {
      if(!all(detCovs$independent [which(!detCovs$independent %in% covariates_numeric)] %in% covariates_categ)) stop(paste("Detection covariate ", paste(detCovs$independent [which(!detCovs$independent %in% covariates_numeric)], collapse = ", "), 
                                                                                                               "is not a factor"))

      # prohibit independent effects of categorical covariates
      if(any(detCovs$independent %in% covariates_categ)) stop("independent effects of categorical site covariates are currently not supported")
      
      # detCovs_categ$independent <- detCovs$independent[detCovs$independent %in% covariates_categ]
      # detCovs$independent       <- detCovs$independent[detCovs$independent %in% covariates_numeric]
      # if(length(detCovs$independent) == 0) detCovs <- modifyList(detCovs, list(independent = NULL))
    }
  }
  
  
  if(!is.null(detCovs$ranef)) {
    
    if(any(detCovs$ranef %in% covariates_categ_categ)) stop("random effects of categorical site covariates on other categorical site covariates are not supported yet (except for random effect of species)")
    
    if(any(!detCovs$ranef %in% c(colnames(covariates), covariates_categ, covariates_numeric_categ))) {
      stop(paste(detCovs$ranef [!detCovs$ranef %in% colnames(covariates)], collapse = ", "), " is not in covariates")
    }
    
    if(any(!detCovs$ranef %in% covariates_numeric)) {
      if(!all(detCovs$ranef [which(!detCovs$ranef %in% c(covariates_numeric, covariates_numeric_categ))] %in% covariates_categ)) stop(paste("Detection covariate ", paste(detCovs$ranef [which(!detCovs$ranef %in% c(covariates_numeric, covariates_numeric_categ))], collapse = ", "), 
                                                                                                                                                                    "is not a factor"))
      detCovs_categ$ranef <- c(detCovs_categ$ranef, 
                               detCovs$ranef[detCovs$ranef %in% c(covariates_categ, covariates_categ_categ)])
      if(length(detCovs_categ$ranef) == 0) detCovs_categ$ranef <- NULL
      detCovs$ranef       <- detCovs$ranef[detCovs$ranef %in% c(covariates_numeric, covariates_numeric_categ)]
      if(length(detCovs$ranef) == 0) detCovs <- modifyList(detCovs, list(ranef = NULL))
    }
  }
  
  
                       
  
  # some more checks for dimensions and data types
  dim_dethist1 <- dim(data_list$ylist[[1]])
  if(!all(sapply(data_list$ylist,   FUN = function(x) all(dim(x) == dim_dethist1)))) stop("dimensions differ between items in data_list$ylist")
  if(!all(sapply(data_list$obsCovs, FUN = function(x) all(dim(x) == dim_dethist1)))) stop("dimensions differ between items in data_list$ylist and data_list$obsCovs")
  
  if(exists("siteCovs", where = data_list)) {
    if(inherits(data_list$siteCovs, "data.frame")) if(nrow(data_list$siteCovs) != dim_dethist1[1]) stop("nrow differs between data_list$siteCovs and the detection histories in data_list$ylist")
  }
  
  
  ## Data augmentation ####
  
  # check if all-0 detection histories present
  if(any(sapply(data_list$ylist, sum, na.rm = T) == 0)) {
    warning("There are all-0 detection histories. It is recommended to instead provide only detected species, and control data augmentation via 'augmentation'.")
  }
  
  
  if(!is.null(augmentation)) {
    if(length(augmentation) != 1) stop("'augmentation' must have length 1")
    if(is.null(names(augmentation))) stop("'augmentation' must be named")
    if(!names(augmentation) %in% c("maxknown", "full"))  stop("name of 'augmentation' must be either 'maxknown' or 'full'")
    augment <- names(augmentation)
    if(inherits(augmentation, "list")) augmentation <- as.vector(augmentation, mode = "numeric")
    
    if(augmentation <= length(data_list$ylist)) stop("'augmentation' must be larger than the number of detected species") 
    
    nspec <- length(data_list$ylist) # number of seen species
    
    nz <- augmentation - length(data_list$ylist)   # number of unseen species added
    data_list$ylist <- c(data_list$ylist, replicate(augmentation - length(data_list$ylist), data_list$ylist[[1]] * 0, simplify = FALSE))
    
  } else {
    augment = "none"
  }
  
  # set indices to use in loops
  speciesMax  <- "M"
  stationMax  <- "J"
  occasionMax <- "maxocc"
  
  speciesMax_value  <- length(data_list$ylist)
  stationMax_value  <- nrow(data_list$ylist[[1]])
  occasionMax_value <- ncol(data_list$ylist[[1]])
  
  
  
  # Priors  ####
  
  model_start <- "model{\n### PRIORS \n"
  
  
  ## Intercept - occupancy - hyperpriors   ####
  
  intercept_occu <- interceptPriors(effect = intercepts$occu, 
                                    type = "occupancy", 
                                    prior_list = prior_list, 
                                    inits_list = inits_list, 
                                    speciesIndex = speciesIndex,
                                    speciesMax = speciesMax,
                                    speciesMax_value = speciesMax_value)
  
  priors_intercept_occu <- intercept_occu$prior
  beta0_formula         <- intercept_occu$formula
  
  ## Intercept - detection - hyperpriors ####
  
  intercept_det <- interceptPriors(effect = intercepts$det, 
                                   type = "detection", 
                                   prior_list = prior_list, 
                                   inits_list = inits_list, 
                                   speciesIndex = speciesIndex,
                                   speciesMax = speciesMax,
                                   speciesMax_value = speciesMax_value)
  
  priors_intercept_det <- intercept_det$prior
  alpha0_formula       <- intercept_det$formula
  
  
  ##  data augmentation priors ####
  
  if(augment == "full") {
    priors_omega <- paste(
      "## Data augmentation parameter",
      "omega ~ dunif(0, 1)",
      "\n", sep = "\n"
    )
    attr(priors_omega, "params") <- c("omega")
    attr(priors_omega, "inits") <- list(omega = inits_list$inits_runif_omega)
  } else {
    priors_omega <- paste(
      "## Data augmentation parameter",
      "# < empty > ",
      "\n", sep = "\n"
    )
  }
  
  
  ## obs Cov - cont - fixed effect priors ####
  prior_obs_header_fixed <- paste("## Continuous observation-level covariates on detection - Fixed effects\n")
  
  if(!is.null(detCovsObservation$fixed)) {
    priors_obscovs_det_fixed <- list()
    
    for(obsCovs_det_index in 1:length(detCovsObservation$fixed)){
      current_cov <- detCovsObservation$fixed[obsCovs_det_index]
      
      priors_obscovs_det_fixed[[obsCovs_det_index]] <- paste(
        paste("# Covariate:", current_cov),
        paste0("alpha.obs.fixed.cont.", current_cov, " ~ ", prior_list$dnorm), 
        "\n", 
        sep = "\n")
      attr(priors_obscovs_det_fixed, "params") <- paste0("alpha.obs.fixed.cont.", detCovsObservation$fixed)
      
      inits_tmp <- vector(mode = "list", length = length(detCovsObservation$fixed))
      names(inits_tmp) <- c(paste0("alpha.obs.fixed.cont.", detCovsObservation$fixed))
      
      for(i in 1:length(inits_tmp)){
        inits_tmp[[i]] <- inits_list$inits_runif_mean_0      #list("runif", list(1))
      }
      attr(priors_obscovs_det_fixed, "inits") <- inits_tmp
      rm(inits_tmp)
    }
  } else {
    priors_obscovs_det_fixed <- list("# < empty > \n\n")
    attr(priors_obscovs_det_fixed, "params") <- NULL
  }
  
  ## obs Cov - cont - random effect priors ####
  prior_obs_header_ranef <- paste("## Continuous observation-level covariates on detection - with random effects\n")
  
  if(!is.null(detCovsObservation$ranef)){
    priors_obscovs_det_ranef <- list()
    alpha_draws_ranef_obs_cont <- list()
    
    for(obsCovs_det_index in 1:length(detCovsObservation$ranef)){
      current_cov <- detCovsObservation$ranef[obsCovs_det_index]
      priors_obscovs_det_ranef[[obsCovs_det_index]] <- paste(
        paste("# Observation Covariate:", current_cov),
        paste0("alpha.obs.ranef.cont.", current_cov, ".mean", " ~ ", prior_list$dnorm),
        paste0("alpha.obs.ranef.cont.", current_cov, ".tau", " ~ ", prior_list$dgamma),
        paste0("alpha.obs.ranef.cont.", current_cov, ".sigma", " <- sqrt(1 / alpha.obs.ranef.cont.", current_cov, ".tau)"),
        "\n", 
        sep = "\n")
      
      alpha_draws_ranef_obs_cont[[obsCovs_det_index]] <- paste("# continuous detection covariates with random effects:", 
                                                               paste0("alpha.obs.ranef.cont.", current_cov, "[", speciesIndex, "] ~ dnorm(alpha.obs.ranef.cont.", current_cov, ".mean", ", alpha.obs.ranef.cont.", current_cov, ".tau)"),
                                                               sep = "\n")
    }
    
    attr(priors_obscovs_det_ranef, "params") <- c(paste0("alpha.obs.ranef.cont.", detCovsObservation$ranef, ".mean"),
                                                  paste0("alpha.obs.ranef.cont.", detCovsObservation$ranef, ".sigma"))
    
    
    # make list of parameters
    inits_tmp <- vector(mode = "list", length = length(detCovsObservation$ranef) * 2)
    names(inits_tmp) <- c(paste0("alpha.obs.ranef.cont.", detCovsObservation$ranef, ".mean"),
                          paste0("alpha.obs.ranef.cont.", detCovsObservation$ranef, ".tau"))
    
    for(i in 1:length(inits_tmp)){
      if(endsWith(names(inits_tmp)[i], "mean")) inits_tmp[[i]] <- inits_list$inits_runif_mean_0
      if(endsWith(names(inits_tmp)[i], "tau"))  inits_tmp[[i]] <- inits_list$inits_runif_tau
    }
    attr(priors_obscovs_det_ranef, "inits") <- inits_tmp
    rm(inits_tmp)
    
    names(alpha_draws_ranef_obs_cont) <- paste0("alpha.obs.ranef.cont.", detCovsObservation$ranef)
    
  } else {
    priors_obscovs_det_ranef <- list("# < empty > \n\n")
    attr(priors_obscovs_det_ranef, "params") <- NULL
    alpha_draws_ranef_obs_cont <- "# continuous observation-level detection covariates: no random effect of species"
  }
  
  
  
  
  ## obs Cov - categ - fixed effect priors ####
  prior_obs_categ_header_fixed <- paste("## Categorical observation-level covariates on detection - Fixed effect\n")
  
  if(!is.null(detCovsObservation_categ$fixed)){
    priors_obscovs_det_categ_fixed <- list()
    
    attr(priors_obscovs_det_categ_fixed, "n_levels") <- rep(NA, times = length(detCovsObservation_categ$fixed))
    
    for(obsCovs_det_categ_index in 1:length(detCovsObservation_categ$fixed)){
      
      obsCovCat_tmp_fact <- factor(data_list$obsCovs[[detCovsObservation_categ$fixed[obsCovs_det_categ_index]]])
      
      current_cov <- detCovsObservation_categ$fixed[obsCovs_det_categ_index]
      current_cov_values <- obsCovCat_tmp_fact   #covariates[, current_cov]
      index_letter <- paste0("index_cat_fixed_obs_categ_", current_cov)
      
      priors_obscovs_det_categ_fixed[[obsCovs_det_categ_index]] <- paste(
        paste("# Covariate:", current_cov),
        paste0("alpha.obs.fixed.categ.", current_cov, "[1] <- 0"),
        paste0("for(", index_letter, " in 2:", length(levels(current_cov_values)), ") {"),
        paste0("alpha.obs.fixed.categ.", current_cov, "[", index_letter, "] ~ ", prior_list$dnorm),
        "}",
        "\n", 
        sep = "\n")
      attr(priors_obscovs_det_categ_fixed, "n_levels")[obsCovs_det_categ_index] <- length(levels(current_cov_values))
    }
    attr(priors_obscovs_det_categ_fixed, "params") <- paste0("alpha.obs.fixed.categ.", detCovsObservation_categ$fixed)
    attr(priors_obscovs_det_categ_fixed, "index_letters") <- paste0("index_cat_fixed_obs_categ_", detCovsObservation_categ$fixed)
    
    inits_tmp <- vector(mode = "list", length = length(detCovsObservation_categ$fixed))
    names(inits_tmp) <- paste0("alpha.obs.fixed.categ.", detCovsObservation_categ$fixed)
    for(n in 1:length(detCovsObservation_categ$fixed)) {
      inits_tmp[[n]][1] <- NA
      inits_tmp[[n]][2:attr(priors_obscovs_det_categ_fixed, "n_levels")[n]] <- rnorm(attr(priors_obscovs_det_categ_fixed, "n_levels")[n] - 1)
    }
    attr(priors_obscovs_det_categ_fixed, "inits") <- inits_tmp
    rm(inits_tmp)
    
  } else {
    priors_obscovs_det_categ_fixed <- list("# < empty > \n\n")
    attr(priors_obscovs_det_categ_fixed, "params") <- NULL
  }
  
  
  ## obs Cov - categ - random effect priors ####
  
  prior_obs_categ_header_ranef <- paste("## Categorical observation-level covariates on detection - with random effects\n")
  if(!is.null(detCovsObservation_categ$ranef)){
    priors_obscovs_det_categ_ranef <- list()
    alpha_draws_ranef_obs_categ <- list()
    
    attr(priors_obscovs_det_categ_ranef, "n_levels") <- rep(NA, times = length(detCovsObservation_categ$ranef))
    
    for(obsCovs_det_categ_index in 1:length(detCovsObservation_categ$ranef)){
      
      obsCovCat_tmp_fact <- factor(data_list$obsCovs[[detCovsObservation_categ$ranef[obsCovs_det_categ_index]]])
      
      current_cov <- detCovsObservation_categ$ranef[obsCovs_det_categ_index]
      current_cov_values <- obsCovCat_tmp_fact   #covariates[, current_cov]
      index_letter <- paste0("index_cat_ranef_obs_categ_", current_cov)
      
      
      
      priors_obscovs_det_categ_ranef[[obsCovs_det_categ_index]] <- paste(
        #paste("# Covariate:", current_cov),
        paste("# Community mean effects of", current_cov),
        paste0("alpha.obs.ranef.categ.", current_cov, ".mean[1] <- 0"),
        paste0("alpha.obs.ranef.categ.", current_cov, ".sigma[1] <- 0"),     # is included in random effect of intercept already
        paste0("alpha.obs.ranef.categ.", current_cov, ".tau[1] <- 0"),
        "",
        paste0("for(", index_letter, " in 2:", length(unique(current_cov_values)), ") {"),
        paste0("alpha.obs.ranef.categ.", current_cov, ".mean[", index_letter, "] ~ ", prior_list$dnorm),   
        paste0("alpha.obs.ranef.categ.", current_cov,  ".tau[", index_letter, "] ~ ", prior_list$dgamma),
        paste0("alpha.obs.ranef.categ.", current_cov,  ".sigma[", index_letter, "] <- sqrt(1 / ", "alpha.obs.ranef.categ.", current_cov,  ".tau[", index_letter, "]", ")"),
        "}",
        "",
        sep = "\n")
      attr(priors_obscovs_det_categ_ranef, "n_levels")[obsCovs_det_categ_index] <- length(unique(current_cov_values))
      
      
      alpha_draws_ranef_obs_categ[[obsCovs_det_categ_index]] <- paste("# categorical detection covariates with random effects:", 
                                                                      paste0("alpha.obs.ranef.categ.", current_cov, "[", speciesIndex, ", 1] <- 0"), 
                                                                      paste0("for (", index_letter, " in 2:",  length(unique(current_cov_values)), "){"),
                                                                      paste0("alpha.obs.ranef.categ.", current_cov, "[", speciesIndex, ", ", index_letter, "] ~ dnorm(alpha.obs.ranef.categ.", current_cov, ".mean[", index_letter, "], alpha.obs.ranef.categ.", current_cov, ".tau[", index_letter, "])"),
                                                                      "}", sep = "\n")
    }
    
    attr(priors_obscovs_det_categ_ranef, "params") <- c(paste0("alpha.obs.ranef.categ.", detCovsObservation_categ$ranef, ".mean"),
                                                        paste0("alpha.obs.ranef.categ.", detCovsObservation_categ$ranef, ".sigma"))
    
    attr(priors_obscovs_det_categ_ranef, "index_letters") <- paste0("index_cat_ranef_obs_", detCovsObservation_categ$ranef)
    
    inits_tmp <- vector(mode = "list", length = length(detCovsObservation_categ$ranef)  * 2)
    names(inits_tmp) <- c(paste0("alpha.obs.ranef.categ.", detCovsObservation_categ$ranef, ".mean"),
                          paste0("alpha.obs.ranef.categ.", detCovsObservation_categ$ranef, ".tau"))
    
    for(n in 1:length(detCovsObservation_categ$ranef)) {
      inits_tmp[[n]][1] <- NA
      inits_tmp[[n]][2:attr(priors_obscovs_det_categ_ranef, "n_levels")[n]] <- rnorm(attr(priors_obscovs_det_categ_ranef, "n_levels")[n] - 1)
      
      inits_tmp[[n + length(detCovsObservation_categ$ranef)]][1] <- NA
      inits_tmp[[n + length(detCovsObservation_categ$ranef)]][2:attr(priors_obscovs_det_categ_ranef, "n_levels")[n]] <- abs(rnorm(attr(priors_obscovs_det_categ_ranef, "n_levels")[n] - 1))
      
    }
    attr(priors_obscovs_det_categ_ranef, "inits") <- inits_tmp
    rm(inits_tmp)
    
    names(alpha_draws_ranef_obs_categ) <- paste0("alpha.obs.ranef.categ.", detCovsObservation_categ$ranef)
  } else {
    priors_obscovs_det_categ_ranef <- list("# < empty > \n\n")
    attr(priors_obscovs_det_categ_ranef, "params") <- NULL
    alpha_draws_ranef_obs_categ <- "# categorical observation covariates: no random effect of species"
  }
  
  
  
  
  
  ## det Cov - cont - fixed effect priors ####
  prior_det_header_fixed <- paste("## Continuous site covariates on detection - Fixed effects\n")
  
  priors_sitecovs_det_fixed <- fixedEffectPriors (effect_names = detCovs$fixed,
                                                  type = "detection", 
                                                  prior_list = prior_list, 
                                                  stationIndex = stationIndex)
  
  
  
  ## det Cov - cont - independent effect priors ####
  prior_det_header_indep <- paste("## Continuous site covariates on detection - Independent effects\n")
  
  priors_sitecovs_det_indep <- independentEffectPriors (effect_names = detCovs$independent,
                                                        type = "detection",
                                                        inits_list = inits_list,
                                                        prior_list = prior_list, 
                                                        stationIndex = stationIndex,
                                                        speciesIndex = speciesIndex,
                                                        speciesMax = speciesMax,
                                                        speciesMax_value = speciesMax_value)
  
  
  
  
  ## det Cov - cont - random effect priors ####
  prior_det_header_ranef <- paste("## Continuous site covariates on detection - with random effects\n")
  
  tmp <- randomEffectPriors(effect_names = detCovs$ranef, type = "detection", covariates = covariates,
                            prior_list = prior_list, inits_list = inits_list, speciesIndex = speciesIndex, stationIndex = stationIndex, 
                            speciesMaxNumber = speciesMax_value, speciesMax = speciesMax)
  priors_sitecovs_det_ranef  <- tmp[[1]]
  alpha_draws_ranef_det_cont <- tmp[[2]]
  rm(tmp)
  
  
  ## det Cov - categ- fixed effect priors ####
  prior_det_categ_header_fixed <- paste("## Categorical site covariates on detection - Fixed effect\n")
  
  priors_sitecovs_det_categ_fixed <- fixedEffectPriorsCateg(effect_names = detCovs_categ$fixed, 
                                                            covariates = covariates,
                                                            type = "detection",
                                                            prior_list = prior_list, 
                                                            stationIndex = stationIndex)
  
  
  
  ## det Cov - categ - random effect priors ####
  prior_det_categ_header_ranef <- paste("## Categorical site covariates on detection - with random effects\n")
  
  tmp <- randomEffectPriorsCateg(effect_names = detCovs_categ$ranef,
                                 covariates = covariates,
                                 type = "detection",
                                 prior_list = prior_list, 
                                 speciesIndex = speciesIndex, 
                                 stationIndex = stationIndex)
  priors_sitecovs_det_categ_ranef <- tmp[[1]]
  alpha_draws_ranef_det_categ <- tmp[[2]]
  rm(tmp)
  
  
  
  
  
  ## occu Cov - cont - fixed effect priors ####
  prior_occu_header_fixed <- paste("## Continuous site covariates on Occupancy - Fixed effects\n")
  
  priors_sitecovs_occu_fixed <- fixedEffectPriors (effect_names = occuCovs$fixed,
                                                   type = "occupancy",
                                                   prior_list = prior_list, 
                                                   stationIndex = stationIndex)
  
  
  ## occu Cov - cont - independent effect priors ####
  prior_occu_header_indep <- paste("## Continuous site covariates on Occupancy - Independent effects\n")
  
  priors_sitecovs_occu_indep <- independentEffectPriors (effect_names = occuCovs$independent,
                                                         type = "occupancy",
                                                         inits_list = inits_list,
                                                         prior_list = prior_list, 
                                                         stationIndex = stationIndex,
                                                         speciesIndex = speciesIndex,
                                                         speciesMax = speciesMax,
                                                         speciesMax_value = speciesMax_value)
  
  
  
  
  
  ## occu Cov - cont  - random effect priors ####
  prior_occu_header_ranef <- paste("## Continuous site covariates on occupancy - with random effects\n")
  tmp <- randomEffectPriors(effect_names = occuCovs$ranef,
                            type = "occupancy",
                            covariates = covariates,
                            prior_list = prior_list, 
                            inits_list = inits_list, 
                            speciesIndex = speciesIndex, 
                            stationIndex = stationIndex, 
                            speciesMaxNumber = speciesMax_value, 
                            speciesMax = speciesMax)
  priors_sitecovs_occu_ranef <- tmp[[1]]
  beta_draws_ranef_occu_cont <- tmp[[2]]
  rm(tmp)
  
  
  
  ## occu Cov - categ - fixed effect priors ####
  prior_occu_categ_header_fixed <- paste("## Categorical site covariates on Occupancy - Fixed effects\n")
  
  priors_sitecovs_occu_categ_fixed <- fixedEffectPriorsCateg(effect_names = occuCovs_categ$fixed,
                                                             covariates = covariates,
                                                             type = "occupancy", 
                                                             prior_list = prior_list,
                                                             stationIndex = stationIndex)
  
  
  
  
  ## occu Cov - categ - random effect priors ####
  prior_occu_categ_header_ranef <- paste("## Categorical site covariates on occupancy - with random effects\n")
  
  tmp <- randomEffectPriorsCateg(effect_names = occuCovs_categ$ranef,
                                 covariates = covariates,
                                 type = "occupancy", 
                                 prior_list = prior_list, 
                                 speciesIndex = speciesIndex, 
                                 stationIndex = stationIndex)
  priors_sitecovs_occu_categ_ranef <- tmp[[1]]
  beta_draws_ranef_occu_categ <- tmp[[2]]
  rm(tmp)
  
  
  
  
  
  
  # random effect of species and station on detection probability  ####
  prior_det_species_station_ranef <- "# Species-station random effect on detection probability\n"
  tmp <-  randomSpeciesStationEffectPriors(doit = speciesSiteRandomEffect$det,
                                           prior_list = prior_list, 
                                           inits_list = inits_list, 
                                           speciesIndex = speciesIndex, 
                                           stationIndex = stationIndex)
  priors_SpeciesStation_det_ranef <- tmp[[1]]
  alpha_draws_SpeciesStation_det_ranef <- tmp[[2]]
  rm(tmp)
  
  
  
  
  before_species_loop <- paste("## Draws of random effects other than species",
                               paste(unlist(alpha_draws_ranef_det_cont)[attr(alpha_draws_ranef_det_cont, "random_effect") == "not_species"]),
                               paste(unlist(beta_draws_ranef_occu_cont)[attr(beta_draws_ranef_occu_cont, "random_effect") == "not_species"]),
                               sep = "\n"
  )
  
  
  # Species loop ####
  loop1 <- paste("\n### MODEL LOOPS \n",
                 "# species loop", 
                 paste0("for (", speciesIndex, " in 1:", speciesMax, "){"),
                 if ("ranef" %in% c(intercepts$det, intercepts$occu)) {
                   paste("##  Draw species-specific random effect parameters from community distributions", 
                         "# intercepts:", sep = "\n")}, 
                 if(intercepts$occu == "ranef") {
                   #paste0("beta0[", speciesIndex, "] ~ dnorm(mean.beta0, tau.beta0)")},
                   paste0("beta0[", speciesIndex, "] ~ dnorm(beta0.mean, beta0.tau)")},
                 if(intercepts$det == "ranef") { 
                   #paste0("alpha0[", speciesIndex, "] ~ dnorm(mean.alpha0, tau.alpha0)")},
                   paste0("alpha0[", speciesIndex, "] ~ dnorm(alpha0.mean, alpha0.tau)")},
                 "",
                 if(augment == "full"){paste0("# Metacommunity membership indicator (data augmentation)\n", "w[", speciesIndex, "] ~ dbern(omega)\n")},
                 paste(unlist(alpha_draws_ranef_det_cont[attr(alpha_draws_ranef_det_cont, "random_effect") == "species"]), collapse = "\n"), 
                 "\n",
                 paste(unlist(alpha_draws_ranef_det_categ), collapse = "\n"),
                 "\n",         
                 paste(unlist(alpha_draws_ranef_obs_cont), collapse = "\n"),
                 "\n",
                 paste(unlist(alpha_draws_ranef_obs_categ), collapse = "\n"),
                 "\n",
                 paste(unlist(beta_draws_ranef_occu_cont[attr(beta_draws_ranef_occu_cont, "random_effect") == "species"]), collapse = "\n"), 
                 "\n", 
                 paste(unlist(beta_draws_ranef_occu_categ), collapse = "\n"),
                 "\n", sep = "\n")
  
  attr(loop1, "params") <- c("beta0", "alpha0", 
                             names(alpha_draws_ranef_det_cont), names(alpha_draws_ranef_det_categ), names(alpha_draws_ranef_obs_cont), names(alpha_draws_ranef_obs_categ), 
                             names(beta_draws_ranef_occu_cont), names(beta_draws_ranef_occu_categ))
  if(augment == "full"){
    attr(loop1, "params") <- c(attr(loop1, "params"), "w")
    attr(loop1, "inits") <- list(w = rep(1, length(data_list$ylist)))
  }
  
  
  
  
  ## construct formula for psi (ecological model - beta parameters)  ####
  
  beta_formula_occu_fixed <- attr(priors_sitecovs_occu_fixed, "formula")
  beta_formula_occu_indep <- attr(priors_sitecovs_occu_indep, "formula")
  beta_formula_occu_ranef <- attr(priors_sitecovs_occu_ranef, "formula")
  beta_formula_occu_categ_fixed <- attr(priors_sitecovs_occu_categ_fixed, "formula")
  beta_formula_occu_categ_ranef <- attr(priors_sitecovs_occu_categ_ranef, "formula")
  
  
  ## station loop ####
  # including formula for psi
  
  if(model == "Occupancy") {
  loop2 <- paste("# station loop", 
                 paste0("for (", stationIndex ," in 1:", stationMax, "){"),
                 paste("\n# Occupancy probability formula"),
                 if (nimble) {
                   paste0("logit(psi[", speciesIndex, ",", stationIndex, "]) <- ", beta0_formula, beta_formula_occu_categ_fixed, beta_formula_occu_categ_ranef, beta_formula_occu_fixed, beta_formula_occu_indep, beta_formula_occu_ranef)
                 },
                 
                 if (!nimble) {
                   paste0("logit.psi[", speciesIndex, ",", stationIndex, "] <- ", beta0_formula, beta_formula_occu_categ_fixed, beta_formula_occu_categ_ranef, beta_formula_occu_fixed, beta_formula_occu_indep, beta_formula_occu_ranef)
                 },
                 if (!nimble) {
                   paste0("psi[", speciesIndex, ",", stationIndex, "] <- exp(logit.psi[", speciesIndex, ",", stationIndex, "]) / (exp(logit.psi[", speciesIndex, ",", stationIndex, "]) + 1)")
                 },
                 
                 ifelse(!nimble, 
                        paste0("z[", speciesIndex, ",", stationIndex, "] ~ dbern(psi[", speciesIndex, ", ", stationIndex, "]", 
                               ifelse(augment == "full", paste0(" * w[", speciesIndex, "])"), ")")), 
                        ""),
                 "\n", sep = "\n"
  )
  }
  
  if(model == "RN") {
    loop2 <- paste("# station loop", 
                   paste0("for (", stationIndex ," in 1:", stationMax, "){"),
                   paste("\n# Abundance formula"),
                   if (nimble) {
                     paste0("log(lambda[", speciesIndex, ",", stationIndex, "]) <- ", beta0_formula, beta_formula_occu_categ_fixed, beta_formula_occu_categ_ranef, beta_formula_occu_fixed, beta_formula_occu_indep, beta_formula_occu_ranef)
                   },
                   
                   if (!nimble) {
                     paste0("log.lambda[", speciesIndex, ",", stationIndex, "] <- ", beta0_formula, beta_formula_occu_categ_fixed, beta_formula_occu_categ_ranef, beta_formula_occu_fixed, beta_formula_occu_indep, beta_formula_occu_ranef)
                   },
                   if (!nimble) {
                     paste0("lambda[", speciesIndex, ",", stationIndex, "] <- exp(log.lambda[", speciesIndex, ",", stationIndex, "])")
                   },
                   paste("\n# convert lambda to psi"),
                   paste0("psi[", speciesIndex, ",", stationIndex, "] <- 1 - dpois(0, lambda[", speciesIndex, ",", stationIndex, "])"),
                   
                   
                   ifelse(!nimble,
                          paste0(paste("\n# Occupancy_status matrix\n"),
                                 # "z[", speciesIndex, ",", stationIndex, "] ~ dbern(psi[", speciesIndex, ",", stationIndex, "]",
                                 "z[", speciesIndex, ",", stationIndex, "] <- ifelse(z_lambda[", speciesIndex, ",", stationIndex, "] >= 1, 1, 0",
                                 ifelse(augment == "full", paste0(" * w[", speciesIndex, "])"), ")")),
                                 
                          ""),
                   ifelse(!nimble, 
                          paste0("\n# Expected abundance matrix\n",
                                 "z_lambda[", speciesIndex, ",", stationIndex, "] ~ dpois(lambda[", speciesIndex, ",", stationIndex, "]", 
                                 ifelse(augment == "full", paste0(" * w[", speciesIndex, "])"), ")")), 
                          ""),
                   "\n", sep = "\n"
    )
  }
  
  
  attr(loop2, "params") <- NULL
  
  
  zin <- t(sapply(data_list$ylist, rowSums, na.rm = TRUE))  # species x Station matrix with n detections
  zin[zin>1] <- 1
  if(model == "RN"){
    if(!nimble) attr(loop2, "inits") <- list(z_lambda = zin)
  } else {
    if(!nimble) attr(loop2, "inits") <- list(z = zin)
  }
  if(nimble)  attr(loop2, "inits") <- NULL
  
  
  ## construct formula for p (detection model - alpha parameters)   ####
  
  alpha_formula_det_fixed <- attr(priors_sitecovs_det_fixed, "formula")
  alpha_formula_det_indep <- attr(priors_sitecovs_det_indep, "formula")
  alpha_formula_det_ranef <- attr(priors_sitecovs_det_ranef, "formula")
  alpha_formula_det_categ_fixed <- attr(priors_sitecovs_det_categ_fixed, "formula")
  alpha_formula_det_categ_ranef <- attr(priors_sitecovs_det_categ_ranef, "formula")
  
  alpha_formula_det_SpeciesStation <- attr(priors_SpeciesStation_det_ranef, "formula")
  
  
  
  if(!is.null(detCovsObservation$fixed)) {
    if(nimble) {
      alpha_formula_obs_fixed <- paste(" + alpha.obs.fixed.cont.", detCovsObservation$fixed, " * ", detCovsObservation$fixed, "[", stationIndex, ", 1:", occasionMax, "]", collapse = "", sep = "")
    } else {
      alpha_formula_obs_fixed <- paste(" + alpha.obs.fixed.cont.", detCovsObservation$fixed, " * ", detCovsObservation$fixed, "[", stationIndex, ", ", occasionIndex, "]", collapse = "", sep = "")
    }
  } else {
    alpha_formula_obs_fixed <- ""
  }
  
  
  if(!is.null(detCovsObservation$ranef)) {
    if(nimble) {
      alpha_formula_obs_ranef <- paste(" + alpha.obs.ranef.cont.", detCovsObservation$ranef, "[", speciesIndex, "] * ", detCovsObservation$ranef, "[", stationIndex, ", 1:", occasionMax, "]", collapse = "", sep = "")
    } else {
      alpha_formula_obs_ranef <- paste(" + alpha.obs.ranef.cont.", detCovsObservation$ranef, "[", speciesIndex, "] * ", detCovsObservation$ranef, "[", stationIndex, ", ", occasionIndex, "]", collapse = "", sep = "")
    }
  } else {
    alpha_formula_obs_ranef <- ""
  }
  
  
  if(!is.null(detCovsObservation_categ$fixed)) {
    if(nimble) {
      alpha_formula_obs_fixed_categ <- paste(" + alpha.obs.fixed.categ.", detCovsObservation_categ$fixed, "[index_tmp_fixed]", collapse = "", sep = "")
    } else {
      alpha_formula_obs_fixed_categ <- paste(" + alpha.obs.fixed.categ.", detCovsObservation_categ$fixed, "[", paste0(detCovsObservation_categ$fixed, "_integer"), "[", stationIndex, ", ", occasionIndex, "]", "]", collapse = "", sep = "")  
    }
  } else {
    alpha_formula_obs_fixed_categ <- ""
  }
  
  if(!is.null(detCovsObservation_categ$ranef)) {
    if(nimble){
      alpha_formula_obs_ranef_categ <- paste(" + alpha.obs.ranef.categ.", detCovsObservation_categ$ranef, "[", speciesIndex, ", ", "index_tmp_ranef", "]", collapse = "", sep = "")
    } else {
      alpha_formula_obs_ranef_categ <- paste(" + alpha.obs.ranef.categ.", detCovsObservation_categ$ranef, "[", speciesIndex, ", ", detCovsObservation_categ$ranef, "_integer", "[", stationIndex, ", ", occasionIndex, "]", "]", collapse = "", sep = "")  
    }
  } else {
    alpha_formula_obs_ranef_categ <- ""
  }
  
  
  if(speciesSiteRandomEffect$det) {
    priors_species_station_det_ranef <- paste("ran", "[", speciesIndex, ", ", stationIndex, "] ~ dnorm(0, alpha.speciesstation.ranef.tau)", collapse = "", sep = "")
    
    alpha_species_station_ranef <- paste("ran", "[", speciesIndex, ", ", stationIndex, "]", collapse = "", sep = "")
  } else {
    alpha_species_station_ranef <- ""
  }
  
  
  ### occasion loop  ####
  
  if(!nimble){
    loop3 <- paste( alpha_draws_SpeciesStation_det_ranef,
                    "# occasion loop", 
                   paste0("for (", occasionIndex, " in 1:", occasionMax, "){"), 
                   
                   paste("# Detection probability formula"),
                   
                   
                   
                   paste0("logit.p[", speciesIndex, ",", stationIndex, ",", occasionIndex, "] <- ", alpha0_formula, alpha_formula_obs_fixed, alpha_formula_obs_ranef, 
                          alpha_formula_obs_fixed_categ, alpha_formula_obs_ranef_categ,
                          alpha_formula_det_categ_fixed, alpha_formula_det_categ_ranef, 
                          alpha_formula_det_fixed, alpha_formula_det_indep, alpha_formula_det_ranef,
                          alpha_formula_det_SpeciesStation
                          ),
                   
                   paste("\n# convert p to real scale"),
                   paste0("p[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] <- exp(logit.p[", speciesIndex, ",", stationIndex, ",", occasionIndex,"]) / (1+exp(logit.p[", speciesIndex, ",", stationIndex, ",", occasionIndex,"]))"),
                   
                   paste("\n# Ensure occasions without effort have p = 0"),
                   
                   if(model == "RN") {
                     paste0("p.e[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] <- p[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] * effort_binary[", stationIndex, ",", occasionIndex,"]")
                   },
                   if(model == "RN") {
                     paste0("p.eff[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] <-  1-(1-p.e[", speciesIndex, ",", stationIndex, ",", occasionIndex, "])^z_lambda[", speciesIndex, ",", stationIndex, "]")
                     },
                   if(model == "Occupancy") {
                     paste0("p.eff[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] <- z[", speciesIndex, ",", stationIndex, "] * p[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] * effort_binary[", stationIndex, ",", occasionIndex,"]")
                     },
                   
                   
                   paste0("y[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] ~ dbern(p.eff[", speciesIndex, ",", stationIndex, ",", occasionIndex,"])"),
                   
                   "\n### generate new data from model under consideration",
                   paste0("new.y[", speciesIndex, ",", stationIndex, ",", occasionIndex,"] ~ dbern(p.eff[", speciesIndex, ",", stationIndex, ",", occasionIndex,"])"),
                   "}   # close occasion loop", 
                   "\n", sep = "\n")
    attr(loop3, "params") <- NULL
  }
  
  if(nimble){
    
    loop3 <- paste(ifelse(!is.null(detCovsObservation_categ$fixed) , 
                          paste0("index_tmp_fixed <- ", paste0(detCovsObservation_categ$fixed, "_integer", "[", stationIndex, ", 1:", occasionMax, "]")),
                          ""),
                   ifelse(!is.null(detCovsObservation_categ$ranef), 
                          paste0("index_tmp_ranef <- ", paste0(detCovsObservation_categ$ranef, "_integer", "[", stationIndex, ", 1:", occasionMax, "]")),
                          ""),
                   paste("# Detection probability formula"),
                   paste0("logit(p[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "]) <- ", alpha0_formula, alpha_formula_obs_fixed, alpha_formula_obs_ranef, 
                          alpha_formula_obs_fixed_categ, alpha_formula_obs_ranef_categ,
                          alpha_formula_det_categ_fixed, alpha_formula_det_categ_ranef, 
                          alpha_formula_det_fixed, alpha_formula_det_indep, alpha_formula_det_ranef,
                          paste0(" * all1row[1:", occasionMax, "]")),              # this is only to ensure that dimensions match if alpha is a single value
                   
                   paste("\n# Ensure occasions without effort have p = 0"),
                   paste0("p.eff[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "] <- p[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "]", 
                          " * effort_binary[", stationIndex, ", 1:", occasionMax, "]"),
                   
                   
                   # NOTE: which functions from nimbleEcology to use for RN-Model? 
                   # to replace: dOcc_v / rOcc_v
                   "\n### calculate probability of observed data ",
                   paste0("y[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "] ~ dOcc_v(probOcc = psi[", speciesIndex, ",", stationIndex, "], probDetect = p.eff[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "], len = ", occasionMax, ")"),
                   
                   "\n### generate new data from model under consideration",
                   paste0("new.y[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "]  <- rOcc_v(n = 1, probOcc = psi[", speciesIndex, ",", stationIndex, "], probDetect = p.eff[", speciesIndex, ",", stationIndex, ",", "1:", occasionMax, "], len = ", occasionMax, ")"), 
                   
                   "\n", sep = "\n")
    attr(loop3, "params") <- NULL
    
    
  }
  
  
  
  ## derived quantities #### 
  
  close_loop2 <-  paste( "### calculate Freeman-Tukey residuals for real and new data",
                         paste0("res[", speciesIndex, ",", stationIndex, "] <- (sqrt(sum(y[", speciesIndex, ",", stationIndex, ", 1:", occasionMax, "])) - sqrt(sum(p.eff[", speciesIndex, ",", stationIndex, ", 1:", occasionMax, "])))^2"),
                         paste0("new.res[", speciesIndex, ",", stationIndex, "] <- (sqrt(sum(new.y[", speciesIndex, ",", stationIndex, ", 1:", occasionMax, "])) - sqrt(sum(p.eff[", speciesIndex, ",", stationIndex, ", 1:", occasionMax, "])))^2"),
                         "}   # close station loop",
                         "\n", sep = "\n")
  attr(close_loop2, "params") <- NULL
  
  
  
  close_loop1a1 <- paste("### sum residuals over stations",
                         paste0("R2[", speciesIndex, "] <- sum(res[", speciesIndex, ", 1:", stationMax, "])"),
                         paste0("new.R2[", speciesIndex, "] <- sum(new.res[", speciesIndex, ", 1:", stationMax, "])"),
                         
                         "\n### species-level Bayesian p-value",
                         paste0("Bpvalue_species[", speciesIndex, "] <- R2[", speciesIndex, "] > new.R2[", speciesIndex, "]"),
                         "\n", sep = "\n")
  attr(close_loop1a1, "params") <- c("R2", "new.R2", "Bpvalue_species")
  
  if(!nimble){
    close_loop1a2 <- paste("\n### Number of occupied stations for each species",
                           paste0("NStationsOccupied[", speciesIndex, "] <- sum(z[", speciesIndex, ", 1:", stationMax, "])"),
                           "\n### species is part of community?",
                           paste0("speciesInCommunity[", speciesIndex ,"] <- 1 - equals(NStationsOccupied[", speciesIndex ,"],0)"),
                           "\n", sep = "\n")
  } else {
    close_loop1a2 <- "# Total number of occupied and community membership indicator are not returned if nimble = TRUE"
  }
  attr(close_loop1a2, "params") <- NULL
  
  
  
  
  
  fractionStationsOccupied <- list()
  
  if(!is.null(occuIntercept_categ$fixed)) {
    
    # get number of stations in each richness category
    stations_richness_categories <- table(covariates[,occuIntercept_categ$fixed])
    names(stations_richness_categories) <- paste0("J", 1:length(stations_richness_categories))
    
    for(s_occu in 1:length(unique(covariates[,occuIntercept_categ$fixed]))){
      
      current_s1 <- names(stations_richness_categories)[s_occu]
      current_s2a <- paste0(stationMax, 1:(s_occu - 1), collapse = " + ")
      current_s2b <- paste0(stationMax, 1:s_occu, collapse = " + ")
      current_s3  <- paste0(stationMax, s_occu)
      
      
      if(s_occu == 1) fractionStationsOccupied[[s_occu]] <- paste(paste0("### Number of stations occupied (in each level of '", richnessCategories, "')"),
                                                   paste0("fractionStationsOccupied[", speciesIndex , ", ", s_occu, "] <- sum(z[", speciesIndex ,", 1:", current_s1, "])/", current_s1), 
                                                   sep = "\n")
      
      if(s_occu > 1 & s_occu != length(unique(covariates[,occuIntercept_categ$fixed]))) fractionStationsOccupied[[s_occu]] <- paste0("fractionStationsOccupied[", speciesIndex , ", ", s_occu, "] <- sum(z[", speciesIndex ,", (", current_s2a, " + 1):(", current_s2b ,")]) / ", current_s3)
      
      if(s_occu == length(unique(covariates[,occuIntercept_categ$fixed]))) fractionStationsOccupied[[s_occu]] <- paste0("fractionStationsOccupied[", speciesIndex , ", ", s_occu, "] <- sum(z[", speciesIndex ,", (", current_s2a, " + 1):", stationMax, "]) / (", stationMax, " - (", current_s2a, "))")      
    }
  } else {
    fractionStationsOccupied[[1]] <- paste("### Fraction of stations occupied",
                            paste0("fractionStationsOccupied[", speciesIndex ,"] <- NStationsOccupied[", speciesIndex ,"] /", stationMax), sep = "\n")
  }
  
  if(!nimble) {
    close_loop1b <- paste(
      paste(unlist(fractionStationsOccupied), collapse = "\n"),
      "\n", sep = "\n")
  } else {
    close_loop1b <- "# Number of stations occupied is not returned when nimble = TRUE\n"
  }
  
  
  if(!is.null(occuIntercept_categ$fixed)) {
    occ <- paste0("occ[", speciesIndex, ", ", 1:length(unique(covariates[,occuIntercept_categ$fixed])), "]<- 1 - equals(fractionStationsOccupied[i,", 1:length(unique(covariates[,occuIntercept_categ$fixed])), "], 0)")
  } else {
    occ <- paste0("occ[", speciesIndex ,"] <- 1 - equals(fractionStationsOccupied[", speciesIndex ,"], 0)")
  }
  
  if(!nimble){
    close_loop1c <- paste(    
      paste0("### Does species occur at all (at any station)", ifelse(!is.null(richnessCategories), paste0("(within each level of '", richnessCategories, "')"), "")),
      paste(occ, collapse = "\n"),
      "}    # close species loop",
      "\n", sep = "\n")
  } else {
    close_loop1c <- paste("}    # close species loop\n\n")
  }
  
  finish_residuals <- paste("###sum residuals over observed species", 
                            paste0("R3 <- sum(R2[1:", speciesMax, "])"),
                            paste0("new.R3 <- sum(new.R2[1:", speciesMax, "])"),
                            paste0("Bpvalue <- R3 > new.R3"),
                            "\n", sep = "\n")
  attr(finish_residuals, "params") <- c("R3", "new.R3", "Bpvalue")
  
  
  
  if(augment == "full") {
    finish_nspecies <- paste("### total number of species", 
                             ifelse(nimble, 
                                    "# not returned if nimble = TRUE", 
                                    paste0("Nspecies <- sum(speciesInCommunity[1:", speciesMax , "])")),
                             "",
                             "### Total number of unobserved species",
                             paste0("n0 <- sum(w[(nspec+1):(nspec+nz)])"),
                             "",
                             "### Total metacommunity size",
                             paste0("Ntotal <- sum(w[])"),
                             "\n", sep = "\n")
    if(nimble)  attr(finish_nspecies, "params") <- c("n0", "Ntotal")
    if(!nimble) attr(finish_nspecies, "params") <- c("Nspecies", "n0", "Ntotal")
    
    
    
  } else {
    finish_nspecies <- paste("### total number of species", 
                             ifelse(nimble, 
                                    "# not returned if nimble = TRUE", 
                                    paste0("Nspecies <- sum(speciesInCommunity[1:", speciesMax , "])")),
                             "\n", sep = "\n")
    if(!nimble) attr(finish_nspecies, "params") <- "Nspecies"
    if(nimble)  attr(finish_nspecies, "params") <- NULL
  } 
  
  
  if(!is.null(occuIntercept_categ$fixed) & !nimble) {
    finish_nspecies_categ <- paste(paste0("### number of species (in each level of '", richnessCategories, "')"),
                                   paste0("for(s_occu in 1:", length(unique(covariates[,occuIntercept_categ$fixed])), "){"), 
                                   paste0("Nspecies_", richnessCategories, "[s_occu] <- sum(occ[1:", speciesMax , ", s_occu])"),
                                   "}",
                                   "", 
                                   sep = "\n")
    attr(finish_nspecies_categ, "params") <- paste0("Nspecies_", richnessCategories)
  } else {
    finish_nspecies_categ <- paste("", sep = "\n")
  }
  
  # species richness at each camera trap station
  if(nimble)  species_richness <-  paste(paste0("### Species richness at every location"),
                                         "# not returned if nimble = TRUE",
                                         "", sep = "\n")
  
  if(!nimble){ species_richness <- paste(paste0("### Species richness at every location"),
                                        paste0("for (", stationIndex, " in 1:", stationMax, "){"),
                                        paste0("Nspecies_station[", stationIndex, "] <- sum(z[1:", speciesMax, ",", stationIndex, "])"),
                                        "}",
                                        "",
                                        "", sep = "\n")
    attr(species_richness, "params") <- "Nspecies_station"
  }
  
  close_model <- paste("}",
                       "", sep = "\n")
    

  
  
  
  
  # Creating output ####
  
  ## combine model code ####
  model_text <- list(model_start, 
                     priors_intercept_occu,
                     priors_intercept_det,
                     
                     priors_omega,
                     
                     # detection covariates (site)
                     prior_det_header_fixed,
                     unlist(priors_sitecovs_det_fixed),
                     
                     prior_det_header_indep,
                     unlist(priors_sitecovs_det_indep),
                     
                     prior_det_header_ranef,
                     unlist(priors_sitecovs_det_ranef),
                     
                     prior_det_categ_header_fixed, 
                     unlist(priors_sitecovs_det_categ_fixed),
                     
                     prior_det_categ_header_ranef,
                     unlist(priors_sitecovs_det_categ_ranef),
                     
                     # detection covariates (occasion)
                     prior_obs_header_fixed,
                     unlist(priors_obscovs_det_fixed),
                     
                     prior_obs_header_ranef,
                     unlist(priors_obscovs_det_ranef),
                     
                     prior_obs_categ_header_fixed,
                     unlist(priors_obscovs_det_categ_fixed),
                     
                     prior_obs_categ_header_ranef,
                     unlist(priors_obscovs_det_categ_ranef),
                     
                     # occupancy covariates (site)
                     prior_occu_header_fixed,
                     unlist(priors_sitecovs_occu_fixed),
                     
                     prior_occu_header_indep,
                     unlist(priors_sitecovs_occu_indep),
                     
                     prior_occu_header_ranef,
                     unlist(priors_sitecovs_occu_ranef),
                     
                     prior_occu_categ_header_fixed,
                     unlist(priors_sitecovs_occu_categ_fixed),
                     
                     prior_occu_categ_header_ranef,
                     unlist(priors_sitecovs_occu_categ_ranef),
                     
                     prior_det_species_station_ranef,
                     unlist(priors_SpeciesStation_det_ranef),
                     
                     before_species_loop,
                     
                     # model
                     loop1, 
                     loop2,
                     loop3,
                     close_loop2,
                     close_loop1a1,
                     close_loop1a2,
                     close_loop1b,
                     close_loop1c,
                     finish_residuals, 
                     finish_nspecies,
                     finish_nspecies_categ,
                     species_richness,
                     close_model)
  
  out <- list()
  out$model <- unlist(model_text)
  
  
  ## combine parameters to monitor  ####
  out$params <- c(unlist(sapply(model_text, attr, "params")), 
                  attr(priors_sitecovs_det_fixed, "params"),
                  attr(priors_sitecovs_det_indep, "params"),
                  attr(priors_sitecovs_det_ranef, "params"),
                  attr(priors_sitecovs_det_categ_fixed, "params"),
                  attr(priors_sitecovs_det_categ_ranef, "params"),
                  attr(priors_obscovs_det_fixed, "params"),
                  attr(priors_obscovs_det_ranef, "params"),
                  attr(priors_obscovs_det_categ_fixed, "params"),
                  attr(priors_obscovs_det_categ_ranef, "params"),
                  attr(priors_sitecovs_occu_fixed, "params"),
                  attr(priors_sitecovs_occu_indep, "params"),
                  attr(priors_sitecovs_occu_ranef, "params"),
                  attr(priors_sitecovs_occu_categ_fixed, "params"),
                  attr(priors_sitecovs_occu_categ_ranef, "params"),
                  attr(priors_SpeciesStation_det_ranef, "params"))
  
  ## combine inits  ####
  inits_out_tmp <- c(do.call(c, lapply(model_text[sapply(model_text, 
                                                         FUN = function(x) !is.null(attr(x, "inits")))], 
                                       attr, "inits")),
                     attr(priors_sitecovs_det_fixed, "inits"),
                     attr(priors_sitecovs_det_indep, "inits"),
                     attr(priors_sitecovs_det_ranef, "inits"),
                     attr(priors_sitecovs_det_categ_fixed, "inits"),
                     attr(priors_sitecovs_det_categ_ranef, "inits"),
                     attr(priors_obscovs_det_fixed, "inits"),
                     attr(priors_obscovs_det_ranef, "inits"),
                     attr(priors_obscovs_det_categ_fixed, "inits"),
                     attr(priors_obscovs_det_categ_ranef, "inits"),
                     attr(priors_sitecovs_occu_fixed, "inits"),
                     attr(priors_sitecovs_occu_indep, "inits"),
                     attr(priors_sitecovs_occu_ranef, "inits"),
                     attr(priors_sitecovs_occu_categ_fixed, "inits"),
                     attr(priors_sitecovs_occu_categ_ranef, "inits"),
                     attr(priors_SpeciesStation_det_ranef, "inits"))
  
  
  inits_list     <- inits_out_tmp[sapply(inits_out_tmp, class) == "list"]
  inits_not_list <- inits_out_tmp[sapply(inits_out_tmp, class) != "list"]
  
  
  
  # make inits function
  inits_fun <- function(x){ 
    c(lapply(inits_list, FUN = function(x) do.call(x[[1]], x[[2]])),
      inits_not_list)
  }
  
  out$inits_fun <- inits_fun
  
  
  
  # prepare model data  ####
  
  # effort_binary = 1/0 indicator for whether station was active or not (to ensure p = 0 when no effort)
  effort_binary <- as.matrix(!is.na(data_list$obsCovs[[effortCov]])) * 1   # replace NA with 0, other values with 1
  
  
  # create data list for jags.model
  if(!"ylist" %in% names(data_list)) stop("Didn't find ylist in data_list")
  y <- simplify2array(data_list$ylist)
  y <- aperm(y,c(3,1,2))
  
  # assign names to data augmentation species
  if(!is.null(augmentation)) {
    if(any(dimnames(y)[[1]] == "")) {
      which_no_name <- which(dimnames(y)[[1]] == "")
      dimnames(y)[[1]][which_no_name] <- paste("species_DA", 1:length(which_no_name), sep = "_")
    }
  }
  
  data.list <- list(
    y = y,
    effort_binary = effort_binary
  )
  
  
  # add site-occasion covariates
  if(!is.null(data_list$obsCovs)) {
    data.list <- c(data.list, data_list$obsCovs[names(data_list$obsCovs) %in% unlist(detCovsObservation)])
    data.list <- c(data.list, data_list$obsCovs[names(data_list$obsCovs) %in% unlist(detCovsObservation_categ)])
    
    # check if any observation covariate is character
    #if(any(sapply(data_list$obsCovs, typeof) == "character")){
    if(!is.null(unlist(detCovsObservation_categ))) {
      
      # if yes, add the corresponding "..._integer" matrix
      data.list <- c(data.list, data_list$obsCovs[grep("_integer$", names(data_list$obsCovs))])    # this does not include original covariate
    }
    
    if(effortCov %in% unlist(detCovsObservation)) {
      if(!effortCov %in% names(data.list)) {
        data.list[effortCov] <- data_list$obsCovs[effortCov]
      }
      data.list[[effortCov]] [effort_binary == 0] <- 0   # replace effort = NA with 0 (doesn't affect calculation of p because of effort_binary)
    }
  }
  
  # add data dimensions
  list_design <- list(M = speciesMax_value,
                      J = stationMax_value,
                      maxocc = occasionMax_value)
  
  names(list_design) <- c(speciesMax, 
                          stationMax, 
                          occasionMax)
  
  data.list <- c(data.list, list_design)
  
  # add site covariates
  if(length(covariates_categ) >= 1)   data.list <- c(data.list, as.list(covariates[covariates_categ]))
  if(length(covariates_numeric) >= 1) data.list <- c(data.list, as.list(covariates[covariates_numeric]))
  
  if(!is.null(occuIntercept_categ$fixed)) {
    tmp <- table(covariates[,occuIntercept_categ$fixed])
    names(tmp) <- paste0("J", 1:length(tmp))
    data.list <- c(data.list, tmp)
  }
  
  # add data augmentation information
  if(augment == "full"){
    data.list <- c(data.list, 
                   list(nz = nz,          # number of unseen species added to the detection history list
                        nspec = nspec))   # number of seen species
  }
  
  
  # for nimble, convert site covariate factors to numeric
  # the values will then be used for indexing the factor levels, and will not be interpreted as continuous values
  if(nimble){
    data.list <- lapply(data.list, FUN = function(x) {
      if(is.factor(x)) {
        as.numeric(x)
      } else {
        x
      }
    })
    data.list$all1row <- rep(1, times = occasionMax_value)
  }
  
  
  out$model_data <- data.list
  
  # save model text file
  if(!is.null(modelFile)) {
    sink(modelFile)
    cat(out$model, fill = FALSE, sep = "")
    sink()
    if(file.exists(modelFile)) {
      message(paste("Wrote model to", modelFile))
    } else {
      message(paste("Model could not be written to", modelFile))
    }
  }
  
  # create output commOccu object
  out2 <- new("commOccu",   
              modelText = out$model,
              params    = out$params,
              inits_fun = out$inits_fun,
              data      = out$model_data,
              input     = data_list,
              nimble    = nimble,
              modelFile = ifelse(!is.null(modelFile), modelFile, "undefined"),
              covariate_info = covariate_info,
              model     = model
              )
  return(out2)
  
}



# Define commOccu class for output ####
#' commOccu objects
#'
#' @slot modelText JAGS model code as a character vector (made up of code chunks, use cat() to print)
#' @slot params Parameters to monitor in the model runs
#' @slot inits_fun Function to create start values for the MCMC chains. It being a function ensures different values in each chain
#' @slot data List with data needed to run the model (detection & effort matrices, site covariates, number of species / stations / occasions)
#' @slot input Input data_list (unchanged)
#' @slot nimble logical indicator for whether it is a Nimble model
#' @slot modelFile Path of the text file containing the model code
#' @slot covariate_info Data frame containing information about covariates. Only used internally in plot_* and predict methods
#' @slot model character indicating whether it is a standard "Occupancy" model or Royle-Nichols ("RN") occupancy model
#'
#' @note 
#' The \code{data} slot is a list of model input data. While the exact content depends on function input, it can be summarized as:
#' 
#' \tabular{ll}{
#' \code{y} \tab array of detection histories. Dimensions are: y[species, station, occasion] \cr
#' \code{effort_binary} \tab matrix of binary (1/0) survey effort. Only used to ensure p = 0 when effort = 0. Dimensions are: effort_binary[station, occasion] \cr
#' \code{site-occasion covariates} \tab The required content of data_list$obsCovs as named matrices with dimensions [station, occasion] \cr
#' \code{site covariates} \tab The required columns of data_list$siteCovs as named vectors (length = number of stations) \cr
#' \code{M} \tab Number of species \cr
#' \code{J} \tab Number of stations \cr
#' \code{maxocc} \tab Number of occasions \cr
#' }
#' 
#' For categorical site-occasion covariates, an addition matrix containing an integer representation of the character matrix with suffix "_integer" is stored in the data slot.
#' 
#' @return \code{commOccu} object
#' 
#' @export
#' 
setClass("commOccu", 
         slots = c(modelText      = "character",
                   params         = "character",
                   inits_fun      = "function",
                   data           = "list",
                   input          = "list",
                   nimble         = "logical",
                   modelFile      = "character", 
                   covariate_info = "data.frame",
                   model          = "character"
                   )
)




# fit method ####


fit.commOccu <- function(object,
                         n.iter = 100,
                         thin = 1,
                         n.burnin = n.iter/2,
                         n.adapt = 0,
                         chains = 3,
                         inits = NULL,
                         compile = TRUE,
                         WAIC	= FALSE,
                         quiet = FALSE,
                         ...) {
  
  if(thin == 0) stop("thin can't be 0")
  if(n.adapt != 0 & isTRUE(object@nimble)) message(paste("nimble models don't use n.adapt. It will be ignored."))
  
  if(isFALSE(object@nimble)){
    
    if(!file.exists(object@modelFile)) stop(paste("modelFile not found under", object@modelFile))
    
    if(isTRUE(WAIC)) warning("WAIC is only returned in Nimble models", immediate. = TRUE)
    
    
    
      mod <- rjags::jags.model(file = object@modelFile, 
                               data = object@data, 
                               inits = object@inits_fun(),
                               n.chain=chains, 
                               n.adapt=n.adapt,
                               quiet = quiet)
      
      out <- rjags::coda.samples(model = mod,
                                variable.names = object@params, 
                                n.iter	= n.iter, 
                                thin = thin)
      
    
    # posterior summaries
      out_mcmclist <- coda::mcmc.list(out)
    
    if(n.burnin >= 1) {
      out2 <- window(out_mcmclist, 
                     start=n.burnin+1, 
                     end = n.iter)
      return(out2)
    } else {
      return(out_mcmclist)
    }
    
  }
  
  
  if(isTRUE(object@nimble)) {
    
    mod <- nimble::readBUGSmodel(object@modelFile,
                                 data = object@data,
                                 inits = object@inits_fun())
    
    
    myMCMC <- nimble::buildMCMC(mod, 
                                monitors = object@params,
                                thin = thin,
                                enableWAIC = WAIC,
                                print = isFALSE(quiet))
    
    if(compile) {
      
      mod_comp <- nimble::compileNimble(mod)
      
      compMCMC <- nimble::compileNimble(myMCMC, project = mod)
      
      out_mcmclist <- nimble::runMCMC(compMCMC,
                                      niter = n.iter, 
                                      nburnin = n.burnin,
                                      thin = thin,
                                      nchains = chains, 
                                      samplesAsCodaMCMC = TRUE,
                                      WAIC = WAIC,
                                      progressBar = isFALSE(quiet),
                                      ...) 
    }
    
    if(!compile) {
      out_mcmclist <- nimble::runMCMC(myMCMC,
                                      niter = n.iter, 
                                      nburnin = n.burnin,
                                      thin = thin,
                                      nchains = chains, 
                                      samplesAsCodaMCMC = TRUE,
                                      WAIC = WAIC,
                                      progressBar = isFALSE(quiet),
                                      ...) 
    }
    
    return(out_mcmclist)
    
    
  }
}

setGeneric("fit", function(object, ...){})


#' Fit a community (multi-species) occupancy model
#'
#' Convenience function for fitting community occupancy models (defined in a commOccu object) in JAGS or Nimble.
#'
#' @param object \code{commOccu} object
#' @param n.iter number of iterations to monitor
#' @param thin thinning interval for monitors
#' @param n.burnin   burnin length. Defaults to half of n.iter.
#' @param n.adapt Length of adaptive phase
#' @param chains number of MCMC chains to run
#' @param inits named list. Initial values to use. If NULL (default), the values from the inits function in \code{object} are used.
#' @param compile logical. If Nimble model, compile model with \code{\link[nimble]{compileNimble}} before running model?
#' @param WAIC logical. Return WAIC (only Nimble models)
#' @param quiet if TRUE messages and progress bar will be suppressed
#' @param ...     additional arguments to pass to \code{\link[nimble]{runMCMC}} (only relevant for Nimble)
#'
#' @details 
#' Models will be fit either in JAGS or Nimble, depending on the decision made in the \code{nimble} argument in \code{\link{communityModel}}.
#' 
#' For Nimble, compilation is strongly recommended for long model runs. Uncompiled models can run extremely slow. Compilation itself can take a while also, and requires that Rtools is available on the system.
#' 
#' This is a convenience function only which hides some of the configuration options. If you require more control over model fitting, you can run all steps individually. See vignette 5 for details. 
#'  
#' @return A coda::mcmc.list
#'
#' @export
#' 
setMethod("fit", signature(object = "commOccu"), 
          fit.commOccu)



# summary method ####

summary.commOccu <- function(object, ...) {
  cat(paste0("commOccu object (for ", ifelse(object@nimble, "Nimble", "JAGS"), ")\n\n"))
  dims <- dim(object@data$y)
  cat(paste(dims[1], "species, ",
            dims[2], "stations, ", 
            dims[3], "occasions\n"))
  
  cat(paste(sum(object@data$effort_binary, na.rm = T), "occasions with effort\n"))
  
  cat(paste("Number of detections (by species):", paste(range(apply(object@data$y, 1, sum, na.rm =T)), collapse = " - "), "\n\n"))
  
  if(exists("siteCovs", object@input)){
    
    cat(paste("Available site covariates:\n", paste(colnames(object@input$siteCovs), collapse = ", "), "\n\n"))
  
    cat(paste("Used site covariates:\n", paste(colnames(object@input$siteCovs) [attr(object@input$siteCovs, "in_model")], collapse = ", "), "\n\n"))
  }
  
  if(exists("obsCovs", object@input)) {
    cat(paste("Available site-occasion covariates:\n", paste(names(object@input$obsCovs), collapse = ", "), "\n\n"))
    
    # doesn't work yet - need an indicator somewhere for what obs covs are used by model
    #cat(paste("Used site-occasion covariates:\n", paste(names(object@input$obsCovs), collapse = ", "), "\n\n"))
  }
  
  
}



#' Summarize community occupancy model
#' 
#' Gives an overview of the number of species, stations and occasions in a \code{commOccu} object. Also returns covariates. 
#' 
#' The summary method is very basic and still work in progress. 
#'
#' @param object  \code{commOccu} object
#' @param ...  currently ignored
#'
#' @return Model summary printed to console
#' @export
#'
setMethod("summary", signature(object = "commOccu"), 
          summary.commOccu)



# other helper functions

# make table with information about covariates
get_cov_info <- function(cov,
                         keyword_nested = keyword_nested,
                         keyword_quadratic = keyword_quadratic,
                         data_list = data_list,
                         type,
                         submodel) {
  
  
  if(type == "obs")  item <- "obsCovs"
  if(type == "site") item <- "siteCovs"
  
  tmp <- unlist(cov)
  
  names(tmp)[grep("fixed", names(tmp))] <- "fixed"
  names(tmp)[grep("ranef", names(tmp))] <- "ranef"
  
  if(submodel == "occu"){
    names(tmp)[grep("independent", names(tmp))] <- "independent"
  }
  
  
  
  tmp_cov1  <- sapply(strsplit(tmp, split = "|", fixed = T), FUN = function(x) x[1])
  
  if(type == "obs")  cov_type  <- sapply(data_list[[item]][tmp_cov1], typeof)
  if(type == "site") cov_type  <- sapply(data_list[[item]][,tmp_cov1, drop = FALSE], class)
  
  cov_type2 <- ifelse(cov_type %in% c("numeric", "double", "integer"), "cont", 
                      ifelse(cov_type %in% c("character", "factor"), "categ", NA))
  

  tmp2 <- data.frame(param = rep("param", times = length(tmp)),
                     submodel = rep(submodel, times = length(tmp)),
                     covariate_type = item,
                     covariate = tmp,
                     data_type = cov_type2,
                     is_quadratic = endsWith(tmp, keyword_quadratic),
                     has_quadratic = paste0(tmp, keyword_quadratic) %in% tmp,
                     ranef = ifelse(names(tmp) == "ranef", T, F), 
                     ranef_nested = grepl(keyword_nested, tmp, fixed = TRUE),
                     ranef_cov = sapply(strsplit(tmp, split = "|", fixed = TRUE), FUN = function(x) ifelse(length(x) == 2, x[2], NA)),
                     independent = ifelse(names(tmp) == "independent", T, F)
  )
  
  

  effect_type <- ifelse(tmp2$ranef, "ranef", ifelse(tmp2$independent, "indep", "fixed"))
  
  tmp2$coef <- paste0(ifelse(tmp2$submodel == "det", "alpha", "beta"), ".",
                 ifelse(tmp2$covariate_type == "obsCovs", "obs.", ""), 
                 effect_type,     #ifelse(tmp2$ranef, "ranef", "fixed"), 
                 ".",
                 tmp2$data_type, ".",
                 gsub("[|+]", "_", tmp)
                 )
  
  rownames(tmp2) <- NULL
  return(tmp2)
}



# helper functions to create priors (both detection and occupancy)  ####


# # paste index letters (not used currently)
# indices <- function(...){
# #   inargs <- list(...)
# #   species  <- ifelse("i" %in% inargs, "i", NA)
# #   station  <- ifelse("j" %in% inargs, "j", NA)
# #   occasion <- ifelse("k" %in% inargs, "k", NA)
# #  #
# # 
# #   paste0("[",
# #          paste(na.omit(c(species, station, occasion)), collapse = ","),
# #         # "]")
# paste0("[",
#        paste(unlist(strsplit(..., split = "")), collapse = ","),
#        "]")
# 
# }
## e.g. indices("ijk") gives "[i,j,k]"



# change number of random numbers created (from default = 1)
modifyInits <- function(inits, n) {
  if(!is.na(n)) {
    inits[[2]][[1]] <- as.numeric(n)
  } 
  return(inits)
}


interceptPriors <- function(effect, 
                            type,
                            prior_list,
                            inits_list, 
                            speciesIndex,
                            speciesMax,
                            speciesMax_value){
  
  
  if(type == "detection") param <- "alpha"
  if(type == "occupancy") param <- "beta"
  
  
  if(effect == "ranef"){
    
    # comenclature 2: alpha0.mean
    param_names <- list(mean  = paste0(param, "0", ".mean"), 
                        tau   = paste0(param, "0", ".tau"),
                        sigma = paste0(param, "0", ".sigma"))
    
    priors_intercept <- paste(
      paste("##", type, "intercept estimate of community (community mean)"),
      
      paste0(param_names$mean, " ~ ", prior_list$dnorm),
      paste0(param_names$tau,  " ~ ", prior_list$dgamma),
      paste0(param_names$sigma, " <- sqrt(1 / ", param_names$tau, ")"),
      
      "\n", sep = "\n")
    
    attr(priors_intercept, "params") <- c(param_names$mean, 
                                          param_names$sigma)
    
    inits_tmp <- list(mean = inits_list$inits_runif_mean_low,
                      tau  = inits_list$inits_runif_tau)
    
    names(inits_tmp) <- c(param_names$mean, 
                          param_names$tau)
    
    attr(priors_intercept, "inits") <- inits_tmp
    
    code_formula <- paste0(param, "0[", speciesIndex, "]")
  } 
  
  if(effect == "fixed") {
    priors_intercept <- paste(
      paste("##", type, "intercept estimate of community (constant across species)"),
      paste0(param, "0 ~ ", prior_list$dnorm),
      "\n", sep = "\n")
    
    inits_tmp <- list(name = inits_list$inits_runif_mean_0)
    names(inits_tmp) <- paste0(param, "0")
    attr(priors_intercept, "inits") <- inits_tmp
    code_formula <- paste0(param, "0")
  }
  
  
  if(effect == "independent") {
    priors_intercept <- paste(
      paste("##", type, "intercept estimate of community (independent between species)"),
      paste0("for(", speciesIndex, " in 1:", speciesMax, ") {"),
      paste0(param, "0[", speciesIndex, "] ~ ", prior_list$dnorm),
      "}",
      "\n", sep = "\n")
    

    # change number of random init values to create (not 1, but n_species)
    inits_tmp <- list(modifyInits(inits_list$inits_runif_mean_0, 
                                  speciesMax_value))
    
    names(inits_tmp) <- paste0(param, "0")
    
    attr(priors_intercept, "inits") <- inits_tmp
    
    code_formula <- paste0(param, "0[", speciesIndex, "]")
    
  }
  
  return(list(prior    = priors_intercept, 
              formula  = code_formula))
}




fixedEffectPriors <- function(effect_names, 
                              type,
                              prior_list, 
                              stationIndex){
  
  if(type == "detection") param <- "alpha"
  if(type == "occupancy") param <- "beta"
  
  if(!is.null(effect_names)){
    priors_list <- list()
    
    for(effect_names_index in 1:length(effect_names)){
      priors_list[[effect_names_index]] <- paste(
        paste("# Covariate:", effect_names[effect_names_index]),
        paste0(param, ".fixed.cont.", effect_names[effect_names_index], " ~ ", prior_list$dnorm), 
        "\n", 
        sep = "\n")
    }
    attr(priors_list, "params") <- paste0(param, ".fixed.cont.", effect_names)
    
    attr(priors_list, "formula") <- paste0(" + ", param, ".fixed.cont.", effect_names, " * ", effect_names, "[", stationIndex, "]", collapse = "", sep = "")
    
  } else {
    priors_list <- list("# < empty > \n\n")
    attr(priors_list, "params") <- NULL
    attr(priors_list, "formula") <- ""
  }
  
  return(priors_list)
}




independentEffectPriors <- function(effect_names, 
                                    type,
                                    inits_list,
                                    prior_list, 
                                    stationIndex,
                                    speciesIndex,
                                    speciesMax,
                                    speciesMax_value){
  
  if(type == "detection") param <- "alpha"
  if(type == "occupancy") param <- "beta"
  
  if(!is.null(effect_names)){
    priors_list <- list()
    
    for(effect_names_index in 1:length(effect_names)){
      priors_list[[effect_names_index]] <- paste(
        paste("# Covariate:", effect_names[effect_names_index]),
        paste0("for(", speciesIndex, " in 1:", speciesMax, ") {"),
        paste0(param, ".indep.cont.", effect_names[effect_names_index], "[", speciesIndex, "] ~ ", prior_list$dnorm), 
        "}",
        "\n", 
        sep = "\n")
    }
    attr(priors_list, "params") <- paste0(param, ".indep.cont.", effect_names)
    
    attr(priors_list, "formula") <- paste0(" + ", param, ".indep.cont.", effect_names,"[", speciesIndex, "]",  " * ", effect_names, "[", stationIndex, "]", collapse = "", sep = "")
    
    
    # make list of parameters
    inits_tmp <- vector(mode = "list", length = length(effect_names))
    names(inits_tmp) <- c(paste0(param, ".indep.cont.", effect_names))
    
    
    for(i in 1:length(inits_tmp)){
      inits_tmp[[i]] <- modifyInits(inits_list$inits_runif_mean_0, 
                                    speciesMax_value)
    }
    
    attr(priors_list, "inits") <- inits_tmp
    
    
  } else {
    priors_list <- list("# < empty > \n\n")
    attr(priors_list, "params") <- NULL
    attr(priors_list, "formula") <- ""
  }
  
  return(priors_list)
}


randomEffectPriors <- function(effect_names, 
                               type,
                               covariates,
                               prior_list,
                               inits_list,
                               speciesIndex, 
                               speciesMaxNumber,
                               speciesMax,
                               stationIndex, 
                               keyword_nested = "+Species"){
  
  if(type == "detection") param <- "alpha"
  if(type == "occupancy") param <- "beta"
  
  effect_names2 <- gsub("|", "_", effect_names, fixed = TRUE)
  effect_names2 <- gsub("+", "_", effect_names2, fixed = TRUE)
  
  if(!is.null(effect_names)){
    priors_list <- list()
    species_draws <- list()
    ranef_index <- vector(mode = "character")
    current_cov2 <- vector(mode = "character")
    ranef_double_all <- vector()
    
    for(effect_names_index in 1:length(effect_names)){
      current_cov <- effect_names[effect_names_index]
      if(grepl("|", current_cov, fixed = TRUE)) {
        
        # this part needs a cleanup
        
        tmp <- unlist(strsplit(current_cov, split = "|", fixed = TRUE))
        current_cov <- gsub("|", "_", current_cov, fixed = TRUE)
        current_cov2[effect_names_index] <- tmp[1]
        if(length(tmp) == 2 & endsWith(tmp[2], keyword_nested)) {
          ranef_index[effect_names_index] <- paste0(speciesIndex, ", ", gsub(keyword_nested, "", tmp[2], fixed = TRUE), "[", stationIndex, "]")  
          ranef_double <- TRUE
          ranef_double_all[effect_names_index] <- TRUE
          attr(ranef_double_all, "nlevels")[effect_names_index] <- speciesMaxNumber
          current_cov <- gsub("+", "_", current_cov, fixed = TRUE)
        } else {
          ranef_index[effect_names_index] <- paste0(tmp[2], "[", stationIndex, "]")
          ranef_double <- FALSE
          ranef_double_all[effect_names_index] <- FALSE
        }
        effect_name_print <- effect_names[effect_names_index]
      } else {
        ranef_index[effect_names_index] <- speciesIndex
        current_cov2[effect_names_index] <- current_cov
        effect_name_print <- paste(current_cov, "Species", sep = "|")
        ranef_double <- FALSE
        ranef_double_all[effect_names_index] <- FALSE
      }
      
      mean.tmp  <- paste0(param, ".ranef.cont.", current_cov, ".mean", ifelse(ranef_double, paste0("[", speciesIndex, "]"), ""))
      tau.tmp   <- paste0(param, ".ranef.cont.", current_cov, ".tau", ifelse(ranef_double, paste0("[", speciesIndex, "]"), ""))
      sigma.tmp <- paste0(param, ".ranef.cont.", current_cov, ".sigma", ifelse(ranef_double, paste0("[", speciesIndex, "]"), ""))
      
      priors_list[[effect_names_index]] <- paste(
        paste("# Covariate:", effect_name_print),
        if(ranef_double) paste0("for(", speciesIndex, " in 1:", speciesMax, "){"),
        paste0(mean.tmp, " ~ ", prior_list$dnorm),
        paste0(tau.tmp, " ~ ", prior_list$dgamma),
        paste0(sigma.tmp, " <- sqrt(1 / ", tau.tmp, ")"),
        if(ranef_double) "}",
        "\n", 
        sep = "\n")
      
      # if only species random effect
      if(ranef_index[effect_names_index] == speciesIndex) {
        species_draws[[effect_names_index]] <- paste(paste("# continuous", type, "covariate with random effects:", effect_name_print), 
                                                     paste0(param, ".ranef.cont.", current_cov, "[", ranef_index[effect_names_index], "] ~ dnorm(", mean.tmp, ", ",  tau.tmp, ")"),
                                                     sep = "\n")
      } else {   
        
        if(ranef_double) {   # two random effects (species + another)
          
          index_letter <- paste0("index_", param, "_ranef_", current_cov)
          species_draws[[effect_names_index]] <- paste(paste("# continuous", type, "covariate with random effects:", effect_name_print), 
                                                       paste0("for(", index_letter, " in 1:", length(unique(covariates[, gsub(keyword_nested, "", tmp[2], fixed = TRUE)])), ") {"),
                                                       paste0(param, ".ranef.cont.", current_cov, "[", speciesIndex, ", ", index_letter, "] ~ dnorm(", mean.tmp, ", ", tau.tmp, ")"),
                                                       "}",
                                                       sep = "\n")
          
          
        } else {    # only other random effect (not species)
          index_letter <- paste0("index_", param, "_ranef_", current_cov)
          species_draws[[effect_names_index]] <- paste(paste("# continuous", type, "covariate with random effects:", effect_name_print), 
                                                       paste0("for(", index_letter, " in 1:", length(unique(covariates[, tmp[2]])), ") {"),
                                                       paste0(param, ".ranef.cont.", current_cov, "[", index_letter, "] ~ dnorm(", mean.tmp, ", ", tau.tmp, ")"),
                                                       "}",
                                                       sep = "\n")
        }
      }
    }
    
    attr(priors_list, "params") <- c(paste0(param, ".ranef.cont.", effect_names2, ".mean"),
                                     paste0(param, ".ranef.cont.", effect_names2, ".sigma"))
    
    
    # make list of parameters
    inits_tmp <- vector(mode = "list", length = length(effect_names2) * 2)
    names(inits_tmp) <- c(paste0(param, ".ranef.cont.", effect_names2, ".mean"),
                          paste0(param, ".ranef.cont.", effect_names2, ".tau"))
    

    
    for(i in 1:length(inits_tmp)){
      if(endsWith(names(inits_tmp)[i], "mean")) if(ranef_double_all[i]){
        inits_tmp[[i]] <- lapply(attr(ranef_double_all, "nlevels"), modifyInits, inits = inits_list$inits_runif_mean_0)[[i]]
      } else {
        inits_tmp[[i]] <- inits_list$inits_runif_mean_0
      }
      
      if(endsWith(names(inits_tmp)[i], "tau"))  if(ranef_double_all[i - length(effect_names)]) {
        inits_tmp[[i]] <- lapply(attr(ranef_double_all, "nlevels"), modifyInits, inits = inits_list$inits_runif_tau)[[i - length(inits_tmp) / 2]]
      } else {
        inits_tmp[[i]] <- inits_list$inits_runif_tau
      }
    }
    attr(priors_list, "inits") <- inits_tmp
    
    attr(priors_list, "formula") <- paste(" + ", param, ".ranef.cont.", effect_names2, "[", ranef_index, "] * ", current_cov2, "[", stationIndex, "]", collapse = "", sep = "")
    
    names(species_draws) <- paste0(param, ".ranef.cont.", effect_names2)
    attr(species_draws, "random_effect") <- ifelse(ranef_index == speciesIndex | (startsWith(ranef_index, speciesIndex) & endsWith(ranef_index, paste0("[", stationIndex, "]"))),
                                                   "species", "not_species")
    
    
    ################# CHECK LINE ABOVE !!! ########
    
  } else {
    priors_list <- list("# < empty > \n\n")
    attr(priors_list, "params") <- NULL
    attr(priors_list, "formula") <- ""
    species_draws <- paste("# continuous", type, "covariates: no random effect of species")
  }
  
  
  return(list(priors_list, 
              species_draws)) 
}



randomSpeciesStationEffectPriors <- function(doit,
                                             prior_list, 
                                             inits_list,
                                             speciesIndex,
                                             stationIndex) {
  
  if(doit) {
  param <- "alpha"
    
  # mean.tmp  <- 0
  tau.tmp   <- "alpha.speciesstation.ranef.tau" #paste0(param, ".ranef.cont.", current_cov, ".tau", ifelse(ranef_double, paste0("[", speciesIndex, "]"), ""))
  sigma.tmp <- "alpha.speciesstation.ranef.sigma" #paste0(param, ".ranef.cont.", current_cov, ".sigma", ifelse(ranef_double, paste0("[", speciesIndex, "]"), ""))
  
  param_name_site_species <- "alpha.speciesstation.ranef"
  
  priors_list <- list()
  
  priors_list[[1]] <- paste(
    #paste("# Random Species-station effect"),
    # if(ranef_double) paste0("for(", speciesIndex, " in 1:", speciesMax, "){"),
    
    
    
    # paste0(mean.tmp, " <- ", 0#prior_list$dnorm),
    paste0(tau.tmp, " ~ ", prior_list$dgamma),
    paste0(sigma.tmp, " <- sqrt(1 / ", tau.tmp, ")"),
    # if(ranef_double) "}",
    "\n", 
    sep = "\n")
  

  attr(priors_list, "params") <- c(sigma.tmp, param_name_site_species)
  
  
  inits_tmp <- vector(mode = "list", length = 1)
  names(inits_tmp) <- "alpha.speciesstation.ranef.tau"
  inits_tmp[[1]] <- inits_list$inits_runif_tau
    
  attr(priors_list, "inits") <- inits_tmp
  
  attr(priors_list, "formula") <- #paste(" + ",  "[", ranef_index, "] * ", current_cov2, "[", stationIndex, "]", collapse = "", sep = "")
    paste(" + ",  param_name_site_species, "[", speciesIndex, ", ", stationIndex, "]", collapse = "", sep = "")
  
  
  species_draws <- paste("# Random effect of species and station on detection probability:" ,
                         paste(param_name_site_species, "[", speciesIndex, ", ", stationIndex, "] ~ dnorm(0, alpha.speciesstation.ranef.tau)", collapse = "", sep = ""),
                         "\n", 
                         sep = "\n")
# beta.ranef.cont.habitat[i] ~ dnorm(beta.ranef.cont.habitat.mean, beta.ranef.cont.habitat.tau)")
  
  } 
  
  
  if(!doit) {
    priors_list <- list("# < empty > \n\n")
    attr(priors_list, "params") <- NULL
    attr(priors_list, "formula") <- ""
    species_draws <- paste("# No random effect of species and station on detection probability")
  }
  
  return(list(priors_list,
              species_draws))
  }




fixedEffectPriorsCateg <- function(effect_names, 
                                   covariates, 
                                   type,
                                   prior_list, 
                                   stationIndex){
  
  if(type == "detection"){
    param <- "alpha"
    type_short <- "det"
  }
  if(type == "occupancy"){
    param <- "beta"
    type_short <- "occu"
  }
  
  
  if(!is.null(effect_names)){
    priors_list <- list()
    attr(priors_list, "n_levels") <- rep(NA, times = length(effect_names))
    
    for(effect_names_index in 1:length(effect_names)){
      
      current_cov <- effect_names[effect_names_index]
      current_cov_values <- covariates[, current_cov]
      index_letter <- paste0("index_cat_ranef_", type_short, "_", current_cov)
      
      
      if(length(unique(current_cov_values)) == 1) stop(paste(type, "covariate", effect_names[effect_names_index], "(fixed effect) has only 1 factor level."), call. = FALSE)
      
      priors_list[[effect_names_index]] <- paste(
        paste("# Covariate:", current_cov),
        paste0(param, ".fixed.categ.", current_cov, "[1] <- 0"),
        paste0("for(", index_letter, " in 2:", length(unique(current_cov_values)), ") {"),
        paste0(param, ".fixed.categ.", current_cov, "[", index_letter, "] ~ ", prior_list$dnorm),
        "}",
        "\n", 
        sep = "\n")
      attr(priors_list, "n_levels")[effect_names_index] <- length(unique(current_cov_values))
    }
    attr(priors_list, "params") <- paste0(param, ".fixed.categ.", effect_names)
    attr(priors_list, "index_letters") <- paste0("index_cat_ranef_", type_short, "_", effect_names)
    
    inits_tmp <- vector(mode = "list", length = length(effect_names))
    names(inits_tmp) <- paste0(param, ".fixed.categ.", effect_names)
    
    for(n in 1:length(effect_names)) {
      inits_tmp[[n]][1] <- NA
      inits_tmp[[n]][2:attr(priors_list, "n_levels")[n]] <- rnorm(attr(priors_list, "n_levels")[n] - 1)
    }
    attr(priors_list, "inits") <- inits_tmp
    rm(inits_tmp)
    
    attr(priors_list, "formula") <- paste(" + ", param,".fixed.categ.", effect_names, "[",   effect_names, "[", stationIndex, "]", "]", 
                                          collapse = "", sep = "")
    
  } else {
    priors_list <- list("# < empty > \n\n")
    attr(priors_list, "params") <- NULL
    attr(priors_list, "formula") <- ""
  }
  return(priors_list)
}


randomEffectPriorsCateg <- function(effect_names, 
                                    covariates, 
                                    type,
                                    prior_list,
                                    speciesIndex, 
                                    stationIndex){
  
  if(type == "detection"){
    param <- "alpha"
    type_short <- "det"
  }
  if(type == "occupancy"){
    param <- "beta"
    type_short <- "occu"
  }
  
  if(!is.null(effect_names)){
    priors_list <- list()
    species_draws <- list()
    
    attr(priors_list, "n_levels") <- rep(NA, times = length(effect_names))
    
    for(effect_names_index in 1:length(effect_names)){
      
      current_cov <- effect_names[effect_names_index]
      current_cov_values <- covariates[, current_cov]
      index_letter <- paste0("index_cat_ranef_", type_short, "_", current_cov)
      
      if(length(unique(current_cov_values)) == 1) stop(paste(type, "covariate", effect_names[effect_names_index], "(random effect) has only 1 factor level." ), call. = FALSE)
      
      priors_list[[effect_names_index]] <- paste(
        paste("# Community mean effects of", current_cov),
        paste0(param, ".ranef.categ.", current_cov, ".mean", "[1] <- 0"),
        paste0(param, ".ranef.categ.", current_cov, ".tau", "[1] <- 0"),
        paste0(param, ".ranef.categ.", current_cov, ".sigma", "[1] <- 0"),     # is included in random effect of intercept already, hence 0
        "",
        paste0("for(", index_letter, " in 2:", length(unique(current_cov_values)), ") {"),
        paste0(param, ".ranef.categ.", current_cov, ".mean",  "[", index_letter, "] ~ ", prior_list$dnorm),   
        paste0(param, ".ranef.categ.", current_cov, ".tau",   "[", index_letter, "] ~ ", prior_list$dgamma),
        paste0(param, ".ranef.categ.", current_cov, ".sigma", "[", index_letter, "] <- sqrt(1 / ", param, ".ranef.categ.", current_cov, ".tau", "[", index_letter, "]", ")"),
        "}",
        "",
        sep = "\n")
      attr(priors_list, "n_levels")[effect_names_index] <- length(unique(current_cov_values))
      
      if(attr(priors_list, "n_levels")[effect_names_index] == 1) stop(paste("covariate", current_cov, "has only 1 unique value:", unique(current_cov_values)), call. = F)
      
      
      species_draws[[effect_names_index]] <- paste(ifelse(effect_names_index == 1, paste("# categorical", type, "covariates with random effects:"), ""),
                                                   paste0("", param, ".ranef.categ.", current_cov, "[", speciesIndex, ", 1] <- 0"), 
                                                   paste0("for (", index_letter, " in 2:",  length(unique(current_cov_values)), "){"),
                                                   paste0("", param, ".ranef.categ.", current_cov, "[", speciesIndex, ", ", index_letter, "] ~ dnorm(", param, ".ranef.categ.", current_cov, ".mean", "[", index_letter, "], ", param, ".ranef.categ.", current_cov, ".tau", "[", index_letter, "])"),
                                                   "}", sep = "\n")
    }
    
    attr(priors_list, "params") <- c(paste0(param, ".ranef.categ.", effect_names, ".mean"),
                                     paste0(param, ".ranef.categ.", effect_names, ".sigma"))
    
    attr(priors_list, "index_letters") <- paste0("index_cat_ranef_", type_short, "_", effect_names)
    
    inits_tmp <- vector(mode = "list", length = length(effect_names)  * 2)
    names(inits_tmp) <- c(paste0(param, ".ranef.categ.", effect_names, ".mean"),
                          paste0(param, ".ranef.categ.", effect_names, ".tau"))
    
    for(n in 1:length(effect_names)) {
      inits_tmp[[n]][1] <- NA
      inits_tmp[[n]][2:attr(priors_list, "n_levels")[n]] <- rnorm(attr(priors_list, "n_levels")[n] - 1)
      
      inits_tmp[[n + length(effect_names)]][1] <- NA
      inits_tmp[[n + length(effect_names)]][2:attr(priors_list, "n_levels")[n]] <- abs(rnorm(attr(priors_list, "n_levels")[n] - 1))
      
    }
    attr(priors_list, "inits") <- inits_tmp
    rm(inits_tmp)
    
    names(species_draws) <- paste0("", param, ".ranef.categ.", effect_names)
    
    attr(priors_list, "formula") <- paste(" + ", param, ".ranef.categ.", effect_names, "[", speciesIndex, ", ", effect_names, "[", stationIndex, "]", "]", 
                                          collapse = "", sep = "")
  } else {
    priors_list <- list("# < empty > \n\n")
    attr(priors_list, "params") <- NULL
    species_draws <- paste("# categorical", type, "covariates: no random effect of species")
    attr(priors_list, "formula") <- ""
  }
  
  
  
  return(list(priors_list, 
              species_draws))
}

