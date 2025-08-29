
plot_effects_commOccu <- function(object,
                                  mcmc.list,
                                  submodel = "state",
                                  response = "occupancy",
                                  speciesSubset,
                                  draws = 1000,
                                  outdir,
                                  level = 0.95,
                                  keyword_quadratic = "_squared",
                                  ...)
{
  
  
  submodel <- match.arg(submodel, choices = c("det", "state"))
  
  if(submodel == "state") {
    keyword_submodel <- "^beta"
    keyword_submodel_short <- "beta"
  }
  if(submodel == "det") {
    keyword_submodel <- "^alpha"
    keyword_submodel_short <- "alpha"
  } 
  
  response <- match.arg(response, choices = c("occupancy", "abundance"))
  if(response == "abundance" & object@model != "RN") stop("response = 'abundance' is only available for Royle-Nichols models.")
  if(response == "abundance" & submodel == "det") warning("response = 'abundance' is not available for the detection submodel.")
  
  # get covariate information for submodel
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  
  if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  
  # get intercept information for submodel
  cov_info_intercept <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "intercept",]

  # subset parameters of submodel
  stopifnot(all(cov_info_subset$coef %in% object@params))
  params_submodel <- object@params[grep(keyword_submodel, object@params)]
  

  # subset posterior matrix to number of draws
  posterior_matrix <- as.matrix(mcmc.list)
  if(hasArg(draws)) {
    if(nrow(posterior_matrix) > draws)     posterior_matrix <- posterior_matrix[sample(1:nrow(posterior_matrix), draws),]
  } 
  
  # subset posterior matrix to current submodel
  posterior_matrix <- posterior_matrix[, grep(keyword_submodel, colnames(posterior_matrix))]
  
  params_covariate <- cov_info_subset$covariate
  if(length(params_covariate) == 0) stop ("No covariates found", call. = F)
  
  
  list_responses <- list()
  
  
  
  
  # loop over covariates
  for(cov in 1:nrow(cov_info_subset)) {
    
    current_cov <- cov_info_subset$covariate[cov]
    current_coef <- cov_info_subset$coef[cov]
    
    
    is_squared <- cov_info_subset$is_quadratic[cov]
    
    if(is_squared) {
      attr(params_covariate, "include") [cov] <- FALSE
      if(gsub(keyword_quadratic, "", current_cov) %in% params_covariate) next
    } 
    attr(params_covariate, "include") [cov] <- TRUE
    

    if(!is.na(cov_info_subset$ranef_cov[cov])){
      warning(paste(current_cov, 
                    " has a random effect other than species. This is currently not supported. Skipping", call. = F))
      next
      }
    
    if(cov_info_subset$ranef_nested[cov])  {
      warning(paste(current_cov, 
                    " has a nested random effect. This is currently not supported. Skipping", call. = F))
      next
      }
    
    
    
    # check if there is a squared version of the current covariate
    
    has_squared <- cov_info_subset$has_quadratic[cov]
    if(paste0(current_cov, keyword_quadratic) %in% params_covariate){
      #has_squared <- TRUE
      squared_cov <- paste0(current_cov, keyword_quadratic)
    } #else {
      #has_squared <- FALSE
    #}
    
    # determine data type of current covariate
    covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
    covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
    
    
    # covariate_is_fixed <- !cov_info_subset$ranef[cov]
    # covariate_is_ranef <- cov_info_subset$ranef[cov]
    
    effect_type <- ifelse(cov_info_subset$ranef[cov], "ranef",
                          ifelse(cov_info_subset$independent[cov], "independent", "fixed"))
    
    covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
    
    
    # create values to predict to
    if(covariate_is_factor) {
      if(covariate_is_site_cov){
        values_to_predict <- seq(1,
                                 length(levels(object@data[[current_cov]])))
      } else {
        values_to_predict <- attr(object@data[[paste0(current_cov, "_integer")]], "levels")
      }
    }
    
    
    if(covariate_is_numeric) {  
      values_to_predict <- seq(min(object@data[[current_cov]]),
                               max(object@data[[current_cov]]), 
                               length.out = 100)
      }
    
    
    # empty matrix for predicted values
    out <- array(data = NA, dim = c(length(values_to_predict),    # number of values to predict
                                    object@data$M,                # number of species
                                    nrow(posterior_matrix)))      # number of posterior draws
    # likewise for intercept
    out_intercept <- out
    
    
    if(has_squared){
      values_to_predict_sq <- values_to_predict ^ 2
      
      out_sq <- array(data = NA, dim = c(length(values_to_predict_sq),
                                         object@data$M, 
                                         nrow(posterior_matrix)))
    }
    
    
    # species loop
    for(i in 1:dim(out)[2]){
      
      if(cov_info_intercept$ranef == TRUE | cov_info_intercept$independent == TRUE){  # random or independent intercepts
        out_intercept[,i,] <- matrix(posterior_matrix[, colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] , 
                 nrow = dim(out_intercept)[1], ncol = dim(out_intercept)[3], byrow = T)
      } else {
        out_intercept[,i,] <- matrix(posterior_matrix[, grepl(paste0(keyword_submodel_short, "0$"), colnames(posterior_matrix))] , 
                 nrow = dim(out_intercept)[1], ncol = dim(out_intercept)[3], byrow = T)
      }
      
        
        
      # # get intercepts
      # if(!paste0(keyword_submodel_short, "0.mean") %in%  object@params) {
      #   # fixed intercept
      #   out_intercept[,i,] <- posterior_matrix[, grepl(paste0(keyword_submodel_short, "0$"), colnames(posterior_matrix))] 
      # } else {
      #   
      #   # random intercept
      #   out_intercept[,i,] <- posterior_matrix[, colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] 
      # }
      
      
      if(covariate_is_numeric) {
        
        if(effect_type == "fixed") {
          index_covariate <- grep(paste0(current_coef, "$"), colnames(posterior_matrix))
        } else {    # ranef or independent
          index_covariate <- grep(paste0(current_coef, "[", i, "]"), colnames(posterior_matrix), fixed = T)
        }
        if(length(index_covariate) == 0) stop(paste("Covariate", current_coef, "not found in posterior matrix"), call. = FALSE)
        if(length(index_covariate) >= 2) stop(paste("Covariate", current_coef, "has more than 2 matches in posterior matrix"), call. = FALSE)
        
        
        out[,i,] <-  sapply(posterior_matrix[, index_covariate], FUN = function(x){
          x * values_to_predict
        })
        
        
        if(has_squared){
          out_sq[,i,] <-  sapply(posterior_matrix[, grep(paste0(squared_cov, "[", i, "]"), colnames(posterior_matrix), fixed = TRUE)], FUN = function(x){
            x * values_to_predict_sq
          })
        }
      }
      
      if(covariate_is_factor) {
        
        if(effect_type == "fixed") index_covariate <- grep(current_coef, colnames(posterior_matrix))
        if(effect_type == "ranef") index_covariate <- grep(paste0(current_coef, "[", i, ","), colnames(posterior_matrix), fixed = T)
        
        
        for(j in 1:length(index_covariate)){
          out[j,i,] <- posterior_matrix[, index_covariate [j]]
        }
      }
      
      suppressWarnings(rm(index_covariate))
      
    }   # end species loop
    
    
    # intercept + linear covariate effect
    if(!has_squared){
      out_comb <- out_intercept + out
    }
    
    # intercept + linear + quadratic covariate effect
    if(has_squared){
      out_comb <- out_intercept + out + out_sq
    }
    
    if(!.hasSlot(object, "model")) {  # legacy support (when no RN model available)
      val <- exp(out_comb) / (exp(out_comb) + 1)    # prediction for each species / habitat value (from mean estimates)
    } else {
      if(object@model == "RN" & submodel == "state"){
        lambda <- exp(out_comb)    # val is lambda. out_comb is log(lambda)
        psi <- 1 - dpois(0, lambda)  
        
        if(response == "abundance") val <- lambda
        if(response == "occupancy") val <- psi
        
      } else {
        val <- exp(out_comb) / (exp(out_comb) + 1) 
      }
    }
    
    # summarize estimates (across posterior samples)
    val.mean  <- apply(val, MARGIN = c(1,2), mean)
    val.lower <- apply(val, MARGIN = c(1,2), quantile, (1-level) / 2, na.rm = FALSE)
    val.upper <- apply(val, MARGIN = c(1,2), quantile, (1 - (1-level) / 2), na.rm = FALSE)
    
    # make data frame for ggplot
    val.mean2  <- reshape2::melt(val.mean)
    val.lower2 <- reshape2::melt(val.lower)
    val.upper2 <- reshape2::melt(val.upper)
    
    names(val.mean2)  <- c("Index", "Species", "mean")
    names(val.lower2) <- c("Index", "Species", "lower")
    names(val.upper2) <- c("Index", "Species", "upper")
    
    vals <- cbind(val.mean2, lower = val.lower2$lower, upper = val.upper2$upper)
    vals <- cbind(cov = values_to_predict,
                   vals)
    colnames(vals) [1] <- current_cov
    
    
    # assign species names (if available)
    if(!is.null(dimnames(object@data$y)[[1]])) {
      vals$Species <- dimnames(object@data$y)[[1]][vals$Species]
    }
    
    vals <- vals[order(vals$Species, vals[, 1]),]
    
    
    if(submodel == "det")  {
      label_filename <- "detection"
      ylabel <- "Detection probability p"
    }
    if(submodel == "state" & object@model == "Occupancy"){
      label_filename <- "occupancy"
      ylabel <- expr(paste("Occupancy probability  ", psi))
    }
    if(submodel == "state" & object@model == "RN")  {
      if(response == "occupancy"){
        label_filename <- "occupancy"
        ylabel <- expr(paste("Occupancy probability  ", psi))
      }
      
      if(response == "abundance"){
        label_filename <- "abundance"
        ylabel <- expr(paste("Local abundance"))
      }
    } 
    
    main <- paste0(ifelse(covariate_is_site_cov, "Site", "Observation"), " covariate: ", current_cov)
    
    
    subtitle <- paste0(ifelse(effect_type == "ranef", "Random effect", ifelse(effect_type == "independent", "Independent effects", "Fixed effect")),
                       ifelse(has_squared, " (with quadratic term)", ""),
                       ifelse(is_squared, " quadratic term (no linear term)", ""))
    
    
    # make cran checks happy
    lower <- NULL
    upper <- NULL
    
    # for squared covariates which have no unsquared version, sqrt-transform covariate and add expand covariate range to negative values (by mirr)
    if(is_squared & !has_squared) {
      vals[,1] <- sqrt(vals[,1])
      
      vals2 <- vals
      vals2[,1] <- -vals2[,1]
      vals <- rbind(vals, vals2)
      
    }
    
    
    # subset species, if requested
    if(hasArg(speciesSubset)) {
      if(any(!speciesSubset %in% unique(vals$Species))) {
        stop(paste("Species", 
                   paste(speciesSubset[!speciesSubset %in% unique(vals$Species)], collapse = ", "), 
                   "not found in model."))
      }
      vals <- vals[vals$Species %in% speciesSubset, ]
    }
    
    
    # plot
    
    combine <- FALSE
    
    if(covariate_is_numeric){
      
      p <- ggplot(vals, aes_string(x = params_covariate[[cov]], y = "mean", group = "Species")) + 
        geom_line() +
        theme_bw() +
        ggtitle(label = main,
                subtitle = subtitle) +
        xlab (ifelse(is_squared, gsub(keyword_quadratic, "", current_cov), current_cov)) +
        ylab(ylabel) +
        xlim(range(vals[, 1])) +
        theme(panel.grid.minor = element_blank())
      
      if(.hasSlot(object, "model")) if(object@model == "Occupancy") p <- p + ylim(0, 1) 
      if(!.hasSlot(object, "model")) p <- p + ylim(0, 1) 
      
      # note for later, can optionally plot all species in  one plot if combine = TRUE (= ggplot code to this point)
      if(!combine){
        p <- p + facet_wrap(~Species) +
          geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = 0.2)
      }
    }
    
    
    if(covariate_is_factor){
      
      # create x axis labels for factors
      if(covariate_is_site_cov){
        vals[,1] <- factor(levels(object@data[[current_cov]]) [vals[,1]],
                           levels = levels(object@data[[current_cov]]))
      } 
      
      p <- ggplot(vals, aes_string(x = params_covariate[[cov]], y = "mean", group = "Species")) + 
        geom_col() +
        facet_wrap(~Species) +
        geom_linerange(aes(ymin = "lower", ymax = "upper")) +
        theme_bw() +
        ggtitle(label = main,
                subtitle = subtitle) +
        xlab (current_cov) +
        ylab(ylabel) +
        theme(panel.grid.minor = element_blank())
      
      if(.hasSlot(object, "model")) if(object@model == "Occupancy") p <- p + ylim(0, 1) 
      if(!.hasSlot(object, "model")) p <- p + ylim(0, 1) 
      
      # don't know yet how to combine species on one plot. 
      
    }
    
    
    if(hasArg(outdir)) {
      ggsave(filename = file.path(outdir, paste0("response_curves_", label_filename, "_", current_cov, "_", Sys.Date(),  ".png")),
             plot = p,
             ...)
    }
    
    list_responses [[cov]] <- p
    
  }
  
  names(list_responses) <- params_covariate[attr(params_covariate, "include")]
  
  return(list_responses)
}


  setGeneric("plot_effects", def = function(object, ...) standardGeneric("plot_effects"))
  
  
  #' Plot Marginal Effects of Covariates
  #' 
  #' Plot marginal effect plots (= response curves if covariates are continuous) for all species in a community (multi-species) occupancy model. Takes into account species-specific intercepts (if any). Currently only supports continuous covariates, not categorical covariates.
  #'
  #' @aliases plot_effects
  #' @param object \code{commOccu} object
  #' @param mcmc.list  mcmc.list. Output of \code{fit} called on a \code{commOccu} object
  #' @param submodel  character. Submodel to get plots for. Can be \code{"det"} (detection submodel) or \code{"state"} (occupancy submodel)
  #' @param response character. response type on y axis. Only relevant for \code{submodel = "state"}. Default is \code{"occupancy"}, can be set to \code{"abundance"} for Royle-Nichols models
  #' @param speciesSubset  character. Species to include in effect plots.
  #' @param draws  integer. Number of draws from the posterior to use when generating the plots. If fewer posterior samples than specified in \code{draws} are available, all posterior samples are used.
  #' @param outdir character. Directory to save plots to (optional)
  #' @param level  numeric. Probability mass to include in the uncertainty interval.
  #' @param keyword_quadratic  character. A suffix in covariate names in the model that indicates a covariate is a quadratic effect of another covariate which does not carry the suffix in its name (e.g. if the covariate is "elevation", the quadratic covariate would be "elevation_squared").
  #' @param ...  additional arguments for \code{\link[ggplot2]{ggsave}} - only relevant if \code{outdir} is defined
  #'
  #' @details
  #' Users who wish to create their own visualizations can use the data stored in the ggplot output. It is accessed via e.g. \code{output$covariate_name$data}
  #' 
  #'
  #' @return A list of ggplot objects (one list item per covariate).
  #' @export
  #' @importFrom ggplot2 geom_vline geom_linerange geom_pointrange element_blank theme labs expr
  #' @importFrom ggplot2 scale_color_manual scale_y_discrete aes_string vars facet_grid facet_wrap ylim geom_col
  # @import coda
  #'
  setMethod("plot_effects", 
            signature = c(object = "commOccu"), 
            plot_effects_commOccu)
  
  
  
  

plot_coef_commOccu <- function(object, 
                               mcmc.list,
                               submodel = "state",
                               speciesSubset,
                               ordered = TRUE,
                               combine = FALSE,
                               outdir,
                               level = c(outer = 0.95, inner = 0.75),
                               colorby = "significance",
                               scales = "free_y",
                               community_lines = FALSE,
                               ...) {
  
  
  submodel <- match.arg(submodel, choices = c("det", "state"))
  colorby  <- match.arg(colorby, choices = c("significance", "Bayesian p-value"))
  if(colorby == "Bayesian p-value") stop(paste("colorby = 'Bayesian p-value' is currently not supported."))
  
  scales <- match.arg(scales, choices = c("free", "free_y"))
  
  if(submodel == "state") {
    keyword_submodel <- "^beta"
    keyword_submodel_short <- "beta"
  }
  if(submodel == "det") {
    keyword_submodel <- "^alpha"
    keyword_submodel_short <- "alpha"
  } 
  
  
  stopifnot(is.logical(ordered))
  stopifnot(is.logical(combine))
  
  if(combine & ordered) {
    message("'combine' and 'ordered' can't both be TRUE. Setting 'ordered = FALSE'")
    ordered <- FALSE
  }
  
  
  # get covariate information for submodel
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  
  if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  
  
  list_responses <- list()
  
  
  # posterior summaries
  stopifnot(length(level) == 2)
  
  posteriorSummary  <- summary(mcmc.list, quantiles = c((1-level[1]) / 2,         # lower outer
                                                        (1-level[2]) / 2,         # lower inner      
                                                        0.5,                      # median
                                                        1-((1-level[2]) / 2),     # upper inner
                                                        1-((1-level[1]) / 2)))    # upper outer
  df_quantiles <- data.frame(posteriorSummary$quantiles)
  # 
  
  # all estimates model parameters
  params_all <- rownames(df_quantiles)
  
  # container for output plots
  p_list <- list()
  df_quantiles_list <- list()
  
  
  # get Bayesian p-values
  df_statistics <- posteriorSummary$statistics
  df_statistics_Bayes_pvals_overall <- as.data.frame(df_statistics[grep("Bpvalue$", rownames(df_statistics)), , drop = F])
  df_statistics_Bayes_pvals_species <- df_statistics[grep("Bpvalue_species", rownames(df_statistics)), ]
  
  # loop over covariates
  for(cov in 1:nrow(cov_info_subset)) {
    
    current_cov  <- cov_info_subset$covariate[cov]
    current_coef <- cov_info_subset$coef[cov]
    
    #if(covariate %in% skip) next
    if(!is.na(cov_info_subset$ranef_cov[cov])){
      warning(paste(current_cov, 
                    " has a random effect other than species. This is currently not supported. Skipping", call. = F))
      next
    }
    
    if(cov_info_subset$ranef_nested[cov])  {
      warning(paste(current_cov, 
                    " has a nested random effect. This is currently not supported. Skipping", call. = F))
      next
    }
    
    
    # determine data type of current covariate
    covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
    covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
    
    
    # covariate_is_fixed <- !cov_info_subset$ranef[cov]
    # covariate_is_indep <- cov_info_subset$independent[cov]
    # covariate_is_ranef <- cov_info_subset$ranef[cov]
    
    
    
    effect_type <- ifelse(cov_info_subset$ranef[cov], "ranef",
                          ifelse(cov_info_subset$independent[cov], "independent", "fixed"))
    
    
    covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
    

    
    if(covariate_is_numeric){
      
      if(effect_type == "fixed")       index_covariate <- grep(paste0(current_coef, "$"), rownames(df_quantiles))
      if(effect_type == "ranef")       index_covariate <- grep(paste0(current_coef, "[" ), rownames(df_quantiles), fixed = T)
      if(effect_type == "independent") index_covariate <- grep(paste0(current_coef, "[" ), rownames(df_quantiles), fixed = T)
      
      
      df_quantiles_i  <- df_quantiles[index_covariate, ]
      
      if(effect_type == "fixed")   df_quantiles_i$type  <- c("mean")
      if(effect_type == "ranef") {
        
        # get community mean
        index_covariate_mean_ranef <- grep(paste0(current_coef, ".mean$"), rownames(df_quantiles))
        
        df_quantiles_i  <- rbind(df_quantiles[index_covariate_mean_ranef, ], df_quantiles_i)
        
        df_quantiles_i$type  <- c("mean", rep("species", times = length(index_covariate)))
      }  
      
      if(effect_type == "independent"){
        df_quantiles_i$type  <- c("species")
      }
      
    }
    
    
    if(covariate_is_factor){
      
      if(covariate_is_site_cov){
        levels_tmp <- levels(object@input$siteCovs[, current_cov])
        nlev <- length(levels_tmp)
      }
      if(!covariate_is_site_cov){
        levels_tmp <- attr(object@data[[paste0(current_cov, "_integer")]], "levels")
        nlev <- length(levels_tmp)
      }
      
      
      if(effect_type == "fixed")  index_covariate <- grep(paste0(current_coef, "["),  rownames(df_quantiles), fixed = T)
      if(effect_type == "ranef")  index_covariate <- grep(paste0(current_coef, "[" ), rownames(df_quantiles), fixed = T)
      
      
      df_quantiles_i  <- df_quantiles[index_covariate, ]  
      
      if(effect_type == "fixed") df_quantiles_i$type  <- c("mean")
      
      if(effect_type == "ranef") {
        
        # add community mean
        index_covariate_mean_ranef <- grep(paste0(current_coef, ".mean"), rownames(df_quantiles), fixed = T)   # does this affect categ fixed effects?
        
        df_quantiles_i  <- rbind(df_quantiles[index_covariate_mean_ranef, ], df_quantiles_i)
        
        df_quantiles_i$type  <- c(rep("mean", times = length(index_covariate_mean_ranef)), rep("species", times = length(index_covariate)))
      }
    }
    
    
    
    colnames(df_quantiles_i)[1:5] <- c("lower_outer", "lower_inner", "median", "upper_inner", "upper_outer")
    
    
    # get significance levels
    significance <- rep("no", times = nrow(df_quantiles_i))
    significance[which(df_quantiles_i$lower_inner < 0 & df_quantiles_i$upper_inner < 0 | 
                         df_quantiles_i$lower_inner > 0 & df_quantiles_i$upper_inner > 0)] <- "inner"
    significance[which(df_quantiles_i$lower_outer < 0 & df_quantiles_i$upper_outer < 0 | 
                         df_quantiles_i$lower_outer > 0 & df_quantiles_i$upper_outer > 0)] <- "outer"
    
    df_quantiles_i$significance  <- significance
    
    # add Bayesian p-values
    
    # if colorby == "Bayesian p-value":
    # slight inconsistency: values in "level" will still be used for error bar width
    # but will affect colors only via Bayesian p-values of the species
    # not via the actual parameter estimates. 
    # So a parameter estimate can be highly significant but grey bc bayesian p-value of the the species is ok
    # it's mostly for model checking, so I guess it's fine
    if(colorby == "Bayesian p-value"){
      if(covariate_is_numeric){
        if(effect_type == "fixed") df_pval <- df_statistics_Bayes_pvals_overall
        if(effect_type == "ranef") df_pval <- rbind(df_statistics_Bayes_pvals_overall, 
                                                    df_statistics_Bayes_pvals_species)
      }
      if(covariate_is_factor){
        if(effect_type == "fixed") df_pval <- df_statistics_Bayes_pvals_overall [rep(1, times = nlev),]
        if(effect_type == "ranef") df_pval <- rbind(df_statistics_Bayes_pvals_overall [rep(1, times = nlev),], 
                                                    df_statistics_Bayes_pvals_species [rep(1:nrow(df_statistics_Bayes_pvals_species), 
                                                                                           times = nlev),])
      }
      
      stopifnot(nrow(df_pval) == nrow(df_quantiles_i))
      
      
      significance2 <- rep("no", times = nrow(df_pval))
      significance2[which(df_pval$Mean < (1-level[2]) / 2 | df_pval$Mean > (1 - (1-level[2]) / 2))] <- "inner"
      significance2[which(df_pval$Mean < (1-level[1]) / 2 | df_pval$Mean > (1 - (1-level[1]) / 2))] <- "outer"
      
      df_quantiles_i$significance2  <- significance2
      
    }
    
    # assign species names
    if(!is.null(dimnames(object@data$y)[[1]])) speciesnames <- dimnames(object@data$y)[[1]]
    if( is.null(dimnames(object@data$y)[[1]])) speciesnames <- seq_len(dim(object@data$y)[1])
    
  
    
    if(effect_type == "ranef")  {
      if(covariate_is_numeric) df_quantiles_i$species <- c("community", speciesnames)
      if(covariate_is_factor) {
        df_quantiles_i$species <- c(rep("community", times = length(index_covariate_mean_ranef)), rep(speciesnames, times = nlev))
      }
    }
    
    if(effect_type == "fixed")     df_quantiles_i$species  <- "community"
    if(effect_type == "independent") df_quantiles_i$species  <- speciesnames
    
    
    
        # subset species, if requested
    if(hasArg(speciesSubset)) {
      if(any(!speciesSubset %in% speciesnames)) {
        stop(paste("Species", 
                   paste(speciesSubset[!speciesSubset %in% speciesnames], collapse = ", "), 
                   "not found in model."))
      }
      df_quantiles_i <- df_quantiles_i[c(1, which(df_quantiles_i$species %in% speciesSubset)), ]
    }
    
    
    
    
    
    # add covariate name as column
    if(covariate_is_numeric){
      df_quantiles_i$covariate  <- current_cov
    }
    
    if(covariate_is_factor){
      if(effect_type == "fixed"){
        if(covariate_is_site_cov)  df_quantiles_i$covariate  <- factor(paste0(current_cov, "_", levels_tmp),
                                                                       labels = levels_tmp)
        if(!covariate_is_site_cov) df_quantiles_i$covariate  <- factor(paste0(current_cov, "_",  levels_tmp),
                                                                       labels = levels_tmp)
      }
      
      if(effect_type == "ranef"){
        if(covariate_is_site_cov)  df_quantiles_i$covariate  <- factor(paste0(current_cov, "_", c(levels_tmp,
                                                                                                  rep(levels_tmp, each = object@data$M))),
                                                                       levels = paste0(current_cov, "_", levels_tmp),
                                                                       labels = levels_tmp)
        
        if(!covariate_is_site_cov) df_quantiles_i$covariate  <- factor(paste0(current_cov, "_", c(levels_tmp,
                                                                                                  rep(levels_tmp, each = object@data$M))),
                                                                       levels = paste0(current_cov, "_", levels_tmp),
                                                                       labels = levels_tmp)
        
      }
    }
    
    
    # sort species (either by median effect size or by names)
    if(ordered) {
      # this currently does not sort categorical covariates with random effects correctly. 
      # Ideally if combine = FALSE they should be sorted in descending order (at least for the second factor level)
      if(covariate_is_numeric){
        df_quantiles_i$species <- factor(df_quantiles_i$species, 
                                         levels = unique(df_quantiles_i$species[order(df_quantiles_i$median)]))
      }
      
      if(covariate_is_factor){
        if(effect_type == "fixed") {
          df_quantiles_i$species <- factor(df_quantiles_i$species, 
                                           levels = unique(df_quantiles_i$species[order(df_quantiles_i$median)]))  
        }
        
        if(effect_type == "ranef"){
          subset_level2 <- df_quantiles_i[df_quantiles_i$covariate == #paste0(current_cov, "_", 
                                            levels_tmp[2]#)
                                          ,]
          
          df_quantiles_i$species <- factor(df_quantiles_i$species, 
                                           levels = subset_level2$species[order(subset_level2$median)])
        }
      }
      
    } else {
      df_quantiles_i$species <- factor(df_quantiles_i$species, levels = unique(rev(df_quantiles_i$species)))  
    }
    
    df_quantiles_list[[cov]]  <- df_quantiles_i
    
    
    # plot
    
    type <- NULL   # just for CRAN checks
    covariate <- NULL
    lower_outer <- NULL
    upper_outer <- NULL
    median <- NULL
    
    if(colorby == "significance")      color_by <- "significance"
    if(colorby == "Bayesian p-value")  color_by <- "significance2"
    
    alpha_community <- ifelse(effect_type == "ranef", 0.3, 0)
    alpha_zero <- 0.3
    color_community <- "blue"
    
    if(!combine){
      
      
      p_list[[cov]] <- ggplot (df_quantiles_i, aes_string(y = "species", x = "median", color = color_by)) +
        
        if(community_lines) {
        # community effect
          p_list[[cov]] <- p_list[[cov]] + 
            geom_vline(data = df_quantiles_i[df_quantiles_i$type == "mean", -which(colnames(df_quantiles_i) == "type")],
                       aes(xintercept = median), col = color_community, linetype = 1, alpha = alpha_community) +
            geom_vline(data = df_quantiles_i[df_quantiles_i$type == "mean", -which(colnames(df_quantiles_i) == "type")],
                       aes(xintercept = lower_outer), col = color_community, linetype = 2, alpha = alpha_community) +
            geom_vline(data = df_quantiles_i[df_quantiles_i$type == "mean", -which(colnames(df_quantiles_i) == "type")],
                       aes(xintercept = upper_outer), col = color_community, linetype = 2, alpha = alpha_community)
        }
      
      p_list[[cov]] <- p_list[[cov]] +
        geom_vline(xintercept = 0, alpha = alpha_zero) +
        
        # species effects
        geom_pointrange(aes_string(xmin = "lower_outer", xmax = "upper_outer")) + 
        geom_linerange( aes_string(xmin = "lower_inner", xmax = "upper_inner"), linewidth = 1) +
        facet_grid(rows = vars(type),
                   cols = vars(covariate),
                   scales = scales,
                   space = "free_y"
        ) +
        xlab ("Effect size") +  ylab(element_blank()) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              strip.background.y = element_blank(),
              strip.text.y = element_blank()) + 
        scale_color_manual(breaks = c("outer", "inner", "no"),
                           values=c("firebrick", "black", "grey50"),
                           guide = "none") +
        ggtitle(paste("Effect sizes:", current_cov))
      
      
      if(!covariate_is_factor) {
        p_list[[cov]] <- p_list[[cov]] + theme(strip.background.x = element_blank(),
                                               strip.text.x = element_blank())
      }

      if(color_by == "significance2"){
        p_list[[cov]] <- p_list[[cov]] + labs(subtitle = "colors indicate Bayesian p-values of species")
      }
      
      
      if(hasArg(outdir)) {
        ggsave(filename = file.path(outdir, paste0("effect_sizes_", submodel, "_", covariate, "_", Sys.Date(),  ".png")),
               plot = p_list[[cov]],
               ...)
      }
    }   
    
  }   # end covariate loop
  
  
  if(combine){
    
    df_quantiles_all  <- do.call(rbind, df_quantiles_list)
    df_quantiles_all$species <- factor(df_quantiles_all$species, 
                                       levels = rev(sort(unique(as.character(df_quantiles_all$species)))))
    
    p <- ggplot (df_quantiles_all, aes_string(y = "species", x = "median", color = color_by))
      
    if(community_lines)  {
      # community effect
     p <- p + 
       geom_vline(data = df_quantiles_all[df_quantiles_all$type == "mean", -which(colnames(df_quantiles_all) == "type")],    
                  aes(xintercept = median), col = color_community, linetype = 1, alpha = alpha_community) +
       geom_vline(data = df_quantiles_all[df_quantiles_all$type == "mean", -which(colnames(df_quantiles_all) == "type")],
                  aes(xintercept = lower_outer), col = color_community, linetype = 2, alpha = alpha_community) +
       geom_vline(data = df_quantiles_all[df_quantiles_all$type == "mean", -which(colnames(df_quantiles_all) == "type")],
                  aes(xintercept = upper_outer), col = color_community, linetype = 2, alpha = alpha_community) 
    }
    p <- p + geom_vline(xintercept = 0, alpha = alpha_zero) +
      # species effects
      geom_pointrange(aes_string(xmin = "lower_outer", xmax = "upper_outer")) + 
      geom_linerange (aes_string(xmin = "lower_inner", xmax = "upper_inner"), size = 1) +
      facet_grid(rows = vars(type), 
                 cols = vars(covariate),
                 scales = scales, 
                 space = "free_y") +
      xlab ("Effect size") +  ylab(element_blank()) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            strip.background.y = element_blank(),
            strip.text.y = element_blank()) + 
      scale_color_manual(breaks = c("outer", "inner", "no"),
                         values=c("firebrick", "black", "grey50"),
                         guide = "none")
    
    if(hasArg(outdir)) {
      ggsave(filename = file.path(outdir, paste0("effect_sizes_", submodel, "_", paste(cov_info_subset$covariate, collapse = "_"), "_", 
                                                 ifelse(!colorby == "significance", "Bayesian_pval_", ""), Sys.Date(),  ".png")),
             plot = p,
             ...)
    }
    return(p)
  }
  
  
  if(!combine){
    names(p_list) <-  cov_info_subset$covariate
    return(p_list)
  }
}



  setGeneric("plot_coef", def = function(object, ...) standardGeneric("plot_coef"))
  
  
  #' Plot effect sizes of covariates in community occupancy model
  #' 
  #' Plot effect sizes for all species in a community (multi-species) occupancy model. Currently only supports continuous covariates, not categorical covariates.
  #'
  #' @aliases plot_coef
  #' @param object \code{commOccu} object
  #' @param mcmc.list  mcmc.list. Output of \code{fit} called on a \code{commOccu} object
  #' @param submodel  character. Submodel to get plots for. Can be \code{"det"} or \code{"state"}
  #' @param speciesSubset  character. Species to include in coefficient plots.
  #' @param ordered logical. Order species in plot by median effect (TRUE) or by species name (FALSE)
  #' @param combine logical. Combine multiple plots into one plot (via facets)?
  #' @param outdir character. Directory to save plots to (optional)
  #' @param level  numeric. Probability mass to include in the uncertainty interval (two values named "outer" and "inner", in that order). Second value (= inner interval) will be plotted thicker.
  #' @param colorby character. Whether to color estimates by \code{"significance"} (of the effect estimates), or \code{"Bayesian p-value"} (of the species). Currently allows only \code{"significance"}.
  #' @param scales character. Passed to \code{\link[ggplot2]{facet_grid}}. Can be \code{"free"} to scale x axes of effect estimates independently, or "free_y" to scale all x axes identically.
  #' @param community_lines logical. Add faint vertical lines to the plot indicating median community effect (solid line) and its confidence interval (first value from \code{level}).
  #' @param ...  additional arguments for \code{\link[ggplot2]{ggsave}} - only relevant if \code{outdir} is defined.
  #'
  #' @return A list of ggplot objects (one list item per covariate).
  #' 
  #' @details
  #' Users who wish to create their own visualizations can use the data stored in the ggplot output. It is accessed via e.g. \code{output$covariate_name$data}
  #' 
  #' @export
  #' 
  #'
  setMethod("plot_coef", 
            signature = c(object = "commOccu"), 
            plot_coef_commOccu)
  
  