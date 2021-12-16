
plot.effects.commOccu <- function(object,       # commOccu object
                                   mcmc.list,    # mcmc.list (output of fit())
                                   submodel = "state",      # "det" or "state"
                                   draws = 1000,    # number of posterior samples to use (will draw random sample from posterior distribution if defined). 
                                   outdir,       # directory to save plots in (optional)
                                   level = 0.95,   # confidence level for CIs in plot
                                   keyword_squared = "_squared",   # the suffix of a covariate that indicates a quadratic effect  (e.g. "elevation" and "elevation_squared" -> will be combined in plot)
                                   ...)                            # additional arguments for ggsave()
{
  
  
  # get site covariates
  siteCovs <- object@input$siteCovs
  
  # get observation covariates
  obsCovs <- object@input$obsCovs
  
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  
  skip <- NULL
  if(any(!is.na(cov_info_subset$ranef_cov))){
    warning(paste(cov_info_subset$covariate[!is.na(cov_info_subset$ranef_cov)], collapse = ", "), 
            " has a random effect other than species. This is currently not supported.", call. = F)
    skip <- c(skip, cov_info_subset$covariate[!is.na(cov_info_subset$ranef_cov)])
  }
  
  if(any(isTRUE(cov_info_subset$ranef_nested ))) {
    warning(paste(cov_info_subset$covariate[isTRUE(cov_info_subset$ranef_nested )], collapse = ", "), 
            " has a nested random effect. This is currently not supported.", call. = F)
    skip <- c(skip, cov_info_subset$covariate[isTRUE(cov_info_subset$ranef_nested)])
  }
  
  
  #identify numeric / categorical site covariates
  siteCovs_numeric  <- siteCovs[, sapply(siteCovs, inherits, "numeric"), drop = FALSE]
  siteCovs_factor   <- siteCovs[, sapply(siteCovs, inherits, "factor"), drop = FALSE]
  
 
  #identify numeric / categorical observation covariates
  obsCovs_numeric  <- obsCovs[sapply(obsCovs, typeof) %in% c("double", "numeric")]
  obsCovs_factor   <- obsCovs[sapply(obsCovs, typeof) ==  "character"]
  
   
  submodel <- match.arg(submodel, choices = c("det", "state"))
  
  if(submodel == "state") {
    keyword_submodel <- "^beta"
    keyword_submodel_short <- "beta"
  }
  if(submodel == "det") {
    keyword_submodel <- "^alpha"
    keyword_submodel_short <- "alpha"
  } 
  
  
  # subset parameters of submodel
  params <- object@params[grep(keyword_submodel, object@params)]
  
  # subset posterior matrix to number of draws
  posterior_matrix <- as.matrix(mcmc.list)
  if(hasArg(draws)) {
    if(nrow(posterior_matrix) > draws)     posterior_matrix <- posterior_matrix[sample(1:nrow(posterior_matrix), draws),]
  } 
  posterior_matrix <- posterior_matrix[,grep(keyword_submodel, colnames(posterior_matrix))]
  
  

  # get covariate that parameters refer to
  params_covariate <- sapply(strsplit(params, split = ".", fixed = TRUE), FUN = function (x) x[length(x)])
  params_covariate <- params_covariate[params_covariate %in% names(object@data)]
  
  if(!is.null(skip)) warning(paste("Skipping covariate:", paste(skip, collapse = ", ")), call. = F)
  
  if(length(params_covariate) == 0) stop ("No covariates found", call. = F)
  
  
  list_responses <- list()
  
  attr(params_covariate, "include") <- rep(FALSE, length(params_covariate))
  
  # loop over covariates
  for(covariate in 1:length(params_covariate)) {
    
    # skip if covariate ends with _squared, and the "non-squared" covariate is available
    if(endsWith(params_covariate[covariate], keyword_squared)){
      is_squared <- TRUE
      if(gsub(keyword_squared, "", params_covariate[covariate]) %in% params_covariate) next
    } else {
      is_squared <- FALSE
    }
    
    if(params_covariate[covariate] %in% skip) next
    
    # check if there is a squared version of the current covariate
    if(paste0(params_covariate[covariate], keyword_squared) %in% params_covariate){
      has_squared <- TRUE
      squared_cov <- paste0(params_covariate[covariate], keyword_squared)
    } else {
      has_squared <- FALSE
    }
    
    # determine data type of current covariate
    covariate_is_numeric <- params_covariate[covariate] %in% c(names(siteCovs_numeric), names(obsCovs_numeric))
    covariate_is_factor  <- params_covariate[covariate] %in% c(names(siteCovs_factor),  names(obsCovs_factor))
    
    # is covariate a site covariate?
    covariate_is_site_cov <- ifelse(params_covariate[covariate] %in% colnames(siteCovs), T, F) 
    
    
    # create values to predict to
    if(covariate_is_factor){
      if(covariate_is_site_cov){
        values_to_predict <- seq(1,
                                 length(levels(object@data[[params_covariate[covariate]]])))
      } else {
        values_to_predict <- attr(object@data[[paste0(params_covariate[covariate], "_integer")]], "levels")
      }
    }
    
    
    
    if(covariate_is_numeric) {  
      values_to_predict <- seq(min(object@data[[params_covariate[covariate]]]),
                               max(object@data[[params_covariate[covariate]]]), 
                               length.out = 100)
      }
    
    
    # matrix for predicted values
    out <- array(data = NA, dim = c(length(values_to_predict),    # number of values to predict
                                    object@data$M,                # number of species
                                    nrow(posterior_matrix)))      # number of posterior draws
    
    out_intercept <- out
    
    
    if(has_squared){
      values_to_predict_sq <- values_to_predict ^ 2
      
      out_sq <- array(data = NA, dim = c(length(values_to_predict_sq),
                                         object@data$M, 
                                         nrow(posterior_matrix)))
    }
    
    
    
    
    # species loop
    for(i in 1:dim(out)[2]){    
      
      # get intercepts
      if(!paste0(keyword_submodel_short, "0.mean") %in%  object@params) { 
        # fixed intercept
        
        out_intercept[,i,] <- posterior_matrix[, grepl(paste0(keyword_submodel_short, "0$"), colnames(posterior_matrix))] 
      } else {
        # random intercept
        
        out_intercept[,i,] <- posterior_matrix[,colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] 
      }

        
        if(covariate_is_numeric) {
          index_covariate_fixed <- grep(paste0(params_covariate[covariate], "$"), colnames(posterior_matrix))
          index_covariate_ranef <- grep(paste0(params_covariate[covariate], "[", i, "]"), colnames(posterior_matrix), fixed = T)
          if(length(index_covariate_fixed) != 0 & length(index_covariate_ranef != 0)) stop(paste("found both fixed and random effect of", params_covariate[covariate], ". This might be a bug"))
          
          
          if(length(index_covariate_fixed) != 0){
            out[,i,] <-  sapply(posterior_matrix[, index_covariate_fixed], FUN = function(x){
              x * values_to_predict
            })
            random <- FALSE
          }
          
          if(length(index_covariate_ranef) != 0){
            out[,i,] <-  sapply(posterior_matrix[, index_covariate_ranef], FUN = function(x){
              x * values_to_predict
            })
            random <- TRUE
          }
          
          if(has_squared){
            out_sq[,i,] <-  sapply(posterior_matrix[, grep(paste0(squared_cov, "[", i, "]"), colnames(posterior_matrix), fixed = TRUE)], FUN = function(x){
              x * values_to_predict_sq
            })
          }
        }
        
        if(covariate_is_factor) {
          index_covariate_fixed <- grep(paste0(keyword_submodel_short, ifelse(covariate_is_site_cov, "", ".obs"), ".fixed.categ.", params_covariate[covariate]), colnames(posterior_matrix))
          index_covariate_ranef <- grep(paste0(keyword_submodel_short, ifelse(covariate_is_site_cov, "", ".obs"), ".ranef.categ.", params_covariate[covariate], "[", i, ","), colnames(posterior_matrix), fixed = T)
          if(length(index_covariate_fixed) != 0 & length(index_covariate_ranef != 0)) stop(paste("found both fixed and random effect of", params_covariate[covariate], ". This might be a bug"))
        
          
          if(length(index_covariate_fixed) != 0){
            for(j in 1:length(index_covariate_fixed)){
              out[j,i,] <- posterior_matrix[, index_covariate_fixed [j]]
            }
            random <- FALSE
          }
          
          if(length(index_covariate_ranef) != 0){
            for(j in 1:length(index_covariate_ranef)){
              out[j,i,] <- posterior_matrix[, index_covariate_ranef [j]]
            }
            random <- TRUE
          }
        }

        suppressWarnings(rm(index_covariate_fixed, index_covariate_ranef))
      }
      
      
      # intercept + linear covariate effect
      if(!has_squared){
        out_comb <- out_intercept + out
      }
      
      # intercept + linear + quadratic covariate effect
      if(has_squared){
        out_comb <- out_intercept + out + out_sq
      }
      
      prob <- exp(out_comb) / (exp(out_comb) + 1)    # prediction for each species / habitat value (from mean estimates)
      
      # summarize estimates (across posterior samples)
      prob.mean  <- apply(prob, MARGIN = c(1,2), mean)
      prob.lower <- apply(prob, MARGIN = c(1,2), quantile, (1-level) / 2)
      prob.upper <- apply(prob, MARGIN = c(1,2), quantile, (1 - (1-level) / 2))
      
      # make data frame for ggplot
      prob.mean2  <- reshape2::melt(prob.mean)
      prob.lower2 <- reshape2::melt(prob.lower)
      prob.upper2 <- reshape2::melt(prob.upper)
      
      names(prob.mean2)  <- c("Index", "Species", "mean")
      names(prob.lower2) <- c("Index", "Species", "lower")
      names(prob.upper2) <- c("Index", "Species", "upper")
      
      probs <- cbind(prob.mean2, lower = prob.lower2$lower, upper = prob.upper2$upper)
      probs <- cbind(cov = values_to_predict,
                     probs)
      colnames(probs) [1] <- params_covariate[[covariate]]
      
      
      # assign species names (if available)
      if(!is.null(dimnames(object@data$y)[[1]])) {
        probs$Species <- dimnames(object@data$y)[[1]][probs$Species]
      }
      
      probs <- probs[order(probs$Species, probs[, 1]),]
      
      
      if(submodel == "det")   ylabel <- "Detection probability p"
      if(submodel == "state") ylabel <- expression(paste("Occupancy probability  ", psi))
      
      main <- paste0(ifelse(covariate_is_site_cov, "Site", "Observation"), "covariate: ", params_covariate[covariate])
      
      
      subtitle <- paste0(ifelse(random, "Random effect", "Fixed effect"),
                        ifelse(has_squared, " (with quadratic term)", ""),
                        ifelse(is_squared, " quadratic term (no linear term)", ""))
      
      
      # make cran checks happy
      lower <- NULL
      upper <- NULL
      
      # for squared covariates which have no unsquared version, sqrt-transform covariate and add negative values
      if(is_squared & !has_squared) {
        probs[,1] <- sqrt(probs[,1])
        
        probs2 <- probs
        probs2[,1] <- -probs2[,1]
        probs <- rbind(probs, probs2)
        
      }
      
      
      
      
      # plot
      
      if(covariate_is_numeric){
        
        p <- ggplot(probs, aes_string(x = params_covariate[[covariate]], y = "mean")) + 
          geom_line() +
          facet_wrap(~Species) +
          geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = 0.2) + 
          theme_bw() +
          ggtitle(label = main,
                  subtitle = subtitle) +
          xlab (ifelse(is_squared, gsub(keyword_squared, "", params_covariate[covariate]), params_covariate[covariate])) +
          ylab(ylabel) +
          xlim(range(probs[, 1])) +
          ylim(0, 1) +
          theme(panel.grid.minor = element_blank())
      }
      
      
      if(covariate_is_factor){
        
        # create x axis labels
        if(covariate_is_site_cov){
          probs[,1] <- levels(object@data[[params_covariate[covariate]]]) [probs[,1]]
        } 
        
        p <- ggplot(probs, aes_string(x = params_covariate[[covariate]], y = "mean")) + 
          geom_col() +
          facet_wrap(~Species) +
          geom_linerange(aes_string(ymin = "lower", ymax = "upper")) +
          theme_bw() +
          ggtitle(label = main,
                  subtitle = subtitle) +
          xlab (params_covariate[covariate]) +
          ylab(ylabel) +
          ylim(0, 1) +
          theme(panel.grid.minor = element_blank())
      }
      
      
      if(hasArg(outdir)) {
        ggsave(filename = file.path(outdir, paste0("response_curves_", params_covariate[covariate], "_", Sys.Date(),  ".png")),
               plot = p,
               ...)
      }
      
      list_responses [[covariate]] <- p
      
      
      attr(params_covariate, "include") [covariate] <- TRUE
      
      # reset has_squared
      has_squared <- FALSE
      has_squared_cov <- NULL
      is_squared <- NULL
      
      #}
    }
    
    names(list_responses) <- params_covariate[attr(params_covariate, "include")]
    
    return(list_responses)
    
  }
  
  
  setGeneric("plot_effects", function(object, ...) standardGeneric("plot_effects"))
  
  
  #' Plot Marginal Effects of Covariates
  #' 
  #' Plot marginal effect plots (= response curves if covariates are continuous) for all species in a community (multi-species) occupancy model. Takes into account species-specific intercepts (if any). Currently only supports continuous covariates, not categorical covariates.
  #'
  #' @aliases plot_effects
  #' @param object \code{commOccu} object
  #' @param mcmc.list  mcmc.list. Output of \code{\link{fit}} called on a \code{commOccu} object
  #' @param submodel  Submodel to get plots for. Can be "det" or "state"
  #' @param draws  Number of draws from the posterior to use when generating the plots. If fewer than draws are available, they are all used
  #' @param outdir Directory to save plots to (optional)
  #' @param level  Probability mass to include in the uncertainty interval
  #' @param keyword_squared  character. A suffix in covariate names in the model that indicates a covariate is a quadratic effect of another covariate which does not carry the suffix in its name.
  #' @param ...  additional arguments for \code{\link[ggplot2]{ggsave}}
  #'
  #'
  #' @return list of ggplot objects
  #' @export
  #' @importFrom ggplot2 geom_vline geom_linerange geom_pointrange element_blank theme 
  #' @importFrom ggplot2 scale_color_manual scale_y_discrete aes_string vars facet_grid facet_wrap ylim geom_col
  # @import coda
  #'
  setMethod("plot_effects", signature(object = "commOccu"), 
            plot.effects.commOccu)
  
  
  
  
  
  
  # To do: 
  # color of points and lines consistent
  # grey / black / red color scheme?
  
  
  plot.coef.commOccu <- function(object, 
                                    mcmc.list,
                                    submodel = "state",
                                    ordered = TRUE,
                                    combine = FALSE,
                                    outdir,
                                    level = c(outer = 0.95, inner = 0.75),
                                    ...) {
    
    
    

    # 
    # #identify numeric / categorical covariates
    # siteCovs_factor  <- siteCovs[, sapply(siteCovs, inherits, "factor")]
    
    
    cov_types <- get_covariate_type(object, submodel = submodel)
    
    siteCovs <- cov_types$siteCovs
    siteCovs_numeric <- cov_types$siteCovs_numeric
    siteCovs_factor  <- cov_types$siteCovs_factor
    obsCovs_numeric <- cov_types$obsCovs_numeric
    obsCovs_factor <- cov_types$obsCovs_factor
    skip <- cov_types$skip
    
    
    
    submodel <- match.arg(submodel, choices = c("det", "state"))
    stopifnot(is.logical(ordered))
    stopifnot(is.logical(combine))
    
    if(combine & ordered) {
      message("'combine' and 'ordered' can't both be TRUE. Setting 'ordered = FALSE'")
      ordered <- FALSE
    }
    
    if(submodel == "state") {
      keyword_submodel <- "^beta"
      keyword_submodel_short <- "beta"
    }
    if(submodel == "det") {
      keyword_submodel <- "^alpha"
      keyword_submodel_short <- "alpha"
    } 
    
    
    # subset parameters of submodel
    params <- object@params[grep(keyword_submodel, object@params)]
    
    # get covariate that beta parameters refer to
    params_covariate <- sapply(strsplit(params, split = ".", fixed = TRUE), FUN = function (x) x[length(x)])
    params_covariate <- params_covariate[params_covariate %in% names(object@data)]
    
    if(length(params_covariate) == 0) stop ("No covariates found", call. = FALSE)
    
    list_responses <- list()
    
    
    # posterior summaries
    stopifnot(length(level) == 2)
    posteriorSummary  <- summary(mcmc.list, quantiles = c((1-level[1]) / 2, 0.5, 1-((1-level[1]) / 2)))
    df_quantiles <- data.frame(posteriorSummary$quantiles)
    
    posteriorSummary2 <- summary(mcmc.list, quantiles = c((1-level[2]) / 2, 0.5, 1-((1-level[2]) / 2)))
    df_quantiles2 <- data.frame(posteriorSummary2$quantiles)
    
    
    # all estimates model parameters
    params_all <- rownames(df_quantiles)
    
    # container for output plots
    p_list <- list()
    df_quantiles_list <- list()
    df_quantiles2_list <- list()
    
    # loop over covariates
    for(i in 1:length(params_covariate)) {
      
      covariate <- params_covariate[i]
      # if(covariate %in% names(siteCovs_factor)) {
      #   message(paste("Categorical covariates are not yet supported: ", covariate))
      #   next
      # }
      
      if(covariate %in% skip) next
      
      
      # determine data type of current covariate
      covariate_is_numeric <- covariate %in% c(names(siteCovs_numeric), names(obsCovs_numeric))
      covariate_is_factor  <- covariate %in% c(names(siteCovs_factor),  names(obsCovs_factor))
      
      # is covariate a site covariate?
      covariate_is_site_cov <- ifelse(covariate %in% colnames(siteCovs), T, F) 
      
      
      
      if(covariate_is_numeric){
        index_covariate_fixed <- grep(paste0(covariate, "$"), params_all)
        index_covariate_ranef <- grep(paste0(covariate, "["), params_all, fixed = T)
        if(length(index_covariate_fixed) != 0 & length(index_covariate_ranef != 0)) stop(paste("found both fixed and random effect of", params_covariate[covariate], ". This might be a bug"))
        
        
        if(length(index_covariate_fixed) != 0){
          df_quantiles_i  <- df_quantiles[index_covariate_fixed, ]
          df_quantiles2_i <- df_quantiles2[index_covariate_fixed, ]
          is_random <- FALSE
          
          df_quantiles_i$type  <- c("mean")
          df_quantiles2_i$type <- c("mean")
        }
        
        if(length(index_covariate_ranef) != 0){
          df_quantiles_i  <- df_quantiles[index_covariate_ranef, ]
          df_quantiles2_i <- df_quantiles2[index_covariate_ranef, ]
          is_random <- TRUE
          
          # get community mean
          index_covariate_mean_ranef <- grep(paste0(covariate, ".mean$"), params_all)
          
          df_quantiles_i  <- rbind(df_quantiles[index_covariate_mean_ranef, ], df_quantiles_i)
          df_quantiles2_i <- rbind(df_quantiles2[index_covariate_mean_ranef, ], df_quantiles2_i)
          
          df_quantiles_i$type  <- c("mean", rep("species", times = length(index_covariate_ranef)))
          df_quantiles2_i$type <- c("mean", rep("species", times = length(index_covariate_ranef)))
        }
      }
      
      
      if(covariate_is_factor){
        
        if(covariate_is_site_cov)  nlev <- nlevels(siteCovs_factor[,covariate])
        if(!covariate_is_site_cov) nlev <- length(unique(as.vector(obsCovs_factor[[covariate]])))
        
        
          
          index_covariate_fixed <- grep(paste0(keyword_submodel_short, ifelse(covariate_is_site_cov, "", ".obs"), ".fixed.categ.", covariate, "["), params_all, fixed = T)
        
          index_covariate_ranef_list <- list()
          for(factor_level in 1:nlev){ 
            index_covariate_ranef_tmp <- grep(paste0(keyword_submodel_short, ifelse(covariate_is_site_cov, "", ".obs"), ".ranef.categ.", covariate), params_all)
            #index_covariate_ranef_list[[factor_level]]     <- index_covariate_ranef_tmp[endsWith(params_all[index_covariate_ranef_tmp], paste0(",", factor_level, "]"))]
            
            # remove estimates for mean and sigma
            index_covariate_ranef <- index_covariate_ranef_tmp[!index_covariate_ranef_tmp %in% c(grep(".mean[", params_all, fixed = T), 
                                                                                                  grep(".sigma[", params_all, fixed = T))]
            
          }
          
        #if(length(index_covariate_fixed) != 0 & length(unlist(index_covariate_ranef_list) != 0)) stop(paste("found both fixed and random effect of", params_covariate[covariate], ". This might be a bug"))
          if(length(index_covariate_fixed) != 0 & length(index_covariate_ranef) != 0) stop(paste("found both fixed and random effect of", params_covariate[covariate], ". This might be a bug"))
          
          
          if(length(index_covariate_fixed) != 0){
            df_quantiles_i  <- df_quantiles[index_covariate_fixed, ]
            df_quantiles2_i <- df_quantiles2[index_covariate_fixed, ]
            is_random <- FALSE
            
            df_quantiles_i$type  <- c("mean")
            df_quantiles2_i$type <- c("mean")
          }
          
          if(length(index_covariate_ranef) != 0){
            df_quantiles_i  <- df_quantiles[index_covariate_ranef, ]
            df_quantiles2_i <- df_quantiles2[index_covariate_ranef, ]
            is_random <- TRUE
            
            # add community mean
            index_covariate_mean_ranef <- grep(paste0(covariate, ".mean["), params_all, fixed = T)   # does this affect categ fixed effects?
          
            
            df_quantiles_i  <- rbind(df_quantiles[index_covariate_mean_ranef, ], df_quantiles_i)
            df_quantiles2_i <- rbind(df_quantiles2[index_covariate_mean_ranef, ], df_quantiles2_i)
            
            
            df_quantiles_i$type  <- c(rep("mean", times = nlev), rep("species", times = length(index_covariate_ranef)))
            df_quantiles2_i$type <- c(rep("mean", times = nlev), rep("species", times = length(index_covariate_ranef)))
          }
       # }
      }
      
        
      
      type <- NULL   # just for CRAN checks
      
      colnames(df_quantiles_i)[1:3] <- c("lower_outer", "median", "upper_outer")
      colnames(df_quantiles2_i)[1:3] <- c("lower_inner", "median", "upper_inner")
      
      
      significance <- rep("no", times = nrow(df_quantiles_i))
      significance[which(df_quantiles2_i$lower_inner < 0 & df_quantiles2_i$upper_inner < 0 | 
                           df_quantiles2_i$lower_inner > 0 & df_quantiles2_i$upper_inner > 0)] <- "inner"
      significance[which(df_quantiles_i$lower_outer < 0 & df_quantiles_i$upper_outer < 0 | 
                           df_quantiles_i$lower_outer > 0 & df_quantiles_i$upper_outer > 0)] <- "outer"
      
      
      df_quantiles_i$significance  <- significance
      df_quantiles2_i$significance <- significance
      
      # assign species names
      if(!is.null(dimnames(object@data$y)[[1]])) speciesnames <- dimnames(object@data$y)[[1]]
      if( is.null(dimnames(object@data$y)[[1]])) speciesnames <- seq_len(dim(object@data$y)[1])
      
      
      if(is_random)  {
        if(covariate_is_numeric) df_quantiles_i$species <- df_quantiles2_i$species <- c("community", speciesnames)
        if(covariate_is_factor)  df_quantiles_i$species <- df_quantiles2_i$species <- c(rep("community", times = nlev), rep(speciesnames, times = nlev))
      }
      if(!is_random) df_quantiles_i$species <- df_quantiles2_i$species <- "community"
      
      
      # sort species (either by median effect size or by names)
      if(ordered) {
        if(covariate_is_factor & is_random) message("Currently coefficients of categorical covariates may not be sorted correctly. This is a known limitation.") #{
        #   
        #   df_quantiles_i_subset <- df_quantiles_i[rowSums(df_quantiles_i [,1:3]) != 0 & 
        #                                             df_quantiles_i$type == "species",]
        #   df_quantiles_i$species <- factor(df_quantiles_i$species, 
        #                                    levels = unique(df_quantiles_i_subset$species[order(df_quantiles_i_subset$median)])
        #   )
        # } else {
        #if(covariate_is_numeric){
        
        
        # this currently does not sort categorical covariates with random effects correctly. 
        # Ideally if combine = FALSE they shou
          df_quantiles_i$species <- factor(df_quantiles_i$species, 
                                           levels = unique(df_quantiles_i$species[order(df_quantiles_i$median)]))
        # }

      } else {
        df_quantiles_i$species <- factor(df_quantiles_i$species, levels = unique(rev(df_quantiles_i$species)))  
      }
      
      if(covariate_is_numeric){
        df_quantiles_i$covariate  <- covariate
        df_quantiles2_i$covariate <- covariate
      }
      
      if(covariate_is_factor){
        if(!is_random){
          
          if(covariate_is_site_cov)  df_quantiles_i$covariate  <- paste0(covariate, "_", levels(siteCovs_factor[,covariate]))
          if(!covariate_is_site_cov) df_quantiles_i$covariate  <- paste0(covariate, "_",  attr(object@data[[paste0(covariate, "_integer")]], "levels")) #unique(as.vector(obsCovs_factor[[covariate]])))
          
          if(covariate_is_site_cov)  df_quantiles2_i$covariate  <- paste0(covariate, "_", levels(siteCovs_factor[,covariate]))
          if(!covariate_is_site_cov) df_quantiles2_i$covariate  <- paste0(covariate, "_",  attr(object@data[[paste0(covariate, "_integer")]], "levels")) #unique(as.vector(obsCovs_factor[[covariate]])))
        }
        
        if(is_random){
          
          if(covariate_is_site_cov)  df_quantiles_i$covariate  <- paste0(covariate, "_", levels(siteCovs_factor[,covariate]))
          if(!covariate_is_site_cov) df_quantiles_i$covariate  <-  paste0(covariate, "_", c(attr(object@data[[paste0(covariate, "_integer")]], "levels"),
                                                                                            rep(attr(object@data[[paste0(covariate, "_integer")]], "levels"), each = object@data$M)))
              #paste0(covariate, "_",  unique(sort(as.vector(obsCovs_factor[[covariate]]))))
          
         
          
          if(covariate_is_site_cov)  df_quantiles2_i$covariate  <- paste0(covariate, "_", levels(siteCovs_factor[,covariate]))
          if(!covariate_is_site_cov) df_quantiles2_i$covariate  <-  paste0(covariate, "_", c(attr(object@data[[paste0(covariate, "_integer")]], "levels"),
                                                                                             rep(attr(object@data[[paste0(covariate, "_integer")]], "levels"), each = object@data$M)))
              #paste0(covariate, "_",  unique(as.vector(obsCovs_factor[[covariate]])))
        }
      }
      
      
      df_quantiles_list[[i]]  <- df_quantiles_i
      df_quantiles2_list[[i]] <- df_quantiles2_i
      
      
      if(!combine){
        
        p_list[[i]] <- ggplot (df_quantiles_i, aes_string(y = "species", x = "median", color = "significance")) +
          geom_vline(xintercept = 0, alpha = 0.2) +
          geom_pointrange(aes_string(xmin = "lower_outer", xmax = "upper_outer")) + 
          geom_linerange(data = df_quantiles2_i, 
                         aes_string(xmin = "lower_inner", xmax = "upper_inner"), size = 1) +
          
          facet_grid(rows = vars(type), 
                     cols = vars(covariate),
                     scales = "free_y", 
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
          ggtitle(paste("Effect sizes:", covariate))
        
        if(!covariate_is_factor) {
          p_list[[i]] <- p_list[[i]] + theme(strip.background.x = element_blank(),
                                             strip.text.x = element_blank())
        }
        
        
        
        
        if(hasArg(outdir)) {
          ggsave(filename = file.path(outdir, paste0("effect_sizes_", submodel, "_", covariate, "_", Sys.Date(),  ".png")),
                 plot = p_list[[i]],
                 ...)
        }
      }
      
    }
    
    
    if(combine){
      
      df_quantiles_all  <- do.call(rbind, df_quantiles_list)
      df_quantiles2_all <- do.call(rbind, df_quantiles2_list)
      
      p <- ggplot (df_quantiles_all, aes_string(y = "species", x = "median", color = "significance")) +
        geom_vline(xintercept = 0, alpha = 0.2) +
        geom_pointrange(aes_string(xmin = "lower_outer", xmax = "upper_outer")) + 
        geom_linerange(data = df_quantiles2_all, 
                       aes_string(xmin = "lower_inner", xmax = "upper_inner"), size = 1) +
        
        facet_grid(rows = vars(type), 
                   cols = vars(covariate),
                   scales = "free_y", 
                   space = "free_y"
        ) +
        # theme(strip.background = element_blank(),
        #       strip.text = element_blank()) +
        
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
        ggsave(filename = file.path(outdir, paste0("effect_sizes_", submodel, "_", paste(params_covariate, collapse = "_"), "_", Sys.Date(),  ".png")),
               plot = p,
               ...)
      }
    }
    
    
    if(!combine){
      
      names(p_list) <-  params_covariate
      return(p_list)
    } else {
      return(p)
    }
  }
  
  
  
  setGeneric("plot_coef", function(object, ...) standardGeneric("plot_coef"))
  
  
  #' Plot effect sizes of covariates in community occupancy model
  #' 
  #' Plot effect sizes for all species in a community (multi-species) occupancy model. Currently only supports continuous covariates, not categorical covariates.
  #'
  #' @aliases plot_coef
  #' @param object \code{commOccu} object
  #' @param mcmc.list  mcmc.list. Output of \code{\link{fit}} called on a \code{commOccu} object
  #' @param submodel  Submodel to get plots for. Can be "det" or "state"
  #' @param ordered logical. Order species in plot by median effect (TRUE) or by species name (FALSE)
  #' @param combine logical. Combine multiple plots into one (via facets)?
  #' @param outdir Directory to save plots to (optional)
  #' @param level  Probability mass to include in the uncertainty interval (two values, second value - inner interval - will be plotted thicker)
  #' @param ...  additional arguments for \code{\link[ggplot2]{ggsave}}
  #'
  #' @return list of ggplot objects
  #' @export
  #' 
  #'
  setMethod("plot_coef", signature(object = "commOccu"), 
            plot.coef.commOccu)
  
  
  
  
  get_covariate_type <- function(object, submodel){
  
    # # get site covariates
    siteCovs <- object@input$siteCovs
    
  # get observation covariates
  obsCovs <- object@input$obsCovs
  
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  
  skip <- NULL
  if(any(!is.na(cov_info_subset$ranef_cov))){
    warning(paste(cov_info_subset$covariate[!is.na(cov_info_subset$ranef_cov)], collapse = ", "), 
            " has a random effect other than species. This is currently not supported.", call. = F)
    skip <- c(skip, cov_info_subset$covariate[!is.na(cov_info_subset$ranef_cov)])
  }
  
  if(any(isTRUE(cov_info_subset$ranef_nested ))) {
    warning(paste(cov_info_subset$covariate[isTRUE(cov_info_subset$ranef_nested )]), 
            " has a nested random effect. This is currently not supported.", call. = F)
    skip <- c(skip, cov_info_subset$covariate[isTRUE(cov_info_subset$ranef_nested)])
  }
  
  
  #identify numeric / categorical site covariates
  siteCovs_numeric  <- siteCovs[, sapply(siteCovs, inherits, "numeric"), drop = FALSE]
  siteCovs_factor   <- siteCovs[, sapply(siteCovs, inherits, "factor"), drop = FALSE]
  
  
  #identify numeric / categorical observation covariates
  obsCovs_numeric  <- obsCovs[sapply(obsCovs, typeof) %in% c("double", "numeric", "integer")]
  obsCovs_factor   <- obsCovs[sapply(obsCovs, typeof) ==  "character"]
  
  
  return(list(siteCovs = siteCovs,
              siteCovs_numeric = siteCovs_numeric, 
              siteCovs_factor = siteCovs_factor, 
              obsCovs_numeric = obsCovs_numeric, 
              obsCovs_factor = obsCovs_factor, 
              skip = skip))
  
  }
  