
plot.effects.commOccu <- function(object,       # commOccu object
                                   mcmc.list,    # mcmc.list (output of fit())
                                   submodel = "state",      # "det" or "state"
                                   draws = 1000,    # number of posterior samples to use (will draw random sample from posterior distribution if defined). 
                                   outdir,       # directory to save plots in (optional)
                                   level = 0.95,   # confidence level for CIs in plot
                                   keyword_squared = "_squared",   # the suffix of a covariate that indicates a quadratic effect  (e.g. "elevation" and "elevation_squared" -> will be combined in plot)
                                   ...)                            # additional arguments for ggsave()
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
  
  # get covariate information for submodel
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  
  if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  

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
      if(gsub(keyword_squared, "", current_cov) %in% params_covariate) next
    }
    

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
    if(paste0(current_cov, keyword_squared) %in% params_covariate){
      has_squared <- TRUE
      squared_cov <- paste0(current_cov, keyword_squared)
    } else {
      has_squared <- FALSE
    }
    
    # determine data type of current covariate
    covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
    covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
    
    
    covariate_is_fixed <- !cov_info_subset$ranef[cov]
    covariate_is_ranef <- cov_info_subset$ranef[cov]
    
    covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
    
    
    # create values to predict to
    if(covariate_is_factor){
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
      
      # get intercepts
      if(!paste0(keyword_submodel_short, "0.mean") %in%  object@params) {
        
        # fixed intercept
        out_intercept[,i,] <- posterior_matrix[, grepl(paste0(keyword_submodel_short, "0$"), colnames(posterior_matrix))] 
      } else {
        
        # random intercept
        out_intercept[,i,] <- posterior_matrix[, colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] 
      }
      
      
      if(covariate_is_numeric) {
        
        if(covariate_is_fixed)  index_covariate <- grep(paste0(current_coef, "$"), colnames(posterior_matrix))
        if(covariate_is_ranef)  index_covariate <- grep(paste0(current_coef, "[", i, "]"), colnames(posterior_matrix), fixed = T)
        
        
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
        
        if(covariate_is_fixed) index_covariate <- grep(cov_info_subset$coef[cov], colnames(posterior_matrix))
        if(covariate_is_ranef) index_covariate <- grep(paste0(cov_info_subset$coef[cov], "[", i, ","), colnames(posterior_matrix), fixed = T)
        
        
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
    colnames(probs) [1] <- current_cov
    
    
    # assign species names (if available)
    if(!is.null(dimnames(object@data$y)[[1]])) {
      probs$Species <- dimnames(object@data$y)[[1]][probs$Species]
    }
    
    probs <- probs[order(probs$Species, probs[, 1]),]
    
    
    if(submodel == "det")   ylabel <- "Detection probability p"
    if(submodel == "state") ylabel <- expression(paste("Occupancy probability  ", psi))
    
    main <- paste0(ifelse(covariate_is_site_cov, "Site", "Observation"), " covariate: ", current_cov)
    
    
    subtitle <- paste0(ifelse(covariate_is_ranef, "Random effect", "Fixed effect"),
                       ifelse(has_squared, " (with quadratic term)", ""),
                       ifelse(is_squared, " quadratic term (no linear term)", ""))
    
    
    # make cran checks happy
    lower <- NULL
    upper <- NULL
    
    # for squared covariates which have no unsquared version, sqrt-transform covariate and add expand covariate range to negative values (by mirr)
    if(is_squared & !has_squared) {
      probs[,1] <- sqrt(probs[,1])
      
      probs2 <- probs
      probs2[,1] <- -probs2[,1]
      probs <- rbind(probs, probs2)
      
    }
    
    
    
    # plot
    
    combine <- FALSE
    
    if(covariate_is_numeric){
      
      p <- ggplot(probs, aes_string(x = params_covariate[[cov]], y = "mean", group = "Species")) + 
        geom_line() +
        theme_bw() +
        ggtitle(label = main,
                subtitle = subtitle) +
        xlab (ifelse(is_squared, gsub(keyword_squared, "", current_cov), current_cov)) +
        ylab(ylabel) +
        xlim(range(probs[, 1])) +
        ylim(0, 1) +
        theme(panel.grid.minor = element_blank())
      
      
      # note for later, can optionally plot all species in  one plot if combine = TRUE (= ggplot code to this point)
      if(!combine){
        p <- p + facet_wrap(~Species) +
          geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = 0.2)
      }
    }
    
    
    if(covariate_is_factor){
      
      # create x axis labels for factors
      if(covariate_is_site_cov){
        probs[,1] <- levels(object@data[[current_cov]]) [probs[,1]]
      } 
      
      p <- ggplot(probs, aes_string(x = params_covariate[[cov]], y = "mean", group = "Species")) + 
        geom_col() +
        facet_wrap(~Species) +
        geom_linerange(aes_string(ymin = "lower", ymax = "upper")) +
        theme_bw() +
        ggtitle(label = main,
                subtitle = subtitle) +
        xlab (current_cov) +
        ylab(ylabel) +
        ylim(0, 1) +
        theme(panel.grid.minor = element_blank())
      
      # don't know yet how to combine species on one plot. 
      
    }
    
    
    if(hasArg(outdir)) {
      ggsave(filename = file.path(outdir, paste0("response_curves_", current_cov, "_", Sys.Date(),  ".png")),
             plot = p,
             ...)
    }
    
    list_responses [[cov]] <- p
    
    
    attr(params_covariate, "include") [cov] <- TRUE
    
    # reset has_squared
    has_squared <- FALSE
    has_squared_cov <- NULL
    is_squared <- NULL
    
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
  
  
  
  

plot.coef.commOccu <- function(object, 
                               mcmc.list,
                               submodel = "state",
                               ordered = TRUE,
                               combine = FALSE,
                               outdir,
                               level = c(outer = 0.95, inner = 0.75),
                               ...) {
  
  
  submodel <- match.arg(submodel, choices = c("det", "state"))
  
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
    
    
    covariate_is_fixed <- !cov_info_subset$ranef[cov]
    covariate_is_ranef <- cov_info_subset$ranef[cov]
    
    covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
    
    
    
    if(covariate_is_numeric){
      
      if(covariate_is_fixed)  index_covariate <- grep(paste0(current_coef, "$"), rownames(df_quantiles))
      if(covariate_is_ranef)  index_covariate <- grep(paste0(current_coef, "[" ), rownames(df_quantiles), fixed = T)
      
      df_quantiles_i  <- df_quantiles[index_covariate, ]
      
      if(covariate_is_fixed)   df_quantiles_i$type  <- c("mean")
      if(covariate_is_ranef) {
        # get community mean
        index_covariate_mean_ranef <- grep(paste0(current_coef, ".mean$"), rownames(df_quantiles))
        
        df_quantiles_i  <- rbind(df_quantiles[index_covariate_mean_ranef, ], df_quantiles_i)
        
        df_quantiles_i$type  <- c("mean", rep("species", times = length(index_covariate)))
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
    
    
    if(covariate_is_fixed)  index_covariate <- grep(paste0(current_coef, "["),  rownames(df_quantiles), fixed = T)
    if(covariate_is_ranef)  index_covariate <- grep(paste0(current_coef, "[" ), rownames(df_quantiles), fixed = T)
    
    
    df_quantiles_i  <- df_quantiles[index_covariate, ]  
    
    if(covariate_is_fixed) df_quantiles_i$type  <- c("mean")
    
    if(covariate_is_ranef) {
      
      # add community mean
      index_covariate_mean_ranef <- grep(paste0(current_coef, ".mean"), rownames(df_quantiles), fixed = T)   # does this affect categ fixed effects?
      
      
      df_quantiles_i  <- rbind(df_quantiles[index_covariate_mean_ranef, ], df_quantiles_i)
      
      
      df_quantiles_i$type  <- c(rep("mean", times = length(index_covariate_mean_ranef)), rep("species", times = length(index_covariate)))
    }
    }
    
    
    
    
    type <- NULL   # just for CRAN checks
    
    colnames(df_quantiles_i)[1:5] <- c("lower_outer", "lower_inner", "median", "upper_inner", "upper_outer")
    
    
    # get significance levels
    significance <- rep("no", times = nrow(df_quantiles_i))
    significance[which(df_quantiles_i$lower_inner < 0 & df_quantiles_i$upper_inner < 0 | 
                         df_quantiles_i$lower_inner > 0 & df_quantiles_i$upper_inner > 0)] <- "inner"
    significance[which(df_quantiles_i$lower_outer < 0 & df_quantiles_i$upper_outer < 0 | 
                         df_quantiles_i$lower_outer > 0 & df_quantiles_i$upper_outer > 0)] <- "outer"
    
    
    df_quantiles_i$significance  <- significance
    
    # assign species names
    if(!is.null(dimnames(object@data$y)[[1]])) speciesnames <- dimnames(object@data$y)[[1]]
    if( is.null(dimnames(object@data$y)[[1]])) speciesnames <- seq_len(dim(object@data$y)[1])
    
    
    if(covariate_is_ranef)  {
      if(covariate_is_numeric) df_quantiles_i$species <- c("community", speciesnames)
      if(covariate_is_factor) {
        #if(covariate_is_site_cov){
        df_quantiles_i$species <- c(rep("community", times = length(index_covariate_mean_ranef)), rep(speciesnames, times = nlev))
          #df_quantiles_i$species <- c(rep("community", times = nlev), rep(speciesnames, times = nlev))
        #} else {
        #  df_quantiles_i$species <- c(rep("community", times = nlev - 1), rep(speciesnames, times = nlev - 1))
        #}
      }
    }
    
    if(covariate_is_fixed) df_quantiles_i$species  <- "community"
    
    
    
    # add covariate name as column
    if(covariate_is_numeric){
      df_quantiles_i$covariate  <- current_cov
    }
    
    if(covariate_is_factor){
      if(covariate_is_fixed){
        
        if(covariate_is_site_cov)  df_quantiles_i$covariate  <- paste0(current_cov, "_", levels_tmp)
        if(!covariate_is_site_cov) df_quantiles_i$covariate  <- paste0(current_cov, "_",  levels_tmp)
        
      }
      
      if(covariate_is_ranef){
        
        if(covariate_is_site_cov)  df_quantiles_i$covariate  <- paste0(current_cov, "_", c(levels_tmp,
                                                                                           rep(levels_tmp, each = object@data$M)))
        if(!covariate_is_site_cov) df_quantiles_i$covariate  <- paste0(current_cov, "_", c(levels_tmp,
                                                                                           rep(levels_tmp, each = object@data$M)))
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
        if(covariate_is_fixed) {
          df_quantiles_i$species <- factor(df_quantiles_i$species, 
                                           levels = unique(df_quantiles_i$species[order(df_quantiles_i$median)]))  
        }
        
        if(covariate_is_ranef){
          subset_level2 <- df_quantiles_i[df_quantiles_i$covariate == paste0(current_cov, "_", levels_tmp[2]),]
          
          df_quantiles_i$species <- factor(df_quantiles_i$species, 
                                           levels = subset_level2$species[order(subset_level2$median)])
        }
        
      }
      
    } else {
      df_quantiles_i$species <- factor(df_quantiles_i$species, levels = unique(rev(df_quantiles_i$species)))  
    }
    
    df_quantiles_list[[cov]]  <- df_quantiles_i
    
    
    # plot
    
    if(!combine){
      
      p_list[[cov]] <- ggplot (df_quantiles_i, aes_string(y = "species", x = "median", color = "significance")) +
        geom_vline(xintercept = 0, alpha = 0.2) +
        geom_pointrange(aes_string(xmin = "lower_outer", xmax = "upper_outer")) + 
        geom_linerange( aes_string(xmin = "lower_inner", xmax = "upper_inner"), size = 1) +
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
        ggtitle(paste("Effect sizes:", current_cov))
      
      
      if(!covariate_is_factor) {
        p_list[[cov]] <- p_list[[cov]] + theme(strip.background.x = element_blank(),
                                               strip.text.x = element_blank())
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
    
    p <- ggplot (df_quantiles_all, aes_string(y = "species", x = "median", color = "significance")) +
      geom_vline(xintercept = 0, alpha = 0.2) +
      geom_pointrange(aes_string(xmin = "lower_outer", xmax = "upper_outer")) + 
      geom_linerange (aes_string(xmin = "lower_inner", xmax = "upper_inner"), size = 1) +
      facet_grid(rows = vars(type), 
                 cols = vars(covariate),
                 scales = "free_y", 
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
      ggsave(filename = file.path(outdir, paste0("effect_sizes_", submodel, "_", paste(cov_info_subset$covariate, collapse = "_"), "_", Sys.Date(),  ".png")),
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
  
  