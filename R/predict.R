# Function to create raster predictions from community occupancy model in JAGS for all species at once


# not implemented:
# - random intercepts optional
# - fixed categorical covariates (not species-specific) - function thinks they are continuous and fails
# - split methods by x = raster or x = data.frame 


setGeneric("predict", function(object, ...) {})




predictionMapsCommunity <- function(object,
                                    mcmc.list,
                                    type,
                                    draws = 1000,
                                    level = 0.95,
                                    interval = c("none", "confidence"),
                                    x,
                                    speciesSubset,
                                    categ_order = "species,factor") 
{
  
  type <- match.arg(type, choices = c("psi", "richness"))
  interval <- match.arg(interval, choices = c("none", "confidence"))
  
  # subset occupancy (beta) parameters
  submodel <- "state"
  params <- object@params[grep("^beta", object@params)]
  
  categ_order <- match.arg(categ_order, choices = c("species,factor", "factor,species"))
  
  
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  
  skip <- NULL
  if(any(!is.na(cov_info_subset$ranef_cov))){
    stop(paste(cov_info_subset$covariate[!is.na(cov_info_subset$ranef_cov)], collapse = ", "), 
            " has a random effect other than species. This is currently not supported.", call. = F)
    skip <- c(skip, cov_info_subset$covariate[!is.na(cov_info_subset$ranef_cov)])
  }
  
  if(any(isTRUE(cov_info_subset$ranef_nested ))) {
    stop(paste(cov_info_subset$covariate[isTRUE(cov_info_subset$ranef_nested )], collapse = ", "), 
            " has a nested random effect. This is currently not supported.", call. = F)
    skip <- c(skip, cov_info_subset$covariate[isTRUE(cov_info_subset$ranef_nested)])
  }
  
  
  
  
  
  
  # subset posterior matrix
  posterior_matrix <- as.matrix(mcmc.list)
  if(hasArg(draws)) {
    posterior_matrix <- posterior_matrix[sample(1:nrow(posterior_matrix), draws),]
  } 
  posterior_matrix <- posterior_matrix[,grep("^beta", colnames(posterior_matrix)), drop = FALSE]
  
  if(nrow(posterior_matrix) > 1000) message("More than 1000 posterior samples. Watch RAM usage")
  
  
  # get covariate that beta parameters refer to
  params_covariate <- sapply(strsplit(params, split = ".", fixed = TRUE), FUN = function (x) x[length(x)])
  params_covariate <- params_covariate[params_covariate %in% names(object@data)]
  
  
  
  list_responses <- list()
  
  # convert raster to covariate data frame
  if(class(x) == "RasterStack") {
    values_to_predict_all <- as.data.frame(raster::values(x))
  } else {
    stop("x must be a data.frame")
  }
  # if(is.data.frame(x)){
  #   #values_to_predict_all <- x
  #   stop()
  # }
  
  
  index_not_na <- which(apply(values_to_predict_all, 1, FUN = function(x) all(!is.na(x))))
  
  values_to_predict <- values_to_predict_all[index_not_na, , drop = F]
  
  
  # create array for intercepts
  array_NA <- array(data = NA, dim = c(nrow(values_to_predict),    # raster cell
                                       object@data$M,    # species
                                       nrow(posterior_matrix)))    # posterior sample
  
  
  # get intercepts
  out_beta0 <- array_NA
  for(i in 1:dim(array_NA)[2]){    # species loop
    out_beta0[,i,] <- posterior_matrix[, grep(paste0("beta0[", i, "]"), colnames(posterior_matrix), fixed = TRUE)]
  }
  gc()
  
  out <- list()
  # loop over covariates
  for(covariate in 1:length(params_covariate)) {
    out[[covariate]] <- array_NA
    
    
    # for continuous covariates (which have only a species index)
    if(length(grep(paste0(params_covariate[covariate], "[", 1, "]"), colnames(posterior_matrix), fixed = TRUE)) == 1) {
      for(i in 1:dim(out[[covariate]] )[2]){    # species loop
        out[[covariate]][,i,] <-  sapply(posterior_matrix[, grep(paste0(params_covariate[covariate], "[", i, "]"), colnames(posterior_matrix), fixed = TRUE)], FUN = function(x){
          x * values_to_predict [, params_covariate[covariate]]
        })
      }
    }
    
    # for categorical covariates (which have two indices: species and factor level - factor levels potentially differ by each cell)
    # below expects parameters to be in order: species - factor level
    
    if(length(grep(paste0(params_covariate[covariate], "[", 1, "]"), colnames(posterior_matrix), fixed = TRUE)) == 0) {
      
      unique_values <- unique(values_to_predict[, params_covariate[covariate]])
      which_cell <- sapply(unique_values, FUN = function(x) which(values_to_predict[, params_covariate[covariate]] == x))
      
      for(factor_level in 1:length(unique_values)) {
        
        for(i in 1:dim(out[[covariate]] )[2]){  # species loop
          
          # make matrix of posterior estimates
          if(categ_order == "species,factor"){
            matrix_tmp <- matrix(sapply(posterior_matrix[, grep(paste0(params_covariate[covariate], "[", i, ",", factor_level, "]"), colnames(posterior_matrix), fixed = TRUE)],
                                        FUN = function(x){x}), ncol = dim(out[[covariate]])[3], nrow = length(which_cell[[factor_level]]), byrow = T)
          }
          if(categ_order == "factor,species"){
            matrix_tmp <- matrix(sapply(posterior_matrix[, grep(paste0(params_covariate[covariate], "[", factor_level, ",", i, "]"), colnames(posterior_matrix), fixed = TRUE)],
                                        FUN = function(x){x}), ncol = dim(out[[covariate]])[3], nrow = length(which_cell[[factor_level]]), byrow = T)
          }
          
          out[[covariate]][which_cell[[factor_level]],i,] <-  matrix_tmp
        }
      }
    }
  }
  
  # sum up individual effects
  logit.psi <- Reduce('+', out) + out_beta0
  psi <- exp(logit.psi) / (exp(logit.psi) + 1)
  
  rm(array_NA, out_beta0, out, logit.psi)
  gc()
  
  
  if(type == "psi") {
    
    if(hasArg(speciesSubset)) warning("speciesSubset is defined, but has no effect when type = 'psi'")
    
    # summarize estimates (across posterior samples)
    psi.mean <- apply(psi, MARGIN = c(1,2), mean)
    psi.sd <- apply(psi, MARGIN = c(1,2), sd)
    
    # make data frame for ggplot
    psi.mean.melt <- reshape2::melt(psi.mean)
    psi.sd.melt <- reshape2::melt(psi.sd)
    
    names(psi.mean.melt) <- c("cell_nr", "Species", "mean")
    names(psi.sd.melt) <- c("cell_nr", "Species", "sd")
    
    if(!is.null(dimnames(object@data$y)[[1]])) {
      psi.mean.melt$Species <- dimnames(object@data$y)[[1]][psi.mean.melt$Species]
      psi.sd.melt$Species <- dimnames(object@data$y)[[1]][psi.sd.melt$Species]
    }
    
    #if(class(x) == "RasterStack") {
    raster_template <- raster::raster(x)
    r_pred_species <- r_pred_sd_species <- list()
    
    for(i in 1:length(unique(psi.mean.melt$Species))) {
      r_pred_species[[i]]    <- raster_template
      r_pred_sd_species[[i]] <- raster_template
      
      raster::values(r_pred_species[[i]]) [index_not_na] <- psi.mean.melt$mean[psi.mean.melt$Species == unique(psi.mean.melt$Species)[i]]
      raster::values(r_pred_sd_species[[i]]) [index_not_na] <- psi.sd.melt$sd[psi.sd.melt$Species == unique(psi.sd.melt$Species)[i]]
    }
    names(r_pred_species) <- names(r_pred_sd_species) <- unique(psi.mean.melt$Species)
    
    stack_out_mean <- raster::stack(r_pred_species)
    stack_out_sd   <- raster::stack(r_pred_sd_species)
    #}
    

    
    if(interval == "confidence"){
      psi.lower <- apply(psi, MARGIN = c(1,2), quantile, ((1-level) / 2))                        # SLOW!!!
      psi.upper <- apply(psi, MARGIN = c(1,2), quantile, (1 - (1-level) / 2))
      
      psi.lower2 <- reshape2::melt(psi.lower)
      psi.upper2 <- reshape2::melt(psi.upper)
      
      names(psi.lower2) <- c("cell_nr", "Species", paste0("lower.ci.", level))
      names(psi.upper2) <- c("cell_nr", "Species", paste0("upper.ci.", level))
      
      if(!is.null(dimnames(object@data$y)[[1]])) {
        psi.lower2$Species <- dimnames(object@data$y)[[1]][psi.lower2$Species]
        psi.upper2$Species <- dimnames(object@data$y)[[1]][psi.upper2$Species]
      }
      
      raster_template <- raster::raster(x)
      r_pred_lower_species <- r_pred_upper_species <- list()
      
      for(i in 1:length(unique(psi.mean.melt$Species))) {
        r_pred_lower_species[[i]] <- raster_template
        r_pred_upper_species[[i]] <- raster_template
        
        raster::values(r_pred_lower_species[[i]]) [index_not_na] <- psi.lower2[psi.lower2$Species == unique(psi.lower2$Species)[i], paste0("lower.ci.", level)]
        raster::values(r_pred_upper_species[[i]]) [index_not_na] <- psi.upper2[psi.upper2$Species == unique(psi.upper2$Species)[i], paste0("upper.ci.", level)]
      }
      names(r_pred_lower_species) <- names(r_pred_upper_species) <- unique(psi.mean.melt$Species)
      
      stack_out_lower   <- raster::stack(r_pred_lower_species)
      stack_out_upper   <- raster::stack(r_pred_upper_species)
      
      return(list(mean  = stack_out_mean,
                  sd    = stack_out_sd,
                  lower = stack_out_lower,
                  upper = stack_out_upper))
    }
    
    if(interval == "none"){
      return(list(mean = stack_out_mean,
                  sd = stack_out_sd))
    }
  }
  
  if(type == "richness") {
    
    # generate occupancy status at each cell / species / posterior sample as random binomial trial (returns vector)
    psi_bin <- rbinom(length(psi), size = 1, prob = psi)
    
    # convert back to array
    psi_bin_array <- array(psi_bin, dim = dim(psi), dimnames = dimnames(psi))
    
    
    # subset richness estimate to certain species, if requested
    if(!hasArg(speciesSubset)) speciesSubset <- 1:dim(psi_bin_array)[2]
    
    if(is.character(speciesSubset)) {
      if(!is.null(dimnames(object@data$y)[[1]])) {
        if(any(!speciesSubset %in% dimnames(object@data$y)[[1]])) {
          stop(paste(paste(speciesSubset[!speciesSubset %in% dimnames(object@data$y)[[1]]], collapse = ", "),
                     "not found in the species names of object@data$y"))
        }
        speciesIndex <- which(dimnames(object@data$y)[[1]] %in% speciesSubset)
      } else {
        stop("speciesSubset is character, but object@data$y does not contain species names")
      }
    }
    
    if(is.numeric(speciesSubset)) speciesIndex <- speciesSubset
    
    # species richness at each pixel for each posterior sample
    psi.bin.sum <- apply(psi_bin_array[,speciesIndex,, drop = FALSE], MARGIN = c(1,3), sum)
    
    
    # Mean of richness estimate
    psi.bin.sum.mean <- apply(psi.bin.sum, 1, mean)
    # SD of richness estimate
    psi.bin.sum.sd <- apply(psi.bin.sum, 1, sd)
    
    # confidence intervals of richness estimate
    if(interval == "confidence"){
      psi.bin.sum.lower <- apply(psi.bin.sum, 1, quantile, ((1-level) / 2))
      psi.bin.sum.upper <- apply(psi.bin.sum, 1, quantile, (1 - (1-level) / 2))
    }
    raster_template <- raster::raster(x)
    
    r.psi.bin.sum.mean <- r.psi.bin.sum.sd <- r.psi.bin.sum.lower <- r.psi.bin.sum.upper <- raster_template
    raster::values(r.psi.bin.sum.mean) [index_not_na] <- psi.bin.sum.mean
    raster::values(r.psi.bin.sum.sd) [index_not_na] <- psi.bin.sum.sd
    
    if(interval == "none"){
      stack_out <- raster::stack(list(mean = r.psi.bin.sum.mean,
                                      sd = r.psi.bin.sum.sd))
    }
    
    if(interval == "confidence"){
      raster::values(r.psi.bin.sum.lower) [index_not_na] <- psi.bin.sum.lower
      raster::values(r.psi.bin.sum.upper) [index_not_na] <- psi.bin.sum.upper
      
      stack_out <- raster::stack(list(mean  = r.psi.bin.sum.mean,
                                      sd    = r.psi.bin.sum.sd,
                                      lower = r.psi.bin.sum.lower,
                                      upper = r.psi.bin.sum.upper))
    }
    
    return(stack_out)
  }
}




#' Spatial predictions from community occupancy models
#' 
#' Create spatial predictions of species occupancy and species richness from community occupancy models and raster stacks.
#'
#' @param object \code{commOccu} object
#' @param mcmc.list  mcmc.list. Output of \code{\link{fit}} called on a \code{commOccu} object
#' @param type character. "psi" for species occupancy estimates, "richness" for species richness estimates
#' @param draws  Number of draws from the posterior to use when generating the plots. If fewer than draws are available, they are all used
#' @param level  Probability mass to include in the uncertainty interval
#' @param interval    # Type of interval calculation. Can be "none" or "confidence". Can be slow for type = psi with many cells and posterior samples. Can be abbreviated.
#' @param x   raster stack. Must be scaled with same parameters as site covariates used in model, and have same names
#' @param speciesSubset  species to include in richness estimates. Can be index number or species names.
#' @param categ_order a temporary argument. Ignore
#'
#' @return A raster stack
#' @importFrom  stats rbinom sd
#' @export
#'

setMethod("predict", signature = c(object = "commOccu"),
          predictionMapsCommunity)
