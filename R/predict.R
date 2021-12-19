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
                                    speciesSubset) 
{
  
  type <- match.arg(type, choices = c("psi", "richness"))
  interval <- match.arg(interval, choices = c("none", "confidence"))

  # subset occupancy (beta) parameters
  submodel <- "state"
  
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
  
  if(nrow(posterior_matrix) > 1000) message("More than 1000 posterior samples. Watch RAM usage")
  
  params_covariate <- cov_info_subset$covariate
  if(length(params_covariate) == 0) stop ("No covariates found", call. = F)
  
  
  list_responses <- list()
  
  
  # convert raster to covariate data frame
  if(class(x) == "RasterStack") {
    values_to_predict_all <- as.data.frame(raster::values(x))
  } else {
    if(is.data.frame(x)) {
      values_to_predict_all <- x
    } else {
      stop("x must be a data.frame")
    }
  }

  
  # identify cells with values
  index_not_na <- which(apply(values_to_predict_all, 1, FUN = function(x) all(!is.na(x))))
  
  values_to_predict_subset <- values_to_predict_all[index_not_na, , drop = F]
  
  
  # create array for intercepts
  array_NA <- array(data = NA, dim = c(nrow(values_to_predict_subset),    # raster cell
                                       object@data$M,                     # species
                                       nrow(posterior_matrix)))           # posterior sample
  
  
  # # get intercepts
  out_beta0 <- array_NA
  
  for(i in 1:dim(array_NA)[2]){    # species loop
    if(!paste0(keyword_submodel_short, "0.mean") %in% object@params) {
      # fixed intercept
      out_beta0[,i,] <- posterior_matrix[, grepl(paste0(keyword_submodel_short, "0$"), colnames(posterior_matrix))] 
    } else {
      # random intercept
      out_beta0[,i,] <- posterior_matrix[, colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] 
    }
  }
  gc()
  
  # check if this works
  if(object.size(out_beta0) / 1e6 * (nrow(cov_info_subset) + 1) > 4000 ) message("Watch RAM usage. At least 4Gb will be required")
  
  
  out <- list()
  # loop over covariates 
  for(cov in 1:nrow(cov_info_subset)) {
    
    current_cov  <- cov_info_subset$covariate[cov]
    current_coef <- cov_info_subset$coef[cov]
    
    if(!is.na(cov_info_subset$ranef_cov[cov])){
      stop(paste(current_cov, 
                    " has a random effect other than species. This is currently not supported. Predictions would be wrong", call. = F))
      next
    }
    
    if(cov_info_subset$ranef_nested[cov])  {
      stop(paste(current_cov, 
                    " has a nested random effect. This is currently not supported. Predictions would be wrong", call. = F))
      next
    }
    
    out[[cov]] <- array_NA
    
    # determine data type of current covariate
    covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
    covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
    
    
    covariate_is_fixed <- !cov_info_subset$ranef[cov]
    covariate_is_ranef <- cov_info_subset$ranef[cov]
    
    covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
    
    
    # species loop
    for(i in 1:dim(out[[cov]])[2]){
      
      if(covariate_is_numeric) {
        
        if(covariate_is_fixed)  index_covariate <- grep(paste0(current_coef, "$"), colnames(posterior_matrix))
        if(covariate_is_ranef)  index_covariate <- grep(paste0(current_coef, "[", i, "]"), colnames(posterior_matrix), fixed = T)
        
        
        out[[cov]][,i,] <-  sapply(posterior_matrix[, index_covariate], FUN = function(x){
          x * values_to_predict_subset[, current_cov]
        })
      }
      
      if(covariate_is_factor) {

        if(covariate_is_fixed) index_covariate <- grep(current_coef, colnames(posterior_matrix))
        if(covariate_is_ranef) index_covariate <- grep(paste0(current_coef, "[", i, ","), colnames(posterior_matrix), fixed = T)

        # this assumes that the numeric values in the raster correspond to the factor levels in the covariate
        # since it uses the raster values to index the posterior matrix
        
        # for data.frames, it seems to work with the categorical column being integer (= factor level, as in as.data.frame(rasterStack)), or proper factor
        out[[cov]][,i,] <- posterior_matrix[, index_covariate[values_to_predict_subset[, current_cov]]]
        
      }
      suppressWarnings(rm(index_covariate))
    }   # end species loop
  }    # end covariate loop
  
  # sum up individual effects
  logit.psi <- Reduce('+', out) + out_beta0
  psi <- exp(logit.psi) / (exp(logit.psi) + 1)
  
  rm(array_NA, out_beta0, out, logit.psi)
  gc()
  
  
  if(type == "psi") {
    
    if(hasArg(speciesSubset)) warning("speciesSubset is defined, but has no effect when type = 'psi'")
    
    # summarize estimates (across posterior samples)
    psi.mean <- apply(psi, MARGIN = c(1,2), mean)
    psi.sd   <- apply(psi, MARGIN = c(1,2), sd)
    
    # make data frame for ggplot
    psi.mean.melt <- reshape2::melt(psi.mean)
    psi.sd.melt   <- reshape2::melt(psi.sd)
    
    names(psi.mean.melt) <- c("cell_nr", "Species", "mean")
    names(psi.sd.melt)   <- c("cell_nr", "Species", "sd")
    
    if(!is.null(dimnames(object@data$y)[[1]])) {
      psi.mean.melt$Species <- dimnames(object@data$y)[[1]][psi.mean.melt$Species]
      psi.sd.melt$Species   <- dimnames(object@data$y)[[1]][psi.sd.melt$Species]
    }
    
    # fill rasters with predicted values
    if(class(x) == "RasterStack") {
    raster_template <- raster::raster(x)
    r_pred_species <- r_pred_sd_species <- list()
    
    for(i in 1:length(unique(psi.mean.melt$Species))) {
      r_pred_species[[i]]    <- raster_template
      r_pred_sd_species[[i]] <- raster_template
      
      raster::values(r_pred_species[[i]]) [index_not_na]    <- psi.mean.melt$mean[psi.mean.melt$Species == unique(psi.mean.melt$Species)[i]]
      raster::values(r_pred_sd_species[[i]]) [index_not_na] <- psi.sd.melt$sd[psi.sd.melt$Species == unique(psi.sd.melt$Species)[i]]
    }
    names(r_pred_species) <- names(r_pred_sd_species) <- unique(psi.mean.melt$Species)
    
    stack_out_mean <- raster::stack(r_pred_species)
    stack_out_sd   <- raster::stack(r_pred_sd_species)
    }

    
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
      
      if(class(x) == "RasterStack") {
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
      } else {
        
        return(data.frame(psi.mean.melt, 
                          sd = psi.sd.melt$sd, 
                          lower = psi.lower2[, 3], 
                          upper = psi.upper2 [,3]))
        
      }
    }
    
    if(interval == "none"){
      if(class(x) == "RasterStack"){
        return(list(mean = stack_out_mean,
                    sd = psi.sd.melt))
      } else {
        return(data.frame(psi.mean.melt, 
                          sd = psi.sd.melt$sd))
      }
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
    psi.bin.sum <- apply(psi_bin_array[, speciesIndex,, drop = FALSE], MARGIN = c(1,3), sum)
    
    
    # Mean of richness estimate
    psi.bin.sum.mean <- apply(psi.bin.sum, 1, mean)
    # SD of richness estimate
    psi.bin.sum.sd <- apply(psi.bin.sum, 1, sd)
    
    # confidence intervals of richness estimate
    if(interval == "confidence"){
      psi.bin.sum.lower <- apply(psi.bin.sum, 1, quantile, ((1-level) / 2))
      psi.bin.sum.upper <- apply(psi.bin.sum, 1, quantile, (1 - (1-level) / 2))
    }
    
    if(class(x) == "RasterStack") {
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
      
    } else {
      if(interval == "none"){
        df_out <- data.frame(#x,
                             mean = psi.bin.sum.mean,
                             sd   = psi.bin.sum.sd)
      }
      
      if(interval == "confidence"){
        df_out <- data.frame(#x, 
                             mean  = psi.bin.sum.mean,
                             sd    = psi.bin.sum.sd, 
                             lower = psi.bin.sum.lower,
                             upper = psi.bin.sum.upper)
      }
      
      return(df_out)
      
    }
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
#' @param interval    # Type of interval calculation. Can be "none" or "confidence". Can be slow for type = "psi" with many cells and posterior samples. Can be abbreviated.
#' @param x   raster stack or data.frame. Must be scaled with same parameters as site covariates used in model, and have same names. 
#' @param speciesSubset  species to include in richness estimates. Can be index number or species names.
#'
#' @return A raster stack or data.frame, depending on x
#' @importFrom  stats rbinom sd
#' @importFrom utils object.size
#' @export
#'

setMethod("predict", signature = c(object = "commOccu"),
          predictionMapsCommunity)
