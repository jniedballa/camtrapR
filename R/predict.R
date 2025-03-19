# Function to create raster predictions from community occupancy model in JAGS for all species at once


# not implemented:
# - nested  / other random effects
# - allow for alpha if it only has site covariates?


setGeneric("predict", function(object, ...) {})




predictionMapsCommunity <- function(object,
                                    mcmc.list,
                                    type,
                                    draws = 1000,
                                    level = 0.95,
                                    interval = c("none", "confidence"),
                                    x = NULL,
                                    aoi = NULL,
                                    speciesSubset,
                                    batch = FALSE,
                                    seed = NULL) 
{
  
  # type <- match.arg(type, choices = c("psi_array", "psi", "richness", "pao"))
  type <- match.arg(type, choices = c("psi_array", "psi", "richness", "pao", "abundance", "lambda_array",
                                      "p_array"))
  interval <- match.arg(interval, choices = c("none", "confidence"))

  
  if(.hasSlot(object, "model")) {
    if(object@model != "RN"){
      if(type %in% c("abundance", "lambda_array")) stop(paste0("type = '", type, "' is only implemented in Royle-Nichols models. The current model is a standard occupancy model"))
    }
  }
  
  # subset occupancy (beta) parameters
  if(type == "p_array") {
    submodel <- "det"
  } else {
    submodel <- "state"
  }
  
  submodel <- match.arg(submodel, choices = c("det", "state"))
  
  if(submodel == "state") {
    keyword_submodel <- "^beta"
    keyword_submodel_short <- "beta"
  }
  if(submodel == "det") {
    keyword_submodel <- "^alpha"
    keyword_submodel_short <- "alpha"
  } 
  

  # get intercept information for submodel
  cov_info_intercept <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "intercept",]
  
  # get covariate information for submodel
  cov_info_subset <- object@covariate_info[object@covariate_info$submodel == submodel & object@covariate_info$param == "param",]
  
  
  # if(nrow(cov_info_subset) == 0) stop(paste("No covariates in submodel", submodel), call. = F)
  
  

  # subset parameters of submodel
  stopifnot(all(cov_info_subset$coef %in% object@params))
  params_submodel <- object@params[grep(keyword_submodel, object@params)]
  
  
  if(!is.null(seed)){
    stopifnot(is.numeric(seed))
    set.seed(seed)
  }
  
  # define function for inverse logit (logit to probability)
  ilogit <- function(x) 1 / (1 + exp(-x))
  
  
  # subset posterior matrix to number of draws
  posterior_matrix <- as.matrix(mcmc.list)
  if(hasArg(draws)) {
    if(nrow(posterior_matrix) > draws){
      selected_rows <- sort(sample(1:nrow(posterior_matrix), draws))
      # print(selected_rows)
      posterior_matrix <- posterior_matrix[selected_rows, , drop = FALSE]
    } else {
      message(paste0("draws (", draws, ") is greater than the number of available samples. Using all samples (", nrow(posterior_matrix), ")."))
    }
  } 
  
  # subset posterior matrix to current submodel
  posterior_matrix <- posterior_matrix[, grep(keyword_submodel, colnames(posterior_matrix)), drop = F]
  
  if(nrow(posterior_matrix) > 1000) message("More than 1000 posterior samples. Watch RAM usage")
  
  params_covariate <- cov_info_subset$covariate
  # if(length(params_covariate) == 0) stop ("No covariates found", call. = F)
  
  
  list_responses <- list()
  
  
  # if x is not defined, use the site covariates from the model input
  if(is.null(x)) x <- object@input$siteCovs
  
  # convert raster to covariate data frame
  if(inherits(x, c("SpatRaster", "RasterStack"))) {
    if(inherits(x, "RasterStack")){
    warning("x is a RasterStack. Please use SpatRaster (from the terra package) in the future. Convert via: 
rast(YourRaster)")
    }

    raster_template <- terra::rast(x, nlyrs = 1, vals = NA)
    
    values_to_predict_all <- as.data.frame(terra::values(x))
  } else {
    if(is.data.frame(x)) {
      values_to_predict_all <- x
    } else {
      stop("x must be a data.frame")
    }
  }

  
  # identify cells with values
  index_not_na <- which(apply(values_to_predict_all[, cov_info_subset$covariate[cov_info_subset$covariate_type == "siteCovs"], drop = F], 
                              1, 
                              FUN = function(x) all(!is.na(x))))
  
  if(hasArg(aoi)) {
    if(inherits(x, "data.frame")) stop("aoi can only be defined if x is a SpatRaster (not a data.frame).")
    if(inherits(aoi, "SpatRaster")) {
      
      which_not_na_aoi    <- which(!is.na(terra::values(aoi)))
      index_not_na <- intersect(index_not_na, which_not_na_aoi)
      
      aoi2 <- terra::rast(aoi)
      terra::values(aoi2) <- NA
      terra::values(aoi2) [index_not_na]  <- 1
      
    } else {
      stop("aoi must be a SpatRaster")
    }
  }
  
  # subset to cells / sites where all covariates have values
  # if submodel has site covariates
  if(nrow(cov_info_subset) >= 1 & any(cov_info_subset$covariate_type == "siteCovs")) {
    values_to_predict_subset <- values_to_predict_all[index_not_na, 
                                                      cov_info_subset$covariate [cov_info_subset$covariate_type == "siteCovs"], 
                                                      drop = F]
  } else {
    # if no covariates just keep all values (won't affect predictions)
    values_to_predict_subset <- values_to_predict_all[, , drop = F]
  }


  
if(type != "p_array") {
  
  if(isTRUE(batch) | is.numeric(batch)) {
    
    # create batches of values to predict on
    split_dataframe_into_batches <- function(df, batch_size) {
      n_rows <- nrow(df)
      n_batches <- ceiling(n_rows / batch_size)
      batches <- vector("list", n_batches)
      for (i in seq_len(n_batches)) {
        start_row <- (i - 1) * batch_size + 1
        end_row <- min(i * batch_size, n_rows)
        batches[[i]] <- df[start_row:end_row, ]
      }
      return(batches)
    }
    
    values_to_predict_subset_list <- split_dataframe_into_batches(values_to_predict_subset, ifelse(isTRUE(batch), 1000, batch))
    
    
    if (!requireNamespace("abind", quietly = TRUE)) {
      stop(paste("Please install the package abind to run this function with batch =", batch))
    }
    
    array_list <- lapply(values_to_predict_subset_list, FUN = function(values_to_predict_subset_i) {
      
      # create array for intercepts
      array_NA <- array(data = NA, dim = c(nrow(values_to_predict_subset_i),    # raster cell
                                           object@data$M,                     # species
                                           nrow(posterior_matrix)))           # posterior sample
      
      
      # # get intercepts
      out_intercept <- array_NA
      
      for(i in 1:dim(array_NA)[2]){    # species loop
        if(cov_info_intercept$ranef == TRUE | cov_info_intercept$independent == TRUE){  # random or independent intercepts
          out_intercept[,i,] <- matrix(posterior_matrix[, colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] , 
                                       nrow = dim(out_intercept)[1], ncol = dim(out_intercept)[3], byrow = T)
        } else {
          out_intercept[,i,] <- matrix(posterior_matrix[, grep(paste0(keyword_submodel_short, "0$"), colnames(posterior_matrix))] , 
                                       nrow = dim(out_intercept)[1], ncol = dim(out_intercept)[3], byrow = T)
        }
      }
      # gc()
      
      # # memory warning (if applicable)
      # if(object.size(out_intercept) / 1e6 * (nrow(cov_info_subset) + 1) > 4000 ){
      #   ram_usage_estimate <- round(object.size(out_intercept) / 1e6 * (nrow(cov_info_subset) + 1) / 1e3) # in Gb
      #   message(paste("Watch RAM usage. At least", ram_usage_estimate, "Gb will be required"))
      # } 
      # 
      
      # memory warning and error check
      if(object.size(out_intercept) / 1e6 * (nrow(cov_info_subset) + 1) > 4000) { # if estimated RAM usage > 4GB
        ram_usage_estimate <- round(object.size(out_intercept) / 1e6 * (nrow(cov_info_subset) + 1) / 1e3) # in GB
        
        # Get total system memory
        total_ram <- NA
        
        # For Windows
        if(.Platform$OS.type == "windows") {
          mem_info <- system("wmic computersystem get totalphysicalmemory", intern = TRUE)
          if(length(mem_info) > 1) {
            total_ram <- as.numeric(mem_info[2]) / 1e9  # Convert bytes to GB
          }
        } else {
          # For Unix-like systems (Linux/Mac)
          if(Sys.info()["sysname"] == "Darwin") {  # macOS
            mem_info <- system("sysctl hw.memsize", intern = TRUE)
            if(length(mem_info) > 0) {
              total_ram <- as.numeric(sub("hw.memsize: ", "", mem_info)) / 1e9  # Convert bytes to GB
            }
          } else {  # Linux
            mem_info <- system("grep MemTotal /proc/meminfo", intern = TRUE)
            if(length(mem_info) > 0) {
              total_ram <- as.numeric(gsub("MemTotal:\\s+|\\s+kB", "", mem_info)) / 1e6  # Convert KB to GB
            }
          }
        }
        
        message(paste("Watch RAM usage. At least", ram_usage_estimate, "GB will be required"))
        
        # Error if we can determine total RAM and it's insufficient
        if(!is.na(total_ram) && ram_usage_estimate > total_ram) {
          stop(paste("Operation requires approximately", ram_usage_estimate, 
                     "GB of RAM, but system only has", round(total_ram, 1), 
                     "GB total. Please reduce number of draws, predict on fewer cells (e.g. lower resolution), or use a system with more RAM."))
        }
      }
      
      out <- list()
      
      # loop over covariates 
      for(cov in 1:nrow(cov_info_subset)) {
        
        current_cov  <- cov_info_subset$covariate[cov]
        current_coef <- cov_info_subset$coef[cov]
        
        if(!current_cov %in% colnames(values_to_predict_subset_i)) {
          stop(paste("Covariate", current_cov, "not found in data for prediction (x)."), call. = FALSE)
        }
        if(!is.na(cov_info_subset$ranef_cov[cov])){
          stop(paste(current_cov, 
                     " has a random effect other than species. This is currently not supported.", call. = F))
          next
        }
        
        if(cov_info_subset$ranef_nested[cov])  {
          stop(paste(current_cov, 
                     " has a nested random effect. This is currently not supported.", call. = F))
          next
        }
        
        out[[cov]] <- array_NA
        
        # determine data type of current covariate
        covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
        covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
        
        
        effect_type <- ifelse(cov_info_subset$ranef[cov], "ranef",
                              ifelse(cov_info_subset$independent[cov], "independent", "fixed"))
        
        # covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
        
        
        # species loop
        for(i in 1:dim(out[[cov]])[2]){
          
          if(covariate_is_numeric) {
            
            if(effect_type == "fixed") {
              index_covariate <- grep(paste0(current_coef, "$"), colnames(posterior_matrix))
            } else {    # ranef or independent
              index_covariate <- grep(paste0(current_coef, "[", i, "]"), colnames(posterior_matrix), fixed = T)
            }
            
            if(length(index_covariate) == 0) stop(paste("Covariate", current_coef, "not found in posterior matrix"), call. = FALSE)
            if(length(index_covariate) >= 2) stop(paste("Covariate", current_coef, "has more than 2 matches in posterior matrix"), call. = FALSE)
            
            
            out[[cov]][,i,] <-  sapply(posterior_matrix[, index_covariate], FUN = function(x){
              x * values_to_predict_subset_i[, current_cov]
            })
          }
          
          if(covariate_is_factor) {
            
            if(effect_type == "fixed") index_covariate <- grep(current_coef, colnames(posterior_matrix))
            if(effect_type == "ranef") index_covariate <- grep(paste0(current_coef, "[", i, ","), colnames(posterior_matrix), fixed = T)
            
            # this assumes that the numeric values in the raster correspond to the factor levels in the covariate
            # since it uses the raster values to index the posterior matrix
            
            # for data.frames, it seems to work with the categorical column being integer (= factor level, as in as.data.frame(rasterStack)), or proper factor
            out[[cov]][,i,] <- t(posterior_matrix[, index_covariate[values_to_predict_subset_i[, current_cov]]])
            
          }
          suppressWarnings(rm(index_covariate))
        }   # end species loop
      }    # end covariate loop
      
      # sum up individual effects
      if(!.hasSlot(object, "model")) {
        logit.psi <- Reduce('+', out) + out_intercept    # this is for legacy versions before RN models were added
        psi <- exp(logit.psi) / (exp(logit.psi) + 1)
        
      } else {
        
        # sum up individual effects
        if(object@model == "Occupancy") {
          logit.psi <- Reduce('+', out) + out_intercept
          psi <- ilogit(logit.psi)
          rm(logit.psi, out)
        }
        # psi <- exp(logit.psi) / (exp(logit.psi) + 1)   # leads to NaN when numbers are very large
        if(object@model == "RN"){
          log.lambda <- Reduce('+', out) + out_intercept
          lambda <- exp(log.lambda)   # lambda is expected abundance   (Poisson intensity / rate parameter)
          rm(log.lambda)
          if(!type %in% c("abundance", "lambda_array")){   # convert to occupancy probability
            psi <- 1-dpois(0, lambda)
          }
        }
      }
      
      if(!type %in% c("abundance", "lambda_array")) {
        return(psi) 
      } else {
        return(lambda)
      }
    
      
    })   # end lapply 
    
    if(type %in% c("abundance", "lambda_array")) {
      lambda <- abind::abind(array_list, along = 1)
    } else {
      psi <- abind::abind(array_list, along = 1)
    } 
    
    rm(array_list)
    
  }
  
  
  
  
  if(isFALSE(batch)) {
    
    # create array for intercepts
    array_NA <- array(data = NA, dim = c(nrow(values_to_predict_subset),    # raster cell
                                         object@data$M,                     # species
                                         nrow(posterior_matrix)))           # posterior sample
    
    
    # # get intercepts
    out_intercept <- array_NA
    
    
    for(i in 1:dim(array_NA)[2]){    # species loop
      if(cov_info_intercept$ranef == TRUE | cov_info_intercept$independent == TRUE){  # random or independent intercepts
        out_intercept[,i,] <- matrix(posterior_matrix[, colnames(posterior_matrix) %in% paste0(keyword_submodel_short, "0", "[", i, "]")] , 
                                     nrow = dim(out_intercept)[1], ncol = dim(out_intercept)[3], byrow = T)
      } else {
        out_intercept[,i,] <- matrix(posterior_matrix[, grepl(paste0("^", keyword_submodel_short, "0$"), colnames(posterior_matrix))] , 
                                     nrow = dim(out_intercept)[1], ncol = dim(out_intercept)[3], byrow = T)
      }
    }
    
    cleanup_threshold <- 500 #Mb
    if(object.size(out_intercept) / 1e6 > cleanup_threshold)     gc()   # if intercept matrix > 500Mb, cleanup
    
    # memory warning (if applicable)
    if(object.size(out_intercept) / 1e6 * (nrow(cov_info_subset) + 1) > 4000 ){
      ram_usage_estimate <- round(object.size(out_intercept) / 1e6 * (nrow(cov_info_subset) + 1) / 1e3) # in Gb
      message(paste("Watch RAM usage. At least", ram_usage_estimate, "Gb will be required"))
    } 
    
    
    if(nrow(cov_info_subset) >= 1) {
    out <- list()
    

    # loop over covariates 
    for(cov in 1:nrow(cov_info_subset)) {
      
      current_cov  <- cov_info_subset$covariate[cov]
      current_coef <- cov_info_subset$coef[cov]
      
      if(!current_cov %in% colnames(values_to_predict_subset)) {
        stop(paste("Covariate", current_cov, "not found in data for prediction (x)."), call. = FALSE)
      }
      if(!is.na(cov_info_subset$ranef_cov[cov])){
        stop(paste(current_cov, 
                   " has a random effect other than species. This is currently not supported.", call. = F))
        next
      }
      
      if(cov_info_subset$ranef_nested[cov])  {
        stop(paste(current_cov, 
                   " has a nested random effect. This is currently not supported.", call. = F))
        next
      }
      
      out[[cov]] <- array_NA
      
      # determine data type of current covariate
      covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
      covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
      
      
      effect_type <- ifelse(cov_info_subset$ranef[cov], "ranef",
                            ifelse(cov_info_subset$independent[cov], "independent", "fixed"))
      
      # covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F) 
      
      
      # species loop
      for(i in 1:dim(out[[cov]])[2]){
        
        if(covariate_is_numeric) {
          
          if(effect_type == "fixed") {
            index_covariate <- grep(paste0(current_coef, "$"), colnames(posterior_matrix))
          } else {    # ranef or independent
            index_covariate <- grep(paste0(current_coef, "[", i, "]"), colnames(posterior_matrix), fixed = T)
          }
          
          if(length(index_covariate) == 0) stop(paste("Covariate", current_coef, "not found in posterior matrix"), call. = FALSE)
          if(length(index_covariate) >= 2) stop(paste("Covariate", current_coef, "has more than 2 matches in posterior matrix"), call. = FALSE)
          
          
          out[[cov]][,i,] <-  sapply(posterior_matrix[, index_covariate], FUN = function(x){
            x * values_to_predict_subset[, current_cov]
          })
        }
        
        if(covariate_is_factor) {
          
          if(effect_type == "fixed") index_covariate <- grep(current_coef, colnames(posterior_matrix))
          if(effect_type == "ranef") index_covariate <- grep(paste0(current_coef, "[", i, ","), colnames(posterior_matrix), fixed = T)
          
          # this assumes that the numeric values in the raster correspond to the factor levels in the covariate
          # since it uses the raster values to index the posterior matrix
          
          # for data.frames, it seems to work with the categorical column being integer (= factor level, as in as.data.frame(rasterStack)), or proper factor
          out[[cov]][,i,] <- t(posterior_matrix[, index_covariate[values_to_predict_subset[, current_cov]]])
          
        }
        suppressWarnings(rm(index_covariate))
      }   # end species loop
    }    # end covariate loop
    
    # sum up individual effects
    if(!.hasSlot(object, "model")) {
      logit.psi <- Reduce('+', out) + out_intercept    # this is for legacy versions before RN models were added
      psi <- exp(logit.psi) / (exp(logit.psi) + 1)
      
    } else {
      
      # sum up individual effects
      if(object@model == "Occupancy") {
        logit.psi <- Reduce('+', out) + out_intercept
        psi <- ilogit(logit.psi)
        rm(logit.psi, out)
      }
      # psi <- exp(logit.psi) / (exp(logit.psi) + 1)   # leads to NaN when numbers are very large
      if(object@model == "RN"){
        log.lambda <- Reduce('+', out) + out_intercept
        lambda <- exp(log.lambda)   # lambda is expected abundance   (Poisson intensity / rate parameter)
        rm(log.lambda)
        if(!type %in% c("abundance", "lambda_array")){   # convert to occupancy probability
          psi <- 1-dpois(0, lambda)
        }
      }
    }
      
    } else {
      if(object@model == "Occupancy") {
        logit.psi <- out_intercept
        psi <- ilogit(logit.psi)
        rm(logit.psi)
      } 
      if(object@model == "RN"){
        log.lambda <- Reduce('+', out) + out_intercept
        lambda <- exp(log.lambda)   # lambda is expected abundance   (Poisson intensity / rate parameter)
        rm(log.lambda)
        if(!type %in% c("abundance", "lambda_array")){   # convert to occupancy probability
          psi <- 1-dpois(0, lambda)
        }
      }
    }
  }   # end  if(!batch) 
  
  
} else {   # = if type == "p_array", do:
  
  if (!requireNamespace("abind", quietly = TRUE)) {
    stop(paste("Please install the package abind to run this function with type =", type))
  }
  
  # get intercept
  
  if(cov_info_intercept$ranef == FALSE  && cov_info_intercept$independent == FALSE  ) {
    index_intercept <- rep(grep('^alpha0$', colnames(posterior_matrix)),
                           times = object@data$M)    # repeat for each species
  } else {
    index_intercept <- grep('alpha0[', colnames(posterior_matrix), fixed=TRUE)
  }
  
  if(length(index_intercept) == 0) stop(paste("error trying to access intercepts for submodel", submodel))
  
  a0_matrix <- posterior_matrix[, index_intercept]
  # order: [draws, species]
  
  # prepare empty list to contain parameters for covariate responses
  a1_matrix_list <- list()
  
  
  if(nrow(cov_info_subset) >= 1) {
  # loop over covariates 
  for(cov in 1:nrow(cov_info_subset)) {
    
    # collect information about current covariate
    current_cov  <- cov_info_subset$covariate[cov]
    current_coef <- cov_info_subset$coef[cov]
    
    covariate_is_numeric <- cov_info_subset$data_type [cov] == "cont"
    covariate_is_factor  <- cov_info_subset$data_type [cov] == "categ"
    
    effect_type <- ifelse(cov_info_subset$ranef[cov], "ranef",
                          ifelse(cov_info_subset$independent[cov], "independent", "fixed"))
    
    covariate_is_site_cov <- ifelse(cov_info_subset$covariate_type [cov] == "siteCovs", T, F)
    covariate_is_site_occasion_cov <- ifelse(cov_info_subset$covariate_type [cov] == "obsCovs", T, F) 
    
    
    # a few checks (should be done before covariate loop to avoid wasting time if things go wrong)
    if(!current_cov %in% colnames(values_to_predict_subset) && covariate_is_site_cov) {
      stop(paste("Covariate", current_cov, "not found in data for prediction (x)."), call. = FALSE)
    }
    
    if(!is.na(cov_info_subset$ranef_cov[cov])){
      stop(paste(current_cov, 
                 " has a random effect other than species. This is currently not supported.", call. = F))
      next
    }
    
    if(cov_info_subset$ranef_nested[cov])  {
      stop(paste(current_cov, 
                 " has a nested random effect. This is currently not supported.", call. = F))
      next
    }
    

    # identify covariate column in MCMC list output
    
    # first construct matching regular expression for:
    # fixed effect
    if(effect_type == "fixed") alpha_regex <- paste0("alpha.*", current_cov, "$")
    # random effect
    if(effect_type %in% c("ranef", "indep")) alpha_regex <- paste0("alpha.*", current_cov, "\\[")
    
    # find matching columns
    alpha_regex_matches <- grep(alpha_regex,  colnames(posterior_matrix))
    
    # error if no match
    if(length(alpha_regex_matches) == 0) stop(paste("Error identifying value column for covariate:", current_cov))
    
    # subset posterior matrix
    a1_matrix <- posterior_matrix[, alpha_regex_matches, drop = F]
    
    # empty list for covariate-specific effects
    p_arr_list <- list()
    
    # loop over species and occasions to fill arrays
    for (i in 1:object@data$M){   # species loop
      # prepare empty array (for current species)
      p_arr_list [[i]] <- array(NA, c(nrow(posterior_matrix), 
                                      object@data$J, 
                                      object@data$maxocc))
      # order: [draws, stations, occasions]
      
      for (k in 1:object@data$maxocc){ # occasion loop
        
        # if covariate is site-occasion covariate, multiply parameter values from
        # posterior draws with covariate values (by occasion)
        if(covariate_is_site_occasion_cov){      
          if(effect_type == "fixed") {
            # a1_matrix contains only 1 column, identical for all species
            p_arr_list [[i]] [,, k]  <- outer(a1_matrix[, 1],  object@input$obsCovs[[current_cov]] [,k])
          } else { 
            # if ranef, one column per species
            p_arr_list [[i]] [,, k]  <- outer(a1_matrix[, i],  object@input$obsCovs[[current_cov]] [,k])
          }
        }
        
        # if covariate is site covariate, multiply estimates with site covariate values
        if(covariate_is_site_cov) {
          if(effect_type == "fixed") {
            # a1_matrix contains only 1 column, identical for all species
            p_arr_list [[i]] [,, k] <- outer(a1_matrix[, 1], object@input$siteCovs[, current_cov])
            } else {
              # if ranef, one column per species
            p_arr_list [[i]] [,, k] <- outer(a1_matrix[, i], object@input$siteCovs[, current_cov])
          }
        }
      }
    }
    
    # combine list of 3d arrays into 4d array
    # oder: [draws, station, occasion, species]
      p_arr_4d <- abind::abind(p_arr_list, along = 4)    
      
      # harmonize order with out_intercept
      # rearrange into order: [station, species, draw, occasion]
      p_arr_4d <- aperm(p_arr_4d,  c(2,4,1,3))   
    
      # store in list of covariate-specific arrays
    a1_matrix_list[[cov]] <- p_arr_4d
  }
  
    
    
    # sum up individual covariate effects
    logit.p <- Reduce('+', a1_matrix_list)  # doesn't include intercept yet
    
    
    # add a0_matrix (species-specific intercepts) to each element
    # a0_matrix is [draw, species]
    for(i in 1:dim(logit.p)[1]) {   # station
      for(k in 1:dim(logit.p)[4]) {   # occasion
        logit.p[i,,,k] <- logit.p[i,,,k] + t(a0_matrix)
      }
    }
    
  } else {
    # browser()
    logit.p <- array(NA, 
                     dim = c(object@data$J,        # stations
                             object@data$M,        # species
                             nrow(posterior_matrix),
                             object@data$maxocc   # occasions
                     ))
    for(i in 1:object@data$J) {          # station
      for(k in 1:object@data$maxocc) {   # occasion
        logit.p[i,,,k] <- t(a0_matrix)
      }
    }
  }
  
      # convert to probability
      p <- ilogit(logit.p)
      
      
      # cleanup
      rm(logit.p, a0_matrix, a1_matrix_list)
      if(exists("a1_matrix")) rm(a1_matrix)
}
  gc()
  

if(type == "p_array") {
  dimnames(p) <- list(index_not_na,
                        rownames(object@data$y))
  return(p)
}
  
  
  # return raw occupancy probabilities [cell, species, posterior_draw]
  if(type == "psi_array") {
    dimnames(psi) <- list(index_not_na,
                          rownames(object@data$y))
    return(psi)
  }
  
  
  # return raw expected abundance [cell, species, posterior_draw]
  if(type == "lambda_array") {
    dimnames(lambda) <- list(index_not_na,
                             rownames(object@data$y))
    return(lambda)
  }
  
  
  # percentage of area occupied (by species)
  if(type == "pao") {
    
    # random binomial trial for each probability
    z <- array(rbinom(length(psi),prob=psi,size=1), dim = dim(psi))
    pao1 <- apply(z, MARGIN = c(2, 3), mean)  # aggregated spatially: [species, draw]
    dimnames(pao1)[1] <- dimnames(object@data$y)[1] #list(names(object@input$ylist))

    
    # summary table
    pao_summary_table <- as.data.frame(t(apply(pao1, MARGIN = 1, summary)))
    
    
    pao.lower <- as.data.frame(t(apply(pao1, MARGIN = 1, quantile, ((1-level) / 2))))
    pao.upper <- as.data.frame(t(apply(pao1, MARGIN = 1, quantile,  (1 - (1-level) / 2))))
    
    pao.lower2 <- reshape2::melt(pao.lower)
    pao.upper2 <- reshape2::melt(pao.upper)
    
    names(pao.lower2) <- c("Species", paste0("lower.ci.", ((1-level) / 2)))
    names(pao.upper2) <- c("Species", paste0("upper.ci.", (1 - (1-level) / 2)))
    

    pao_summary_table <- cbind(pao_summary_table [, "Min.", drop = F],
                               pao.lower2[,2, drop = F],
                               pao_summary_table [, c("1st Qu.", "Median", "Mean", "3rd Qu.")],
                               pao.upper2[,2, drop = F],
                               pao_summary_table [, "Max.", drop = F])
    
    pao_summary_table <- round(pao_summary_table, 3)
    
    # table with all posterior draws for all species (e.g. for ggplot2)
    pao_melt <- reshape2::melt(pao1)
    colnames(pao_melt) <- c("Species", "draw", "PAO")
    pao_melt <- pao_melt[order(pao_melt$Species, pao_melt$draw),]
    pao_melt$Species <- factor(pao_melt$Species, labels = #names(object@input$ylist))
                                 dimnames(object@data$y)[[1]])
    rownames(pao_melt) <- NULL

    
    if(hasArg(aoi)) {
      return(list(pao_summary = pao_summary_table, 
                  pao_matrix  = pao1,
                  pao_df      = pao_melt,
                  aoi         = aoi2))
    } else {
      return(list(pao_summary = pao_summary_table, 
                  pao_matrix  = pao1,
                  pao_df      = pao_melt))
    }

  }
  
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
    if(inherits(x, "SpatRaster")) {
    
    r_pred_species <- r_pred_sd_species <- list()
    
    for(i in 1:length(unique(psi.mean.melt$Species))) {
      r_pred_species[[i]]    <- raster_template
      r_pred_sd_species[[i]] <- raster_template
      
      terra::values(r_pred_species[[i]])    [index_not_na] <- psi.mean.melt$mean[psi.mean.melt$Species == unique(psi.mean.melt$Species)[i]]
      terra::values(r_pred_sd_species[[i]]) [index_not_na] <- psi.sd.melt$sd[psi.sd.melt$Species == unique(psi.sd.melt$Species)[i]]
    }
    names(r_pred_species) <- names(r_pred_sd_species) <- unique(psi.mean.melt$Species)
    
    stack_out_mean <- terra::rast(r_pred_species)
    stack_out_sd   <- terra::rast(r_pred_sd_species)
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
      
      if(inherits(x, "SpatRaster")) {
      r_pred_lower_species <- r_pred_upper_species <- list()
      
      for(i in 1:length(unique(psi.mean.melt$Species))) {
        r_pred_lower_species[[i]] <- raster_template
        r_pred_upper_species[[i]] <- raster_template
        
        terra::values(r_pred_lower_species[[i]]) [index_not_na] <- psi.lower2[psi.lower2$Species == unique(psi.lower2$Species)[i], paste0("lower.ci.", level)]
        terra::values(r_pred_upper_species[[i]]) [index_not_na] <- psi.upper2[psi.upper2$Species == unique(psi.upper2$Species)[i], paste0("upper.ci.", level)]
      }
      names(r_pred_lower_species) <- names(r_pred_upper_species) <- unique(psi.mean.melt$Species)
      
      stack_out_lower   <- terra::rast(r_pred_lower_species)
      stack_out_upper   <- terra::rast(r_pred_upper_species)
      
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
      if(inherits(x, "SpatRaster")){
        return(list(mean = stack_out_mean,
                    sd   = stack_out_sd))
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
    
    if(inherits(x, "SpatRaster")) {
      
      r.psi.bin.sum.mean <- r.psi.bin.sum.sd <- r.psi.bin.sum.lower <- r.psi.bin.sum.upper <- raster_template
      terra::values(r.psi.bin.sum.mean) [index_not_na] <- psi.bin.sum.mean
      terra::values(r.psi.bin.sum.sd) [index_not_na] <- psi.bin.sum.sd
      
      if(interval == "none"){
        stack_out <- terra::rast(list(mean = r.psi.bin.sum.mean,
                                        sd   = r.psi.bin.sum.sd))
      }
      
      if(interval == "confidence"){
        terra::values(r.psi.bin.sum.lower) [index_not_na] <- psi.bin.sum.lower
        terra::values(r.psi.bin.sum.upper) [index_not_na] <- psi.bin.sum.upper
        
        stack_out <- terra::rast(list(mean  = r.psi.bin.sum.mean,
                                        sd    = r.psi.bin.sum.sd,
                                        lower = r.psi.bin.sum.lower,
                                        upper = r.psi.bin.sum.upper))
      }
      
      
      return(stack_out)
      
    } else {
      if(interval == "none"){
        df_out <- data.frame(mean = psi.bin.sum.mean,
                             sd   = psi.bin.sum.sd)
      }
      
      if(interval == "confidence"){
        df_out <- data.frame(mean  = psi.bin.sum.mean,
                             sd    = psi.bin.sum.sd, 
                             lower = psi.bin.sum.lower,
                             upper = psi.bin.sum.upper)
      }
      
      return(df_out)
      
    }
  }
  
  if(type == "abundance") {
    
    if(hasArg(speciesSubset)) warning("speciesSubset is defined, but has no effect when type = 'abundance'")
    
    # summarize estimates (across posterior samples)
    lambda.mean <- apply(lambda, MARGIN = c(1,2), mean)
    lambda.sd   <- apply(lambda, MARGIN = c(1,2), sd)
    
    # make data frame for ggplot
    lambda.mean.melt <- reshape2::melt(lambda.mean)
    lambda.sd.melt   <- reshape2::melt(lambda.sd)
    
    names(lambda.mean.melt) <- c("cell_nr", "Species", "mean")
    names(lambda.sd.melt)   <- c("cell_nr", "Species", "sd")
    
    if(!is.null(dimnames(object@data$y)[[1]])) {
      lambda.mean.melt$Species <- dimnames(object@data$y)[[1]][lambda.mean.melt$Species]
      lambda.sd.melt$Species   <- dimnames(object@data$y)[[1]][lambda.sd.melt$Species]
    }
    
    # fill rasters with predicted values
    if(inherits(x, "SpatRaster")) {
      r_pred_species <- r_pred_sd_species <- list()
      
      for(i in 1:length(unique(lambda.mean.melt$Species))) {
        r_pred_species[[i]]    <- raster_template
        r_pred_sd_species[[i]] <- raster_template
        
        terra::values(r_pred_species[[i]])    [index_not_na] <- lambda.mean.melt$mean[lambda.mean.melt$Species == unique(lambda.mean.melt$Species)[i]]
        terra::values(r_pred_sd_species[[i]]) [index_not_na] <- lambda.sd.melt$sd[lambda.sd.melt$Species == unique(lambda.sd.melt$Species)[i]]
      }
      names(r_pred_species) <- names(r_pred_sd_species) <- unique(lambda.mean.melt$Species)
      
      stack_out_mean <- terra::rast(r_pred_species)
      stack_out_sd   <- terra::rast(r_pred_sd_species)
    }
    
    
    if(interval == "confidence"){
      lambda.lower <- apply(lambda, MARGIN = c(1,2), quantile, ((1-level) / 2))                        # SLOW!!!
      lambda.upper <- apply(lambda, MARGIN = c(1,2), quantile, (1 - (1-level) / 2))
      
      lambda.lower2 <- reshape2::melt(lambda.lower)
      lambda.upper2 <- reshape2::melt(lambda.upper)
      
      names(lambda.lower2) <- c("cell_nr", "Species", paste0("lower.ci.", level))
      names(lambda.upper2) <- c("cell_nr", "Species", paste0("upper.ci.", level))
      
      if(!is.null(dimnames(object@data$y)[[1]])) {
        lambda.lower2$Species <- dimnames(object@data$y)[[1]][lambda.lower2$Species]
        lambda.upper2$Species <- dimnames(object@data$y)[[1]][lambda.upper2$Species]
      }
      
      if(inherits(x, "SpatRaster")) {
        r_pred_lower_species <- r_pred_upper_species <- list()
        
        for(i in 1:length(unique(lambda.mean.melt$Species))) {
          r_pred_lower_species[[i]] <- raster_template
          r_pred_upper_species[[i]] <- raster_template
          
          terra::values(r_pred_lower_species[[i]]) [index_not_na] <- lambda.lower2[lambda.lower2$Species == unique(lambda.lower2$Species)[i], paste0("lower.ci.", level)]
          terra::values(r_pred_upper_species[[i]]) [index_not_na] <- lambda.upper2[lambda.upper2$Species == unique(lambda.upper2$Species)[i], paste0("upper.ci.", level)]
        }
        names(r_pred_lower_species) <- names(r_pred_upper_species) <- unique(lambda.mean.melt$Species)
        
        stack_out_lower   <- terra::rast(r_pred_lower_species)
        stack_out_upper   <- terra::rast(r_pred_upper_species)
        
        return(list(mean  = stack_out_mean,
                    sd    = stack_out_sd,
                    lower = stack_out_lower,
                    upper = stack_out_upper))
      } else {
        
        return(data.frame(lambda.mean.melt, 
                          sd = lambda.sd.melt$sd, 
                          lower = lambda.lower2[, 3], 
                          upper = lambda.upper2 [,3]))
        
      }
    }
    
    if(interval == "none"){
      if(inherits(x, "SpatRaster")){
        return(list(mean = stack_out_mean,
                    sd   = stack_out_sd))
      } else {
        return(data.frame(lambda.mean.melt, 
                          sd = lambda.sd.melt$sd))
      }
    }
  } # close if(type == "abundance")
  
}




#' Predictions from community occupancy models
#' 
#' Create (spatial) predictions of species occupancy and species richness from community occupancy models and spatial rasters or covariate data frames.
#'
#' @param object \code{commOccu} object
#' @param mcmc.list  mcmc.list. Output of \code{\link{fit}} called on a \code{commOccu} object
#' @param type character. "psi" for species occupancy estimates, "richness" for species richness estimates, "pao" for percentage of area occupied (by species), "psi_array" for raw occupancy probabilities in an array. For Royle-Nichols models, "abundance" for species abundance, or "lambda_array" for raw species abundance estimates in an array. "p_array" for raw detection probabilities in an array.
#' @param draws  Number of draws from the posterior to use when generating the plots. If fewer than draws are available, they are all used
#' @param level  Probability mass to include in the uncertainty interval
#' @param interval  Type of interval calculation. Can be "none" or "confidence" (can be abbreviated). Calculation can be slow for type = "psi" with many cells and posterior samples.
#' @param x   SpatRaster, data.frame or NULL. Must be scaled with same parameters as site covariates used in model, and have same names. If NULL, use site covariate data frame from model input (\code{commOccu} object in parameter \code{object}) 
#' @param aoi SpatRaster with same dimensions as x (if x is a SpatRaster), indicating the area of interest (all cells with values are AOI, all NA cells are ignored). If NULL, predictions are made for all cells.
#' @param speciesSubset  species to include in richness estimates. Can be index number or species names.
#' @param batch logical or numeric. If FALSE, all raster cells / data frame rows will be processed at once (can be memory intensive). If TRUE, computation is conducted in batches of 1000. If numeric, it is the desired batch size.
#' @param seed numeric. Seed to use in \code{set.seed} for reproducible results (ensures that \code{draws} are identical).
#'
#' @details Processing can be very memory-intensive. If memory is insufficient, use the  \code{batch} parameter. This can enable processing for higher numbers of \code{draws} or very large rasters / data frames. 
#' 
#' @return A SpatRaster or data.frame, depending on \code{x} (for type = "psi", "abundance", "richness". If type = "pao", a list. If type = "psi_array" or "lambda_array", a 3D-array [site, species, draw]. If type = "p_array", a 4D-array [site, species, draw, occasion].
#' 
#' @aliases predict
#' @method predict commOccu
#' @importFrom stats rbinom dpois sd
#' @importFrom utils object.size
#' @importFrom methods .hasSlot
#' @export


setMethod("predict", signature = c(object = "commOccu"),
          predictionMapsCommunity)

