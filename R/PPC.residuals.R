#' Calculate Residuals from MCMC Output of Occupancy Models
#' 
#' @description
#' Posterior Predictive Check (PPC) function that calculates Freeman-Tukey (FT) 
#' residuals, Pearson"s Chi-squared residuals, or deviance from MCMC output of 
#' occupancy models. This function compares observed data to simulated data from 
#' the posterior distribution to assess model fit.
#' 
#' 
#' @param y Observations as either a site vector or site by occasion matrix. For matrix 
#'   format, use NA for unsampled occasions.
#' @param p Array of posterior samples for detection probability (p). Dimensions should be 
#'   iterations by sites (by occasion optionally). For RN models, p should represent 
#'   individual-level detection (not conditional on local abundance).
#' @param psi Array of posterior samples for occupancy probability (psi). Dimensions should 
#'   be iterations by sites. For RN models, psi should represent expected abundance
#' @param model Character indicating model type: either "Occupancy" or "RN" 
#'   (Royle-Nichols).
#' @param type Type of residual to calculate: "FT" (Freeman Tukey), "PearChi2" 
#'   (Pearson Chi-squared), or "Deviance" (not technically a residual).
#' @param K Number of occasions as either a scalar or site vector. Calculated automatically 
#'   if y is a matrix.
#' @param z.cond Logical. If TRUE, new data is conditioned on estimated z (testing only 
#'   detection model fit). If FALSE, generates new z for each posterior sample 
#'   (testing complete model).
#' @param zhat Optional matrix with same dimensions as psi containing estimates of z from 
#'   the same model. If not provided, will be generated internally.
#' @param nmax Maximum site-level abundance (default = 20). Only used if model="RN". 
#'   Higher values increase computation time. Warning given if set too low.
#' @param return.residuals Logical. If TRUE (default), returns residuals along with 
#'   Bayesian p-value.
#' @param return.z Logical. If TRUE, returns z values conditional on y, and unconditional 
#'   z's if \code{z.cond = FALSE}. Note: if zhat is provided, the returned conditional-on-y z 
#'   values will be identical to those provided.
#'   
#'   
#'   
#' @return If return.residuals=TRUE (default), returns a list containing:
#'   \itemize{
#'     \item res.obs - residuals for observed data
#'     \item res.new - residuals for newly generated data
#'     \item BP - Bayesian p-value
#'   }
#'   If return.residuals=FALSE, returns only the Bayesian p-value.
#'   
#'   
#' @details
#' This function helps assess model fit for occupancy models using various types of residuals:
#' 
#' \subsection{Types of Residuals}{
#'   \itemize{
#'     \item Freeman-Tukey (FT):
#'           \deqn{R_j = (\sqrt{y_j} - \sqrt{E(y_j)})^2}
#'           Measures the squared difference between the square root 
#'           of observed detections and the square root of expected detections 
#'           at each site.
#'     \item Pearson Chi-squared:
#'           \deqn{R_j = \left(\frac{y_j - E(y_j)}{\sqrt{Var(y_j)}}\right)^2}
#'           Measures the squared difference between observed and 
#'           expected detections, standardized by the theoretical variance calculated 
#'           from the model parameters.
#'     \item Deviance:
#'           \deqn{R_j = -2\log[y_j|\theta_j, K_j]}
#'           Measures the contribution of each site to the overall model 
#'           likelihood, quantifying the discrepancy between observed data and model 
#'           predictions based on likelihood ratios
#'   }
#' 
#'   Where:
#'   \itemize{
#'     \item \eqn{y_j} is the number of detections of the species at site j, out of 
#'           \eqn{K_j} repeated surveys
#'     \item \eqn{E(y_j) = K_j p_j z_j}, with \eqn{p_j} = species detection probability 
#'           and \eqn{z_j} = occupancy state (1 if occupied, 0 otherwise)
#'     \item \eqn{Var(y_j) = p_j z_j (1 - p_j z_j) K_j}
#'     \item For Royle-Nichols occupancy models, the term \eqn{p_j z_j} is replaced with 
#'           \eqn{1 - (1 - r_j)^{N_j}}, where \eqn{r_j} = individual detection probability 
#'           and \eqn{N_j} = local abundance
#'     \item For Deviance, \eqn{\theta_j} is either occupancy and species detection probability 
#'           at site j (\eqn{\psi_j, p_j}) for regular occupancy models, or expected abundance 
#'           and individual detection probability (\eqn{\lambda_j, r_j}) for Royle-Nichols 
#'           occupancy models
#'   }
#' }
#' 
#' \subsection{Bayesian P-values}{
#'   The function calculates Bayesian p-values as a measure of model fit. These values:
#'   \itemize{
#'     \item Range from 0 to 1
#'     \item Values close to 0.5 suggest good model fit
#'     \item Values close to 0 or 1 suggest poor fit
#'     \item Are calculated by comparing observed residuals to residuals from 
#'           simulated data
#'   }
#' }
#' 
#' \subsection{Conditional vs Unconditional Assessment}{
#'   The z.cond parameter allows for two types of model assessment:
#'   \itemize{
#'     \item z.cond = TRUE: Tests only the detection component of the model, fixing 
#'           occupancy/abundance to estimates from the model, rather than generating them anew
#'     \item z.cond = FALSE: Tests the complete model, including both occupancy and 
#'           detection components
#'   }
#' }
#' 
#' @note 
#' FT and Chi-squared residuals have been extensively tested. Deviance calculations 
#' have undergone less testing and are only available for scenarios with constant 
#' detection probability across occasions. FT and Chi-squared residuals can handle 
#' varying detection probabilities.
#' 
#' 
#' @section Warning: This is a beta version of the function. While it has been tested extensively, 
#'   not all possible data configurations may have been captured in testing. This is 
#'   particularly true for:
#'   
#'   \itemize{
#'     \item Deviance calculations (type = "Deviance")
#'     \item Royle-Nichols models (model = "RN")
#'   }
#'   If you encounter issues with the function, please contact the package developers.
#' 
#' @author Rahel Sollmann
#' 
#' @references
#' Sollmann, Rahel. Occupancy models and the "good fit, bad prediction" dilemma. 
#' Ecology (submitted)
#' 
#' @importFrom stats dbinom rmultinom rpois
#' 
#' @export


PPC.residuals <- function(y, 
                          p, 
                          psi, 
                          model = c("Occupancy", "RN"),
                          type = c("FT", "PearChi2", "Deviance"),
                          K = NULL, 
                          z.cond = TRUE, 
                          zhat = NULL, 
                          nmax = 20,
                          return.residuals = TRUE,
                          return.z = TRUE) {
  
  #### some input data checks
  
  model <- match.arg(model)
  type  <- match.arg(type)
  
  if(model=="RN"){warning("For model=RN, matrix/array provided for
                         p must contain individual level detection probability 
                         (i.e., detection probability NOT conditional on local abundance) and
			psi must contain expected abundance.")
    ##check that nmax is sufficient
    if(dpois(nmax, max(psi))>0.01) warning("Probability of nmax at highest value 
                                            of expected abundance (psi) >0.01; 
                                            consider increasing nmax.") 
  }
  
  if(type =="Deviance" && is.matrix(y)) {stop("Deviance-based PPC currently only available for data aggregated over occasions.")}
  if (!is.null(K) && is.matrix(y)){warning("With occasion-explicit data, K is automatically derived from y (ncol); input value for K is ignored")}
  if (all(K==1)  && is.vector(y)){stop("With aggregated data, K must be >1 at least for some sites")}
  if (is.null(K)  && is.vector(y)){stop("With aggregated data, K must be supplied")}
  
  ###check that p,psi, zhat are given as arrays, not mcmc lists
  if(is.list(p) | is.list(psi) | is.list(zhat))stop("Posterior samples of p, psi (and zhat if provided) need to be in array form, not lists.")
  
  ###check number of posterior draws for p, psi, and, if applicable, zhat
  if(dim(p)[1] != dim(psi)[1]){stop("Dimensions of p and psi do not match; they need to have the same number of posterior draws (1st dimension)")}
  if(!is.null(zhat)){
    if(dim(zhat)[1] != dim(psi)[1]) stop("Dimensions of zhat do not match dimensions of psi and p; they need to have the same number of posterior draws (1st dimension)")
  }
  
  ###check spatial dimensions
  J <- ifelse(is.vector(y), length(y), nrow(y))
  if(dim(p)[2] != J |
     dim(psi)[2] != J){stop("Dimensions of p or psi do not match dimensions of data y; they need to have the same number of sites (2nd dimension)")}
  if(!is.null(zhat)){
    if(dim(zhat)[2] != dim(psi)[2]) stop("Dimensions of zhat do not match dimensions of psi and p; they need to have the same number of sites (2nd dimension)")
  }
  
  ###check temporal dimensions
  if(is.matrix(y)){
    
    # maxocc<-ncol(y)  # not used
    if(length(dim(p))<3){stop("If providing occasion specific observations, y, p must also be occasion-specific (ie, 3-dimensional array)")}
    
    ## set p for unsampled occasions to 0 - looks good
    if (sum(is.na(y))>0) {
      # message("Observations y contain NAs (missing data); detection probability
      #       for these sites and occasions will be set to 0 automatically.")   # I downgraded this to message. May not even be necessary at all?
      y.na <- matrix(FALSE, dim(y)[1], dim(y)[2])
      y.na[is.na(y)] <- TRUE
      y.arr <- aperm(array(y.na, c(dim(y), dim(p)[1])), c(3,1,2))
      p[y.arr]<-0
    }
  }
  
  if (is.vector(y) & length(dim(p))==3) stop("If p is occasion specific (3d array), y must also be occasion-specific (ie, matrix)")
  
  if(length(dim(p))==3 & is.matrix(y)){
    if(dim(p)[3]!=dim(y)[2])stop("Number of occasions in p does not match number of occasions in y; both objects need to have the same number of occasions.")
  }
  
  
  #### start calculations ##
  
  ##create var object that will be overwritten if type="PearChi2"
  var<-NULL
  
  ##identify known occupied sites
  if(is.vector(y)){
    n<-rep(as.numeric(y>0), each=nrow(psi))
    ##make vector of number of occasions
    if(length(K)==1){kvec=rep(K, length(as.vector(p)))}else{
      kvec<-rep(K, each=nrow(p))
    }
    ##set aggregated y to y
    y.agg<-y
  } 
  
  if(is.matrix(y)){
    #calculate aggregated y
    y.agg<-apply(y,1,sum, na.rm=TRUE)
    n<-rep(as.numeric(y.agg>0), each=nrow(psi))
    ##in this case, k is always 1
    kvec<-1
  }
  
  
  
  ##generate values of z for data if not supplied    ####
  if (is.null(zhat)){
    
    ## binary z for regular occu model
    if (model=="Occupancy"){
      
      if(is.matrix(y)){
        ##create vector of p(no detection)
        p0<-as.vector(apply(p, 1:2, function(x) prod(1-x)))
      } else {
        p0<-(1-as.vector(p))^kvec}
      
      ##p(presence given no detection)
      psi.eff<-(p0 * as.vector(psi))/
        ((p0 * as.vector(psi)) + (1-as.vector(psi)))
      z<-rbinom(prod(dim(psi)), 1, psi.eff)
      
      ##set to 1 for sites with observations
      z.real<-z
      z.real[which(n==1)]<-1
      
    } ## end if(occu) statement 
    
    ##same for RN model 
    if (model=="RN"){
      ##set range of possible abundance values
      N.poss<-0:nmax
      lam.arr<-array(psi, c(dim(psi), nmax+1))
      
      if(is.matrix(y)){
        ## 4d
        r.arr<-array(p, c(dim(p), nmax+1))
        N.arr<-array(rep(N.poss, each=prod(dim(p))), c(dim(p), nmax+1))
        
        # site by occ data - first repeat for posterior samples
        y1<-aperm(array(y, c(dim(y), nrow(psi)) ), c(3,1,2))
        # then repeat that for each possible N
        y.arr<-array(y1, c(dim(y1), nmax+1))
        
        #conditional-on-N p detection
        p.arr<-1-(1-r.arr)^N.arr
        ##prob (y|z) - product over occasions (na.rm bc y could have NAs)
        prob<-apply(dbinom(y.arr, 1, p.arr), c(1, 2, 4),prod, na.rm=TRUE)
        ##prob (z) - only needs one 3d slice of 4d N array
        probz<-dpois(N.arr[,,1,], lam.arr)
        
      } else { ##for vector data format
        ##make arrays
        r.arr<-array(p, c(dim(p), nmax+1))
        y.arr<-array(rep(y.agg, each=nrow(psi)), c(dim(psi), nmax+1))
        N.arr<-array(rep(N.poss, each=prod(dim(p))), c(dim(p), nmax+1))
        k.arr<-array(matrix(kvec, dim(p)), c(dim(p), nmax+1))
        
        #conditional-on-N p detection
        p.arr<-1-(1-r.arr)^N.arr
        ##prob (y|z)
        prob<-dbinom(y.arr, k.arr, p.arr)
        ##prob (z)
        probz<-dpois(N.arr, lam.arr)
      }
      
      ##generate new N conditional on y
      z.real<-apply(prob*probz, 1:2, function(x)which(rmultinom(1,1,x)>0)-1)
      z.real<-as.vector(z.real)
    } # end RN if statement
    
  } else { #end if(is.null(zhat))
    ##if provided from model, use those - works for both model types
    z.real=as.vector(zhat)}
  
  
  
  ## generate new z from model if z.cond=FALSE; otherwise set z.new=z.real   #####
  
  if (z.cond){ z.new<-z.real} else{
    if (model == "RN") {
      z.new<-rpois(prod(dim(psi)), as.vector(psi))} else {
        z.new<-rbinom(prod(dim(psi)), 1, as.vector(psi))}
  }
  
  
  
  ## generate new observations and calculate expectations for aggregated y   ####
  if(is.vector(y)){
    
    ##get p conditional on state
    if(model =="Occupancy") {p.eff<-as.vector(p)*z.real}
    if(model =="RN") {p.eff<-1-(1-as.vector(p))^z.real}
    
    # if(!expected){
    Ey <- (kvec* p.eff) 
    
    ##only calculate variance if type="Pearson" or "PearChi2"
    ##taken from McCullaugh and Nelder book (not Warton code)
    if (type %in% c("Pearson", "PearChi2")){
      var <- kvec*(p.eff)  *(1 - p.eff)
    }
    
    if(z.cond) {
      #generate new observations from p conditioned on same state
      ynew<-rbinom(prod(dim(p)), kvec, p.eff)
      Ey.new<-Ey
      var.new<-var
      p.eff.new<-p.eff
    } else {
      if(model=="Occupancy") {p.eff.new<-as.vector(p)*z.new}
      if(model=="RN") {p.eff.new<-1-(1-as.vector(p))^z.new}
      #generate new observations from p conditioned on new state
      ynew<-rbinom(prod(dim(p)), kvec, p.eff.new)
      
      Ey.new<-kvec* p.eff.new
      
      if (type %in% c("Pearson", "PearChi2")){
        var.new<-kvec*(p.eff.new)*(1-p.eff.new)
      }
    }
    
  } #end if y is vector
  
  
  if(is.matrix(y)){
    
    ##get p conditional on state
    if(model=="Occupancy") {p.eff<-as.vector(p)*rep(z.real, ncol(y))}
    if(model=="RN") {p.eff<-1-(1-as.vector(p))^rep(z.real, ncol(y))}
    
    #sum p"s over occasions 
    p.agg<-apply(array(p.eff, dim(p)), 1:2, sum)
    
    # if(!expected){
    Ey<-as.vector(p.agg)
    
    if (type %in% c("Pearson", "PearChi2")){
      pvar<-apply(array(p.eff, dim(p)), 1:2, function(x) sum(x*(1-x)))
      var<-as.vector(pvar)
    }
    
    if(z.cond) {
      ##generate new data from p conditioned on same state
      yn3d<-array(rbinom(prod(dim(p)), kvec, p.eff),
                  dim(p))
      ynew<-as.vector(apply(yn3d, 1:2, sum))
      
      Ey.new<-Ey
      var.new<-var
      p.eff.new<-p.eff
    } else{
      
      if(model=="Occupancy") {p.eff.new<-as.vector(p)*rep(z.new, ncol(y))}
      if(model=="RN") {p.eff.new<-1-(1-as.vector(p))^rep(z.new, ncol(y))}
      
      p.agg.new<-apply(array(p.eff.new, dim(p)), 1:2, sum)
      
      Ey.new<-as.vector(p.agg.new)
      
      ##generate new data from p conditioned on new state
      yn3d<-array(rbinom(prod(dim(p)), kvec, p.eff.new),
                  dim(p))
      ynew<-as.vector(apply(yn3d, 1:2, sum))
      
      if (type %in% c("Pearson", "PearChi2")){
        pvar<-apply(array(p.eff.new, dim(p)), 1:2, function(x) sum(x*(1-x)))
        var.new<-as.vector(pvar)
      }
    }
  }
  
  ## calculate residuals, deviance
  
  if(type=="FT"){
    res.new<-(sqrt(ynew) - sqrt(Ey.new) )^2
    res<-( sqrt(rep(y.agg, each=nrow(psi))) - 
             sqrt(Ey) ) ^2
  }
  
  if(type=="PearChi2"){
    res.new<-( ( ynew - Ey.new )/
                 ( sqrt(var.new) +0.0001) )^2
    res<- (( rep(y.agg, each=nrow(psi)) - Ey ) /
             ( sqrt(var) + 0.0001 ) )^2
  }
  
  
  if(type == "Deviance"){
    
    if(model=="Occupancy"){
      
      # if (is.vector(y)){ ##use dbinom to account for permutations!
      #for new data
      pd.new<-dbinom(ynew, kvec, as.vector(p)) * as.vector(psi) + #always
        (1-as.vector(psi))*(ynew==0) #only sites with no detections
      
      res.new<- -2*log(pd.new)
      
      ##for observed data 
      yarr<-as.vector(rep(y.agg, each=nrow(psi)))
      pd<-dbinom(yarr, kvec, as.vector(p)) * as.vector(psi) + #always
        (1-as.vector(psi))*(yarr==0) #only sites with no detections
      
      res<--2*log(pd)
    } #end if occu 
    
    if(model=="RN"){
      
      
      ##calculate prob, probz if not already calculated
      ## ie, if zhat was provided to function
      if (!is.null(zhat)){
        N.poss<-0:nmax
        lam.arr<-array(psi, c(dim(psi), nmax+1))
        
        ##make arrays
        r.arr<-array(p, c(dim(p), nmax+1))
        y.arr<-array(rep(y.agg, each=nrow(psi)), c(dim(psi), nmax+1))
        N.arr<-array(rep(N.poss, each=prod(dim(p))), c(dim(p), nmax+1))
        k.arr<-array(matrix(kvec, dim(p)), c(dim(p), nmax+1))
        
        #conditional-on-N p detection
        p.arr<-1-(1-r.arr)^N.arr
        ##prob (y|z)
        prob<-dbinom(y.arr, k.arr, p.arr)
        ##prob (z)
        probz<-dpois(N.arr, lam.arr)
      }
      
      ##for observed data
      #integrate out state by summing over possible states
      pd<-apply(prob*probz, 1:2, sum)
      res<--2*log(as.vector(pd))
      
      ##for new data
      y.arr.new <- array(ynew, c(dim(psi), nmax+1))
      prob.new <- dbinom(y.arr.new, k.arr, p.arr)
      pd.new <- apply(prob.new*probz, 1:2, sum)
      res.new <- -2*log(as.vector(pd.new))
      
    } #end if RN
  } #end if deviance
  
  res.out <- matrix(res, dim(psi))
  resnew.out <- matrix(res.new, dim(psi))
  
  
  pval <-  mean(apply(res.out, 1, sum) >  apply(resnew.out, 1, sum) )
  
  out <- pval
  if(return.residuals){
    out<-list(BP = pval, res.obs = res.out, res.new = resnew.out)} 
  if(return.z){
    if(z.cond){
      out<-list(BP = pval, res.obs=res.out, res.new = resnew.out,
                zhat = matrix(z.real, dim(psi)))} else {
                  out <- list(BP = pval, res.obs = res.out, res.new = resnew.out,
                              zhat = matrix(z.real, dim(psi)),
                              z.new = matrix(z.new, dim(psi)))
                }
    
  }
  return(out)
}





#' Calculate Community-Level Posterior Predictive Checks for Occupancy Models
#' 
#' @description
#' A wrapper function that applies Posterior Predictive Checks (PPC) across multiple 
#' species in a community occupancy model (from \code{\link{communityModel}}.
#' It calculates species-specific and community-level Bayesian p-values to assess 
#' model fit. The function accepts both \code{\link{predict}} output format and 
#' list format for model parameters.
#' 
#' @details
#' This function extends the single-species PPC analysis to the community level by:
#' \itemize{
#'   \item Applying residual calculations to each species in the community
#'   \item Aggregating results to assess community-level model fit
#'   \item Providing both species-specific and community-level diagnostics
#' }
#' 
#' @param p Either a 4D array [stations, species, iterations, occasions] from 
#' \code{\link{predict}} or a list of 3D arrays [iterations, stations, occasions], 
#' one per species
#' @param psi Either a 3D array [stations, species, iterations] from 
#' \code{\link{predict}} or a list of 2D arrays [iterations, stations], one per species
#' @param y List of detection histories, one matrix/vector per species
#' @param K Number of occasions as either a scalar or site vector. Calculated automatically 
#'   if y is a list of matrices.
#' @param zhat List of z estimate matrices, one per species (optional). Each matrix 
#'   should follow the format specified in \code{\link{PPC.residuals}}.
#' @param z.cond Logical. If TRUE, new data is conditioned on estimated z (testing only 
#'   detection model fit). If FALSE, generates new z for each posterior sample 
#'   (testing complete model).
#' @param model Character indicating model type ("Occupancy" or "RN")
#' @param type Character indicating residual type ("FT", "PearChi2", or "Deviance")
#' @param input_format Character indicating format of p and psi ("predict" or "lists")
#' @param show_progress Logical; whether to show a progress bar during computation 
#' (if package pbapply is available)
#' @param return.residuals Logical. If TRUE, returns species-specific residuals along 
#'   with Bayesian p-values. If FALSE, returns only the p-values.
#' @param ... Additional arguments passed to \code{\link{PPC.residuals}}.
#'   
#'   
#' @details
#' This function provides flexibility in input formats to accommodate different workflows:
#' \itemize{
#'   \item Direct output from camtrapR's predict() function (4D/3D arrays)
#'   \item Lists of species-specific arrays (for custom workflows)
#' }
#' 
#' 
#' 
#' @return If return.residuals=TRUE, returns a list containing:
#'   \itemize{
#'     \item bp_values - Data frame with species-specific and community-level 
#'           Bayesian p-values
#'     \item species_results - List containing the complete PPC.residuals output 
#'           for each species
#'   }
#'   If return.residuals=FALSE, returns only the data frame of Bayesian p-values.
#'   
#' @seealso \code{\link{PPC.residuals}} for details on the underlying single-species 
#'   calculations
#'   
#' @author Rahel Sollmann
#' 
#' @export
#' 
#'
#' @examples 
#' \dontrun{
#' 
#' # Create and fit model 
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
#' species_to_include <- unique(recordTableSample$Species)
#' 
#'   DetHist_list <- detectionHistory(
#'     recordTable         = recordTableSample,
#'     camOp                = camop_no_problem,
#'     stationCol           = "Station",
#'     speciesCol           = "Species",
#'     recordDateTimeCol    = "DateTimeOriginal",
#'     species              = species_to_include,
#'     occasionLength       = 7,
#'     day1                 = "station",
#'     datesAsOccasionNames = FALSE,
#'     includeEffort        = TRUE,
#'     scaleEffort          = TRUE,
#'     timeZone             = "Asia/Kuala_Lumpur"
#' )
#' 
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
#' data_list <- list(ylist = DetHist_list$detection_history,
#'                   siteCovs = sitecovs,
#'                   obsCovs = list(effort = DetHist_list$effort))
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
#' 
#' 
#' # create predictions for p and psi
#' draws <- 100
#' 
#' p <- predict(object = mod.jags,
#'              mcmc.list = fit.jags,
#'              type = "p_array",
#'              draws = draws)
#' # output is in order [station, species, draw, occasion]
#'  
#'  psi <- predict(object = mod.jags,
#'                 mcmc.list = fit.jags,
#'                 type = "psi_array",
#'                 draws = draws)
#'  # output is in order [station, species, draw]
#' 
#' 
#' 
#' ppc_comm <- PPC.community(
#'   p = p,
#'   psi = psi,
#'   y = mod.jags@input$ylist, 
#'   model = "Occupancy",
#'   type = "FT")
#'   
#' # Bayesian p-values
#'  ppc_comm$BP
#'  
#'    
#'  str(ppc_comm$residuals)
#'  
#'  # get individual species PPC results
#'  ppc_species <- ppc_comm$residuals[[1]] # first species
#'  
#'  plot(apply(ppc_species$res.obs, 2, mean), apply(ppc_species$res.new, 2, mean),
#'  xlab = "Observed residuals",
#'  ylab = "Predicted residuals"
#'  )
#'  
#'  abline(0,1)  # diagonal line is not visible due to tiny data set
#'   
#'  }

PPC.community <- function(p, 
                          psi, 
                          y, 
                          input_format = c("predict", "lists"),
                          K = NULL, 
                          model = c('Occupancy', 'RN'),
                          zhat = NULL, 
                          z.cond = TRUE, 
                          type = c('FT', 'PearChi2', 'Deviance'),
                          return.residuals = TRUE,
                          show_progress = TRUE,
                          ...){
  
  
  input_format <- match.arg(input_format)
  model <- match.arg(model)
  type <- match.arg(type)
  
  if (!is.list(y)) stop("y must be a list of detection histories")
  n_species <- length(y)
  if (n_species == 0) stop("y list is empty")
  
  
  # Input validation and format detection
  if (input_format == "predict") {
    # Expect arrays from predict()
    if (!is.array(p) || !is.array(psi)) 
      stop("When input_format='predict', p and psi must be arrays")
    
    if (!(length(dim(p)) == 4 && length(dim(psi)) == 3)) {
      stop("When input_format='predict', expecting 4D array for p and 3D array for psi")
    }
    
    
    # check array dimensions
    p_dims <- dim(p)
    psi_dims <- dim(psi)
    
    if (length(p_dims) != 4) 
      stop("p must be 4D array [iterations, species, stations, occasions]")
    if (length(psi_dims) != 3) 
      stop("psi must be 3D array [iterations, species, stations]")
    
    # Check dimensions match
    if (p_dims[2] != n_species || psi_dims[2] != n_species)
      stop("Number of species in p/psi arrays doesn't match y list")
    if (p_dims[1] != psi_dims[1])
      stop("Number of stations in p and psi arrays don't match")
    
    
    # reorder arrays
    # p:   Reorder from [stations, species, iterations, occasions] -> [iterations, stations, species, occasions]
    p <- aperm(p, c(3, 1, 2, 4))    
    # psi: Reorder from [stations, species, iterations]  -> [iterations, stations, species]
    psi <- aperm(psi, c(3, 1, 2))   
    
    # convert arrays to lists (by species)
    p_list <- apply(p, 3, function(x) x, simplify = FALSE)
    psi_list <- apply(psi, 3, function(x) x, simplify = FALSE)
    
  } else {
    
    # Validation for list input
    if (!is.list(p) || !is.list(psi))
      stop("When input_format='lists', p and psi must be lists")
    if (length(p) != n_species || length(psi) != n_species)
      stop("Length of p and psi lists must match y list")
    
    
    # Check each element in lists
    for(i in seq_len(n_species)) {
      if (!is.array(p[[i]]) || length(dim(p[[i]])) != 3)
        stop("Each element in p list must be a 3D array [iterations, stations, occasions]")
      if (!is.array(psi[[i]]) || length(dim(psi[[i]])) != 2)
        stop("Each element in psi list must be a 2D array [iterations, stations]")
      
      # Check matching dimensions within each species
      if (dim(p[[i]])[1] != dim(psi[[i]])[1])
        stop(sprintf("Number of posterior draws don't match in p and psi for species %d", i))
      if (dim(p[[i]])[2] != dim(psi[[i]])[2])
        stop(sprintf("Number of stations don't match in p and psi for species %d", i))
    }
    
    p_list <- p
    psi_list <- psi
  }
  
  # Validate consistency
  n_species <- length(y)  # ylist length is our reference
  if (length(p_list) != n_species || length(psi_list) != n_species) {
    stop("Number of species in p, psi, and y must match")
  }
  
  
  y_list <- y
  
  # run with progress indicator if requested and if pbapply is available
  mapFun <- if (show_progress && requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbmapply
  } else {
    base::mapply
  }
  
  
  # Create zhat_list that's either the provided list or a list of NULLs
  zhat_list <- if(is.list(zhat)) {
    zhat
  } else {
    replicate(length(y_list), NULL, simplify = FALSE)
  }
  
  # run PPC.residuals on all species
  BP.spec <- mapFun(function(p_sp, psi_sp, y_sp, z_sp) {
    PPC.residuals(p = p_sp, 
                  psi = psi_sp, 
                  y = y_sp, 
                  K = K, 
                  model = model,
                  zhat = z_sp,  # will be NULL if zhat wasn't provided
                  z.cond = z.cond, 
                  type = type,
                  return.residuals = TRUE,
                  ...)
  }, p_list, psi_list, y_list, zhat_list, SIMPLIFY = FALSE)
  
  
  # obtain overall Bayesian p-value
  # sum residuals over sites, cbind over all species, then sum across species
  obs.all<-apply(do.call(cbind, lapply(BP.spec, function(x) apply(x$res.obs, 1, sum))), 1, sum)
  new.all<-apply(do.call(cbind, lapply(BP.spec, function(x) apply(x$res.new, 1, sum))), 1, sum)
  
  BP.comm<-mean(obs.all>new.all)
  
  ## bundle into data frame
  if(is.null(names(y_list))){
    spnames<-paste0('Species', 1:length(y_list))} else {
      spnames<- names(y_list)}
  
  df.out<-data.frame(Species=c(spnames, 'Community'),
                     BP = c(sapply(BP.spec, function(x)x$BP), BP.comm))
  if(return.residuals){
    names(BP.spec)<-spnames
    return(list(BP=df.out, residuals=BP.spec))} else{
      return(df.out)
    }
}
