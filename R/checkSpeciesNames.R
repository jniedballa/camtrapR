#' Check species names against the ITIS taxonomic database
#' 
#' The function checks species names (common or scientific names) provided by
#' the user with the ITIS taxonomic database (\url{http://www.itis.gov/}) via
#' functions from the package \pkg{taxize}. It returns both common and
#' scientific names, the taxon authors, taxon rank name and status, the TSN
#' (taxonomic serial numbers) and ITIS urls.
#' 
#' Arguments \code{searchtype}, \code{accepted} and \code{ask} are passed on to
#' \code{\link[taxize]{get_tsn}}.
#' 
#' @param speciesNames character. Vector of species names to check. Either
#' common names or scientific names.
#' @param searchtype character. Type of names specified in \code{speciesNames}.
#' One of 'scientific' or 'common'.
#' @param accepted logical. Return only accepted valid names? If TRUE, invalid
#' names are returned as NA. Set to FALSE to return both accepted and
#' unaccepted names.
#' @param ask logical. Should the function be run in interactive mode? If TRUE
#' and more than one TSN is found for a species, the user is asked to choose
#' one. If FALSE, NA is returned for multiple matches.
#' @return A \code{data.frame} with the names supplied by the user, matching
#' common and scientific names, taxon author and year, taxonomic rank, status,
#' TSNs (taxonomic serial numbers) and ITIS urls.
#' 
#' @author Juergen Niedballa
#' 
#' @references \url{http://www.itis.gov/}
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' 
#' species_common <- c("Leopard Cat", "moonrat")
#' 
#' # ask = TRUE. Multiple matches for leopard cat will cause menu to pop up asking user input.
#' 
#' species.names.check1 <- checkSpeciesNames(speciesNames = species_common,
#'                                           searchtype   = "common",
#'                                           accepted     = TRUE,
#'                                           ask          = TRUE)
#' 2   # we choose entry 2
#' species.names.check1
#' 
#' 
#' # ask = FALSE. Multiple matches for leopard cat will cause NA.
#' 
#' species.names.check2 <- checkSpeciesNames(speciesNames = species_common,
#'                                           searchtype   = "common",
#'                                           accepted     = TRUE,
#'                                           ask          = FALSE)
#' species.names.check2
#' 
#' 
#' 
#' # search for scientific names
#' 
#' species_scientific <- c("Tragulus", "Prionailurus bengalensis")
#' 
#' species.names.check3 <- checkSpeciesNames(speciesNames = species_scientific,
#'                                           searchtype   = "scientific",
#'                                           accepted     = TRUE,
#'                                           ask          = TRUE)
#' species.names.check3
#' }
#' 
#' 
#' @export checkSpeciesNames
#' 
checkSpeciesNames <- function(speciesNames,
                              searchtype,
                              accepted = TRUE,
                              ask = TRUE
){

  if (!requireNamespace("taxize", quietly = TRUE)) {
    stop("Please install the package taxize to run this function")
  }
  if (!requireNamespace("ritis", quietly = TRUE)) {
    stop("Please install the package ritis to run this function")
  }
  # check input
  searchtype <- match.arg(searchtype, choices =  c("scientific", "common"))
  stopifnot(is.logical(accepted))
  stopifnot(is.character(speciesNames) | is.factor(speciesNames))
  speciesNames <- unique(as.character(speciesNames))

  file.sep <- .Platform$file.sep

  # query ITIS TSN (taxnonomic serial number)
  tsns <- try(
    taxize::get_tsn(sci_com    = speciesNames,
                    searchtype = searchtype,
                    accepted   = accepted,
                    ask        = ask,
                    messages   = FALSE)
  )
  
  # # NOTE: possible alternative
  # if(searchtype == "common") {
  #   search_common(speciesNames)
  # }
  # if(searchtype == "scientific") {
  #   search_common(search_scientific)
  # }
  
  
  # if try returned an error, end function
  if(inherits(tsns, "try-error")) {
    message(paste("error in get_tsn. Exiting without results:\n", 
                  tsns, sep = ""))
    return(invisible(NULL))
  }
  

  tsns <- taxize::as.tsn(unique(tsns), check = FALSE)    # remove duplicates

  # warning if a name was not found
  if(any(is.na(tsns))){
    not.matched <- which(is.na(tsns))
    warning(paste("found no matches for", length(not.matched), "name(s):\n",  paste(speciesNames[not.matched], collapse = ", ")), immediate. = TRUE, call. = FALSE)
    tsns_worked <- taxize::as.tsn(tsns[-not.matched], check = FALSE)
  } else {
    tsns_worked <- tsns
  }

  if(length(tsns_worked) >= 1){

    scientific <- common <- author <- rankname  <- taxon_status <- data.frame(matrix(NA,
                                                                                     nrow = length(tsns_worked),
                                                                                     ncol = 2),
                                                                              stringsAsFactors = FALSE)

    colnames(scientific)   <- c("tsn", "combinedname")
    colnames(common)       <- c("tsn", "commonName")
    colnames(author)       <- c("tsn", "authorship")
    colnames(rankname)     <- c("tsn", "rankname")
    colnames(taxon_status) <- c("tsn", "taxonUsageRating")


    for(i in 1:length(tsns_worked)){

      scientific_tmp   <- ritis::scientific_name  (tsns_worked[i])   # get scientific names
      common_tmp       <- ritis::common_names     (tsns_worked[i])   # get common names
      author_tmp       <- ritis::taxon_authorship (tsns_worked[i])   # get author
      rankname_tmp     <- ritis::rank_name        (tsns_worked[i])   # get rank name

      if("tsn" %in% colnames(scientific_tmp)) {
        scientific[i,] <- scientific_tmp [c("tsn", "combinedname")]
      }

      if("tsn" %in% colnames(common_tmp)) {
        # if more than 1 common name, condense
        if(table(common_tmp$tsn) > 1) {
          common2 <- tapply(common_tmp$commonName, INDEX = common_tmp$tsn, FUN = paste, collapse = file.sep)
          common_tmp <- data.frame(commonName = common2,
                                   tsn        = rownames(common2),
                                   stringsAsFactors = FALSE)
        }
        common[i,] <- common_tmp [,c("tsn", "commonName")]
      }

      if("tsn" %in% colnames(author_tmp)) {
        author[i,] <- author_tmp [c("tsn", "authorship")]
      }

      if("tsn" %in% colnames(rankname_tmp)) {
        rankname[i,] <- rankname_tmp [c("tsn", "rankname")]
      }

      if(accepted == FALSE){
        taxon_status_tmp <- ritis::core_metadata (tsns_worked[i])  # get taxonomic status

        if("tsn" %in% colnames(taxon_status_tmp)) {
          taxon_status[i,] <- taxon_status_tmp [c("tsn", "taxonUsageRating")]
        }
      }
    }


    # make outtable
    dat.out <- data.frame(user_name       = speciesNames,
                          tsn             = as.numeric(tsns))


    dat.out <- merge(x = dat.out, y = scientific, by = "tsn", all.x = TRUE, sort = FALSE)
    dat.out <- merge(x = dat.out, y = common,     by = "tsn", all.x = TRUE, sort = FALSE)
    dat.out <- merge(x = dat.out, y = author,     by = "tsn", all.x = TRUE, sort = FALSE)
    dat.out <- merge(x = dat.out, y = rankname,   by = "tsn", all.x = TRUE, sort = FALSE)

    dat.out$itis_url <- NA
    dat.out$itis_url [match(tsns_worked, dat.out$tsn)]       <- attributes(tsns_worked)$uri

    colnames(dat.out)[colnames(dat.out) == "combinedname"] <- "scientificName"

    if(accepted == FALSE){
      dat.out <- merge(x = dat.out, y = taxon_status, by = "tsn", all.x = TRUE, sort = FALSE)
    } else {
      dat.out$taxon_status[!is.na(dat.out$tsn)]  <- "valid"
    }

    dat.out.sorted <- dat.out[match(dat.out$user_name, speciesNames),]
    
    return(dat.out.sorted)

  } else {stop("found no TSNs for speciesNames", call. = FALSE)}   # error if no tsns found
}
