checkSpeciesNames <- function(speciesNames,
                              searchtype,
                              accepted = TRUE,
                              ask = TRUE
){

  # check input
  if(searchtype  %in% c("scientific", "common") == FALSE) stop ("'searchtype' must be 'scientific' or 'common'")
  stopifnot(is.logical(accepted))
  stopifnot(is.character(speciesNames) | is.factor(speciesNames))
  speciesNames <- unique(as.character(speciesNames))

  file.sep <- .Platform$file.sep

  # query ITIS TSN (taxnonomic serial number)
  tsns <- get_tsn(searchterm = speciesNames,
                  searchtype = searchtype,
                  verbose    = FALSE,
                  accepted   = accepted,
                  ask        = ask)

  tsns <- as.tsn(unique(tsns), check = FALSE)    # remove duplicates

  # warning if a name was not found
  if(any(is.na(tsns))){
    not.matched <- which(is.na(tsns))
    warning(paste("found no matches for", length(not.matched), "name(s):\n",  paste(speciesNames[not.matched], collapse = ", ")), immediate. = TRUE, call. = FALSE)
    tsns_worked <- as.tsn(tsns[-not.matched], check = FALSE)
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

    return(dat.out)

  } else {stop("found no TSNs for speciesNames", call. = FALSE)}   # error if no tsns found
}