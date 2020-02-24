\name{checkSpeciesNames}
\alias{checkSpeciesNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check species names against the ITIS taxonomic database
}
\description{
The function checks species names (common or scientific names) provided by the user with the ITIS taxonomic database (\url{http://www.itis.gov/}) via functions from the package \pkg{taxize}. It returns both common and scientific names, the taxon authors, taxon rank name and status, the TSN (taxonomic serial numbers) and ITIS urls.

}
\usage{
checkSpeciesNames(speciesNames,
                  searchtype,
                  accepted = TRUE,
                  ask = TRUE
)
}

\arguments{
  \item{speciesNames}{
  character. Vector of species names to check. Either common names or scientific names.
}
  \item{searchtype}{
  character. Type of names specified in \code{speciesNames}. One of 'scientific' or 'common'.
}
  \item{accepted}{
  logical. Return only accepted valid names? If TRUE, invalid names are returned as NA. Set to FALSE to return both accepted and unaccepted names.
}
  \item{ask}{
  logical. Should the function be run in interactive mode? If TRUE and more than one TSN is found for a species, the user is asked to choose one. If FALSE, NA is returned for multiple matches.
}

}

\details{
Arguments \code{searchtype}, \code{accepted} and \code{ask} are passed on to \code{\link[taxize]{get_tsn}}.
}

\value{
A \code{data.frame} with the names supplied by the user, matching common and scientific names, taxon author and year, taxonomic rank, status, TSNs (taxonomic serial numbers) and ITIS urls.
}

 \references{
\url{http://www.itis.gov/}
 }

\author{
Juergen Niedballa
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

\dontrun{


species_common <- c("Leopard Cat", "moonrat")

# ask = TRUE. Multiple matches for leopard cat will cause menu to pop up asking user input.

species.names.check1 <- checkSpeciesNames(speciesNames = species_common,
                                          searchtype   = "common",
                                          accepted     = TRUE,
                                          ask          = TRUE)
2   # we choose entry 2
species.names.check1


# ask = FALSE. Multiple matches for leopard cat will cause NA.

species.names.check2 <- checkSpeciesNames(speciesNames = species_common,
                                          searchtype   = "common",
                                          accepted     = TRUE,
                                          ask          = FALSE)
species.names.check2



# search for scientific names

species_scientific <- c("Tragulus", "Prionailurus bengalensis")

species.names.check3 <- checkSpeciesNames(speciesNames = species_scientific,
                                          searchtype   = "scientific",
                                          accepted     = TRUE,
                                          ask          = TRUE)
species.names.check3
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
