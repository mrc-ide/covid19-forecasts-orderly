#' SerialInterval
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
#' 
# SI
SI_gamma_dist_EpiEstim <- function(mu,si_std,SItrunc){
  SI_Distr <- EpiEstim::discr_si(seq(0, SItrunc), mu, si_std) # sapply(0:SItrunc, function(e) EpiEstim::DiscrSI(e,mu,mu*cv) )
  SI_Distr <- SI_Distr / sum(SI_Distr)
  return(list(dist = SI_Distr, SItrunc = SItrunc))
}

