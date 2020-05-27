# serial interval
gamma_dist_EpiEstim <-function(si_mu,si_std,SItrunc){
  SI_Distr <- EpiEstim::discr_si(k = seq(0, SItrunc), mu = si_mu, sigma = si_std) # sapply(0:SItrunc, function(e) EpiEstim::DiscrSI(e,mu,mu*cv) )
  SI_Distr <- SI_Distr / sum(SI_Distr)
  return(list(dist = SI_Distr, SItrunc = SItrunc))
}
