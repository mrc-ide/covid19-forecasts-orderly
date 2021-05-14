######################################################################
## Compute APE estimate for a specific k
# From: Parag, KV, and Donnelly, CA. (2019) “Optimising Renewal Models for 
# Real-Time Epidemic Prediction and Estimation” BioRxiv: 835181.
######################################################################

# Assumptions
# - uses Poisson renewal equation
# - gamma prior required on R

# Inputs - incidence curve (Iday), SI distribution (sidistr), total infectiousness (Lday)
# gamma prior R hyperparameters [a b] (Rprior), confidence (a)
# Output - list of best model and best window size for APE and PMSE

apeSpecific <- function(Iday, sidistr, Lday, Rprior, a, trunctime, idStr, k){
  
  # Decide to plot
  wantPlot = 1
  
  # No. time points considered
  tday = length(Iday)
  # Possible window lengths
  print(paste0(c('Window is:', k), collapse = ' '))
  
  # Compute APE(k), output is [[ape, pmse, prob, Rhat, Rhatci, Inex, Inexcim alpha, beta, pr]]
  apeSet = apePredPost(k, sidistr, Lday, Iday, Rprior, a, trunctime)
  ape = apeSet[[1]]; pmse = apeSet[[2]]
  # Output results for k
  apeEstim = list(k, apeSet)
  
}