######################################################################
## Compute APE estimate and get confidence
# From: Parag, KV, and Donnelly, CA. (2019) “Optimising Renewal Models for
# Real-Time Epidemic Prediction and Estimation” BioRxiv: 835181.
######################################################################

# Assumptions
# - uses Poisson renewal equation
# - gamma prior required on R

# Inputs - incidence curve (Iday), SI distribution (sidistr), total infectiousness (Lday)
# gamma prior R hyperparameters [a b] (Rprior), confidence (a)
# Output - list of best model and best window size for APE and PMSE

apeEstim <- function(Iday, sidistr, Lday, Rprior, a, trunctime, idStr){

  # Decide to plot
  wantPlot = 0

  # No. time points considered
  tday = length(Iday)
  # Possible window lengths
  k = seq(7, tday-trunctime); lenk = length(k)
  print(paste0(c('Windows from:', k[1], 'to', k[lenk]), collapse = ' '))

  # APE scores and outpute
  ape = rep(0, lenk); pmse = ape; apeSet = list()

  # For every window length compute R estimate and I predictions
  for(i in 1:lenk){
    # Compute APE(k), output is [[ape, pmse, prob, Rhat, Rhatci, Inex, Inexci]]
    apeSet[[i]] = apePredPost(k[i], sidistr, Lday, Iday, Rprior, a, trunctime)
    # Metrics for squared error
    ape[i] = apeSet[[i]][[1]]; pmse[i] = apeSet[[i]][[2]]
  }

  # Optimal model and estimates/predictions
  best1 = which.min(ape); kbest1 = k[best1]
  modBest1 = apeSet[[best1]]
  best2 = which.min(pmse); kbest2 = k[best2]
  modBest2 = apeSet[[best2]]

  # Plot ape curve vs k
  if(wantPlot){
    # setEPS()
    # postscript(paste0(c('ape_pmse_', idStr, '.eps'), collapse = ''))

    quartz()
    par(mfrow=c(2,1))
    plot(k, ape, type = "l", bty = 'l', lwd = 2, col='red',
         xlab = paste0("k (k* = ", kbest1, ")"), ylab = 'APE')
    lines(c(kbest1, kbest1), c(min(ape),max(ape)), col = 'black', type = "h", lwd = 2)

    plot(k, pmse, type = "l", bty = 'l', lwd = 2, col='red',
         xlab = paste0("k (k* = ", kbest2, ")"), ylab = 'PMSE')
    lines(c(kbest2, kbest2), c(min(pmse),max(pmse)), col = 'black', type = "h", lwd = 2)
    dev.copy2eps(file=paste0(c('ape_pmse_', idStr, '.eps'), collapse = ''))
  }

  # Output is best model and k
  print(paste0(c('Best k [ape pmse] = ', kbest1, 'and', kbest2), collapse = ' '))
    apeEstim = list(
      best_k_ape = kbest1,
      best_set_ape = modBest1,
      best_k_pmse = kbest2,
      best_set_pmse = modBest2
    )
}
