######################################################################
## Negative-binomial posterior and APE score
# From: Parag, KV, and Donnelly, CA. (2019) “Optimising Renewal Models for
# Real-Time Epidemic Prediction and Estimation” BioRxiv: 835181.
######################################################################

# Assumptions
# - uses Poisson renewal equation (as in EpiEstim)
# - gamma prior required on R => negative binomial predictions

# Inputs - window length (k), SI distribution (sidistr), total infectiousness (Lday),
# incidence (Iday), max time (tPts), confidence 100*c(a, 1-a)%, gamma shape-scale (Rprior),
# start time for all windows (trunc)

# Output - APE score (ape), prob of next data (prob), R(t) estimate mean and confidence (Rhat, Rhatci),
# I(t+1) prediction mean and confidence (Inexhat, Inexci)

apePredPost <- function(k, sidistr, Lday, Iday, Rprior, a, trunc){

  # Length of time vector to consider
  nPts = length(Iday)
  # Range of index time points - starts from trunc
  ir = seq(trunc, nPts-1)

  # Grouped incidence and infectiousness
  B = rep(0, nPts-1); A = B;

  # Offset Lday so for next point i.e. t+1
  #Lday = Lday[2:length(Lday)]

  # At each time before compute historical R
  for(i in seq(1, nPts-1)){
    # Most ancient window time, truncate if negative
    idlast = max(i - k + 1, 1)
    # Look-back window of k (or less) indices
    idback = seq(i, idlast, -1)
    # Relevant incidence sum (B) and total infectiousness sum (A)
    B[i] = sum(Iday[idback]); A[i] = sum(Lday[idback]);
  }

  # Shape-scale gamma R(t) parameters
  alpha = Rprior[1] + B
  beta = 1./(1/Rprior[2] + A)

  # Truncate vectors
  alpha = alpha[ir]; beta = beta[ir]

  # Posterior predictive negative binomial parameter
  pr = Lday[ir]*beta; pr = pr/(1 + pr)

  # Posterior mean estimate of R(t)
  Rhat = alpha*beta
  # Confidence interval (95%) on R(t) estimate
  Rhatci = matrix(-1, 2, length(Rhat))
  Rhatci[1,] = qgamma(a, shape = alpha, scale = beta)
  Rhatci[2,] = qgamma(1-a, shape = alpha, scale = beta)

  # Posterior mean prediction of I(t+1)
  Inexhat = Lday[ir]*Rhat # check Lam[i] is for i+1 prediction <---
  #Inexhat = qnbinom(0.5, size = alpha, prob = 1-pr)

  # Confidence interval (95%) on I(t+1) projection
  Inexci = matrix(-1, 2, length(Inexhat))
  Inexci[1,] = qnbinom(a, size = alpha, prob = 1-pr)
  Inexci[2,] = qnbinom(1-a, size = alpha, prob = 1-pr)

  # Probability of next incidence value at t+1s
  prob = dnbinom(Iday[ir+1], size = alpha, prob = 1-pr)
  # APE score at this k, assume log 1/0 = 0 <----- check
  ape = -sum(log(prob[prob != 0]))
  # PMSE score
  pmse = sum((Inexhat - Iday[ir+1])^2)

  # Main outputs including parameters
    apePredPost = list(
        ape = ape,
        pmse = pmse,
        prob = prob,
        rhat = Rhat,
        rhatci = Rhatci,
        post_mean_tplus1 = Inexhat,
        tplus1_ci = Inexci,
        alpha = alpha,
        beta = beta,
        post_negbin_pr = pr
    )
}
