#' MCMC iterate
#'
#' run the MCMC to sample posterior of R and initial coniditions at each location
#' FYI: this is called internally by adapt_tuning
#'
#' @param incidence the incidence for the time window during which we assume Rt to be constant.
#'           I is a dataframe, first column are dates then incidence for all locations
#'           nb of row is the size of time widows, dates must be sequential
#'
#' @param N_geo integer of  numbers of locations
#'
#' @param iter integer, the number of iteration for the MCMC
#'
#' @param theta0 vector of inital parameters, here taken from the last MCMC iteration after tuning (save some burn-in)
#'
#' @param s variance of proposal distributions (log-normal) - tuned previously
#'
#' @param SI Serial interval distribution (see SI_gamma_dist_EpiEstim)
#'
#' @param mu0: initial conidtions to guaranty that if R=1, then we predict the number of cases in the future will stablise at the mean number of cases observed in the time window
#'              mu0 is also used as the mean of the (exponential) prior for intial conditions estimated
#'
#' @details  res a list containing 2 matrices: theta: matrix of posterior samples
#'                      and logL: matrix of associated log-likelihood
#' @export
#'

MCMC_iter <- function(incidence,N_geo,iter,theta0,s,SI,mu0,over_disp = NA, upper_log_i0){

  I <- incidence[,-1]     # remove dates (everything needs to be consecutive days)

  #############################################################################
  # for MCMC
  thetas <- matrix(NA,iter,N_geo*2)  # Rt's and initial conditions for each location
  L <- thetas                         # store likelihoods

  #############################################################################
  # get the daily 'forces of infections'
  lambda <- lambda_fct(param = theta0 , I = t(I), N_l = N_geo ,
                       ws = rev(SI$dist) , SItrunc = SI$SItrunc)

  L1 <- Like1(lambda,t(I),theta0[1:N_geo])

  L[1,] <- rep(L1,2)
  thetas[1,] <- theta0

  #############################################################################
  # sampling
  for (i in 2:iter){
    #print(i)
    for (j in 1:2){
      Ts <- theta0
      lambdaT <- lambda

      J <- (j-1)*N_geo+(1:N_geo)
      # propose new parameter j
      if (j <= 1){
        Ts[J] <- Ts[J]*exp(s[J]*rnorm(N_geo, 0, 1))
        idx <- which(Ts[J] > 10)
        Ts[J][idx] <- theta0[J][idx]

      }else{
        Ts[J] <- Ts[J]+(s[J]*rnorm(N_geo, 0 , 1))
        idx <- which(Ts[J] > upper_log_i0)
        Ts[J][idx] <- theta0[J][idx]
      }
      # get the new 'force of infection'
      lambdaT <- lambda_fct(param = Ts , I = t(I), N_l = N_geo ,
                            ws = rev(SI$dist) , SItrunc = SI$SItrunc)
      # get the likelihood for proposae value

      Lint <- Like1(lambdaT,t(I),Ts[1:N_geo])

      ## get the ratio with previous value of parameter and correct
      ## for porposal (and, only for initial conditions,
      ## prior distribution
      if (j <= 1){
        r <- exp(Lint-L1)*Ts[J]/theta0[J]
      }else{
        r <- exp(Lint-L1)   #*exp(1/mu0[j-N_geo]*(theta0[j]-Ts[j])) # with weak exponential prior
      }
     # accept or reject
      for (h in 1:N_geo){
        if (runif(1,0,1) <= r[h]) {
          theta0[J[h]] <- Ts[J[h]]  # if accept, keep new parameter value
          lambda[h,] <- lambdaT[h,]   # if accept, keep new force of infection
          L1[h] <- Lint[h]          # if accept, keep new lieklihood
        }
      }
      L[i,J] <- L1    # store final likelihood
    }
    thetas[i,] <- theta0  # store final parameter values for this iteration
  }

  ####
  res <- list(theta = thetas, logL = L)
  return(res)
}
