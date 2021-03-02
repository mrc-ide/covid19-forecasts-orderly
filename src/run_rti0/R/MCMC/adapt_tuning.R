#' adaptative tuning
#'
#' tune proposal and give good initial values to start 'proper' MCMC
#'  try to tune variance toward 20% acceptance
#'
#' @param repli number of time the variance of the proposal is tuned (10 tends to be ok)
#'
#' @param within_iter iterations for evaluate the accpetance with new proposal variances
#'
#' @param sigma original variance to start with
#'
#' @param others same as in MCMC_iter function
#'
#'
#' @details res list of 2 vectors: theta0: posterior samples at the last iterations
#'                       sigma: new variance for the proposal distribution
#' @export
#'
## adaptative tuning
# tune proposal and give good initial values to start 'proper' MCMC
# repli: number of time the variance of the proposal is tuned (10 tend to be ok)
# within_iter: iteration for evaluate the accpetance with new proposal variances
# sigma: original variance to start with
# others: as in MCMC_iter
adapt_tuning <- function(repli,I,N_geo,within_iter,theta0,sigma,SI,mu0,
                         over_disp = NA, upper_log_i0,
                      lower_log_i0, lower_r, upper_r) {

  new_sigma <- sigma
  for (i in 1:repli){
    # run MCMC with wthin_iter iterations
    res <- MCMC_iter(incidence = I ,
                     N_geo = N_geo,
                     iter = within_iter,
                     theta0 = theta0,
                     s = new_sigma,
                     SI = SI,
                     mu0 = mu0,
                     over_disp = over_disp, upper_log_i0)

    # colSums(diff(res$theta)!=0)/(within_iter-1)
    # tune the variance according to accpetance
    new_sigma <- adapt(theta = res$theta,
                       s = new_sigma,
                       it = within_iter)

    # new starting value for the parameters for the next round of MCMC
    theta0 <- res$theta[within_iter,]
    # plot(res$theta[,2],ylim=c(0,50))
  }
  res <- list(theta0 = theta0,
              sigma = new_sigma)
}


