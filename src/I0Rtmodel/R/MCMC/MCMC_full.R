#' MCMC iterate
#'
#' run the MCMC to sample posterior of R and initial coniditions at each location 
#' FYI: this is called internally by adapt_tuning
#' 
#' @param I the incidence for the time window during which we assume Rt to be constant.  
#'           I is a dataframe, first column are dates then incidence for all locations
#'           nb of row is the size of time widows, dates must be sequential
#' 
#' @param N_geo integer of  numbers of locations
#'                   
#' @param iter integer, the number of iteration for the MCMC
#'
#' @param theta0 vector of inital parameters
#'
#' @param s variance of proposal distributions (log-normal) 
#' 
#' @param SI Serial interval distribution (see SI_gamma_dist_EpiEstim)
#' 
#' @param mu0: initial conidtions to guaranty that if R=1, then we predict the number of cases in the future will stablise at the mean number of cases observed in the time window
#'              mu0 is also used as the mean of the (exponential) prior for intial conditions estimated
#'              
#' @param repli number of time the variance of the proposal is tuned (10 tends to be ok)
#' 
#' @param within_iter iterations for evaluate the accpetance with new proposal variances
#' 
#' @details  res a list containing 2 matrices: theta: matrix of posterior samples
#'                      and logL: matrix of associated log-likelihood
#' @export
#' 

MCMC_full <- function(I,N_geo,iter,theta0,s,SI,mu0,repli_adapt,within_iter, over_disp = NA){
 
  res0 <- adapt_tuning(repli = repli_adapt,
                       I = I,
                       N_geo = N_geo,
                       within_iter = within_iter,
                       theta0 = theta0,
                       sigma = s,
                       SI = SI,
                       mu0 = mu0, 
                       over_disp = over_disp)
  # adaptative tuning bit: we run an MCMC with rep/10 iterations, then
  # adjust the proposal variance to reach 0.2
  # do again using parameter value from the last iteration of the previous MCMC
  # repeat 10 times
  # from experience, this is enough to tunes proposal variances well, but worth checking
  # see below for final acceptance rate output 
  # see Rscript/MCMC_Rt_2018.R for full function
  
  # print('halfway!')             # message halfway through (effectively, including tuning, we do 2xrep iterations)
  
  res <- MCMC_iter(incidence = I ,
                   N_geo = N_geo,
                   iter = iter,
                   theta0 = res0$theta0,
                   s = res0$sigma,
                   SI = SI,
                   mu0 = mu0,
                   over_disp = over_disp)
  # run the MCMC to sample posterior of R and initial coniditions at each location
  # FYI: this is called internally by adapt_tuning
  # see Rscript/MCMC_Rt_2018.R for full function
  # needs:
  # I: the incidence for the time window during which we assume Rt to be constant. 
  # N_geo: the number of locations
  # iter: the number of iteration for the MCMC
  # theta0: inital parameters, here taken from the last MCMC iteration after tuning (save some burn-in)
  # s: variance of proposal distribution (log-normal)
  # SI: the serial interval(use SI_gamma_dist_EpiEstim to define), or need to be a list with vector dist for the daily distribution and SItrunc: an integer for the threshold of serial interval, if SItrunc=40, then dist is 41 element long to include day 0
  # mu0: initial conidtions to guaranty that if R=1, then we predict the number of cases in the future will stablise at the mean number of cases observed in the time window
  # mu0 is also used as the mean of the (exponential) prior for intial conditions estimated
  
 
  return(res)
}

