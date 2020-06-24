#' 'force of infection' 
#'
#' return incidence weighted by serial interval for the time window of interest 
#' internal to the MCMC
#' 
#' @param param: 'force of infection' matrix (incidence weighted by serial interval),
#'                  column number of days, row: number of locations   
#'                  
#' @param I matrix of observed incidence, same dimension as lambda
#' 
#' @param N_l integer of  numbers of locations
#'
#' @param ws vector reversed serial interval distribution (output from SI_gamma_dist_EpiEstim reverse in the MCMC function) )
#'
#' @param SItrunc integer, threshold of serial interval distribution (see SI_gamma_dist_EpiEstim)
#'
#' 
#' @details lambda incidence weighted by serial interval
#' @export
#' 

lambda_fct<-function(param, I, N_l, ws, SItrunc){
  
  # reconstruct expected daily cases in the past
  I0 <- matrix(0,N_l,100)
  # I0[,1] <- param[(N_l+1):(2*N_l)] #/ (1-param[(1):(N_l)])
  I0[,1] <- exp(param[(N_l+1):(2*N_l)] )  #/ (1-param[(1):(N_l)])
  for (i in 2:100){
    f <- max(c(1,(i-SItrunc)))
    I0[,i] <- param[1:N_l]*(I0[,f:i]%*%ws[((SItrunc+1)-(i-f)):(SItrunc+1)])
    # I0[,i] <- param[1:N_l]*eigenMapMatMult(I0[,f:i],ws[((SItrunc+1)-(i-f)):(SItrunc+1)])
  }
  
  I_full <- cbind(I0,I)
  # predict future incidence weighted by serial interval for the time window (this multiplied by R give mean expected number of cases)
  lambda <- matrix(NA,N_l,ncol(I))
  for (i in 101:ncol(I_full)){
    
    lambda[,i-100] <- I_full[,(i-SItrunc):i]%*%ws
  }
  
  return(lambda)
}



