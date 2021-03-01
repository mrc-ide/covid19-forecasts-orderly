#' get likelihood
#'
#' get Poisson likelihood
#' internal to the MCMC
#'
#' @param lambda: 'force of infection' matrix (incidence weighted by serial interval),
#'                  column number of days, row: number of locations
#'
#' @param I matrix of observed incidence, same dimension as lambda
#'
#' @param R0 vector of reproduction numbers per locations
#'
#' @details  L log likelihood
#' @export
#'

Like1<-function(lambda,I,R0){
  R <- matrix(R0,length(R0),ncol(I))
  L <- rowSums(-R*lambda+I*log(R*lambda),na.rm=TRUE) # poisson likelihood (or bits we are interested in!)
  # L <- sum(rowSums(-R*lambda+I*log(R*lambda),na.rm=TRUE),na.rm=TRUE) # poisson likelihood (or bits we are interested in!)
  ## debugging, turn off later.
  ##L <- rep(1, length(R0))
  return(L)
}

