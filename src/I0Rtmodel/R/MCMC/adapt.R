#' tuning
#'
#' tune variance proposal
#'  try to tune variance toward 20% acceptance
#' 
#' @param theta matrix, samples of posterior distribution. ncol: nb parameters, nrow: nb samples
#' 
#' @param s vector, proposal variances used to obtain posterior samples theta
#'                   
#' @param it integer, nro of theta
#'
#' 
#' @details lambda incidence weighted by serial interval
#' @export

adapt <- function(theta,s,it){
  Acc <- colSums(diff(theta)!=0)/(it-1)   # current acceptance rate
  s_out <- Acc/.2*s                       # reduce variance if accptance was too low, or increase variance if accpetance was too high (work well!)
  s_out[which(Acc==0)] <- s[which(Acc==0)]/2   # if never accpeted half the variance of the proposal 
  return(s_out)
}

