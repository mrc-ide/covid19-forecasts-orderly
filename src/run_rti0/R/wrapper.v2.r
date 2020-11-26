
res <- MCMC_full(I = incidence_inference,
                 N_geo = N_geo,
                 iter = rep,
                 theta0 = theta0,
                 s = sigma_prop,
                 SI = SI,
                 mu0 = mu0,
                 repli_adapt = 10,
                 within_iter = rep/10)



Acc <- colSums(diff(res$theta)!=0)/rep          # acceptance rate (should be close to .2)
Acc



# plot traces
plot(res$logL[,1])                                  # of likelihood
layout(matrix(1:4,2,2,byrow = TRUE))
for (i in 1:N_geo) plot(res$theta[,i],main=paste0('R0',country[i]))              # of R's
for (i in 1:N_geo) {
  # plot(res$theta[,N_geo+i])        # of initial conditions
  plot(res$theta[,N_geo+i]*res$theta[,i],main=paste0('I0',country[i]))        # of initial conditions
}
# for (i in 1:N_geo) {
#   # plot(res$theta[,N_geo+i])        # of initial conditions
#   plot(res$theta[,i],res$theta[,N_geo+i]*res$theta[,i],main=country[i])        # of initial conditions
# }


if (N_geo>1){
  R_est <- apply(res$theta[,1:N_geo],2,
                 quantile,c(.5,.025,.975))   # median and 95%CrI of Rs by locations
  I0_est <- apply(res$theta[,(N_geo+1):(2*N_geo)],2,
                  quantile,c(.5,.025,.975))   # median and 95%CrI of I0 
  
}else{
  R_est <- quantile(res$theta[,1],c(.5,.025,.975))
  I0_est <- quantile(res$theta[,2],c(.5,.025,.975))
}
# mu0
# save.image('test_on_simulation1.RData')             # saving so far


layout(matrix(1,1,1,byrow = TRUE))
errbar(1:N_geo,R_est[1,],R_est[2,],R_est[3,],
       xlab = '', ylab = 'R', bty = 'n',xaxt = "n")
       # xlab = '', ylab = 'R',ylim = c(0,3), bty = 'n',xaxt = "n")
      
lines(c(1,N_geo),rep(1,2), col = 'red')

axis(1, at=1:N_geo, labels=country,las=2)
