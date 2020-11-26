# forward projection

I_pred <- Proj_Pois(Results = res ,
                    NR_samples = NR_samples, 
                    Nsim_per_samples = Nsim_per_samples,
                    day_forward = day_forward,
                    N_geo = N_geo,
                    SI = SI)

# function to project forward, see Rscript/useful_functions.R
# needs the parameters estimated (Results)
# number of simulations
# week_forward: number of week projected from the START of the time window, SO: includes the period of time
# for which we have data!!! e.g. if we have 2 weeks of data and want to project for the 4 weeks 
# after that, week_forward should be 6.
# require number of locations + serial interval
#

# temp <- as.matrix(t(I_pred[,1,101:156]))
# I2 <- build_projections(t(temp), dates = NULL, cumulative = FALSE)


# plot the observed incidence since march, the time window of interest and the median and 95%CrI predictions

I_plot <- tail(I,14)
# incidence_inference
layout(matrix(1:4,2,2,byrow = TRUE))
for(i in 1:N_geo){
  CI_pred <- apply(I_pred[[i]],2,quantile,c(.5,.025,.975),na.rm=TRUE)
  plot(I_plot$dates,I_plot[,i+1],
       xlim=c(I_plot$dates[1], tail(incidence_inference$dates,1)+day.project), 
       ylim = c(0,1+max(c(incidence_inference[,1+i],as.vector(CI_pred)))),
       xlab = 'time', ylab = 'incidence', main =country[i],bty='n')
  
  lines(incidence_inference$dates,incidence_inference[,i+1], type = 'p',pch=16,col='black')
  
  x <- 1:ncol(CI_pred) + incidence_inference$dates[1] - 1
  lines(x ,CI_pred[1,],col='blue',lwd=2)
  polygon(c(x,rev(x)), c(CI_pred[2,],rev(CI_pred[3,])), 
          col = rgb(0,0,1,.2), border = FALSE )
  
  legend('topleft',legend = c('for inference'), pch=16,col='black',bty='n')
}

