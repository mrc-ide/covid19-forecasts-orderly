## ----options, include = FALSE, message = FALSE, warning = FALSE, error = FALSE----
set.seed(1)

day.project <- 7
t.window.range <- 10

iterations <- 5e4


indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

deaths_to_use = raw_data[["D_active_transmission"]]

country <- raw_data$Country
N_geo <- length(country)


## SI
### 1. Estimating R_t with EpiEstim
si_mean <- raw_data$si_mean[1]
si_std <- raw_data$si_std[1]
# CV_SI <- si_std / si_mean
SItrunc <- 20

## # serial interval estimate used: mean = 3.96, sd =  4.75
## # from Du et al. from The University of Texas at Austin and University of Hong Kong
## # (https://www.medrxiv.org/content/10.1101/2020.01.28.20019299v4.full.pdf)
## # (https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v2.full.pdf)
SI <- SI_gamma_dist_EpiEstim(
  mu = si_mean, si_std  = si_std, SItrunc = SItrunc
)

## -----------------------------------------------------------------------------

t.proj.start <- as.Date(week_ending) + 1

t.window <- seq(
  t.proj.start - t.window.range, t.proj.start - 1, 1
)

incidence_inference <- deaths_to_use[which(deaths_to_use$dates %in% t.window),]




## ----MCMC1, eval=TRUE, echo=TRUE----------------------------------------------

# initial proposal variances (they are now tuned!)
sigma_prop <- rep(0.1, N_geo * 2)
# initial incidence conditions
if ( N_geo > 1 ){
  mu0 <- as.numeric(log(colMeans(incidence_inference[,-1]) * si_mean))
} else {
  mu0 <- as.numeric(log(mean(incidence_inference[,-1]) * si_mean))
}
# initially, we assume R=1 and choose initial condition accordingly, i.e. with mu0 case and R=1
# we expect the number of daily cases to stabilised at the mean of the observed incidence in the
# time window of interest
# this is use for the prior of initial number of cases, i.e. as the mean of an exponential distribution
# in practice, the mu0 cases will happen 100 days before the start of the time windows
# initial parameter R=1 (time # of locations, and initial number of cases in the past)
theta0 <- c(rep(1, N_geo), mu0)

## ----forward1, eval=TRUE , echo= TRUE-----------------------------------------
# save.image(file = 'int_23MArch.RData')
                                        # load(file = 'int_23MArch.RData')
res <- MCMC_full(
  I = incidence_inference,
  N_geo = N_geo,
  iter = rep,
  theta0 = theta0,
  s = sigma_prop,
  SI = SI,
  mu0 = mu0,
  repli_adapt = 10,
  within_iter = iterations / 10
)

# acceptance rate (should be close to .2)
Acc <- colSums(diff(res$theta) != 0) / iterations
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


if (N_geo > 1) {
  # median and 95%CrI of Rs by locations
  R_est <- apply(
    res$theta[,1:N_geo], 2, quantile,c(.5,.025,.975)
  )
  I0_est <- apply(
    res$theta[,(N_geo+1):(2*N_geo)], 2, quantile,c(.5,.025,.975)
  )   # median and 95%CrI of I0
} else {
  R_est <- quantile(
    res$theta[,1], c(.5,.025,.975)
  )
  I0_est <- quantile(
    res$theta[,2], c(.5,.025,.975)
  )
}
# mu0
# save.image('test_on_simulation1.RData')             # saving so far


layout(matrix(1,1,1,byrow = TRUE))
errbar(1:N_geo,R_est[1,],R_est[2,],R_est[3,],
       xlab = '', ylab = 'R', bty = 'n',xaxt = "n")
       # xlab = '', ylab = 'R',ylim = c(0,3), bty = 'n',xaxt = "n")

lines(c(1,N_geo),rep(1,2), col = 'red')

axis(1, at=1:N_geo, labels=country,las=2)

### Project forward
NR_samples <- nrow(res$theta)/10
Nsim_per_samples <-  10

day_forward <- day.project + t.window.range






# forward projection

I_pred <- Proj_Pois(
  Results = res ,
  NR_samples = NR_samples,
  Nsim_per_samples = Nsim_per_samples,
  day_forward = day_forward,
  N_geo = N_geo,
  SI = SI
)

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

I_plot <- tail(I, 14)
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






## -----------------------------------------------------------------------------
results <- list(
  week_ending =raw_data$week_ending,
  Threshold_criterion_4weeks = raw_data$Threshold_criterion_4weeks,
  Threshold_criterion_7days = raw_data$Threshold_criterion_7days,
  I_active_transmission = raw_data$I_active_transmission,
  Country = raw_data$Country,
  res_MCMC = res,
  I_pred = I_pred,
  t.proj.start = t.proj.start,
  t.window.range = t.window.range,
  day.project = day.project,
  SI = SI
)



## -----------------------------------------------------------------------------
N_geo <- length(results$Country)

Rt_last <- data.frame(results$res_MCMC$theta[,1:N_geo])
names(Rt_last) <- results$Country
if (nrow(Rt_last) > 1e4){
  f <- round(seq(1,nrow(Rt_last),length.out = 1e4))
  Rt_last <- Rt_last[f,]
}


Predictions <- list()
for (i in 1:N_geo){
  temp <- results$I_pred[[i]]
  temp <- as.data.frame(temp[,8:14])
  names(temp) <- as.character(seq(results$t.proj.start,results$t.proj.start+7-1,1))
  if (nrow(temp)>1e4){
    f <- round(seq(1,nrow(temp),length.out = 1e4))
    temp <- temp[f,]
  }
  Predictions[[as.character(results$Country[i])]] <- temp
}

Std_results <- list(
  I_active_transmission = raw_data$I_active_transmission,
  D_active_transmission = raw_data$D_active_transmission,
  Country = results$Country,
  Rt_last = Rt_last,
  Predictions = Predictions
)


saveRDS(
  object = Std_results,
  file = paste0('RtI0_Std_results_week_end_',week_ending,'.SI1.rds' )
)

