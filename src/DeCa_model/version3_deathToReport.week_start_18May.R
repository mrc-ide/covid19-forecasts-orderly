## ----options, include = FALSE, message = FALSE, warning = FALSE, error = FALSE----
set.seed(1)
library(knitr)
library(Hmisc)
library(EpiEstim)



## -----------------------------------------------------------------------------

week_ending <-  as.Date(week_ending)
# delay_report_death <- 10 # need checking!!
mu_delta <- 10
s_delta <- 2

day.project <- 7
t.window.range <- 7

rep <- 2e4



## ------------------------------------------------------------------
infile <- glue::glue(
  "{dirname(covid_19_path)}/model_inputs/data_{week_ending}.rds"
)
input_data <- readRDS(file = infile)

ascertainr_deaths <- input_data$D_active_transmission
ascertainr_cases <- input_data$I_active_transmission

countries <- purrr::set_names(input_data$Country, input_data$Country)

N_geo <- length(countries)
# week_ending <- d$week_ending



## -----------------------------------------------------------------------------

SI_gamma_dist_EpiEstim <- function(mu,si_std,SItrunc){
  SI_Distr <- EpiEstim::discr_si(seq(0, SItrunc), mu, si_std) # sapply(0:SItrunc, function(e) EpiEstim::DiscrSI(e,mu,mu*cv) )
  SI_Distr <- SI_Distr / sum(SI_Distr)
  return(list(dist = SI_Distr, SItrunc = SItrunc))
}


trunc <- 30

## checked that this gives the same result as Pierre's function
report_to_death_distr <- function(mu, std, trunc) {
  out <- EpiEstim::discr_si(seq(0, trunc), mu, std)
  out / sum(out)
}

report_to_death <- report_to_death_distr(
  mu = mu_delta, std = s_delta, trunc = trunc
)

ggplot() + geom_col(aes(x = 0:30, y = w_delta$dist))



## -------------------------------------------------------------------
## Checked that this gives exactly the same results as Pierre's code
weighted_cases <- purrr::map(
  countries,
  function(country) {
    message(country)
    x <- matrix(abs(ascertainr_cases[[country]]), ncol = 1)
    ascertainr::weighted_incid(
      incid = x, weights = report_to_death, trunc = SItrunc
    )
  }
)

## -------------------------------------------------------------------
library(binom)
n_post <- 1e4
# check posterior
# binconf(x = 5, n = 10, method = 'exact')
# hpd <- binom.bayes(
#   x = 5, n = 10.5, type = "central", conf.level = 0.95, tol = 1e-9)
# x <- rbeta(n = 1e4, shape1 = hpd$shape1, shape2 = hpd$shape2)
# c(mean(x), quantile(x,c(.025,.975)))
temp <- D
temp[,-1] <- NA
# do inference
r_it<-array(NA, dim = c(nrow(I), N_geo,n_post))
summary_r <- list(median = temp, low = temp, high = temp)


t.window <- 7

for (i in 1:nrow(I)){
  f <- seq(max(c(1,i-t.window+1)),i)

  for (j in 1:N_geo){

    if (sum(weighted_I[f,j]) > 0){

      hpd <- binom.bayes( x = sum(D[f,j+1]), n = sum(weighted_I[f,j]),
                          type = "central", conf.level = 0.95, tol = 1e-9)

      r_it[i,j,] <- rbeta(n = n_post, shape1 = hpd$shape1, shape2 = hpd$shape2)
      if (sum(D[f,j+1]) > sum(weighted_I[f,j])) r_it[i,j,] <- 1

      summary_r$median[i,j+1] <- median(r_it[i,j,],na.rm = TRUE)
      summary_r$low[i,j+1] <- quantile(r_it[i,j,],.025,na.rm = TRUE)
      summary_r$high[i,j+1] <- quantile(r_it[i,j,],.975,na.rm=TRUE)
    }
    # c(mean(x), quantile(x,c(.025,.975)))
  }
}


deaths_to_cases <- purrr::map(
  countries,
  function(country) {
    message(country)
    x <- weighted_cases[[country]]
    deaths <- matrix(ascertainr_deaths[[country]], ncol = 1)
    ascertainr::ratio_deaths_cases(
       wtd_incid = x, deaths = deaths, nsamples = 10000
    )
  }
)

quantiles_to_df <- function(mat, probs = c(0.025, 0.50, 0.975)) {
  qntls <- t(apply(mat, 1, quantile, probs = probs, na.rm = TRUE))
  out <- data.frame(qntls)
  colnames(out) <- scales::percent(probs, accuracy = 0.1)
  out
}
## These are in the same ball-park, but not the same as Pierre's code
## for instance, last 6 median values from Pierre' code are:
## 0.4007925 0.3961255 0.3724869 0.5057754 0.5104002 0.4044060
## while from my code are:
## 0.3826683 0.3846517 0.3622162 0.4735149 0.5226517 0.4538020
## could be due to stochastic nature of binom.bayes which doesn't
## accept a seed as far as I can see.
deaths_to_cases_qntls <- purrr::imap_dfr(
  deaths_to_cases,
  function(rit, country) {
    message(country)
    df <- quantiles_to_df(rit)
    scaled_deaths <- ascertainr_deaths[[country]] /
      max(ascertainr_deaths[[country]], na.rm = TRUE)

    scaled_cases <- ascertainr_cases[[country]] /
      max(ascertainr_cases[[country]], na.rm = TRUE)

    ## Align deaths on day t with cases on day t - 10
    scaled_cases <- stats::lag(scaled_cases, n = round(mu_delta))

    df <- cbind(
      date = ascertainr_deaths[["dates"]],
      scaled_deaths = scaled_deaths,
      scaled_cases = scaled_cases,
      df
    )
    df
  }, .id = "country"
)

## ---- plotRatioCD-------------------------------------------------------------

f <- which(
  summary_r$median$dates >= as.Date(c('01/03/2020'),format = '%d/%m/%Y')
)

results_plot_death_case <- list()

layout(matrix(1:4,2,2))
for (i in 1:N_geo){

  plot(summary_r$median$dates,summary_r$median[,i+1],lwd = 2,
       #ylim = c(0,max(a[f,2:4],na.rm=TRUE)),
       ylim=c(0,1),
       type='l',
       xlim = c(as.Date(c('01/03/2020'),format = '%d/%m/%Y'),
                week_ending+4),
       bty ='n',
       main = snakecase::to_any_case(country[i], case = "title"),
       col = rgb(0,0,1),
       xlab = '', ylab = 'ratio D to C',
       xaxt="n")
  axis(1, at=as.Date(c('2020-03-01','2020-03-15','2020-04-01','2020-04-15','2020-05-01'),format = '%Y-%m-%d'),
       labels = c('2020-03-01','2020-03-15','2020-04-01','2020-04-15','2020-05-01'),las=1)

  polygon(c(summary_r$median$dates,rev(summary_r$median$dates)),
          c(summary_r$low[,i+1],rev(summary_r$high[,i+1])),
          border = NA,
          col = rgb(0,0,1,0.2))
  f2 <- which( I$dates %in% summary_r$median$dates)

  f2<-f2[f]
  inc <- cbind(I[f2-round(mu_delta),i+1],D[f2,i+1])
  lines(summary_r$median$dates[f],
        inc[,1]/max(c(inc[,1],inc[,2])),
        type = 'p', pch=16,col='black')

  lines(summary_r$median$dates[f],
        inc[,2]/max(c(inc[,1],inc[,2])),
        type = 'p', pch=16,col='red')

  if(country[i] == "Canada"){
    legend('topleft',legend = c('ratio','death','reported cases'),bty='n',
           lwd=c(3,NA,NA),pch=c(NA,16,16),col = c(rgb(0,0,1),rgb(1,0,0),rgb(0,0,0)))
  }

  temp1 <- data.frame(dates = summary_r$median$dates,
                     median_ratio = summary_r$median[,i+1],
                     low_ratio = summary_r$low[,i+1],
                     up_ratio = summary_r$high[,i+1])
  temp2 <- data.frame(dates = summary_r$median$dates[f],
                     I_t_minus_meanDelay = inc[,1],
                     D_t = inc[,2])
  temp <- merge(temp1,temp2)
  results_plot_death_case[[country[i]]] <- temp
}

saveRDS(
  object = results_plot_death_case,
  file =  paste0('DeCa_Std_Ratio_plot_', week_ending,'.rds' )
)


## -----------------------------------------------------------------------------
# # check parameters of beta dist
# shape1 <- 2
# shape2 <- 5
# qbeta(.025, shape1 = shape1, shape2 = shape2)
# qbeta(.975, shape1 = shape1, shape2 = shape2)
# shape1/(shape1+shape2)
# x <- rbeta(1e4,shape1 = shape1,shape2 = shape2)
# c(mean(x),quantile(x,c(.025,.975)))
# hist(rbeta(1e4,shape1 = shape1,shape2 = shape2))


#parameters
CFR_esti <- c(1.38, 1.23, 1.53)/100
# function to get parameters
f1 <- function(shape){
  res <- c(shape[1]/(shape[1]+shape[2]),
           qbeta(.025, shape1 = shape[1], shape2 = shape[2]),
           qbeta(.975, shape1 = shape[1], shape2 = shape[2]))
  res <- sum((res*100-CFR_esti*100)^2)
  return(res)
}

n <- 5e2
Shape1 <- rep(seq(300,350,length.out = n),each = n )
Shape2 <- rep(seq(22500,23500,length.out = n), n )
res <- rep(NA,n*n)
for (i in 1:(n*n)){
  res[i] <- f1(c(Shape1[i],Shape2[i]))
}
f <- which(res == min(res))
params <- c(Shape1[f],Shape2[f])

params
shape <- params

x <- rbeta(n_post,shape1 = params[1],shape2 = params[2])
# c(mean(x),quantile(x,c(.025,.975)))
c(shape[1]/(shape[1]+shape[2]),
           qbeta(.025, shape1 = shape[1], shape2 = shape[2]),
           qbeta(.975, shape1 = shape[1], shape2 = shape[2]))
(c(shape[1]/(shape[1]+shape[2]),
           qbeta(.025, shape1 = shape[1], shape2 = shape[2]),
           qbeta(.975, shape1 = shape[1], shape2 = shape[2])) - CFR_esti)*100

# sum((c(mean(x),quantile(x,c(.025,.975)))*100-CFR_esti*100)^2)
hist(x)

post_CFR <- x


## -----------------------------------------------------------------------------

# do inference
rho_it<-array(NA, dim = c(nrow(I), N_geo,n_post))
summary_rho <- list(median = temp,
                    low = temp,
                    high = temp)
for (i in 1:nrow(I)){
  for (j in 1:N_geo){

    rho_it[i,j,] <- post_CFR/r_it[i,j,]
    rho_it[i,j,which(rho_it[i,j,]>1)] <- 1
    summary_rho$median[i,j+1] <- median(rho_it[i,j,],na.rm = TRUE)
    summary_rho$low[i,j+1] <- quantile(rho_it[i,j,], 0.025,na.rm = TRUE)
    summary_rho$high[i,j+1] <- quantile(rho_it[i,j,], 0.975,na.rm=TRUE)

  }
}



## -----------------------------------------------------------------------------

f <- seq(1,nrow(D)-round(mu_delta))

I_true <- array(NA, dim = c(nrow(I), N_geo,n_post))
summary_I_true <- list(median = temp,
                    low = temp,
                    high = temp)
for (i in f){
  for (j in 1:N_geo){
    I_true[i,j,] <- D[i+round(mu_delta),j+1] + rnbinom(n = n_post, size = D[i+round(mu_delta),j+1] , prob = post_CFR)
    summary_I_true$median[i,j+1] <- median(I_true[i,j,],na.rm = TRUE)
    summary_I_true$low[i,j+1] <- quantile(I_true[i,j,],.025,na.rm = TRUE)
    summary_I_true$high[i,j+1] <- quantile(I_true[i,j,],.975,na.rm=TRUE)
  }
}




## -----------------------------------------------------------------------------
f <- seq(nrow(D)-round(mu_delta)+1,nrow(D))

for (i in f){
  for (j in 1:N_geo){
    I_true[i,j,] <- I[i,j+1] + rnbinom(n = n_post, size = I[i,j+1] , prob = rho_it[i,j,])
    summary_I_true$median[i,j+1] <- median(I_true[i,j,],na.rm = TRUE)
    summary_I_true$low[i,j+1] <- quantile(I_true[i,j,],.025,na.rm = TRUE)
    summary_I_true$high[i,j+1] <- quantile(I_true[i,j,],.975,na.rm=TRUE)
  }
}


## -----------------------------------------------------------------------------
sum(summary_I_true$median[,which(country %in% 'Spain')+1],na.rm=TRUE)/47e6
sum(summary_I_true$low[,which(country %in% 'Spain')+1],na.rm=TRUE)/47e6
sum(summary_I_true$high[,which(country %in% 'Spain')+1],na.rm=TRUE)/47e6



## -----------------------------------------------------------------------------

last_week <- data.frame(country = country,
                       median = rep(NA,N_geo),
                        low = rep(NA,N_geo),
                        high = rep(NA,N_geo))
for (i in 1:N_geo){
  last_week[i,c(2,3,4)] <- quantile(colSums(I_true[(nrow(D)-7+1):nrow(D),i,],na.rm=TRUE),c(.5,.025,.975),na.rm=TRUE)
}

# summary
summary_14days <- data.frame(
    country = country,
    deaths_to_reported_ratio = glue::glue(
        "{round(summary_r$median[nrow(D),-1], 3) }",
        " ({round(summary_r$low[nrow(D),-1],3)} - {round(summary_r$high[nrow(D),-1],3)})",
    ),
    estimated_reporting = glue::glue(
        "{scales::percent(as.numeric(round(summary_rho$median[nrow(D),-1], 3)), accuracy = 0.1)}",
        " ({scales::percent(as.numeric(round(summary_rho$low[nrow(D),-1],3)), accuracy = 0.1)} - {scales::percent(as.numeric(round(summary_rho$high[nrow(D),-1],3)), accuracy = 0.1)})",
    ),
    factor_to_real_size = glue::glue(
        "{round(1/summary_rho$median[nrow(D),-1], 2)}",
        " ({round(1/summary_rho$high[nrow(D),-1], 2)} - {round(1/summary_rho$low[nrow(D),-1], 2)})",
    ),
    Observed_case_last_week = colSums(tail(I[,-1],7)),
    Predicted_True_case_last_week = glue::glue(
        "{prettyNum(signif(last_week$median,digits = 3), big.mark = ",")} ",
        "({prettyNum(signif(last_week$low,digits = 3), big.mark = ",")}",
        " - {prettyNum(signif(last_week$high,digits = 3), big.mark = ",")})"
        )
)

summary_14days <- summary_14days[order(temp[,1],decreasing = TRUE),]
## TODO check why we have NAs
summary_14days <- na.omit(summary_14days)
readr::write_csv(
    x = summary_14days,
    path = paste0('../Team.output/summary_DeathToRepoted_14days_',week_ending,'.csv')
)



## -----------------------------------------------------------------------------
## unformatted
f <- function(df, col) {

    df <- tidyr::gather(df, country, !!col)
    df[[col]] <- round(df[[col]], 3)
    out <- data.frame(df[[col]])
    colnames(out) <- col
    out
}

unformatted_summary_14days <- data.frame(
    country = country,
    Observed_case_last_week = colSums(tail(I[,-1],7)),
    Predicted_True_case_last_week_50 = last_week$median,
    Predicted_True_case_last_week_025 = last_week$low,
    Predicted_True_case_last_week_975 = last_week$high
    )

tmp <- list(
    f(summary_r$median[nrow(D),-1], "deaths_to_reported_ratio_50"),
    f(summary_r$low[nrow(D),-1], "deaths_to_reported_ratio_025"),
    f(summary_r$high[nrow(D),-1], "deaths_to_reported_ratio_975"),
    f(summary_rho$median[nrow(D),-1], "estimated_reporting_50"),
    f(summary_rho$low[nrow(D),-1], "estimated_reporting_025"),
    f(summary_rho$high[nrow(D),-1], "estimated_reporting_975")
)

tmp <- do.call(what = 'cbind', args = tmp)
unformatted_summary_14days <- cbind(unformatted_summary_14days, tmp)
readr::write_csv(
    x = unformatted_summary_14days,
    path = paste0('../Team.output/unformatted_summary_DeathToRepoted_14days_',week_ending,'.csv')
)


## -----------------------------------------------------------------------------


I_lastweek <- colSums(tail(I[,-1],7))/7
sd_lastweek <- (apply(tail(I[,-1],7),2,sd))

cbind(I_lastweek,sd_lastweek)
Predictions <- list()
date_pre <- week_ending + seq(1,7,by=1)

for( i in 1:length(country)){


  repo <- matrix(sample(x = r_it[nrow(I),i,],size = 7*n_post,replace = TRUE),nrow = n_post, ncol = 7)
  I_aug <- matrix(rpois(n = 7*n_post, lambda = I_lastweek[i]),nrow = n_post, ncol = 7)

  param <- epitrix::gamma_mucv2shapescale(mu = I_lastweek[i], cv = sd_lastweek[i]/I_lastweek[i])
  # I_aug <- matrix(rpois(n = 7*n_post, lambda = I_lastweek[i]),nrow = n_post, ncol = 7)
  I_aug <- matrix(rgamma(n = 7*n_post, shape = param$shape, scale = param$scale),nrow = n_post, ncol = 7)


  I_old <- matrix( tail(I[,i+1],w_delta$SItrunc), nrow = n_post, ncol = w_delta$SItrunc, byrow = TRUE )
  I_augm <- cbind(I_old,I_aug)

  weighted_I_augm <- matrix(NA, nrow = n_post, ncol = ncol(I_augm))
  ws <- rev(w_delta$dist)

  for (j in 2:ncol(I_augm)){
    f <- max(c(1,(j-w_delta$SItrunc)))
    weighted_I_augm[,j] <- ((I_augm[,f:j])%*%ws[((SItrunc+1)-(j-f)):(SItrunc+1)])

  }
  weighted_I_augm <- weighted_I_augm[,(ncol(weighted_I_augm)-7+1) : (ncol(weighted_I_augm))]
  d_exp <- data.frame(matrix(NA,nrow = n_post,7))
  for (k in 1:7){
    d_exp[,k] <- rbinom(n = n_post, size = round(weighted_I_augm[,k]), prob = repo[,k])
  }
  names(d_exp) <- as.character(date_pre)
  Predictions[[as.character(country[i])]][[1]] <- d_exp
  Predictions[[as.character(country[i])]][[2]] <- d_exp

}


## -----------------------------------------------------------------------------

t.window <- 10

Rt_last <-list()
for (si in 1:2){

  SI <- gamma_dist_EpiEstim(si_mu =  d$si_mean[si],
                            si_std  = d$si_std[si],
                            SItrunc = 30)


  for (i in 1:N_geo){

    new_D <- apply(Predictions[[i]][[si]],2,median,na.rm=TRUE)
    obs <- c(D[,c(i+1)],new_D)

    epi_res <- EpiEstim::estimate_R(obs,method = 'non_parametric_si',
                                    config = EpiEstim::make_config(list(
                                      mean_prior = 1,
                                      si_distr = SI$dist,
                                      t_start = length(obs)-t.window+1,
                                      t_end = length(obs))))
    epi_res <- epi_res$R
    param <- epitrix::gamma_mucv2shapescale(mu = epi_res$`Mean(R)`,cv = epi_res$`Std(R)`/epi_res$`Mean(R)`)

    Rt_last[[as.character(country[i])]][[si]] <- rgamma(n = n_post, shape = param$shape, scale = param$scale)
  }
}




## -----------------------------------------------------------------------------

# temp <- list(rep(NA, n_post),rep(NA, n_post))
# Rt_last <-list()
# for (i in 1:N_geo){
#   Rt_last[[as.character(country[i])]] <- temp
# }


Std_results <- list(I_active_transmission = d$I_active_transmission,
                    D_active_transmission = d$D_active_transmission,
                    Country = d$Country,
                    Rt_last = Rt_last,
                    Predictions = Predictions)



saveRDS(object = Std_results,
        file = paste0('RData/DeCa_Std_results_week_end_',week_ending,'.rds' ))

# save.image(file = paste0('RData/Full_results_week_end_',week_ending,'.RData'))


