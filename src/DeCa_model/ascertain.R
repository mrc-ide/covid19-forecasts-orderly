## ----options, include = FALSE, message = FALSE, warning = FALSE, error = FALSE----
set.seed(1)

## -----------------------------------------------------------------------------

week_ending <-  as.Date(week_ending)
# delay_report_death <- 10 # need checking!!
mu_delta <- 10
s_delta <- 2
day.project <- 7
t.window.range <- 7
rep <- 2e4
n_post <- 1e4
SItrunc <- 30

## ------------------------------------------------------------------
infile <- glue::glue(
  "{dirname(covid_19_path)}/model_inputs/data_{week_ending}.rds"
)
input_data <- readRDS(file = infile)

ascertainr_deaths <- input_data$D_active_transmission
ascertainr_cases <- input_data$I_active_transmission

countries <- purrr::set_names(input_data$Country, input_data$Country)


# week_ending <- d$week_ending

## checked that this gives the same result as Pierre's function

report_to_death <- report_to_death_distr(
  mu = mu_delta, std = s_delta, trunc = SItrunc
)

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

saveRDS(weighted_cases, "weighted_cases.rds")

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

saveRDS(deaths_to_cases_qntls, "deaths_to_cases_qntls.rds")
## ------------------------------------------------------------------
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


post_CFR <- x


ascertainment <- purrr::map(
  countries,
  function(country) {
    ascertainr::ascertainment(
      cfr_distr = post_CFR,
      death_to_case = deaths_to_cases[[country]]
    )
  }
)
## Same as above, results in the same ball park as Pierre',
## Pierre's rho for Yemen:
## 0.03438778 0.03465549 0.03694185 0.02719840 0.02697257 0.03408934
## From tje packaged code
## 0.03597449 0.03582383 0.03803099 0.02911610 0.02633064 0.03032737
ascertainment_qntls <- purrr::map_dfr(
  ascertainment,
  function(rho) {
    df <- quantiles_to_df(rho)
    df <- cbind(
      date = ascertainr_deaths[["dates"]],
      df
    )
    df
  }, .id = "country"
)

saveRDS(ascertainment_qntls, "ascertainment_qntls.rds")

episize_prev <- purrr::map(
  countries,
  function(country) {
    idx <- seq(
      1, length(ascertainr_deaths[[country]]) - round(mu_delta)
    )
    ascertainr::episize_before_mu(
      deaths = matrix(
        ascertainr_deaths[[country]][idx], ncol = 1
      ),
      mu_delta = mu_delta,
      cfr_distr = post_CFR
    )
  }
)
## As above, results in the same ball park.
## For Yemen, from Pierre's code:
##          dates Yemen
## 126 2020-05-04    51
## 133 2020-05-11    52
## 134 2020-05-12   266
## 149 2020-05-27   555
## For Yemen, from packaged code:
##         date Yemen
## 1 2020-05-04    51
## 2 2020-05-11    50
## 3 2020-05-12   268

episize_prev_qntls <- purrr::imap_dfr(
  episize_prev,
  function(x, country) {
    idx <- seq(
      1, length(ascertainr_deaths[[country]]) - round(mu_delta))
    df <- quantiles_to_df(x)
    cbind(date = ascertainr_deaths[["dates"]][idx],df)
  }, .id = "country"
)

saveRDS(episize_prev_qntls, "episize_before_mu_qntls.rds")

episize_projected <- purrr::map(
  countries,
  function(country) {
    idx <- seq(
      from = length(
        ascertainr_cases[[country]]) - round(mu_delta) + 1,
      to = length(ascertainr_deaths[[country]])
    )
    ascertainr::episize_after_mu(
      cases = matrix(ascertainr_cases[[country]][idx], ncol = 1),
      rho = ascertainment[[country]][idx, ]
    )
  }
)
## same as above.
## Pierre's code:
##          dates Yemen
## 155 2020-06-02   891
## 156 2020-06-03  1280
## 157 2020-06-04    99
## 158 2020-06-05  1813
## 159 2020-06-06   579
## 160 2020-06-07   108
## ascertainr code:
##     country       date  50.0%
## 535   Yemen 2020-06-02  845.5
## 536   Yemen 2020-06-03 1244.0
## 537   Yemen 2020-06-04   96.0
## 538   Yemen 2020-06-05 1698.0
## 539   Yemen 2020-06-06  592.0
## 540   Yemen 2020-06-07  120.0

episize_projected_qntls <- purrr::imap_dfr(
  episize_projected,
  function(x, country) {
    idx <- seq(
      from = length(
        ascertainr_deaths[[country]]
      ) - round(mu_delta) + 1,
      to = length(ascertainr_deaths[[country]])
    )
    df <- quantiles_to_df(x)
    cbind(date = ascertainr_deaths[["dates"]][idx], df)
  }, .id = "country"
)

saveRDS(episize_projected_qntls, "episize_after_mu_qntls.rds")

cases_augmented <- purrr::map(
  countries,
  function(country) {
    message(country)
    incid <- abs(ascertainr_cases[[country]])
    avg_last_week <- sum(tail(incid, 7)) / 7
    sd_last_week <- sd(tail(incid, 7))
    param <- epitrix::gamma_mucv2shapescale(
      mu = avg_last_week, cv = sd_last_week/ avg_last_week
    )
    i_augm <- matrix(
      rgamma(n = 7 * n_post, shape = param$shape, scale = param$scale),
      nrow = n_post,
      ncol = 7
    )
    i_old <- matrix(
      tail(incid, SItrunc),
      nrow = n_post,
      ncol = SItrunc,
      byrow = TRUE
    )
    cbind(i_old, i_augm)
  }
)

saveRDS(cases_augmented, "cases_augmented.rds")

weighted_cases_augm <- purrr::map(
  countries,
  function(country) {
    message(country)
    out <- apply(
      cases_augmented[[country]],
      2,
      function(x) {
        x <- matrix(x[2:length(x)], ncol = 1)
        ascertainr::weighted_incid(
          incid = x, weights = report_to_death, trunc = SItrunc
       )
     }
    )
    out <- out[ , seq(to = ncol(out), length.out = 7)]
  }
 )

saveRDS(weighted_cases_augm, "weighted_cases_augm.rds")

predictions <- purrr::map(
  countries,
  function(country) {
    reporting <- matrix(
      sample(
        x = tail(deaths_to_cases[[country]], 1),
        size = 7 * n_post,
        replace = TRUE
      ),
      nrow = n_post,
      ncol = 7
    )
    weighted_i <- weighted_cases_augm[[country]]
    d_exp <- data.frame(matrix(NA, nrow = n_post, 7))
    for (k in 1:7){
      d_exp[, k] <- rbinom(
        n = n_post,
        size = round(weighted_i[, k]),
        prob = reporting[,k]
      )
    }
    d_exp
  }
)

t.window <- 10
r_estim <- map2(
  input_data$si_mean,
  input_data$si_std,
  function(s_mean, s_sd) {
    si_distr <- gamma_dist_EpiEstim(
      si_mu = s_mean, si_std = s_sd, SItrunc = 30
    )
    purrr::map(
      countries,
      function(country){
        message(country)
        pred <- apply(predictions[[country]], 2, median, na.rm = TRUE)
        obs <- c(abs(ascertainr_cases[[country]]), pred)
        res <- estimate_R(
          obs,
          method = 'non_parametric_si',
          config = make_config(
            list(
              mean_prior = 1,
              si_distr = si_distr$dist,
              t_start = length(obs) - t.window + 1,
              t_end = length(obs))
          )
        )
        res <- res$R
        param <- epitrix::gamma_mucv2shapescale(
          mu = res$`Mean(R)`, cv = res$`Std(R)`/res$`Mean(R)`
        )
        rgamma(n = n_post, shape = param$shape, scale = param$scale)
      }
    )
  }
)

out <- list(
  I_active_transmission = input_data$I_active_transmission,
  D_active_transmission = input_data$D_active_transmission,
  Country = input_data$Country,
  Rt_last = r_estim,
  Predictions = list(
    si_1 = predictions, si_2 = predictions
  )
)

saveRDS(
  object = out,
  file = paste0('DeCa_Std_results_week_end_',week_ending,'.rds' )
)

saveRDS(
  object = out,
  file = paste0('DeCa_latest.rds' )
)


