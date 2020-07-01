## ----options, include = FALSE, message = FALSE, warning = FALSE, error = FALSE----
set.seed(1)
dir.create("figures")
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
######################################################################
############ Weighted cases/ Unweighted cases ########################
wtd_df <- purrr::map_dfr(
  weighted_cases,
  function(x) {
    data.frame(
      dates = ascertainr_cases[["dates"]],
      cases = x[, 1]
    )
  }, .id = "country"
)
unwtd_df <- tidyr::gather(ascertainr_cases, country, cases, -dates)
unwtd_df$category <- "cases"
wtd_df$category <- "weighted_cases"
x <- rbind(unwtd_df, wtd_df)

npages <- length(countries) / 4
p <- ggplot(
  x, aes(dates, cases, col = category)
) + geom_line(size = 1.1) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

for (page in 1:npages) {
  p <- p +
    facet_wrap_paginate(
      ~country, nrow = 4, ncol = 1, page = page, scales = "free_y"
    )
  ggsave(filename = glue::glue("figures/wtd_cases_{page}.png"), p)
}
######################################################################
######################################################################
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

######################################################################
######################################################################
## Structure needed for plotting
##        dates median_ratio    low_ratio  up_ratio I_t_minus_meanDelay D_t
## 1 2020-03-01    0.4957478 0.0015739647 0.9981560                   0   0
## 2 2020-03-02    0.4827861 0.0014560397 0.9978068                   0   0
## 3 2020-03-03    0.4211129 0.0011746700 0.9957840                   0   0
## 4 2020-03-04    0.3423065 0.0008028309 0.9842211                   0   0
## 5 2020-03-05    0.2734532 0.0006902830 0.9658236                   0   0
## 6 2020-03-06    0.2141229 0.0005716901 0.9314065                   1   0
######################################################################
######################################################################

## These are in the same ball-park, but not the same as Pierre's code
## for instance, last 6 median values from Pierre' code are:
## 0.4007925 0.3961255 0.3724869 0.5057754 0.5104002 0.4044060
## while from my code are:
## 0.3826683 0.3846517 0.3622162 0.4735149 0.5226517 0.4538020
## could be due to stochastic nature of binom.bayes which doesn't
## accept a seed as far as I can see.
deaths_to_cases_qntls <- purrr::imap(
  deaths_to_cases,
  function(rit, country) {
    message(country)
    df <- quantiles_to_df(rit)
    df <- dplyr::select(
      df, low_ratio = `2.5%`, median_ratio = `50.0%`, up_ratio = `97.5%`
      )

    ymax <- max(
      max(ascertainr_deaths[[country]], na.rm = TRUE),
      max(ascertainr_cases[[country]], na.rm = TRUE)
    )

    scaled_deaths <- ascertainr_deaths[[country]] / ymax
    scaled_cases <- ascertainr_cases[[country]] / ymax

    ## Align deaths on day t with cases on day t - 10
    scaled_cases <- stats::lag(scaled_cases, n = round(mu_delta))

    df <- cbind(
      dates = ascertainr_deaths[["dates"]],
      D_t = scaled_deaths,
      I_t_minus_meanDelay = scaled_cases,
      df
    )
    df
  }##, .id = "country"
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
  res <- c(
    shape[1]/(shape[1]+shape[2]),
    qbeta(.025, shape1 = shape[1], shape2 = shape[2]),
    qbeta(.975, shape1 = shape[1], shape2 = shape[2])
  )
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
ascertainment_qntls <- purrr::map(
  ascertainment,
  function(rho) {
    df <- quantiles_to_df(rho)
    cbind(
      date = ascertainr_deaths[["dates"]],
      df
    )
  }
)

saveRDS(ascertainment_qntls, "ascertainment_qntls.rds")

######################################################################
################# ascertainment quantiles ############################
######################################################################
## p <- ggplot(x) +
##   geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
##   geom_line(aes(x = date, y = `50.0%`), size = 1.1) +
##   theme_minimal() +
##   xlab("") +
##   ylab("Ascertainment")

## for (page in 1:npages) {
##   p <-  p +
##     facet_wrap_paginate(~country, ncol = 1, nrow = 4, page = page)
##   ggsave(glue::glue("ascertainment_{page}.png"), p)
## }
######################################################################
######################################################################

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

episize_projected_qntls <- purrr::imap(
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
  }
)
saveRDS(episize_projected_qntls, "episize_after_mu_qntls.rds")


######################################################################
#################### Episize quantiles ###############################
######################################################################
######################################################################
x <- dplyr::bind_rows(episize_projected_qntls, .id = "country")
x <- rbind(episize_prev_qntls, x)

p <- ggplot(x) +
  geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
  geom_line(aes(x = date, y = `50.0%`), size = 1.1) +
  theme_minimal() +
  xlab("") +
  ylab("Episize")

for (page in 1:npages) {
  y <- tail(ascertainr_deaths[["dates"]], 1) - round(mu_delta)
  p <-  p +
    geom_vline(xintercept = y) +
    facet_wrap_paginate(
      ~country, ncol = 1, nrow = 4, page = page, scales = "free_y"
  )
  ggsave(glue::glue("figures/episize_{page}.png"), p)
}

######################################################################
######################################################################

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

######################################################################
######################################################################
weighted_cases_augm <- purrr::map(
  countries,
  function(country) {
    message(country)
    out <- apply(
      cases_augmented[[country]],
      1,
      function(x) {
        x <- matrix(x[2:length(x)], ncol = 1)
        ascertainr::weighted_incid(
          incid = x, weights = report_to_death, trunc = SItrunc
       )
      }
    )
    out <- t(out)
    out <- out[ , seq(to = ncol(out), length.out = 7)]
  }
)
saveRDS(weighted_cases_augm, "weighted_cases_augm.rds")
######################################################################
######################################################################
x <- purrr::map2_dfr(
  cases_augmented,
  weighted_cases_augm,
  function(y, z) {
    out <- data.frame(
      t(apply(y, 2, quantile, probs = c(0.025, 0.5, 0.975))),
      check.names = FALSE
    )
    out$category <- "augmented"
    out$dates <- c(
      tail(ascertainr_cases$dates, SItrunc),
      seq(
        from = max(ascertainr_cases$dates) + 1, length.out = 7, by = "1 day"
      )
    )
    wtd_augmented <- data.frame(
      t(apply(z, 2, quantile, probs = c(0.025, 0.5, 0.975))),
      check.names = FALSE
    )
    dates <- seq(
      tail(ascertainr_cases$dates, 1), length.out = 7, by = "1 day"
    )
    wtd_augmented$dates <- dates
    wtd_augmented$category <- "weighted augmented"
    rbind(out, wtd_augmented)
  }, .id = "country"
)

p <- ggplot(x) +
  geom_ribbon(
    aes(x = dates, ymin = `2.5%`, ymax = `97.5%`, fill = category), alpha = 0.3
  ) +
  geom_line(aes(x = dates, y = `50%`, col = category), size = 1.1) +
  theme_minimal() +
  xlab("") +
  ylab("Episize")

for (page in 1:npages) {

  p <-  p +
    facet_wrap_paginate(
      ~country, ncol = 1, nrow = 4, page = page, scales = "free_y"
  )
  ggsave(glue::glue("figures/weighted_augmented_cases_{page}.png"), p)
}
######################################################################
######################################################################

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
        prob = reporting[, k]
      )
    }
    colnames(d_exp) <- seq(
      from = as.Date(week_ending) + 1, length.out = 7, by = "1 day"
    )
    list(si_1 = d_exp, si_2 = d_exp)
  }
)
######################################################################
######################################################################
################# Visualisation #####################################
######################################################################
######################################################################
pred_qntls <- purrr::map(
  predictions,
  function(pred) {
    pred <- pred[[2]]
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)

### Test against Pierre's outputs for previous weeks
pierre <- readRDS(
  glue::glue("{covid_19_path}DeCa_Std_results_week_end_{week_ending}.rds")
)
pierre_qntls <- purrr::map(
  pierre[["Predictions"]],
  function(pred) {
    pred <- pred[[2]]
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)

purrr::walk(
  countries,
  function(country) {
    x <- pred_qntls[[country]]
    y <- pierre_qntls[[country]]
    z <- dplyr::bind_rows(list(packaged = x, old = y), .id = "category")
    z <- z[z$`.width` == 0.75, ]
    p <- ggplot(z) +
      geom_ribbon(
        aes(x = dates, ymin = .lower, ymax = .upper, fill = category),
        alpha = 0.3
      ) +
      geom_line(aes(dates, val, col = category)) +
      theme_minimal()

    ggsave(glue::glue("figures/{country}_compare.png"), p)
  }
)



purrr::iwalk(
  pred_qntls,
  function(pred, cntry) {
    obs <- ascertainr_deaths[, c("dates", cntry)]
    obs$deaths <- obs[[cntry]]
    xintercept <- as.numeric(as.Date(week_ending)) + 0.5
    p <- ggplot() +
      geom_point(data = obs, aes(dates, deaths)) +
      geom_lineribbon(
        data = pred,
        aes(
          x = dates, y = val, ymin = .lower, ymax = .upper
        )
      ) +
      scale_fill_brewer(palette = "Greens") +
      scale_x_date(limits = c(as.Date("2020-03-01"), NA)) +
      geom_vline(xintercept = xintercept, linetype = "dashed") +
      theme_minimal() +
      xlab("") + ylab("Deaths") +
      theme(legend.position = "none") +
      ggtitle(
        glue::glue("Projections for {cntry} for week starting {week_ending}")
      )

    ggsave(glue::glue("figures/projections_{cntry}.png"), p)
  }
)

######################################################################
######################################################################


t.window <- 10
r_estim <- purrr::map(
  countries,
  function(country){
    message(country)
    pred <- apply(
      predictions[[country]][[1]], 2, median, na.rm = TRUE
    )
    obs <- c(abs(ascertainr_deaths[[country]]), pred)
    purrr::map2(
      input_data$si_mean,
      input_data$si_std,
      function(s_mean, s_sd) {
        si_distr <- gamma_dist_EpiEstim(
          si_mu = s_mean, si_std = s_sd, SItrunc = 30
        )
        res <- EpiEstim::estimate_R(
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
        stats::rgamma(n = n_post, shape = param$shape, scale = param$scale)
      }
    )
  }
)

out <- list(
  I_active_transmission = input_data$I_active_transmission,
  D_active_transmission = input_data$D_active_transmission,
  Country = input_data$Country,
  R_last = r_estim,
  Predictions = predictions
)

saveRDS(
  object = out,
  file = paste0('DeCa_Std_results_week_end_',week_ending,'.rds' )
)

saveRDS(object = out, file = paste0('DeCa_latest.rds'))


#####################################################################
############################# Summary ###############################
#####################################################################
## country,deaths_to_reported_ratio,estimated_reporting,factor_to_real_size,Observed_case_last_week,Predicted_True_case_last_week
## Yemen,0.32 (0.248 - 0.396),4.3% (3.4% - 5.7%),23.23 (17.66 - 29.64),232,5230 (4290 - 6400)
## United_States_of_America,0.038 (0.037 - 0.038),36.7% (32.7% - 40.8%),2.73 (2.45 - 3.05),154465,434000 (390000 - 486000)
## United_Kingdom,0.091 (0.086 - 0.096),15.1% (13.3% - 17.0%),6.64 (5.89 - 7.51),9507,66600 (59700 - 74700)
## United_Arab_Emirates,0.003 (0.002 - 0.005),100.0% (100.0% - 100.0%),1 (1 - 1),3632,3630 (3630 - 3630)
summary_14days <- purrr::map_dfr(
  countries,
  function(country) {
    summary_r <- deaths_to_cases_qntls[[country]]
    summary_rho <- ascertainment_qntls[[country]]
    last_week <- quantile(
      colSums(episize_projected[[country]], na.rm = TRUE),
      c(0.5, 0.025, 0.975),
      na.rm = TRUE
    )
    last_week <- data.frame(last_week) %>%
      tibble::rownames_to_column() %>%
      tidyr::spread(rowname, last_week)

    data.frame(
      country = country,
      deaths_to_reported_ratio = glue::glue(
        "{round(summary_r$median_ratio[nrow(ascertainr_deaths)], 3) }",
        " ({round(summary_r$low_ratio[nrow(ascertainr_deaths)], 3)} - {round(summary_r$up_ratio[nrow(ascertainr_deaths)],3)})",
      ),
      estimated_reporting = glue::glue(
        "{scales::percent(as.numeric(round(summary_rho$`50.0%`[nrow(ascertainr_deaths)], 3)), accuracy = 0.1)}",
        " ({scales::percent(as.numeric(round(summary_rho$`2.5%`[nrow(ascertainr_deaths)],3)), accuracy = 0.1)} - {scales::percent(as.numeric(round(summary_rho$`97.5%`[nrow(ascertainr_deaths)],3)), accuracy = 0.1)})",
        ),
      factor_to_real_size = glue::glue(
        "{round(1/summary_rho$`50.0%`[nrow(ascertainr_deaths)], 2)}",
        " ({round(1/summary_rho$`97.5%`[nrow(ascertainr_deaths)], 2)} - {round(1/summary_rho$`2.5%`[nrow(ascertainr_deaths)], 2)})",
        ),
      Observed_case_last_week = sum(tail(ascertainr_cases[[country]], 7)),
      Predicted_True_case_last_week = glue::glue(
          "{prettyNum(signif(last_week$`50%`, digits = 3), big.mark = ",")} ",
          "({prettyNum(signif(last_week$`2.5%`, digits = 3), big.mark = ",")}",
          " - {prettyNum(signif(last_week$`97.5%`, digits = 3), big.mark = ",")})"
      )
    )
  }
)

## TODO check why we have NAs
summary_14days <- na.omit(summary_14days)
readr::write_csv(
  x = summary_14days,
  path = 'summary_DeathToRepoted_14days.csv'
)
