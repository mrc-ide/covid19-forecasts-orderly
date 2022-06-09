## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2022-06-05"))
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
model_input <- readRDS("model_input.rds")
ascertainr_deaths <- model_input$D_active_transmission
ascertainr_cases <- model_input$I_active_transmission

exclude <- readRDS("exclude.rds")
## Massive backlog reported + -ve deaths
exclude <- c(exclude, "Ecuador")
countries <- purrr::set_names(model_input$Country, model_input$Country)
countries <- countries[! countries %in% exclude]

## To make the schematic
# countries <- list(Peru = "Peru")

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

## ifr <- readRDS("population_weighted_ifr.rds")

## ascertainment <- purrr::map(
##   countries,
##   function(country) {
##     message(country)
##     if (country == "Cote_dIvoire") idx <- "Cote_dIvoire"
##     else if (country == "Timor_Leste") idx <- "Timor-Leste"
##     else idx <- snakecase::to_title_case(country)

##     ascertainr::ascertainment(
##       cfr_distr = ifr[[idx]],
##       death_to_case = deaths_to_cases[[country]]
##     )
##   }
## )
## ## Same as above, results in the same ball park as Pierre',
## ## Pierre's rho for Yemen:
## ## 0.03438778 0.03465549 0.03694185 0.02719840 0.02697257 0.03408934
## ## From tje packaged code
## ## 0.03597449 0.03582383 0.03803099 0.02911610 0.02633064 0.03032737
## ascertainment_qntls <- purrr::map(
##   ascertainment,
##   function(rho) {
##     df <- quantiles_to_df(rho)
##     cbind(
##       date = ascertainr_deaths[["dates"]],
##       df
##     )
##   }
## )

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
######################################################################
######################################################################
####### Figures for SI
## dates <- seq(
##   from = tail(ascertainr_cases$dates, 1) + 1, length.out = 7, by = "1 day"
## )

## augm_incid <- cases_augmented[["Spain"]][ , 31:37]
## qntls <- apply(augm_incid, 2, quantile, probs = c(0.025, 0.5, 0.975))
## qntls <- as.data.frame(qntls)
## colnames(qntls) <- dates

## qntls <- tibble::rownames_to_column(qntls) %>%
##   tidyr::gather(date, val, -rowname) %>%
##   tidyr::spread(rowname, val)


## obs <- ascertainr_cases[, c("dates", "Spain")]
## qntls$date <- as.Date(qntls$date)
## obs$dates <- as.Date(obs$dates)

## p1 <- ggplot() +
##   geom_line(data = obs, aes(dates, Spain), size = 1.1) +
##   geom_segment(
##     aes(
##       x = tail(obs$dates, 7)[1], xend = tail(obs$dates, 7)[7],
##       y = mean(tail(obs$Spain, 7)),
##       yend = mean(tail(obs$Spain, 7))
##     ), linetype = "dashed"
##   ) +
##   geom_vline(
##     xintercept = as.numeric(tail(obs$dates, 7)[1]), alpha = 0.5
##   ) +
##   geom_vline(
##     xintercept = as.numeric(tail(obs$dates, 7)[7]), alpha = 0.5
##   ) +
##   geom_ribbon(
##     data = qntls, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
##     alpha = 0.2, fill = "red"
##   ) +
##   geom_line(data = qntls, aes(date, `50%`), col = "red") +
##   scale_x_date(
##     date_breaks = "2 weeks", date_labels = "%d - %b",
##     limits = c(as.Date("2020-02-15"), NA)
##   ) +
##   ylab("Daily cases") +
##   theme_minimal() + theme(axis.title.x = element_blank())

## p2 <- ggplot() +
##   geom_col(aes(1:31, report_to_death), alpha = 0.5) +
##   geom_vline(xintercept = 7) +
##   ylab("Probability of death") +
##   xlab("Days since case report") +
##   theme_minimal()

## p <- cowplot::plot_grid(p1, p2, ncol = 2, labels = "auto")

## ggsave("case_augmentation.png", p)

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
## pierre <- readRDS(
##   glue::glue("{covid_19_path}DeCa_Std_results_week_end_{week_ending}.rds")
## )
## pierre_qntls <- purrr::map(
##   pierre[["Predictions"]],
##   function(pred) {
##     pred <- pred[[2]]
##     pred <- tidyr::gather(pred, dates, val)
##     qntls <- dplyr::group_by(pred, dates) %>%
##       ggdist::median_qi(.width = c(0.75, 0.95))
##     qntls$dates <- as.Date(qntls$dates)
##     qntls
##   }
## )

## purrr::walk(
##   countries,
##   function(country) {
##     x <- pred_qntls[[country]]
##     y <- pierre_qntls[[country]]
##     z <- dplyr::bind_rows(list(packaged = x, old = y), .id = "category")
##     z <- z[z$`.width` == 0.75, ]
##     p <- ggplot(z) +
##       geom_ribbon(
##         aes(x = dates, ymin = .lower, ymax = .upper, fill = category),
##         alpha = 0.3
##       ) +
##       geom_line(aes(dates, val, col = category)) +
##       theme_minimal()

##     ggsave(glue::glue("figures/{country}_compare.png"), p)
##   }
## )



purrr::iwalk(
  pred_qntls,
  function(pred, cntry) {
    obs <- ascertainr_deaths[, c("dates", cntry)]
    obs$deaths <- obs[[cntry]]
    p <- rincewind::plot_projections(obs, pred)
    p <- p +
      ggtitle(
        glue::glue("Projections for {cntry} for week starting {week_ending}")
      )

    ggsave(glue::glue("figures/projections_{cntry}.png"), p)
  }
  )

## cntry <- "Peru"
## deaths <- ascertainr_deaths[, c("dates", cntry)]
## obs <- ascertainr_cases[, c("dates", cntry)]
## pred <- pred_qntls[[cntry]]
## obs$Peru <- slider::slide_dbl(obs$Peru, mean, .before = 3, .after = 3)
## deaths$Peru <- slider::slide_dbl(deaths$Peru, mean, .before = 3, .after = 3)
## ## Just so we can see it properly in the schematic
## pred$`.upper` <- pred$`.upper` + 500
## pred$`val` <- pred$`val` + 250
## deaths$Peru <- deaths$Peru + 250



## wtd <- data.frame(
##   dates = obs$dates + 10,
##   y = weighted_cases[["Peru"]]
## )

## ggplot() +
##   geom_line(data = obs, aes(dates, Peru)) +
##   geom_line(data = deaths, aes(dates, Peru)) +
##   geom_line(
##     data = wtd, aes(dates, y),
##     col = "blue", size = 1.1, linetype = "dashed"
##   ) +
##   geom_line(
##     data = pred, aes(dates, val), col = "red"
##   ) +
##   geom_ribbon(
##     data = pred[pred$`.width` == 0.95, ],
##     aes(dates, ymin = .lower, ymax = .upper),
##     fill = "red", alpha = 0.3
##   ) +
##   scale_x_date(limits = c(as.Date("2020-05-01") , NA)) +
##   theme_classic()

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
      model_input$si_mean,
      model_input$si_std,
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
  I_active_transmission = model_input$I_active_transmission,
  D_active_transmission = model_input$D_active_transmission,
  Country = model_input$Country,
  R_last = r_estim,
  Predictions = predictions
)

saveRDS(object = out, file = paste0('DeCa_latest.rds'))
