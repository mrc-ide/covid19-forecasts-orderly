## ----options, include = FALSE, message = FALSE, warning = FALSE, error = FALSE----
## orderly::orderly_develop_start(parameters = list(week_ending = "2020-04-19", short_run = TRUE))
set.seed(1)
dir.create("figures")
day.project <- 7
t.window.range <- 10

if (short_run) {
  iterations <- 5e2
} else {
  iterations <- 10e4
}



model_input <- readRDS("model_input.rds")
deaths_to_use <- model_input$D_active_transmission


exclude <- readRDS("exclude.rds")
country <- model_input$Country
country <- country[! country %in% exclude]

deaths_to_use <- deaths_to_use[ ,c("dates", country)]

N_geo <- length(country)
SItrunc <- 20

t.proj.start <- as.Date(week_ending) + 1

t.window <- seq(
  t.proj.start - t.window.range, t.proj.start - 1, 1
)

incidence_inference <- deaths_to_use[which(deaths_to_use$dates %in% t.window), ]

# initial proposal variances (they are now tuned!)
sigma_prop <- rep(0.1, N_geo * 2)
# initial incidence conditions
if (N_geo > 1) {
  mu0 <- purrr::map(
    model_input$si_mean,
    function(mu) {
      as.numeric(log(colMeans(incidence_inference[, -1]) * mu))
    }
  )
} else {
  mu0 <- purrr::map(
    model_input$si_mean,
    function(mu) {
      as.numeric(log(mean(incidence_inference[, -1]) * mu))
    }
  )
}

# initially, we assume R=1 and choose initial condition accordingly, i.e. with mu0 case and R=1
# we expect the number of daily cases to stabilised at the mean of the observed incidence in the
# time window of interest
# this is use for the prior of initial number of cases, i.e. as the mean of an exponential distribution
# in practice, the mu0 cases will happen 100 days before the start of the time windows
# initial parameter R=1 (time # of locations, and initial number of cases in the past)
theta0 <- purrr::map(mu0, ~ c(rep(1, N_geo), .))


si_distrs <- purrr::map2(
  model_input$si_mean,
  model_input$si_std,
  function(mu, std) {
    SI_gamma_dist_EpiEstim(
      mu = mu, si_std = std, SItrunc = SItrunc
    )
  }
)

res <- purrr::pmap(
  list(
    si_distr = si_distrs,
    theta = theta0,
    mu = mu0
  ),
  function(si_distr, theta, mu) {
    MCMC_full(
      I = incidence_inference,
      N_geo = N_geo,
      iter = iterations,
      theta0 = theta,
      s = sigma_prop,
      SI = si_distr,
      mu0 = mu,
      repli_adapt = 10,
      within_iter = iterations / 10
    )
  }
)

## Diagnostics
theta <- res[[1]][[1]]
purrr::iwalk(
  country,
  function(country_to_use, index) {
    rt_trace <- theta[, index]
    i0_trace <- theta[, index + N_geo]

    p1 <- ggplot() +
      geom_line(aes(seq_along(rt_trace), rt_trace)) +
      theme_minimal() +
      ggtitle(country_to_use)

    p2 <- ggplot() +
      geom_line(aes(seq_along(i0_trace), i0_trace)) +
      theme_minimal()

    p <- cowplot::plot_grid(p1, p2, ncol = 1)
    ggsave(glue::glue("figures/{country_to_use}_trace_plots.png"), p)

  }
)




# acceptance rate (should be close to .2)
acc <- purrr::map(res, ~ colSums(diff(.$theta) != 0) / iterations)
acc[[1]]
acc[[2]]

## R_est <- purrr::map(
##   res,
##   function(r) {
##     if (N_geo > 1) {
##       apply(
##         r$theta[, 1:N_geo], 2, quantile, c(0.5, 0.025, 0.975)
##       )
##     } else {
##       quantile(r$theta[, 1], c(0.5, 0.025, 0.975))
##     }
##   }
## )


## I0_est <- purrr::map(
##   res,
##   function(r) {
##     if (N_geo > 1) {
##       apply(
##         r$theta[, (N_geo + 1):(2 * N_geo)],
##         2,
##         quantile, c(0.5, 0.025, 0.975)
##       )
##     } else {
##       quantile(r$theta[, 2], c(0.5, 0.025, 0.975))
##     }
##   }
##   )
saveRDS(res, "rti0_all_results.rds")

### Project forward

Nsim_per_samples <- 10

day_forward <- day.project + t.window.range

# forward projections
I_pred <- purrr::map2(
  res,
  si_distrs,
  function(r, si_distr) {
    NR_samples <- 1000 ##nrow(r$theta) / 10
    out <- Proj_Pois(
      Results = r,
      NR_samples = NR_samples,
      Nsim_per_samples = Nsim_per_samples,
      day_forward = day_forward,
      N_geo = N_geo,
      SI = si_distr
    )
    names(out) <- country
    out
  }
)

Rt_last <- purrr::map(
  res,
  function(r) {
    out <- data.frame(r$theta[, 1:N_geo])
    names(out) <- country
    if (nrow(out) > 1e4) {
      f <- round(seq(1, nrow(out), length.out = 1e4))
      out <- out[f, ]
    }
    out
  }
)

Rt_last <- purrr::map(
  country,
  function(cntry) {
    purrr::map(Rt_last, ~ .[[cntry]])
  }
)
names(Rt_last) <- country

dates_pred <- as.character(
  seq(t.proj.start, t.proj.start + 7 - 1, 1)
)

Predictions <- purrr::map(
  country,
  function(cntry) {
    purrr::map(
      I_pred,
      function(pred_si) {
        out <- pred_si[[cntry]]
        out <- as.data.frame(out[, 8:14])
        names(out) <- dates_pred
        if (nrow(out) > 1e4) {
          f <- round(seq(1, nrow(out), length.out = 1e4))
          out <- out[f, ]
        }
        out
      }
    )
  }
)

names(Predictions) <- country

Std_results <- list(
  I_active_transmission = model_input$I_active_transmission,
  D_active_transmission = model_input$D_active_transmission,
  Country = country,
  R_last = Rt_last,
  Predictions = Predictions
)

## saveRDS(
##   object = Std_results,
##   file = paste0("RtI0_Std_results_week_end_", week_ending, ".rds")
## )

saveRDS(object = Std_results, file = paste0("RtI0_latest_output.rds"))

#### Diagonistic Plots ##############################################



pred_qntls <- purrr::map(
  Predictions,
  function(pred) {
    pred <- pred[[2]]
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)



purrr::iwalk(
  pred_qntls,
  function(pred, cntry) {
    obs <- deaths_to_use[, c("dates", cntry)]
    obs$deaths <- obs[[cntry]]
    p <- rincewind::plot_projections(obs, pred)
    p <- p +
      ggtitle(
        glue::glue("Projections for {cntry} for week starting {week_ending}")
      )

    ggsave(glue::glue("figures/projections_{cntry}.png"), p)
  }
)
