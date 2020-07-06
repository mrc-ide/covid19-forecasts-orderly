## ----options, include = FALSE, message = FALSE, warning = FALSE, error = FALSE----
## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-08", short_run = TRUE))
set.seed(1)
dir.create("figures")
day.project <- 7
t.window.range <- 10

if (short_run) {
  iterations <- 5e2
} else {
  iterations <- 5e4
}



indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

deaths_to_use <- raw_data[["D_active_transmission"]]

country <- raw_data$Country
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
    raw_data$si_mean,
    function(mu) {
      as.numeric(log(colMeans(incidence_inference[, -1]) * mu))
    }
  )
} else {
  mu0 <- purrr::map(
    raw_data$si_mean,
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
  raw_data$si_mean,
  raw_data$si_std,
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




# acceptance rate (should be close to .2)
acc <- purrr::map(res, ~ colSums(diff(.$theta) != 0) / iterations)
acc[[1]]
acc[[2]]

R_est <- purrr::map(
  res,
  function(r) {
    if (N_geo > 1) {
      apply(
        r$theta[, 1:N_geo], 2, quantile, c(0.5, 0.025, 0.975)
      )
    } else {
      quantile(r$theta[, 1], c(0.5, 0.025, 0.975))
    }
  }
)


I0_est <- purrr::map(
  res,
  function(r) {
    if (N_geo > 1) {
      apply(
        r$theta[, (N_geo + 1):(2 * N_geo)],
        2,
        quantile, c(0.5, 0.025, 0.975)
      )
    } else {
      quantile(r$theta[, 2], c(0.5, 0.025, 0.975))
    }
  }
)

### Project forward

Nsim_per_samples <- 10

day_forward <- day.project + t.window.range

# forward projections
I_pred <- purrr::map2(
  res,
  si_distrs,
  function(r, si_distr) {
    NR_samples <- nrow(r$theta) / 10
    out <- Proj_Pois(
      Results = r,
      NR_samples = NR_samples,
      Nsim_per_samples = Nsim_per_samples,
      day_forward = day_forward,
      N_geo = N_geo,
      SI = si_distr
    )
    names(out) <- raw_data$Country
    out
  }
)

Rt_last <- purrr::map(
  res,
  function(r) {
    out <- data.frame(r$theta[, 1:N_geo])
    names(out) <- raw_data$Country
    if (nrow(out) > 1e4) {
      f <- round(seq(1, nrow(out), length.out = 1e4))
      out <- out[f, ]
    }
    out
  }
)

Rt_last <- purrr::map(
  raw_data$Country,
  function(country) {
    purrr::map(Rt_last, ~ .[[country]])
  }
)
names(Rt_last) <- raw_data$Country

dates_pred <- as.character(
  seq(t.proj.start, t.proj.start + 7 - 1, 1)
)

Predictions <- purrr::map(
  raw_data$Country,
  function(country) {
    purrr::map(
      I_pred,
      function(pred_si) {
        out <- pred_si[[country]]
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

names(Predictions) <- raw_data$Country

Std_results <- list(
  I_active_transmission = raw_data$I_active_transmission,
  D_active_transmission = raw_data$D_active_transmission,
  Country = raw_data$Country,
  R_last = Rt_last,
  Predictions = Predictions
)

saveRDS(
  object = Std_results,
  file = paste0("RtI0_Std_results_week_end_", week_ending, ".rds")
)

saveRDS(
  object = Std_results,
  file = paste0("RtI0_latest_output.rds")
)

#### Diagonistic Plots ##############################################
## pdf("likelihood.pdf")
## plot(res[[2]]$logL[, 1]) # of likelihood
## dev.off()

## plot("r0.pdf")
## layout(matrix(1:4, 2, 2, byrow = TRUE))
## for (i in 1:N_geo) {
##   plot(res[[2]]$theta[, i], main = paste("R0", country[i]))
## }
## for (i in 1:N_geo) {
##   plot(
##     res[[2]]$theta[, N_geo + i] * res[[2]]$theta[, i],
##     main = paste("I0", country[i])
##   )
## }
## dev.off()
I_plot <- tail(deaths_to_use, 14)
                                        # incidence_inference
pdf("ci_pred.pdf")
layout(matrix(1:4, 2, 2, byrow = TRUE))

for (i in 1:N_geo) {
  CI_pred <- apply(
    I_pred[[2]][[i]], 2, quantile, c(.5, .025, .975),
    na.rm = TRUE
  )

  plot(I_plot$dates, I_plot[, i + 1],
       xlim = c(
         I_plot$dates[1], tail(incidence_inference$dates, 1) + day.project
       ),
       ylim = c(
         0, 1 + max(c(incidence_inference[, 1 + i], as.vector(CI_pred)))
       ),
    xlab = "time", ylab = "incidence", main = country[i], bty = "n"
  )

  lines(
    incidence_inference$dates,
    incidence_inference[, i + 1],
    type = "p",
    pch = 16,
    col = "black"
  )

  x <- 1:ncol(CI_pred) + incidence_inference$dates[1] - 1
  lines(x, CI_pred[1, ], col = "blue", lwd = 2)
  polygon(c(x, rev(x)), c(CI_pred[2, ], rev(CI_pred[3, ])),
    col = rgb(0, 0, 1, .2), border = FALSE
  )

  legend("topleft", legend = c("for inference"), pch = 16, col = "black", bty = "n")
}
dev.off()



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

### Test against Pierre's outputs for previous weeks
## pierre <- readRDS(
##   glue::glue("{covid_19_path}RtI0_Std_results_week_end_{week_ending}.rds")
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
##   raw_data$Country,
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
