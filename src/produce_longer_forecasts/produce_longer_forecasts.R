dir.create("figures")
indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

si_distrs <- purrr::map2(
  raw_data[["si_mean"]],
  raw_data[["si_std"]],
  function(mu, sigma) {
    reparams <- epitrix::gamma_mucv2shapescale(
      mu = mu, cv = sigma / mu
    )
    miss_at_most <-0.001
    cutoff <- ceiling(
      qgamma(
        1 - miss_at_most,
        shape = reparams$shape,
        scale = reparams$scale
      )
    )
    EpiEstim::discr_si(k = 0:cutoff, mu = mu, sigma = sigma)
  }
)

names(si_distrs) <- c("si_1", "si_2")
si <- si_distrs[[use_si]]

deaths_to_use <- raw_data[["D_active_transmission"]]

ts_to_incid <- function(ts, date_col, case_col) {

  first_date <- min(ts[[date_col]])
  last_date <- max(ts[[date_col]])
  x <- tidyr::uncount(ts, weights = ts[[case_col]])
  out <- incidence(
    x[[date_col]],
    first_date = first_date,
    last_date = last_date
  )
  out
}

tall_deaths <- gather(
  deaths_to_use, key = country, value = deaths, -dates
) %>%
  split(.$country) %>%
  map(
    ~ ts_to_incid(
  ts = ., date_col = "dates", case_col = "deaths"
 )
)


rt_estimates <- readRDS("combined_rt_estimates.rds")

rt_estimates <- purrr::keep(
  rt_estimates, ~ length(.$weeks_combined) > 1
)

countries <- stats::setNames(names(rt_estimates), names(rt_estimates))

date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_sim <- 1000

projections <- purrr::map(
  countries,
  function(country) {
    n_days <- length(rt_estimates[[country]]$weeks_combined) * 7
    x <- tall_deaths[[country]]
    rt <- rt_estimates[[country]]$rt_samples
    out <- purrr::map(
      seq_len(sims_per_rt),
      function(i) {
        projections::project(
          x = x,
          R = rt,
          si = si,
          n_sim = n_sim,
          n_days = n_days,
          R_fix_within = TRUE,
          model = "poisson"
        ) %>%
          as.matrix() %>% t
      }
    )
    do.call(what = "rbind", args = out)
  }
)



pred_qntls <- purrr::map(
  projections,
  function(pred) {
    pred <- data.frame(pred, check.names = FALSE)
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = 0.95)
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)

all_deaths <- readRDS("latest_deaths_wide_no_filter.rds")

purrr::iwalk(
  pred_qntls,
  function(pred, cntry) {
    obs <- all_deaths[, c("dates", cntry)]
    obs <- obs[obs$dates <= max(pred$dates) + 7, ]
    obs$deaths <- obs[[cntry]]

    p <- ggplot() +
      geom_point(data = obs, aes(dates, deaths), col = "blue") +
      geom_ribbon(
        data = pred, aes(x = dates, ymin = .lower, ymax = .upper),
        alpha = 0.3
      ) +
      geom_line(
        data = pred, aes(dates, val), size = 1.2
      ) +
      theme_minimal() +
      scale_x_date(
        date_breaks = "3 weeks", limits = c(as.Date("2020-03-01"), NA)
      ) +
      xlab("") +
      ylab("Daily Incidence")

    p <- p +
      ggtitle(
        glue::glue("Projections for {cntry} for week starting {week_ending}")
      )
    ggsave(glue::glue("figures/projections_{cntry}.png"), p)
  }
)
