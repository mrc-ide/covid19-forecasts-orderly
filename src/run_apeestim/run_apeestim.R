dir.create("figures")
indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

deaths_to_use = raw_data[["D_active_transmission"]]

## Convert to incidence object
tall_deaths <- gather(
  deaths_to_use, key = country, value = deaths, -dates
) %>%
  split(.$country) %>%
  map(
    ~ ts_to_incid(
  ts = ., date_col = "dates", case_col = "deaths"
 )
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

##apeestim
inftvty <- purrr::map(
  tall_deaths,
  function(deaths) {
    map(
      si_distrs,
      function(si_distr) {
        incid <- as.numeric(incidence::get_counts(deaths))
        EpiEstim::overall_infectivity(
          incid, si_distr
        )
      }
    )
  }
)

## For each country, for each SI Distribution,
r_apeestim <- purrr::imap(
  tall_deaths,
  function(deaths, country) {
    ##deaths <- tall_deaths[[country]]
    r_prior <- c(1, 5)
    a <- 0.025
    trunctime <- first_nonzero_incidence(deaths)
    message("Truncating for ", country, " at ", trunctime)
    out <- purrr::map(
      si_distrs,
      function(si_distr) {
        incid <- as.numeric(incidence::get_counts(deaths))
        inftvty <- EpiEstim::overall_infectivity(
          incid, si_distr
        )
        apeEstim(
          incid,
          si_distr,
          inftvty,
          r_prior,
          a,
          trunctime,
          country
        )
      }
    )
    out
  }
)

## Each element of r_apeestim is a list with the following
## components: "best_k_ape", "best_set_ape", "best_k_pmse",
## "best_set_pmse". To use the window determined according to the
## APE score, use best_set_ape, which is again a list with
## "ape", "pmse", "prob", "rhat", "rhatci", "post_mean_tplus1",
## "tplus1_ci", "alpha", "beta", "post_negbin_pr". We want the
## last values of alpha and beta.
n_sim <- 1000
rsamples_ape <- map(
  r_apeestim,
  function(r_country) {
    out <- map(
      r_country,
      function(r_si) {
        shape <- tail(
          r_si[["best_set_ape"]][["alpha"]], 1
        )
        scale <- tail(
          r_si[["best_set_ape"]][["beta"]], 1
        )
        rgamma(n_sim, shape = shape, scale = scale)
      }
    )
  }
)
date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_days <- 7
## Projections using projections package with
## Poisson offspring distribution
ape_projections <- purrr::map2(
  tall_deaths,
  rsamples_ape,
  function(df, rt) {
    df <- subset(df, to = as.Date(date_to_project_from))
    purrr::map2(
      rt, si_distrs,
      function(rt_si, si) {
        out <- map(
          seq_len(sims_per_rt),
          function(i) {
            projections::project(
              x = df,
              R = rt_si,
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
  }
)

pred_qntls <- purrr::map(
  ape_projections,
  function(pred) {
    pred <- pred[[2]]
    pred <- data.frame(pred, check.names = FALSE)
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


## ## Save in format required
out <- saveRDS(
  object = list(
    I_active_transmission = raw_data[["I_active_transmission"]],
    D_active_transmission = raw_data[["D_active_transmission"]],
    Country = raw_data[["Country"]],
    R_last = rsamples_ape,
    Predictions = ape_projections
  ),
  file = "apeestim_model_outputs.rds"
)

saveRDS(
  object = r_apeestim,
  file = "r_apeestim.rds"
)
