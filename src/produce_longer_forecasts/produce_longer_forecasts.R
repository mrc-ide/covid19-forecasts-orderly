## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"))
## infiles <- list.files(pattern = "*.rds")
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


tall_deaths <- tidyr::gather(
  deaths_to_use, key = country, value = deaths, -dates
) %>%
  split(.$country) %>%
  purrr::map(
  ~ rincewind:::ts_to_incid(ts = ., date_col = "dates", case_col = "deaths")
)


unwtd_rt_estimates <- readRDS("combined_rt_estimates.rds")
wtd_rt_estimates_across_countries <- readRDS(
  "combined_weighted_estimates_across_countries.rds"
)
wtd_rt_estimates_per_country <- readRDS(
  "combined_weighted_estimates_per_country.rds"
)


## rt_estimates <- purrr::keep(
##   rt_estimates, ~ length(.$weeks_combined) > 1
## )

countries <- setNames(
  names(unwtd_rt_estimates), names(unwtd_rt_estimates)
)

date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_sim <- 1000

unwtd_projections <- purrr::map(
  countries,
  function(country) {
    message(country)
    n_days <- length(
      unwtd_rt_estimates[[country]]$weeks_combined
    ) * 7
    x <- tall_deaths[[country]]
    rt <- unwtd_rt_estimates[[country]]$rt_samples
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

wtd_across_all_projections <- purrr::map(
  countries,
  function(country) {
    message(country)
    n_days <- length(
      wtd_rt_estimates_across_countries[[country]]$weeks_combined
    ) * 7
    x <- tall_deaths[[country]]
    rt <- wtd_rt_estimates_across_countries[[country]]$rt_samples
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

wtd_per_country_projections <- purrr::map(
  countries,
  function(country) {
    message(country)
    n_days <- length(
      wtd_rt_estimates_per_country[[country]]$weeks_combined
    ) * 7
    x <- tall_deaths[[country]]
    rt <- wtd_rt_estimates_per_country[[country]]$rt_samples
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

saveRDS(unwtd_projections, "unwtd_projections.rds")
saveRDS(
  wtd_per_country_projections, "wtd_per_country_projections.rds"
)

saveRDS(
  wtd_across_all_projections, "wtd_across_all_projections.rds"
)
