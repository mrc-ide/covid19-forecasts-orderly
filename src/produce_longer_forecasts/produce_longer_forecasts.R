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
shape <- c(Shape1[f], Shape2[f])
cfr_samples <- rbeta(1000, shape1 = shape[1],shape2 = shape[2])

population <- readr::read_csv("ecdc_pop2018.csv")


proportion_susceptible <- function(deaths_per_capita, cfr) {
  1 - (deaths_per_capita / cfr)
}

unwtd_reff <- imap(
  unwtd_rt_estimates,
  function(rt, country) {
    deaths_so_far <- sum(
      deaths_to_use[deaths_to_use$dates <= week_ending, country]
    )
    pop <- population$pop_data2018[population$countries_and_territories == country]
    deaths_per_capita <- deaths_so_far / pop
    p <- proportion_susceptible(deaths_per_capita, cfr_samples)
    list(
      p_susceptible = p,
      r_eff = rt$rt_samples * p
    )
  }
)

## ws T X 1 matrix where N is the number of simulations,
## flip SI before calling this function
## deaths N X T matrix where N is the number of simulations
## R numeric.
force_of_infection <- function(deaths, ws, R) {
  (deaths %*% ws) * R
}
## deaths is a vector of deaths
## r_eff = r_obs / p_susceptible
## p_susceptible is the proportion susceptible at the point at which
## we start projecting ahead.
project_with_saturation <- function(deaths, r_eff, p_susceptible, si, n_sim = 100, n_days, cfr, pop) {

  deaths_obs <- sum(deaths)
  I0 <- matrix(
    deaths, ncol = length(deaths), nrow = n_sim, byrow = TRUE
  )
  day_max <- length(deaths) + n_days
  si <- c(si, rep(0, day_max - length(si)))
  out <- list(
    r_effective = vector(mode = "list", length = n_days),
    p_s = vector(mode = "list", length = n_days)
  )
  for (day in 1:n_days) {
    message(day)
    R <- r_eff * p_susceptible
    R <- sample(R, n_sim)
    ws <- tail(rev(si), length(deaths) + day - 1)
    ws <- matrix(
      ws, ncol = 1,  nrow = length(deaths) + day - 1
    )
    lambda <- force_of_infection(I0, ws, R)
    tmp <- quantile(lambda)
    message(paste(tmp, collapse = " "))
    pred <- map_int(lambda, function(x) rpois(1, x))
    I0 <- cbind(I0, pred)
    deaths_so_far <- rowSums(I0)
    deaths_per_cap <- deaths_so_far / pop
    p_susceptible <- proportion_susceptible(deaths_per_cap, cfr)

    out[["r_effective"]][[day]] <- R
    out[["p_s"]][[day]] <- p_susceptible
  }

  out[["incid_and_pred"]] <- I0
  out
}

unwtd_projections <- map(
  countries,
  function(country) {
    message(country)
    pop <- population$pop_data2018[population$countries_and_territories == country]
    n_days <- length(
      unwtd_rt_estimates[[country]]$weeks_combined
    ) * 7
    x <- deaths_to_use[[country]]
    ##rt <- unwtd_rt_estimates[[country]]$rt_samples
    rt <- unwtd_reff[[country]][["r_eff"]]
    p <- unwtd_reff[[country]][["p_susceptible"]]
    out <- purrr::map(
      seq_len(sims_per_rt),
      function(i) {
        project_with_saturation(
          deaths = x,
          r_eff = rt,
          p_susceptible = p,
          si = si,
          n_sim = n_sim,
          n_days = n_days,
          cfr = cfr_samples,
          pop
        )
      }
    )
    ##do.call(what = "rbind", args = out)
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
    ##do.call(what = "rbind", args = out)
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
