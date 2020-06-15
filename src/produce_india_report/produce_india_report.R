report_to_death_distr <- function(mu, std, trunc) {
    out <- EpiEstim::discr_si(seq(0, trunc), mu, std)
    out / sum(out)
}

cfr_distr <- rbeta(
    10000, shape1 = 319.4389, shape2 = 22872.75
)
## Mean and standard deviation of reporting to death delay
mu_delta <- 10 ## days
std_delta <- 2  ## days
trunc <- 30

## Read in the innput data prepared by centralised team
input_data <- readr::read_rds("india.rds")

ascertainr_deaths <- input_data$D_active_transmission
ascertainr_cases <- input_data$I_active_transmission

## For plotting
cases_tall <- tidyr::gather(ascertainr_cases, states, cases, -dates)
deaths_tall <- tidyr::gather(ascertainr_deaths, states, deaths, -dates)

cases_tall$dates <- as.Date(cases_tall$dates)
deaths_tall$dates <- as.Date(deaths_tall$dates)

ggplot() +
  geom_point(data = cases_tall, aes(dates, cases), color = "black") +
  geom_point(data = deaths_tall, aes(dates, deaths), color = "red") +
  facet_wrap(~states, ncol = 2, scales = "free_y") +
  theme_classic()



## create a named list, as we use names everywhere from here on.
countries <- purrr::set_names(
  input_data$Country, input_data$Country
)

report_to_death <- report_to_death_distr(
  mu = mu_delta, std = std_delta, trunc = trunc
)

weighted_cases <- purrr::map(
  countries,
  function(country) {
    x <- matrix(ascertainr_cases[[country]], ncol = 1)
    ascertainr::weighted_incid(
      incid = x, weights = report_to_death, trunc = trunc
    )
  }
)


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
  },
  .id = "state"
)

ascertainment <- purrr::map(
  countries,
  function(country) {
    ascertainr::ascertainment(
      cfr_distr = cfr_distr,
      death_to_case = deaths_to_cases[[country]]
    )
  }
)

ascertainment_qntls <- purrr::map_dfr(
  ascertainment,
  function(rho) {
    df <- quantiles_to_df(rho)
    df <- cbind(
      date = ascertainr_deaths[["dates"]],
      df
    )
    df
  },
  .id = "country"
)


ncases_before_mu <- purrr::map(
  countries,
  function(country) {
    idx <- seq(
      1,
      length(ascertainr_deaths[[country]]) - round(mu_delta)
    )

    ascertainr::episize_before_mu(
      deaths = matrix(
        ascertainr_deaths[[country]][idx],
        ncol = 1
      ),
      mu_delta = mu_delta,
      cfr_distr = cfr_distr
    )
  }
)

ncases_before_mu_qntls <- purrr::imap_dfr(
  ncases_before_mu,
  function(x, country) {
    idx <- seq(
      1,
      length(ascertainr_deaths[[country]]) - round(mu_delta)
    )
    df <- quantiles_to_df(x)
    df <- cbind(
      date = ascertainr_deaths[["dates"]][idx],
      df
    )
    df
  },
  .id = "state"
)


ncases_after_mu <- purrr::map(
  countries,
  function(country) {
     idx <- seq(
      from = length(
        ascertainr_cases[[country]]
      ) - round(mu_delta) + 1,
      to = length(ascertainr_deaths[[country]])
    )
    message(paste0(idx, collapse = " "))
    ascertainr::episize_after_mu(
      cases = matrix(ascertainr_cases[[country]][idx], ncol = 1),
      rho = ascertainment[[country]][idx, ]
    )
  }
)

ncases_after_mu_qntls <- purrr::imap_dfr(
  ncases_after_mu,
  function(x, country) {
    idx <- seq(
      from = length(
        ascertainr_deaths[[country]]
      ) - round(mu_delta) + 1,
      to = length(ascertainr_deaths[[country]])
    )
    df <- quantiles_to_df(x)
    df <- cbind(
      date = ascertainr_deaths[["dates"]][idx],
      df
    )
    df
  },
  .id = "state"
)

deaths_projected <- purrr::map(
  countries,
  function(country) {
    idx <- seq(
      from = length(
        ascertainr_cases[[country]]
      ) - round(mu_delta) + 1,
      to = length(ascertainr_deaths[[country]])
    )

    ascertainr::episize_after_mu(
      cases = matrix(ascertainr_cases[[country]][idx], ncol = 1),
      rho = ascertainment[[country]][idx, ]
    )
  }
)
