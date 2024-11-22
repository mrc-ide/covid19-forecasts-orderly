## Set up for orderly2

orderly_parameters(week_ending = NULL,
                   location = NULL,
                   short_run = NULL,
                   reconstructed = NULL)

# To use version of jointlyr that can project ahead 28 days:
#remotes::install_github("mrc-ide/jointlyr@project28")

packages <- c("purrr", "jointlyr", "rstan", "tidyr", "dplyr")
lapply(packages, require, character.only = TRUE)

orderly_artefact(
  "Model outputs",
  c(
    "rti0_model_outputs.rds",
    "r_rti0.rds"
  )
)

orderly_dependency(
  "prepare_jhu_data",
  "latest",
  c("model_input.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest",
  c("model_input_reconstructed.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "produce_epi_params",
  "latest",
  c("si_distrs.rds")
)


##-----------------------------------------------------------------

set.seed(1)

if (reconstructed){
  model_input <- readRDS("model_input_reconstructed.rds")
} else {
  model_input <- readRDS("model_input.rds")
}

if(location == "all") {
  location <- model_input$State
} else {
  location <- c(
    "California", "Colorado", "Delaware", "Hawaii", "Maryland", "Missouri",
    "New Jersey", "New York", "North Dakota", "Ohio", "Pennsylvania",
    "Tennessee", "Texas"
  )
}

deaths_to_use <- model_input$D_active_transmission %>% filter(dates <= week_ending)

tall_deaths <- gather(
  deaths_to_use[,c("dates", location)], key = province_state, value = deaths, -dates
) %>%
  split(.$province_state)

si_distr <- readRDS("si_distrs.rds")$si_1

if (short_run) {
  iter <- 100
  chains <- 1
} else {
  iter <- 20000
  chains <- 2
}

## Sliding window of four-week ahead projections
projection_week <- seq(from = as.Date(week_ending) - (7 * 4),
                       to = as.Date("2020-03-15"), by = -7)

## Generate stan fit
# Joint estimation of incidence and reproduction number
all_samples <- purrr::map(
  set_names(projection_week),
  function(proj_week) {
    message("Projection week: ", proj_week)
    purrr::imap(
  tall_deaths,
  function(death_data, location){
    print(location)
    death_data <- death_data %>% filter(dates <= as.Date(proj_week))
    incid <- tail(death_data$deaths, 10) # Take last 10 days of death data
    fit <- jointlyr::jointly_estimate(window = 10, # window of data used for estimation
                               window_back = 100, # length of time incidence should be estimated
                               incid, # numeric vector of length matching window
                               si_distr = si_distr, seed = 42, iter = iter,
                               chains = chains)
    rstan::extract(fit)
  }
    )
  }
)

## Take 1000 samples of foi and draw 10 samples from Poisson distribution
projections <- imap(
  all_samples,
  function(samples_by_state, proj_week) {
    message("Processing projection week: ", proj_week)
    purrr::map(
      samples_by_state,
      function(samples) {
        if (is.null(samples)) {
          return(NULL)
        } else {
    foi <- samples[["incid_est"]][, 111:138] # foi for latest 28 days
    if (short_run){
      index <- sample(nrow(foi), nrow(foi), replace = FALSE)
    } else {
      index <- sample(nrow(foi), 1000, replace = FALSE)
    }
    foi <- foi[index, ]
    projections <- matrix(NA, nrow = 10000, ncol = 28)
    for (day in 1:28) {
      projections[, day] <- rep(foi[, day], each = 10)
    }
    projections <- apply(projections, c(1, 2), function(x) rpois(1, x))
        }
      }
    )
  }
)


## Take 10000 samples from r_est estimates to be consistent with other model outputs
r_est <- imap(
  all_samples,
  function(samples_by_state, proj_week) {
    message("Processing projection week: ", proj_week)
    purrr::map(
      samples_by_state,
      function(samples) {
        if (is.null(samples)) {
          return(NULL)
        } else {
    r_est <- samples[['rt_est']]
    if (short_run){ 
      sample(r_est, length(r_est), replace = FALSE)
    } else {
      sample(r_est, 10000, replace = FALSE)
    }
        }
      }
    )
  }
)

## Save in format required
out <- saveRDS(
  object = list(
    I_active_transmission = model_input[["I_active_transmission"]][, c("dates", location)],
    D_active_transmission = model_input[["D_active_transmission"]][, c("dates", location)],
    State = location,
    R_last = r_est,
    Predictions = projections
  ),
  file = "rti0_model_outputs.rds"
)

saveRDS(object = r_est, file = "r_rti0.rds")



