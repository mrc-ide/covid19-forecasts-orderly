## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-02-28", location = "Florida", short_run = TRUE, reconstructed = TRUE))
set.seed(1)

if (reconstructed){
  model_input <- readRDS("model_input_reconstructed.rds")
} else {
  model_input <- readRDS("model_input.rds")
}

deaths_to_use <- model_input$D_active_transmission

incid <- tail(deaths_to_use[[location]], 10)

si_distrs <- readRDS("si_distrs.rds")

if (short_run) {
  iter <- 100
  chains <- 1
} else {
  iter <- 20000
  chains <- 2
}

## Generate stan fit
fit <- purrr::map(
  si_distrs,
  function(si_distr) {
    jointlyr::jointly_estimate(10, 100, incid, si_distr = si_distr,
                               seed = 42, iter = iter, chains = chains)
  }
)

## Extract values from stan fit
all_samples <- map(fit, rstan::extract)

## Take 1000 samples of foi and draw 10 samples from Poisson distribution
projections <- map(
  all_samples,
  function(samples) {
    foi <- samples[["incid_est"]][, 111:117]
    if (short_run){
      index <- sample(nrow(foi), nrow(foi), replace = FALSE)
    } else {
      index <- sample(nrow(foi), 1000, replace = FALSE)
    }
    foi <- foi[index, ]
    projections <- matrix(NA, nrow = 10000, ncol = 7)
    for (day in 1:7) {
      projections[, day] <- rep(foi[, day], each = 10)
    }
    projections <- apply(projections, c(1, 2), function(x) rpois(1, x))
  }
)


## Take 10000 samples from r_est estimates to be consistent with other model outputs
r_est <- map(
  all_samples,
  function(samples) {
    r_est <- samples[['rt_est']]
    if (short_run){ 
      sample(r_est, length(r_est), replace = FALSE)
    } else {
      sample(r_est, 10000, replace = FALSE)
    }
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



