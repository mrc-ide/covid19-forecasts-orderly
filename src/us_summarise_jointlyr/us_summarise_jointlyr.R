## Set up for orderly2

orderly_parameters(week_ending = NULL,
                   reconstructed = NULL,
                   incidence_type = NULL)

orderly_artefact(
  "Collated model outputs for all locations",
  c("rti0_qntls.rds",
    "rti0_qntls_weekly.rds",
    "rti0_crps.rds",
    "rti0_crps_weekly.rds"
  )
)

packages <- c("dplyr", "tidyr", "purrr", "tibble", "scoringutils")
lapply(packages, require, character.only = TRUE)

source("R/utils.R")

orderly_dependency(
  "prepare_jhu_data",
  paste0("latest(parameter:week_ending == '", as.character(week_ending), "')"),
  c("model_input.rds" = "latest_model_input.rds")
)

orderly_dependency(
  "us_run_jointlyr",
  paste0("latest(parameter:week_ending == '", as.character(week_ending),
         "' && parameter:reconstructed ==", reconstructed, ")"),
  c("rti0_model_outputs.rds" = "rti0_model_outputs.rds")
)

##-----------------------------------------------------------------

probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

rti0_outputs <- readRDS("rti0_model_outputs.rds")

rti0_predictions <- rti0_outputs[['Predictions']]

## For coverage probability we want to know the proportion of the 10,000 simulations where
# the credible interval contains the true value e.g. for a well calibrated model
# the 95% coverage probability should be 95% and the 50% coverage probability should be 50%

rti0_qntls <- extract_predictions_qntls_multi(rti0_predictions, probs)
rti0_qntls_weekly <- daily_to_weekly_multi(rti0_predictions, probs)

saveRDS(rti0_qntls, "rti0_qntls.rds")
saveRDS(rti0_qntls_weekly, "rti0_qntls_weekly.rds")


## Estimate the CRPS

# Add true inc
model_input <- readRDS("model_input.rds")

if (incidence_type == "deaths") {
  inc_to_use <- model_input$D_active_transmission %>% filter(dates <= week_ending)
} else if (incidence_type == "cases") {
  inc_to_use <- model_input$I_active_transmission %>% filter(dates <= week_ending)
}

# Daily CRPS
# For each prediction day compare to true incidence using crps_sample(observed, predicted)
rti0_crps <- crps_estimates_daily(rti0_predictions, inc_to_use)
saveRDS(rti0_crps, "rti0_crps.rds")

# Weekly CRPS
rti0_crps_weekly <- crps_estimates_weekly(rti0_predictions, inc_to_use)
saveRDS(rti0_crps_weekly, "rti0_crps_weekly.rds")

