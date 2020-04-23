probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
## This is copied from produce_enseble_outputs task.
## TODO Clean up.
weights <- readr::read_csv("model_weights.csv")
## Weights derived from the last forecast period.
prev_week <- as.Date(week_ending) - 7
weights <- dplyr::filter(
  weights, forecast_date == prev_week
  )
## Set the model name to the model for which weights are to be used.
## This will help us in matching up weights with models later.
weights$model <- glue::glue(
  "{weights$model}_Std_results_week_end_{week_ending}"
  )

##probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
##weeks_ending <- readr::read_rds("latest_week_ending.rds")

output_files <- list.files(covid_19_path)
output_files <- output_files[grepl(x = output_files, pattern = week_ending)]

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)
names(week_ending) <- week_ending
message("For week ending ", week_ending)
message("Output Files ", paste0(output_files, collapse = " "))

model_outputs <- purrr::map(
  output_files, ~ readRDS(paste0(covid_19_path, .))
)


## For each, for each country, pool projections from diff models
## In the output, the first level is week, 2nd is country and 3rd
## is SI.
outputs <- purrr::map(model_outputs, ~ .[["Predictions"]])
countries <- names(outputs[[1]])
names(countries) <- countries

ensemble_model_predictions <- purrr::map(
  countries,
  function(country) {
    message(country)
    message(names(outputs))
    ## y is country specific output
    y <- purrr::map(outputs, ~ .[[country]])
    ## y has 2 components, one for each SI.
    y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
    y_2 <- purrr::map(y, ~ .[[2]]) ## si_1
    ## Get weights
    weights_si1 <- weights[weights$si == "si_1", ]
    weights_si1 <- weights_si1[match(names(outputs), weights_si1$model), ]
    weights_si1 <- dplyr::pull(weights_si1, model_weights)
    weights_si1[is.na(weights_si1)] <- 0

    weights_si2 <- weights[weights$si == "si_2", ]
    weights_si2 <- weights_si2[match(names(outputs), weights_si2$model), ]
    weights_si2 <- dplyr::pull(weights_si2, model_weights)
    weights_si2[is.na(weights_si2)] <- 0


    out <- list(
      pool_predictions(y_1, weights_si1),
      pool_predictions(y_2, weights_si2)
    )
  }
)


ensemble_daily_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
   ~ extract_predictions_qntls(., probs),
  .id = "country"
)

ensemble_weekly_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(x) {
        message(colnames(x))
        daily_to_weekly(x, probs)
      },
      .id = "country"
)

readr::write_rds(
  x = ensemble_model_predictions,
  "wtd_ensemble_model_predictions.rds"
)

readr::write_rds(
  x = ensemble_daily_qntls,
  path = "wtd_ensemble_daily_qntls.rds"
)

readr::write_rds(
  x = ensemble_weekly_qntls,
  path = "wtd_ensemble_weekly_qntls.rds"
)
