## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-15"))
## probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
## weeks_ending <- readr::read_rds("latest_week_ending.rds")

output_files <- list.files(covid_19_path)
output_files <- output_files[grepl(x = output_files, pattern = week_ending)]

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)
names(week_ending) <- week_ending
message("For week ending ", week_ending)

message("Output Files ", output_files)

model_outputs <- purrr::map(
  output_files, ~ readRDS(paste0(covid_19_path, .))
)

## Equal weighted models
## First Level is model, 2nd is country, 3rd is SI.
idx <- grep(x = names(model_outputs), pattern = week_ending)
outputs <- purrr::map(model_outputs[idx], ~ .[["Predictions"]])
names(outputs) <-  sapply(
  names(outputs), function(x) strsplit(x, "_")[[1]][1]
)

countries <- names(outputs[[1]])
names(countries) <- countries

ensemble_model_predictions <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
        wts <- data.frame(
          model = names(outputs),
          normalised_wt = 1
        )
        wts <- list(si_1 = wts, si_2 = wts)
        f(outputs, country, wts)
      }
    )
  }
)


## Model weights derived from last week's forecasts only.
weights_prev_week <- readRDS("weights_prev_week.rds")
weights_all_prev_weeks <- readRDS("weights_all_prev_weeks.rds")

prev_week <- as.Date(week_ending) - 7
## This will be a list with two components corresponding to the two
## serial intervals used.
weights_prev_week <- weights_prev_week[[as.character(prev_week)]]
weights_all_prev_weeks <- weights_all_prev_weeks[[as.character(prev_week)]]



## weights_prev_week has a date associated with it
## so that weights_prev_week[[1]] is what we really want
weights_prev_week_normalised <- purrr::map(
  weights_prev_week, normalise_weights
)

weights_all_prev_weeks_normalised <- purrr::map(
  weights_all_prev_weeks, normalise_weights
)

## Sanity check:  purrr::map(normalised_wts, ~ sum(unlist(.)))
wtd_ensb_prev_week <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
        wts <- rep(1, length(outputs))
        names(wts) <- names(outputs)
        wts <- list(si_1 = wts, si_2 = wts)
        f(outputs, country, weights_prev_week_normalised)
      }
    )
  }
)


wtd_ensb_all_prev_weeks <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
        wts <- rep(1, length(outputs))
        names(wts) <- names(outputs)
        wts <- list(si_1 = wts, si_2 = wts)
        f(outputs, country, weights_all_prev_weeks_normalised)
      }
    )
  }
)

saveRDS(
  object = wtd_ensb_prev_week,
  "wtd_ensb_prev_week.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks,
  "wtd_ensb_all_prev_weeks.rds"
)


ensemble_daily_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred, ~ extract_predictions_qntls(., probs),
      .id = "country"
    )
  },
  .id = "proj"
)

ensemble_weekly_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred,
      function(x) {
        message(colnames(x))
        daily_to_weekly(x)
      },
      .id = "country"
    )
  },
  .id = "proj"
)


wtd_ensb_prev_week_daily_qntls <- purrr::map_dfr(
  wtd_ensb_prev_week,
  function(pred) {
    purrr::map_dfr(
      pred, ~ extract_predictions_qntls(., probs),
      .id = "country"
    )
  },
  .id = "proj"
)

wtd_ensb_prev_week_weekly_qntls <- purrr::map_dfr(
  wtd_ensb_prev_week,
  function(pred) {
    purrr::map_dfr(
      pred,
      function(x) {
        message(colnames(x))
        daily_to_weekly(x)
      },
      .id = "country"
    )
  },
  .id = "proj"
)

wtd_ensb_all_prev_weeks_daily_qntls <- purrr::map_dfr(
  wtd_ensb_all_prev_weeks,
  function(pred) {
    purrr::map_dfr(
      pred, ~ extract_predictions_qntls(., probs),
      .id = "country"
    )
  },
  .id = "proj"
)

wtd_ensb_all_prev_weeks_weekly_qntls <- purrr::map_dfr(
  wtd_ensb_all_prev_weeks,
  function(pred) {
    purrr::map_dfr(
      pred,
      function(x) {
        message(colnames(x))
        daily_to_weekly(x)
      },
      .id = "country"
    )
  },
  .id = "proj"
)

saveRDS(
  object = ensemble_model_predictions,
  file = "ensemble_model_predictions.rds"
)

saveRDS(
  object = wtd_ensb_prev_week,
  file = "wtd_ensb_prev_week.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks,
  file = "wtd_ensb_all_prev_weeks.rds"
)


saveRDS(
  object = ensemble_daily_qntls,
  file = "ensemble_daily_qntls.rds"
)

saveRDS(
  object = ensemble_weekly_qntls,
  file = "ensemble_weekly_qntls.rds"
)

saveRDS(
  object = wtd_ensb_prev_week_daily_qntls,
  file = "wtd_ensb_prev_week_daily_qntls.rds"
)

saveRDS(
  object = wtd_ensb_prev_week_weekly_qntls,
  file = "wtd_ensb_prev_week_weekly_qntls.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks_daily_qntls,
  file = "wtd_ensb_all_prev_weeks_daily_qntls.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks_weekly_qntls,
  file = "wtd_ensb_all_prev_weeks_weekly_qntls.rds"
)

