probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
output_files <- list.files(covid_19_path)
output_files <- output_files[grepl(x = output_files, pattern = week_ending)]
output_files <- output_files[!grepl(x = output_files, pattern = "sbsm")]
names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)
names(week_ending) <- week_ending
message("For week ending ", week_ending)

message("Output Files \n", paste(output_files, collapse = "\n"))

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

## Model weights derived from last week's forecasts only.
weights_prev_week <- readRDS("weights_prev_week.rds")
weights_all_prev_weeks <- readRDS("weights_all_prev_weeks.rds")

prev_week <- as.Date(week_ending) - 7
## ## This will be a list with two components corresponding to the two
## ## serial intervals used.
weights_prev_week <- weights_prev_week[[as.character(prev_week)]]
weights_all_prev_weeks <- weights_all_prev_weeks[[as.character(prev_week)]]



weights_prev_week_normalised <- purrr::map(
  weights_prev_week, ~ normalise_weights(., "wt_empirical")
)

weights_all_prev_weeks_normalised <- purrr::map(
  weights_all_prev_weeks, ~ normalise_weights(., "wt_empirical")
)

## ## Sanity check:  purrr::map(normalised_wts, ~ sum(unlist(.)))
wtd_ensb_prev_week <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
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
        daily_to_weekly(x, prob = probs)
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
        daily_to_weekly(x, probs)
      },
      .id = "country"
    )
  },
  .id = "proj"
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
