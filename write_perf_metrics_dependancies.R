## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "produce_performace_metrics.R",
  environment = list(covid_19_path = "COVID19_INPUT_PATH"),
  sources = c("R/utils.R"),
  parameters = "exclude",
  artefacts = list(
    data = list(
    description = "Model performance metrics",
    filenames = c(
      "model_predictions_error.csv",
      "wtd_all_prev_weeks_error.csv",
      "wtd_prev_week_error.csv",
      "unwtd_pred_error.csv"
    )
  )
 ),
 packages = c("dplyr", "tidyr", "assessr")
)

wtd_weeks <- list(
  "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07"
)

unwtd_weeks <- list(
  "2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07"
)

dependancies <- purrr::map(
  wtd_weeks,
  function(week) {
  y <- list(
    produce_weighted_ensemble = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use =  list(
      "wtd_ensb_prev_week.rds",
      "wtd_ensb_all_prev_weeks.rds"
      )
    )
   )
    infiles <- purrr::map(
      y$produce_weighted_ensemble$use,
      function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$produce_weighted_ensemble$use) <- glue::glue("{infiles}_{week}.rds")
    y
 }
)

dependancies2 <- purrr::map(
  unwtd_weeks,
  function(week) {
  y <- list(
    produce_ensemble_outputs = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use = list("ensemble_model_predictions.rds")
    )
  )
  infiles <- purrr::map(
    y$produce_ensemble_outputs$use,
    function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
  )
  names(y$produce_ensemble_outputs$use) <- glue::glue(
    "unwtd_{infiles}_{week}.rds"
  )
  y
 }
)

prepare_ecdc_data <- list(
  prepare_ecdc_data = list(
  id = "latest",
  use = list(
    `model_input.rds` = "latest_deaths_wide_no_filter.rds"
  )
)
)

x$depends <- list(
  dependancies,
  dependancies2,
  prepare_ecdc_data
)

con <- file("src/produce_performace_metrics/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
