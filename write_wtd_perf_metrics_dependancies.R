## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "produce_performace_metrics_ensb.R",
  environment = list(covid_19_path = "COVID19_INPUT_PATH"),
  sources = c("R/utils.R"),
  parameters = "window",
  artefacts = list(
    data = list(
    description = "Model performance metrics",
    filenames = list(
      "wtd_all_prev_weeks_error.csv",
      "wtd_prev_week_error.csv",
      "unwtd_pred_error.csv"
    )
  )
 ),
 packages = c("dplyr", "tidyr", "assessr", "slider")
)

wtd_weeks <- list(
  "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07", "2020-06-14",
  "2020-06-21", "2020-06-28"
)


dependancies2 <- purrr::map(
  wtd_weeks,
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

dependancies3 <- purrr::map(
  wtd_weeks,
  function(week) {
  y <- list(
    produce_weighted_ensemble = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use = list("wtd_ensb_prev_week.rds", "wtd_ensb_all_prev_weeks.rds")
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

dependancies5 <- list(
  list(
    prepare_ecdc_data = list(
      id = "latest",
      use = list(
        "model_input.rds" =  "latest_deaths_wide_no_filter.rds"
      )
    )
  )
)

dependancies <- c(dependancies2, dependancies3, dependancies5)



x$depends <- dependancies

con <- file("src/produce_performance_metrics_ensemble/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
