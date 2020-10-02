## Generate orderly.yml for collate_model_outputs
dependencies_weighted_performance <- function(week) {

  x <- list(
    script = "produce_performace_metrics_ensb.R",
    environment = list(covid_19_path = "COVID19_INPUT_PATH"),
    sources = c("R/utils.R"),
    parameters = c("window", "week_ending"),
    artefacts = list(
      data = list(
        description = "Model performance metrics",
        filenames = list("unwtd_pred_error.csv")
      )
    ),
    packages = c("dplyr", "tidyr", "assessr", "slider", "purrr")
  )


  dependancies2 <- list(
    list(
      produce_ensemble_outputs = list(
        id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
        use = list(
          unwtd_ensemble_model_predictions.rds =
            "ensemble_model_predictions.rds"
        )
      )
    )
  )


  week_next <- as.Date(week) + 7
  dependancies5 <- list(
    list(
      prepare_ecdc_data = list(
        id = glue::glue("latest(parameter:week_ending == \"{week_next}\")"),
        use = list(
          "model_input.rds" = "latest_deaths_wide_no_filter.rds"
        )
      )
    )
  )

  dependancies <- c(dependancies2, dependancies5)
  x$depends <- dependancies

  x
}
