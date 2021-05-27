## Generate orderly.yml for collate_model_outputs
dependencies_ensb_performance <- function(week) {

  x <- list(
    script = "produce_performace_metrics_ensb.R",
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


dependencies_indvdl_performance <- function(week) {

  x <- list(
    script = "produce_performace_metrics.R",
    sources = c("R/utils.R"),
    parameters = c("window", "week_ending"),
    artefacts = list(
      data = list(
        description = "Model performance metrics",
        filenames = list("model_predictions_error.csv")
      )
    ),
    packages = c("dplyr", "tidyr", "assessr", "slider", "purrr")
  )


  dependancies2 <- list(
    list(
      run_apeestim = list(
        id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
        use = list(
          apeestim_latest_output.rds =
            "apeestim_model_outputs.rds"
        )
      )
    )
  )

  dependancies3 <- list(
    list(
      run_rti0 = list(
        id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
        use = list(
          RtI0_latest_output.rds =
            "RtI0_latest_output.rds"
        )
      )
    )
  )

  dependancies4 <- list(
    list(
      DeCa_model = list(
        id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
        use = list(
          DeCa_latest_output.rds = "DeCa_latest.rds"
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

  dependancies <- c(
    dependancies2, dependancies3, dependancies4,
    dependancies5
  )
  x$depends <- dependancies

  x
}

## To run on the servr
week <- commandArgs(TRUE)
x <- dependencies_ensb_performance(week)
con <- file(
  here::here("src/produce_performance_metrics_ensemble/orderly.yml"),
  "w"
)
yaml::write_yaml(x, con)
close(con)

x <- dependencies_indvdl_performance(week)
con <- file(
  here::here("src/produce_performace_metrics/orderly.yml"),
  "w"
)
yaml::write_yaml(x, con)
close(con)
