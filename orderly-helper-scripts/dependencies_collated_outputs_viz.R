## Generate orderly.yml for collate_model_outputs
collated_outputs_viz <- function(latest_week) {
  x <- list(
  script = "compare_ensemble_outputs.R",
  artefacts = list(
    data = list(
    description = "Forecasts collated",
    filenames = list("Brazil_forecasts.png")
  )
  ),
  parameters = list("use_si", "latest_week"),
  sources = list("R/utils.R"),
  packages = c("dplyr", "purrr", "tidyr", "ggplot2", "ggpubr",
              "ggforce", "snakecase", "glue", "cowplot", "rincewind")
)

  dependancies <- list (
    list(
      collate_model_outputs = list(
        id = "latest",
        use = list(
          "unweighted_qntls.rds" = "unweighted_qntls.rds",
          "unweighted_rt_qntls.rds" = "unweighted_rt_qntls.rds"
        )
      )
    ),
    list(
      prepare_ecdc_data = list(
        id = glue("latest(parameter:week_ending == \"{latest_week}\")"),
        use = list(
          "model_input.rds" = "latest_deaths_wide_no_filter.rds"
        )
      )
    ),
    list(
      produce_baseline_error = list(
        id = glue("latest(parameter:week_ending == \"2020-09-27\")"),
        use = list(
          "exclude.rds" = "exclude.rds"
        )
      )
    )
  )

  x$depends <- dependancies
  x

}


