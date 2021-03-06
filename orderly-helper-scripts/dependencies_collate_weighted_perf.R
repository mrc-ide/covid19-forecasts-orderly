## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_performace_metrics_ensb.R",
  artefacts = list(
    data = list(
      description = "Model performance metrics",
      filenames = list("unwtd_pred_error.csv")
    )
  ),
  packages = c("dplyr", "purrr", "readr")
)

dependancies <- map(
  weeks,
  function(week) {
      y <- list(
        produce_performance_metrics_ensemble = list(
          id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
          use = list("unwtd_pred_error.csv")
        )
      )
      infiles <- map(
        y$produce_performance_metrics_ensemble$use,
        function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
      )
      names(y$produce_performance_metrics_ensemble$use) <-
        glue::glue("{infiles}_{week}.csv")
      y
  }
)

dependancies2 <- map(
  weeks,
  function(week) {
      y <- list(
        produce_performace_metrics = list(
          id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
          use = list("model_predictions_error.csv")
        )
      )
      infiles <- map(
        y$produce_performace_metrics$use,
        function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
      )
      names(y$produce_performace_metrics$use) <-
        glue("{infiles}_{week}.csv")
      y
  }
)

x$depends <- c(dependancies, dependancies2)


con <- file(
  here::here("src/collate_weighted_performance_metrics/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
