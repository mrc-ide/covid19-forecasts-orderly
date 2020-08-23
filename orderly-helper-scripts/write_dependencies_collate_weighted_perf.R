## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_performace_metrics_ensb.R",
  environment = list(covid_19_path = "COVID19_INPUT_PATH"),
  parameters = c("window", "week_ending"),
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
 packages = c("dplyr", "purrr")
)
weeks <- list(
  "2020-03-29", "2020-04-05",
  "2020-04-12",
  "2020-04-19", "2020-04-26", "2020-05-03", "2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07",
  "2020-06-14", "2020-06-21", "2020-06-28", "2020-07-05",
  "2020-07-12", "2020-07-19", "2020-07-26", "2020-08-02",
  "2020-08-09"
)

dependancies <- purrr::map(
  weeks,
  function(week) {
      y <- list(
        produce_performance_metrics_ensemble = list(
          id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
          use = list("unwtd_pred_error.csv")
        )
      )
      infiles <- purrr::map(
        y$produce_performance_metrics_ensemble$use,
        function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
      )
      names(y$produce_performance_metrics_ensemble$use) <-
        glue::glue("{infiles}_{week}.csv")
      y
  }
)

x$depends <- dependancies


con <- file(
  here::here("src/collate_weighted_performance_metrics/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
