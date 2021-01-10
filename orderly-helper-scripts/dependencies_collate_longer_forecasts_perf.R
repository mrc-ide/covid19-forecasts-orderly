## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "script.R",
  artefacts = list(
    data = list(
    description = "Error for long forecasts",
    filenames = list(
      "long_projections_error_daily.rds",
      "long_projections_error_weekly.rds"
    )
  )
 ),
 packages = c("dplyr", "purrr")
)

dependancies <- purrr::map(
  weeks,
  function(week) {
      y <- list(
        produce_longer_forecasts_metrics = list(
          id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
          use = list("long_projections_error.rds")
        )
      )
      infiles <- purrr::map(
        y$produce_longer_forecasts_metrics$use,
        function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
      )
      names(y$produce_longer_forecasts_metrics$use) <-
        glue::glue("{infiles}_{week}.rds")
      y
  }
)


dependancies6 <- list(
  list(
    produce_baseline_error = list(
      id = "latest(parameter:latest_week == \"2020-11-29\")",
      use = list("exclude.rds" =  "exclude.rds")
    )
  )
)

x$depends <- c(dependancies, dependancies6)


con <- file(
  here::here("src/collate_longer_forecasts_metrics/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
