## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "produce_performace_metrics_ensb.R",
  environment = list(covid_19_path = "COVID19_INPUT_PATH"),
  sources = c("R/utils.R"),
  parameters = c("window", "week_ending"),
  artefacts = list(
    data = list(
    description = "Model performance metrics",
    filenames = list(
      ##"wtd_all_prev_weeks_error.csv",
      ##"wtd_prev_week_error.csv",
      "unwtd_pred_error.csv"
    )
  )
 ),
 packages = c("dplyr", "tidyr", "assessr", "slider", "purrr")
)


dependancies2 <- list(
  list(
    produce_ensemble_outputs = list(
      id = glue::glue("latest(parameter:week_ending == \"{week_ending}\")"),
      use = list(unwtd_ensemble_model_predictions.rds = "ensemble_model_predictions.rds")
    )
  )
)

## dependancies3 <- purrr::map(
##   wtd_weeks,
##   function(week) {
##   y <- list(
##     produce_weighted_ensemble = list(
##       id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
##       use = list("wtd_ensb_prev_week.rds", "wtd_ensb_all_prev_weeks.rds")
##     )
##   )
##   infiles <- purrr::map(
##     y$produce_weighted_ensemble$use,
##     function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
##   )
##   names(y$produce_weighted_ensemble$use) <- glue::glue("{infiles}_{week}.rds")
##   y
##  }
## )

dependancies5 <- list(
  list(
    prepare_ecdc_data = list(
      id = "latest",
      use = list(
        "model_input.rds" = "latest_deaths_wide_no_filter.rds"
      )
    )
  )
)

##dependancies <- c(dependancies2, dependancies3, dependancies5)
dependancies <- c(dependancies2, dependancies5)


x$depends <- dependancies

con <- file(
  here::here("src/produce_performance_metrics_ensemble/orderly.yml"),
  "w"
)
yaml::write_yaml(x, con)
close(con)
