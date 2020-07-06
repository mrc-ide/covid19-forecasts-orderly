## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "produce_performace_metrics.R",
  environment = list(covid_19_path = "COVID19_INPUT_PATH"),
  sources = c("R/utils.R"),
  parameters = "exclude",
  artefacts = list(
    data = list(
    description = "Model performance metrics",
    filenames = "model_predictions_error.csv"
  )
 ),
 packages = c("dplyr", "tidyr", "assessr")
)

unwtd_weeks <- list(
  "2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07", "2020-06-14",
  "2020-06-21", "2020-06-28"
)

dependancies2 <- purrr::map(
  unwtd_weeks,
  function(week) {
  y <- list(
    run_rti0 = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use = "RtI0_latest_output.rds"
    )
    ## run_apeestim = list(
    ##   id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
    ##   use = "apeestim_model_outputs.rds"
    ## ),
    ## DeCa_model = list(
    ##   id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
    ##   use = "DeCa_latest.rds"
    ## )
  )
  names(y[[1]]$use) <- glue::glue("RtI0_Std_results_week_end_{week}.rds")
  ##names(y[[2]]$use) <- glue::glue("sbkp_Std_results_week_end_{week}.rds")
  ##names(y[[3]]$use) <- glue::glue("DeCa_Std_results_week_end_{week}.rds")
  y
 }
)

dependancies2$prepare_ecdc_data <- list(
  id = "latest",
  use = list(
    `model_input.rds` = "latest_deaths_wide_no_filter.rds"
  )
)


x$depends <- dependancies2

con <- file("src/produce_performace_metrics/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
