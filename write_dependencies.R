## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_model_outputs.R",
  artefacts = list(
    data = list(
    description = "Collated model outputs (quantiles)",
    filenames = c(
      "unweighted_qntls.rds", "wtd_prev_week_qntls.rds",
      "wtd_all_prev_weeks_qntls.rds", "unweighted_rt_qntls.rds",
      "wtd_prev_week_rt_qntls.rds", "wtd_rt_all_prev_week_qntls.rds",
      "wtd_prev_week_rt_samples.rds", "wtd_all_prev_week_rt_samples.rds",
      "unwtd_rt_samples.rds"
    )
  )
 ),
 packages = c("dplyr", "tidyr")
)

wtd_weeks <- list(
  "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07",
  "2020-06-14", "2020-06-21", "2020-06-28", "2020-07-05"
)

unwtd_weeks <- list(
  "2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07",
  "2020-06-14", "2020-06-21", "2020-06-28", "2020-07-05"
)

dependancies <- purrr::map(
  wtd_weeks,
  function(week) {
  y <- list(
    produce_weighted_ensemble = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use =  list(
      "wtd_ensb_prev_week_daily_qntls.rds",
      "wtd_ensb_all_prev_weeks_daily_qntls.rds",
      "wtd_rt_prev_week.rds",
      "wtd_rt_all_prev_week.rds",
      "wtd_rt_prev_week_qntls.rds",
      "wtd_rt_all_prev_week_qntls.rds"
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
      use = list(
        "ensemble_daily_qntls.rds",
        "ensemble_model_rt.rds",
        "ensemble_model_rt_samples.rds"
      )
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

x$depends <- c(dependancies, dependancies2, dependancies5)

con <- file("src/collate_model_outputs/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
