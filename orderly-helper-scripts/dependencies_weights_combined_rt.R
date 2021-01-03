## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "script.R",
  parameters = c("week_ending", "use_si"),
  artefacts = list(
    data = list(
    description = "Weights for combined Rt estimates",
    filenames = c("across_countries.rds", "per_country.rds", "country_weeks.rds")
  )
 ),
 packages = c(
   "dplyr", "tidyr", "ggplot2", "purrr",
   "rincewind", "projections", "incidence", "glue", "assessr"
 )
)


week_starting <- as.Date("2020-03-08")

weeks_needed <- seq(
  from = week_starting, to = week_ending, by = "7 days"
)

dependances <- purrr::map(
  weeks_needed,
  function(week) {
  y <- list(
    produce_ensemble_outputs = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use =  list("ensemble_model_rt_samples.rds")
    )
   )
   infiles <- purrr::map(
     y$produce_ensemble_outputs$use,
     function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
  )
  names(y$produce_ensemble_outputs$use) <- glue::glue("{infiles}_{week}.rds")
  y
 }
)

dependancies5 <- list(
  list(
    prepare_ecdc_data = list(
      id = glue::glue("latest(parameter:week_ending == week_ending)"),
      use = list(
        "latest_deaths_wide_no_filter.rds" =  "latest_deaths_wide_no_filter.rds",
        "model_input.rds" = "latest_model_input.rds"
      )
    )
  )
)

dependancies6 <- list(
  list(
    produce_baseline_error = list(
      id = glue::glue("latest(parameter:latest_week == \"2020-11-29\")"),
      use = list("exclude.rds" =  "exclude.rds")
    )
  )
)

x$depends <- c(dependances, dependancies5, dependancies6)



con <- file(
  here::here("src/produce_weights_combined_rt/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)

##parameters <- list(week_ending = as.character(week_ending), use_si = "si_2")
##orderly::orderly_run("produce_combined_rt", parameters = parameters)
