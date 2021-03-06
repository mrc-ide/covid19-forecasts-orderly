x <- list(
  script = "script.R",
  parameters = c("week_ending", "use_si"),
  artefacts = list(
    data = list(
    description = "Collated model outputs (quantiles)",
    filenames = c(
      "weekly_iqr.rds",
      "combined_rt_estimates.rds",
      "combined_weighted_estimates_per_country.rds",
      "combined_weighted_estimates_across_countries.rds"
    )
  )
 ),
 packages = c(
   "dplyr", "tidyr", "ggplot2", "purrr", "rincewind", "ggpmisc",
   "purrr", "glue"
 ),
 sources =  "R/utils.R"
)

args <- commandArgs(TRUE)
week_ending <- as.Date(args)
## We will combine at most 8 weeks of data
week_starting <- max(
  as.Date("2020-03-08"), as.Date(week_ending) - 8 * 7
)

weeks_needed <- seq(
  from = week_starting, to = week_ending, by = "7 days"
)


## Generate orderly.yml for collate_model_outputs
## In principle we could look all the way back to March when combining
## Rt estimates. However that doesn't make a lot of sense.
## Particularly when we produce weighted estimates, as the
## probability decays exponentially, we would be sampling a
## negligible number from weeks further back. Therefore we can set an
## upper limit on the number of weeks which we are willing to combine.
## This can have a huge difference in run time when we
max_week_combine <- min(20, length(weeks_needed))
weeks_needed <- tail(weeks_needed, max_week_combine)

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
    produce_weights_combined_rt = list(
      id = glue::glue("latest(parameter:week_ending == \"{week_ending}\")"),
      use = list(
        "across_countries_beta.rds" =  "across_countries.rds",
        "per_country_beta.rds" = "per_country.rds",
        "country_weeks.rds" = "country_weeks.rds"
      )
    )
  )
)

dependancies6 <- list(
  list(
    produce_baseline_error = list(
      id = "latest(parameter:latest_week == \"2020-11-29\")",
      use = list("exclude.rds" =  "exclude.rds")
    )
  )
)

x$depends <- c(dependances, dependancies5, dependancies6)



con <- file(
  here::here("src/produce_combined_rt/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)

##parameters <- list(week_ending = as.character(week_ending), use_si = "si_2")
##orderly::orderly_run("produce_combined_rt", parameters = parameters)
