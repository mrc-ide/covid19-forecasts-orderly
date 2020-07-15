## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "script.R",
  parameters = c("week_ending", "use_si"),
  artefacts = list(
    data = list(
    description = "Collated model outputs (quantiles)",
    filenames = c("weekly_iqr.rds", "combined_rt_estimates.rds")
  )
 ),
 packages = c("dplyr", "tidyr", "ggplot2", "purrr", "rincewind"),
 sources =  "R/utils.R"
)


week_starting <- as.Date("2020-03-08")
week_ending <- as.Date("2020-05-24")
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

x$depends <- c(dependances)



con <- file("src/produce_combined_rt/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)

##parameters <- list(week_ending = as.character(week_ending), use_si = "si_2")
##orderly::orderly_run("produce_combined_rt", parameters = parameters)
