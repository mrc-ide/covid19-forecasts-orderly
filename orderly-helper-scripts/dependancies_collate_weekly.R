x <- list(
  script = "collate_weekly_outputs.R",
  artefacts = list(
    data = list(
    description = "Collated model outputs for all locations",
    filenames = c(
      "us-ensemble-rt.rds",
      "us-ensemble-forecasts.rds",
      "rti0-rt.rds",
      "rti0-forecasts.rds",
      "apestim-rt.rds",
      "apeestim-forecasts.rds",
      "deca-rt.rds",
      "deca-forecasts.rds"
    )
    )
  ),
  packages = c("dplyr", "tidyr",
               "ggdist", "purrr", "ggplot2")
)

d1 <- map(
  locations,
  function(location) {
    query <- glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:location == \"{location}\")"
    )
    y <- list(
      run_jointlyr = list(
        id = query,
        use =  list(
          "rti0_model_outputs.rds"
        )
      )
    )
    infiles <- map(
       y$run_jointlyr$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$run_jointlyr$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

d2 <- map(
  locations,
  function(location) {
    query <- glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:location == \"{location}\")"
    )
    y <- list(
      run_deca = list(
        id = query,
        use =  list("DeCa_latest.rds")
      )
    )
    infiles <- map(
       y$run_deca$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$run_deca$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

d3 <- map(
  locations,
  function(location) {
    query <- glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:location == \"{location}\")"
    )
    y <- list(
      run_apeestim = list(
        id = query,
        use =  list("apeestim_model_outputs.rds")
      )
    )
    infiles <- map(
       y$run_apeestim$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$run_apeestim$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

d4 <- map(
  locations,
  function(location) {
    query <- glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:location == \"{location}\")"
    )
    y <- list(
      produce_ensemble_outputs = list(
        id = query,
        use =  list(
          "ensemble_model_rt_samples.rds",
          "ensemble_model_predictions.rds"
        )
      )
    )
    infiles <- map(
       y$produce_ensemble_outputs$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$produce_ensemble_outputs$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

x$depends <- c(d1, d2, d3, d4)

con <- file(
  here::here("src/collate_weekly_outputs/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
