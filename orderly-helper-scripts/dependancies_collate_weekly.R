x <- list(
  script = "collate_weekly_outputs.R",
  artefacts = list(
    data = list(
    description = "Collated model outputs for all locations",
    filenames = c(
      "us_ensemble_rt_qntls.rds",
      "us_ensemble_forecasts_qntls.rds",
      "rti0_qntls.rds",
      "apeestim_qntls.rds",
      "deca_qntls.rds",
      "rti0_rt_qntls.rds",
      "apeestim_rt_qntls.rds",
      "deca_rt_qntls.rds"
    )
    )
  ),
  parameters = "week_ending",
  packages = c("dplyr", "tidyr", "purrr", "tibble")
)

queries <- glue(
  "latest(parameter:week_ending == \"{week}\" ",
  "&& parameter:location == \"{locations}\")"
)


d1 <- map2(
  locations, queries, function(location, query) {
    y <- list(
      us_run_jointlyr = list(
        id = query,
        use =  list(
          "rti0_model_outputs.rds"
        )
      )
    )
    infiles <- map(
       y$us_run_jointlyr$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$us_run_jointlyr$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

d2 <- map2(
  locations, queries, function(location, query) {
    y <- list(
      us_run_deca = list(
        id = query,
        use =  list("DeCa_latest.rds")
      )
    )
    infiles <- map(
       y$us_run_deca$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$us_run_deca$use) <- glue("{infiles}_{location}.rds")
  y
 }
)


d3 <- map2(
  locations, queries, function(location, query) {
    y <- list(
      us_run_apeestim = list(
        id = query,
        use =  list("apeestim_model_outputs.rds")
      )
    )
    infiles <- map(
       y$us_run_apeestim$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$us_run_apeestim$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

d4 <- map2(
  locations, queries, function(location, query) {
    y <- list(
      us_produce_ensemble_outputs = list(
        id = query,
        use =  list(
          "ensemble_model_rt.rds",
          "ensemble_daily_qntls.rds"
        )
      )
    )
    infiles <- map(
       y$us_produce_ensemble_outputs$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$us_produce_ensemble_outputs$use) <- glue("{infiles}_{location}.rds")
  y
 }
)

x$depends <- c(d1, d2, d3, d4)

con <- file(
  here::here("src/us_collate_weekly_outputs/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
