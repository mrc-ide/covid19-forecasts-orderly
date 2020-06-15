x <- list(
  script = "collate_model_outputs.R",
  artefacts = list(
    description = "Collated model outputs (quantiles)",
    filenames = c(
      "unweighted_qntls.rds", "wtd_prev_week_qntls.rds",
      "wtd_all_prev_weeks_qntls.rds", "unweighted_rt_qntls.rds",
      "wtd_prev_week_rt_qntls.rds", "wtd_rt_all_prev_week_qntls.rds",
      "wtd_prev_week_rt_samples.rds", "wtd_all_prev_week_rt_samples.rds",
      "unwtd_rt_samples.rds"
    )
  ),
  packages = c("dplyr", "tidyr")
)

wtd_weeks <- list(
  "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07"
)

unwtd_weeks <- list(
  "2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29","2020-04-05",
  "2020-04-12", "2020-04-19", "2020-04-26", "2020-05-03","2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07"
)

dependancies <- vector(
  mode = "list", length = length(wtd_weeks) + length(unwtd_weeks)
)

for (idx in seq_along(wtd_weeks)) {
  week <- wtd_weeks[[idx]]
  y <- list(
    produce_weighted_ensemble = list(
      id = glue::glue("latest(parameter:week_ending == \"{week}\")"),
      use = list(
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

  names(y$produce_weighted_ensemble$use) <- glue(
    "{infiles}_{week}.rds"
  )
  dependancies[[idx]] <- y
}

indices <- seq_along(unwtd_weeks)


for (idx in  indices) {
  message(idx)
  week <- unwtd_weeks[[idx]]
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

  names(y$produce_ensemble_outputs$use) <- glue(
    "unwtd_{infiles}_{week}.rds"
  )
  dependancies[[idx]] <- y
}

x$depends <- dependancies
yaml::write_yaml(x, "src/collate_model_outputs/orderly.yml")
