## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_longer_forecasts.R",
  artefacts = list(
    data = list(
    description = "Collated model outputs (quantiles)",
    filenames = list(
      "unwtd_projections_qntls.rds",
      "wtd_across_all_projections_qntls.rds",
      "wtd_per_country_projections_qntls.rds"
    )
  )
 ),
 packages = c(
   "dplyr", "tidyr", "ggdist", "ggplot2", "rincewind", "purrr", "glue"
 )
)

week_starting <- as.Date("2020-03-29")
week_ending <- as.Date("2020-07-12")

weeks_needed <- seq(
  from = week_starting, to = week_ending, by = "7 days"
)

use_si <- "si_2"

dependancies <- purrr::map(
  weeks_needed,
  function(week) {
    query <- glue::glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:use_si == \"{use_si}\")"
    )
    y <- list(
      produce_longer_forecasts = list(
        id = query,
        use =  list(
          "unwtd_projections.rds",
          "wtd_per_country_projections.rds",
          "wtd_across_all_projections.rds"
        )
      )
    )
    infiles <- purrr::map(
       y$produce_longer_forecasts$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$produce_longer_forecasts$use) <- glue::glue("{infiles}_{week}.rds")
  y
 }
)

dependancies5 <- list(
  list(
    prepare_ecdc_data = list(
      id = "latest",
      use = list(
        "latest_deaths_wide_no_filter.rds" =  "latest_deaths_wide_no_filter.rds"
      )
    )
  )
)

x$depends <- c(dependancies, dependancies5)


con <- file("src/collate_longer_forecasts/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
