## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_longer_forecasts.R",
  artefacts = list(
    data = list(
    description = "Collated model outputs (quantiles)",
    filenames = list(
      "unwtd_projections_qntls.rds",
      "wtd_per_country_projections_qntls.rds",
      "unwtd_ps_qntls.rds",
      "wtd_per_country_ps_qntls.rds",
      "unwtd_reff_qntls.rds",
      "wtd_per_country_reff_qntls.rds",
      "unwtd_reff_qntls_with_underreporting.rds",
      "wtd_per_country_reff_qntls_with_underreporting.rds"
    )
  )
 ),
 packages = c(
   "dplyr", "tidyr", "ggdist", "ggplot2", "rincewind", "purrr", "glue",
   "gridExtra"
 )
)

use_si <- "si_2"

dependancies <- purrr::map(
  weeks,
  function(week) {
    query <- glue::glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:use_si == \"{use_si}\")"
    )
    y <- list(
      produce_longer_forecasts = list(
        id = query,
        use =  list(
          "unweighted_pred_qntls.rds",
          "weighted_per_country_pred_qntls.rds",
          "unweighted_reff_qntls.rds",
          "unweighted_ps_qntls.rds",
          "weighted_per_country_ps_qntls.rds",
          "weighted_per_country_reff_qntls.rds",
          "unweighted_reff_with_underreporting.rds",
          "weighted_per_country_reff_with_underreporting.rds"
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
        "latest_deaths_wide_no_filter.rds" = "latest_deaths_wide_no_filter.rds"
      )
    )
  )
)

x$depends <- c(dependancies, dependancies5)


con <- file(
  here::here("src/collate_longer_forecasts/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
