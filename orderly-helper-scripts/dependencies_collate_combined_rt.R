## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_combined_rt.R",
  artefacts = list(
    data = list(
    description = "Collated combined rt estimates (quantiles)",
    filenames = c(
      "combined_rt_qntls.rds",
      "weekly_iqr.rds",
      "combined_weighted_estimates_across_countries.rds",
      "combined_weighted_estimates_per_country.rds"
    )
  )
  ),
  sources = c("R/utils.R"),
 packages = c("dplyr", "tidyr", "ggdist", "purrr", "ggplot2")
)


dependancies <- purrr::map(
  weeks,
  function(week) {
    query <- glue::glue(
      "latest(parameter:week_ending == \"{week}\" ",
       " && parameter:use_si == \"{use_si}\")"
    )
    y <- list(
      produce_combined_rt = list(
        id = query,
        use =  list(
          "combined_rt_estimates.rds",
          "weekly_iqr.rds",
          "combined_weighted_estimates_per_country.rds",
          "combined_weighted_estimates_across_countries.rds"
        )
      )
    )
    infiles <- purrr::map(
       y$produce_combined_rt$use,
       function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$produce_combined_rt$use) <- glue::glue("{infiles}_{week}.rds")
  y
 }
)

x$depends <- dependancies

con <- file(here::here("src/collate_combined_rt/orderly.yml"), "w")
yaml::write_yaml(x, con)
close(con)
