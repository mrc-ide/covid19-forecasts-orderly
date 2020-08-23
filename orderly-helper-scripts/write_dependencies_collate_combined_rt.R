## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "collate_combined_rt.R",
  artefacts = list(
    data = list(
    description = "Collated combined rt estimates (quantiles)",
    filenames = "combined_rt_qntls.rds"
  )
 ),
 packages = c("dplyr", "tidyr", "ggdist")
)

week_starting <- as.Date("2020-03-22")
week_ending <- as.Date("2020-07-19")

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
      produce_combined_rt = list(
        id = query,
        use =  list("combined_rt_estimates.rds", "weekly_iqr.rds")
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

con <- file("src/collate_combined_rt/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
