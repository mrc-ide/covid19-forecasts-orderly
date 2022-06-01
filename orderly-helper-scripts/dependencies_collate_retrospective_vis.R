## Generate orderly.yml for collate_model_outputs
x <- list(
  script = "script.R",
  resources = "country-and-continent-codes-list.csv",
  parameters = "week_ending",
  artefacts = list(
    staticgraph = list(
    description = "Number of countries included",
    filenames = c("n_included_line.png", "epicurve_by_continent.png",
                  "epicurve_pline.png", "countries_included_each_week.csv")
  )
 ),
 packages = c("cowplot", "dplyr", "ggplot2", "ggthemes", "glue", "purrr",
              "rincewind")
)

unwtd_weeks <- seq(
  from = as.Date("2021-05-09"), to = as.Date(week), by = "7 days"
)
not_run <- c("2021-04-04", "2021-05-02", "2021-05-30", "2021-08-29",
             "2021-10-03", "2021-12-26", "2022-01-02", "2022-04-17")
unwtd_weeks <- unwtd_weeks[! unwtd_weeks %in% as.Date(not_run)]

x$depends <- map(
  unwtd_weeks,
  function(week) {
  y <- list(
    prepare_ecdc_data = list(
      id = glue("latest(parameter:week_ending == \"{week}\")"),
      use = list("latest_model_input.rds")
    )
  )
  names(y[[1]]$use) <- glue("model_input_{week}.rds")
  y
 }
)

con <- file("src/produce_retrospective_vis/orderly.yml", "w")
yaml::write_yaml(x, con)
close(con)
