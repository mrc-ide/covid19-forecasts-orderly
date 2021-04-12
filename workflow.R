library(orderly)
library(purrr)
library(glue)

week <- "2021-04-11"
a <- orderly_run(
  "prepare_jhu_data/",
  parameters = list(week_ending = as.character(week))
)
## a <- "20210412-135535-3e5ac231"
model_input <- readRDS(
  glue("draft/prepare_jhu_data/{a}/latest_model_input.rds")
)
locations <- model_input$State


## Debugging
## locations <- c("Alabama", "California", "Montana", "Texas", "Michigan")

walk(
  locations, function(location) {
    orderly_run(
      "src/run_jointlyr",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

walk(
  locations, function(location) {
    orderly_run(
      "src/run_apeestim/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

walk(
  locations, function(location) {
    orderly_run(
      "src/run_deca/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

walk(
  locations, function(location) {
    orderly_run(
      "src/produce_ensemble_outputs",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)


source("orderly-helper-scripts/dependancies_collate_weekly.R")

orderly_run("collate_weekly_outputs",
            parameters = list(week_ending = as.character(week)),
            use_draft = "newer")

orderly_run(
  "produce_weekly_figs", parameters = list(week_ending = week),
  use_draft = "newer"
)
