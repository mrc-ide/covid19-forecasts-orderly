library(orderly)
library(purrr)
library(glue)

week <- "2021-04-18"

a <- orderly_run("download_jhu_data")
orderly_commit(a)

a <- orderly_run(
  "prepare_jhu_data/",
  parameters = list(week_ending = as.character(week))
)
## a <- "20210419-113500-1a96fbe1"
orderly_commit(a)

model_input <- readRDS(
  glue("archive/prepare_jhu_data/{a}/latest_model_input.rds")
)
locations <- model_input$State


## Debugging
## locations <- c("Alabama", "California", "Montana", "Texas", "Michigan")

walk(
  locations, function(location) {
    a <- orderly_run(
      "src/run_jointlyr",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
    orderly_commit(a)
  }
)

walk(
  locations, function(location) {
    a <- orderly_run(
      "src/run_apeestim/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
    orderly_commit(a)
  }
)

walk(
  locations, function(location) {
    a <- orderly_run(
      "src/run_deca/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
    orderly_commit(a)
  }
)

walk(
  locations, function(location) {
    a <- orderly_run(
      "src/produce_ensemble_outputs",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
    orderly_commit(a)
  }
)


source("orderly-helper-scripts/dependancies_collate_weekly.R")

a <- orderly_run(
  "collate_weekly_outputs", use_draft = "newer",
  parameters = list(week_ending = week),
  )
orderly_commit(a)

a <- orderly_run(
  "produce_weekly_figs", parameters = list(week_ending = week),
  use_draft = "newer"
)
orderly_commit(a)
