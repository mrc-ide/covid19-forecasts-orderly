library(orderly)
library(purrr)
library(glue)

week <- "2021-08-08"

a <- orderly_run("download_jhu_data")
orderly_commit(a)

a <- orderly_run(
  "prepare_jhu_data",
  parameters = list(week_ending = as.character(week)),
  use_draft = "newer"
)
## a <- "20210419-113500-1a96fbe1"
orderly_commit(a)

orderly_pull_archive(
  "prepare_jhu_data",
  parameters = list(week_ending = as.character(week))
)

model_input <- readRDS(
  "archive/prepare_jhu_data/20210802-101528-3d402952/latest_model_input.rds"
)
locations <- model_input$State


## Debugging
## locations <- c("Alabama", "California", "Montana", "Texas", "Michigan")

walk(
  locations, function(location) {
    a <- orderly_run(
      "src/us_run_jointlyr",
      parameters = list(
        location = location,
        week_ending = as.character(week),
        short_run = FALSE
      ), use_draft = "newer"
    )
    orderly_commit(a)
  }
)

walk(
  locations, function(location) {
    a <- orderly_run(
      "src/us_run_apeestim/",
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
      "src/us_run_deca/",
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
      "src/us_produce_ensemble_outputs",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
    orderly_commit(a)
  }
)


source("orderly-helper-scripts/dependancies_collate_weekly.R")

a <- orderly_run(
  "src/us_collate_weekly_outputs", use_draft = "newer",
  parameters = list(week_ending = week),
)
orderly_commit(a)

a <- orderly_run(
  "src/us_produce_weekly_figs", parameters = list(week_ending = week),
  use_draft = "newer"
)
orderly_commit(a)


### On the server
cat(
  sprintf("\n orderly run us_run_jointlyr short_run=FALSE week_ending=2021-08-01 location=\"%s\"", locations),
  file = "us-runs.sh"
)

cat(
  sprintf("\n orderly run us_run_apeestim week_ending=2021-08-01 location=\"%s\"", locations),
  file = "us-runs.sh", append = TRUE
)

cat(
  sprintf("\n orderly run us_run_deca week_ending=2021-08-01 location=\"%s\"", locations),
  file = "us-runs.sh", append = TRUE
)


cat(
  sprintf("\n orderly run us_produce_ensemble_outputs week_ending=2021-08-01 location=\"%s\"", locations),
  file = "us-runs.sh", append = TRUE
)

cat(
  sprintf("\n orderly run us_collate_weekly_outputs week_ending=2021-08-01"),
  file = "us-runs.sh", append = TRUE
)

cat(
  sprintf("\n orderly run us_produce_weekly_figs week_ending=2021-08-01"),
  file = "us-runs.sh", append = TRUE
)
