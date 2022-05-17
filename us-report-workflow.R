library(orderly)
library(purrr)
library(glue)

week <- "2022-05-15"
use_draft <- "newer"
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
  "archive/prepare_jhu_data/20211220-131558-96fd9b10/latest_model_input.rds"
)
locations <- model_input$State
locations <- locations[!locations %in% c("Florida", "Ohio", "Nebraska")]

source("orderly-helper-scripts/dependancies_collate_weekly.R")
### On the server
cat(
  sprintf("\n orderly run us_run_jointlyr short_run=FALSE week_ending=%s location=\"%s\"", week, locations),
  file = "us-runs.sh"
)

cat(
  sprintf("\n orderly run us_run_apeestim week_ending=%s location=\"%s\"", week, locations),
  file = "us-runs.sh", append = TRUE
)

cat(
  sprintf("\n orderly run us_run_deca week_ending=%s location=\"%s\"", week, locations),
  file = "us-runs.sh", append = TRUE
)


cat(
  sprintf("\n orderly run us_produce_ensemble_outputs week_ending=%s location=\"%s\"", week, locations),
  file = "us-runs.sh", append = TRUE
)

cat(
  sprintf("\n orderly run us_collate_weekly_outputs week_ending=%s", week),
  file = "us-runs.sh", append = TRUE
)

cat(
  sprintf("\n orderly run us_produce_weekly_figs week_ending=%s", week),
  file = "us-runs.sh", append = TRUE
)
