## set commit to FALSE and commit manually once you are happy with the
## outputs
basic_workflow <- function(week, use_draft = "newer", commit = FALSE) {
  parameter <- list(week_ending = week)
  message("Preparing data for week ", week)
  m1 <- orderly_run(
    "prepare_ecdc_data",
    parameters = parameter
  )
  if (commit) orderly_commit(m1)

  message("Running Model 1, this will take long.")
  parameter <- list(
    week_ending = week, short_run = FALSE
  )
  m1 <- orderly_run(
    "run_rti0",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(m1)

  message("Running Model 2.")
  parameter <- list(week_ending = week)
  m2 <- orderly_run(
    "run_apeestim",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(m2)

  message("Running Model 3.")
  m3 <- orderly_run(
    "DeCa_model",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(m3)

  indv <- orderly_run(
    "process_individual_models",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(indv)

  unwtd <- orderly_run(
    "produce_ensemble_outputs",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(unwtd)

  message("Performance metrics for ensemble model; week = ", week)
  source("orderly-helper-scripts/write_dependencies_weighted_performance.R")
  parameter <- list(week_ending = week, window = 1)
  m1 <- orderly_run(
    "produce_performance_metrics_ensemble/",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(m1)

}

## These functions have not been configured to pull in week-specific
## outputs, they will always pull in the latest runs of dependancies.
report_workflow <- function(week, use_draft = "newer", commit = FALSE) {
  a <- orderly_run("format_model_outputs/", use_draft = use_draft)
  if (commit) orderly_commit(a)

  a <- orderly_run("produce_maps/", use_draft = use_draft)
  if (commit) orderly_commit(a)

  a <- orderly_run("produce_retrospective_vis/", use_draft = use_draft)
  if (commit) orderly_commit(a)

  a <- orderly_run(
    "produce_visualisations/",
    parameters = list(week_ending_vis = week),
    use_draft = use_draft
  )
  if (commit) orderly_commit(a)

  a <- orderly_run("produce_full_report", use_draft = use_draft)
  if (commit) orderly_commit(a)
}


## Collate week specific outputs
collation_workflow <- function(weeks, use_draft = "newer", commit = FALSE) {

  source("write_dependencies_collate_model_outputs.R")
  a <- orderly_run("collate_model_outputs", use_draft = use_draft)
  if (commit) orderly_commit(a)

  source(
    "orderly-helper-scripts/write_dependencies_collate_weighted_perf.R"
  )
  m1 <- orderly_run("collate_weighted_performance_metrics/")
  if (commit) orderly_commit(m1)

}

post_collation_workflow <- function(use_draft = "newer", commit = FALSE) {

  a <- orderly_run(
    "produce_performance_metrics_vis", use_draft = use_draft
  )
  if (commit) orderly_commit(a)
  a <- orderly_run(
    "compare_ensemble_outputs", use_draft = use_draft
  )
  if (commit) orderly_commit(a)
}

use_draft <- "newer"
weeks <- seq(
  from = as.Date("2020-03-08"),
  to = as.Date("2020-09-20"),
  by = "7 days"
)


aa <- orderly_run(
  "produce_performace_metrics",
  parameters = list(window = 1), use_draft = use_draft
)
orderly_commit(aa)
orderly_push_archive(a)






