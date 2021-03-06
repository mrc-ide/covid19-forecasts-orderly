## set commit to FALSE and commit manually once you are happy with the
## outputs
basic_workflow <- function(week, use_draft = "newer", commit = FALSE) {
  parameter <- list(week_ending = week)
  message("Preparing data for week ", week)
  m1 <- orderly_run("prepare_ecdc_data", parameters = parameter)
  if (commit) orderly_commit(m1)

  message("Running Model 1, this will take long.")
  parameter <- list(week_ending = week, short_run = FALSE)
  m1 <- orderly_run(
    "run_rti0", parameters = parameter, use_draft = use_draft
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

}

## Performance assessment in week N requires observations for week N + 1
## to be available. That is why these tasks cannot be run with the
## weekly workflow
performance_workflow <- function(week, use_draft = "newer", commit = FALSE) {
  message("Performance metrics for ensemble model; week = ", week)
  x <- dependencies_ensb_performance(week)
  con <- file(
    here::here("src/produce_performance_metrics_ensemble/orderly.yml"),
    "w"
  )
  yaml::write_yaml(x, con)
  close(con)

  parameter <- list(week_ending = as.character(week), window = 1)
  m1 <- orderly_run(
    "produce_performance_metrics_ensemble",
    parameters = parameter, use_draft = use_draft
  )
  if (commit) orderly_commit(m1)

  x <- dependencies_indvdl_performance(week)
  con <- file(
    here::here("src/produce_performace_metrics/orderly.yml"),
    "w"
  )
  yaml::write_yaml(x, con)
  close(con)

  m2 <- orderly_run(
    "produce_performace_metrics",
    parameters = parameter, use_draft = use_draft
  )
}

## These functions have not been configured to pull in week-specific
## outputs, they will always pull in the latest runs of dependancies.
report_workflow <- function(week, use_draft = "newer", commit = FALSE) {

  a <- orderly_run(
    "format_model_outputs",
    use_draft = use_draft, parameter = list(week_ending = week)
  )
  if (commit) orderly_commit(a)

  a <- orderly_run(
    "produce_maps",
    use_draft = use_draft, parameter = list(week_ending = week)
  )
  if (commit) orderly_commit(a)

  a <- orderly_run(
    "produce_retrospective_vis",
    use_draft = use_draft, parameter = list(week_ending = week)
  )
  if (commit) orderly_commit(a)

  a <- orderly_run(
    "produce_visualisations",
     parameters = list(week_ending = week), use_draft = use_draft
  )
  if (commit) orderly_commit(a)

  a <- orderly_run(
    "produce_full_report",
    use_draft = use_draft,
    parameter = list(week_ending = week)
  )

  if (commit) orderly_commit(a)
}


## Collate week specific outputs
collation_workflow <- function(weeks, use_draft = "newer", commit = FALSE) {

  source("orderly-helper-scripts/dependencies_collate_model_outputs.R")
  a <- orderly_run("collate_model_outputs", use_draft = use_draft)
  if (commit) orderly_commit(a)

  ##weeks <- head(weeks, -1)
  source(
    "orderly-helper-scripts/dependencies_collate_weighted_perf.R"
  )
  m1 <- orderly_run(
    "collate_weighted_performance_metrics", use_draft = use_draft
  )
  if (commit) orderly_commit(m1)

}

post_collation_workflow <- function(latest_week, use_draft = "newer", commit = FALSE) {

  a <- orderly_run(
    "produce_performance_metrics_vis",
    use_draft = use_draft,
    parameters = list(use_si = "si_2")
  )
  if (commit) orderly_commit(a)
  ## x <- collated_outputs_viz(latest_week)
  ## con <- file(
  ##   here::here("src/compare_ensemble_outputs/orderly.yml"), "w"
  ## )
  ## yaml::write_yaml(x, con)
  ## close(con)

  a <- orderly_run(
    "compare_ensemble_outputs",
    use_draft = use_draft,
    parameters = list(use_si = "si_2", latest_week = latest_week)
  )
  if (commit) orderly_commit(a)
}

library(purrr)
library(glue)
library(orderly)
source("orderly-helper-scripts/dependencies_weighted_performance.R")
source("orderly-helper-scripts/dependencies_collated_outputs_viz.R")

use_draft <- "newer"
## This is the sunday before the Monday for which we are producing
## the report.
weeks <- seq(
  from = as.Date("2020-03-08"),
  ##to = as.Date("2021-03-01"),
  to = as.Date("2020-11-29"),
  by = "7 days"
)

weeks <- as.character(weeks)
purrr::walk(weeks, function(x) basic_workflow(x))
purrr::walk(head(weeks, -1), function(x) performance_workflow(x))
collation_workflow(weeks)


### Other tasks that need to be run with the latest data
orderly_run(
  "produce_baseline_error",
  use_draft = "newer",
  parameters = list(week_starting = head(weeks, 1),
                    latest_week = tail(weeks, 1))
)



orderly_run(
  "compare_with_null_model",
  use_draft = "newer", parameters = list(use_si = "si_2")
)

post_collation_workflow(tail(weeks, 1))
## To run on server, one-off scripts
writeLines(
  sprintf("orderly run prepare_ecdc_data week_ending=%s", weeks),
  "runs-20210428.sh"
)
## baseline error only needs observed data
writeLines(
  sprintf("orderly run produce_baseline_error week_starting=2020-02-22 latest_week=%s", weeks),
  "runs-20210428.sh"
)

writeLines(
  sprintf("orderly run run_rti0 short_run=FALSE week_ending=%s", weeks),
  "runs-20210428.sh"
)

outfile <- "performance-metrics.sh"
for (week in weeks) {
  cat(
    sprintf("\n Rscript orderly-helper-scripts/dependencies_weighted_performance.R %s", week),
    file = outfile, append = TRUE
  )
  cat(
    sprintf("\n orderly run produce_performace_metrics window=1 week_ending=%s", week),
    file = outfile, append = TRUE
  )
}
