library(orderly)
use_draft <- "newer"

## weeks <- seq(
##   from = as.Date("2020-10-04"),
##   to = as.Date("2020-10-25"),
##   by = "7 days"
## )

## weeks <- seq(
##   from = as.Date("2020-03-29"),
##   to = as.Date("2020-10-25"),
##   by = "7 days"
## )


weeks <- seq(
  from  = as.Date("2020-03-29"), to = as.Date("2020-11-29"),
  by = "7 days"
)

weeks <- as.character(weeks)
use_si <- "si_2"

for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  parameter <- list(week_ending = week, use_si = use_si)
  source("orderly-helper-scripts/dependencies_weights_combined_rt.R")
  a <-  orderly::orderly_run(
    "produce_weights_combined_rt", parameters = parameter, use_draft = use_draft
  )
  ##orderly::orderly_commit(a)
}


for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  parameter <- list(week_ending = week, use_si = use_si)
  source("orderly-helper-scripts/dependencies_combined_rt.R")
  m1 <- orderly::orderly_run(
    "produce_combined_rt", parameters = parameter, use_draft = use_draft
  )
}

for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  parameter <- list(week_ending = week, use_si = use_si)
  m2 <- orderly_run(
    "produce_longer_forecasts", parameters = parameter,
    use_draft = use_draft
  )
}


for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  parameter <- list(week_ending = week, use_si = use_si)
  m2 <- orderly_run(
    "produce_longer_forecasts_no_ps", parameters = parameter,
    use_draft = use_draft
  )
}


for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  parameter <- list(week_ending = week, window = 1, latest_week = "2021-01-03")
  orderly::orderly_run(
    "produce_longer_forecasts_metrics", parameters = parameter, use_draft = use_draft
  )
}


for (week in weeks) {
  message("################ ", week, "#############################")
  parameter <- list(week_ending = week, use_si = use_si)
  a <-  orderly_run(
    "assign_epidemic_phase", parameters = parameter, use_draft = use_draft
  )
  ##orderly::orderly_commit(a)
}

orderly_run("collate_epidemic_phase", use_draft = use_draft)


## Collation
week_starting <- as.Date(head(weeks, 1)[[1]])
week_ending <- as.Date(tail(weeks, 1)[[1]])
source(
  "orderly-helper-scripts/dependencies_collate_combined_rt.R"
)
orderly::orderly_run("collate_combined_rt", use_draft = "newer")

source(
  "orderly-helper-scripts/dependencies_collate_longer_forecasts.R"
)
orderly::orderly_run("src/collate_longer_forecasts/", use_draft = "newer")

orderly::orderly_run(
  "produce_longer_forecasts_viz", use_draft = "newer"
)

source("orderly-helper-scripts/dependencies_collate_longer_forecasts_perf.R")

orderly::orderly_run(
  "collate_longer_forecasts_metrics", use_draft = "newer"
)

orderly::orderly_run(
  "produce_longer_forecasts_perf_viz", use_draft = "newer"
)
