library(orderly)
library(purrr)
location <- "Arizona"
weeks <- seq(
  from = as.Date("2021-01-03"), to = as.Date("2021-03-17"),
  by = "7 days"
)

map(
  weeks, function(week) {
    orderly_run(
      "src/prepare_jhu_data/",
      parameters = list(week_ending = as.character(week)))
  }
)


map(
  weeks, function(week) {
    orderly_run(
      "src/run_jointlyr",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

map(
  weeks, function(week) {
    orderly_run(
      "src/run_apeestim/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

map(
  weeks, function(week) {
    orderly_run(
      "src/run_deca/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

map(
  weeks, function(week) {
    orderly_run(
      "src/produce_ensemble_outputs",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)






