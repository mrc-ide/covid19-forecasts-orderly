library(orderly)
library(purrr)
library(glue)
location <- "Florida"
weeks <- seq(
  from = as.Date("2021-01-03"), to = as.Date("2021-03-17"),
  by = "7 days"
)

a <- orderly_run(
  "prepare_jhu_data/",
  parameters = list(week_ending = as.character(week))
)

model_input <- glue("draft/prepare_jhu_data/{a}/latest_model_input.rds")
locations <- model_input$State


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



orderly_run(
  "src/produce_figures/", parameters = list(location = location),
  use_draft = "newer"
)





