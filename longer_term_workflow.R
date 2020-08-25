weeks <- list(
  "2020-03-29", "2020-04-05",
  "2020-04-12"
  #"2020-04-19", "2020-04-26", "2020-05-03", "2020-05-10",
  #"2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07",
  #"2020-06-14", "2020-06-21", "2020-06-28", "2020-07-05",
  #"2020-07-12", "2020-07-19", "2020-07-26", "2020-08-02",
  #"2020-08-09"
)
use_si <- "si_2"

for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  parameter <- list(week_ending = week, use_si = use_si)
  #source("write_dependencies_weights_combined_rt.R")
  #a <-  orderly::orderly_run(
  #  "produce_weights_combined_rt", parameters = parameter
  #)
  #orderly::orderly_commit(a)

  source("orderly-helper-scripts/write_dependencies_combined_rt.R")
  m1 <- orderly::orderly_run(
    "produce_combined_rt", parameters = parameter
  )
  ##orderly::orderly_commit(m1)
  ##orderly::orderly_push_archive(name = "produce_combined_rt", id = m1)

  #m2 <- orderly::orderly_run(
  #  "produce_longer_forecasts", parameters = parameter, use_draft = FALSE
  #)
  #orderly::orderly_commit(m2)
  #orderly::orderly_push_archive(
  #  name = "produce_longer_forecasts", id = m2
  #)
}

source("write_dependencies_collate_combined_rt.R")
orderly::orderly_run("collate_combined_rt", use_draft = FALSE)
source("write_dependencies_collate_longer_forecasts.R")
orderly::orderly_run(
  "src/collate_longer_forecasts/", use_draft = TRUE
)

orderly::orderly_run(
  "src/produce_longer_forecasts_viz/", use_draft = TRUE
)

