weeks <- list(
  ##"2020-03-22", "2020-03-29", "2020-04-05", "2020-04-12",
  "2020-04-19", "2020-04-26", "2020-05-03", "2020-05-10",
  "2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07",
  "2020-06-14", "2020-06-21", "2020-06-28"
)
use_si <- "si_2"

for (week in weeks) {
  message("################ ", week, "#############################")
  week_ending <- as.Date(week)
  source("write_dependencies_combined_rt.R")
  parameter <- list(week_ending = week, use_si = use_si)
  m1 <- orderly::orderly_run(
    "produce_combined_rt", parameters = parameter
  )
  orderly::orderly_commit(m1)
  ##orderly::orderly_push_archive(name = "produce_combined_rt", id = m1)

  m2 <- orderly::orderly_run(
    "produce_longer_forecasts", parameters = parameter
  )
  orderly::orderly_commit(m2)
  #orderly::orderly_push_archive(
  #  name = "produce_longer_forecasts", id = m2
  #)
}
