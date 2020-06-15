workflow <- function(parameter) {
  #a <- orderly::orderly_run("produce_ensemble_outputs", parameters = parameter)
  #orderly::orderly_commit(a)

  a <- orderly::orderly_run("produce_weighted_ensemble", parameters = parameter)
  orderly::orderly_commit(a)

}


a <- orderly::orderly_run("produce_performace_metrics")
orderly::orderly_commit(a)

a <- orderly::orderly_run("compute_model_weights")
orderly::orderly_commit(a)


weeks <- list(
##  "2020-03-08",
  "2020-03-15",
  "2020-03-22",
  "2020-03-29",
  "2020-04-05",
  "2020-04-12",
  "2020-04-19",
  "2020-04-26",
  "2020-05-03",
  "2020-05-10",
  "2020-05-17",
  "2020-05-24"
)

for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week)
  workflow(parameter)
}


a <- orderly::orderly_run("compare_ensemble_outputs")
