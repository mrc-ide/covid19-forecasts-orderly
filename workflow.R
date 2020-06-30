use_draft <- TRUE
weeks <- list(
  "2020-03-08",
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
  "2020-05-24",
  "2020-05-31",
  "2020-06-07",
  "2020-06-14",
  "2020-06-21"
)

for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week, short_run = FALSE)
  m1 <- orderly::orderly_run(
    "run_rti0", parameters = parameter, use_draft = use_draft
  )
  ## orderly::orderly_commit(m1)
  ##orderly::orderly_push_archive(m1)
}

for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week)
  m2 <- orderly::orderly_run(
    "run_apeestim", parameters = parameter, use_draft = use_draft
  )
  ##orderly::orderly_commit(m2)
  ##orderly::orderly_push_archive(m2)
}


for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week)
  m3 <- orderly::orderly_run(
    "DeCa_model", parameters = parameter, use_draft = use_draft
  )
  ## orderly::orderly_commit(m3)
  ##orderly::orderly_push_archive(m3)
}

for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week)
  m3 <- orderly::orderly_run(
    "process_individual_models", parameters = parameter, use_draft = use_draft
  )
  ## orderly::orderly_commit(m3)
  ##orderly::orderly_push_archive(m3)
}

for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week)
  unwtd <- orderly::orderly_run(
    "produce_ensemble_outputs", parameters = parameter, use_draft = use_draft
  )
  ## orderly::orderly_commit(unwtd)
  ## orderly::orderly_push_archive(unwtd)
}

## for (week in weeks) {
##   message("################ ", week, " #############################")
##   parameter <- list(week_ending = week)
##   unwtd <- orderly::orderly_run(
##     "produce_ensemble_outputs", parameters = parameter, use_draft = use_draft
##   )
##   orderly::orderly_commit(unwtd)
##   orderly::orderly_push_archive(unwtd)
## }


a <- orderly::orderly_run(
  "produce_performace_metrics",
  parameters = list(exclude = "2020-06-14"), use_draft = use_draft
)
orderly::orderly_commit(a)
orderly::orderly_push_archive(a)


a <- orderly::orderly_run("compute_model_weights", use_draft = use_draft)
orderly::orderly_commit(a)
orderly::orderly_push_archive(a)

## For the first week, we don't have weighted ensemble.
weeks <- week[-1]

for (week in weeks) {
  message("################ ", week, " #############################")
  parameter <- list(week_ending = week)
  orderly::orderly_run(
    "produce_weighted_ensemble",
    parameters = parameter,
    use_draft = TRUE
  )
}

orderly::orderly_run("collate_model_outputs", use_draft = TRUE)


a <- orderly::orderly_run("compare_ensemble_outputs")
