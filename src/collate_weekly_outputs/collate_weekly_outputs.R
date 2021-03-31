## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-03-28"))
## infiles <- list.files(pattern = "*.rds")

probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
dates_pred <- seq(
  as.Date(week_ending) + 1, length.out = 7, by = "1 day"
)


collate_outputs <- function(infiles, pattern) {
  model_files <- grep(pattern, infiles, value = TRUE)
  model_outputs <- map(model_files, readRDS)

  states <- gsub(pattern, "", model_files) %>%
    gsub("_", "", x = .) %>%
    gsub(".rds", "", x = .)

  names(model_outputs) <- states
  ## [[2]] for si_2
  model_outputs <- map(
    model_outputs, function(x) x[["Predictions"]][[2]]
  )
  model_qntls <- map_dfr(
    model_outputs, function(x) {
      out <- data.frame(
        apply(x, 2, quantile, probs = probs), check.names = FALSE
      )
      out <- rownames_to_column(out, var = "qntl")
      out
    }, .id = "state"
  )
  colnames(model_qntls)[3:ncol(model_qntls)] <- as.character(dates_pred)
  model_qntls
}

collate_rt <- function(infiles, pattern) {
  model_files <- grep(pattern, infiles, value = TRUE)
  model_outputs <- map(model_files, readRDS)

  states <- gsub(pattern, "", model_files) %>%
    gsub("_", "", x = .) %>%
    gsub(".rds", "", x = .)

  names(model_outputs) <- states
  ## [[2]] for si_2
  model_outputs <- map(
    model_outputs, function(x) x[["R_last"]][[2]]
  )
  model_qntls <- map_dfr(
    model_outputs, function(x) {
      out <- data.frame(
       out2 = quantile(x, probs = probs), check.names = FALSE
      )
      out <- rownames_to_column(out, var = "qntl")
      out
    }, .id = "state"
  )
  model_qntls
}
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
rti0_qntls <- collate_outputs(infiles, "rti0_model_outputs")
deca_qntls <- collate_outputs(infiles, "DeCa_latest")
apeestim_qntls <- collate_outputs(infiles, "apeestim_model_outputs")

rti0_rt_qntls <- collate_rt(infiles, "rti0_model_outputs")
deca_rt_qntls <- collate_rt(infiles, "DeCa_latest")
apeestim_rt_qntls <- collate_rt(infiles, "apeestim_model_outputs")


model_files <- grep(
  "ensemble_model_predictions", infiles, value = TRUE
)

states <- gsub("ensemble_model_predictions", "", model_files) %>%
  gsub("_", "", x = .) %>%
  gsub(".rds", "", x = .)

names(model_files) <- states

ensemble_qntls <- map_dfr(model_files, readRDS, .id = "state")


model_files <- grep(
  "ensemble_model_rt_qntls", infiles, value = TRUE
)
names(model_files) <- states
ensemble_rt_qntls <- map_dfr(model_files, readRDS, .id = "state")


saveRDS(ensemble_rt_qntls, "us_ensemble_rt_qntls.rds")
saveRDS(ensemble_qntls, "us_ensemble_forecasts_qntls.rds")
saveRDS(rti0_qntls, "rti0_qntls.rds")
saveRDS(deca_qntls, "deca_qntls.rds")
saveRDS(apeestim_qntls, "apeestim_qntls.rds")
saveRDS(rti0_rt_qntls, "rti0_rt_qntls.rds")
saveRDS(deca_rt_qntls, "deca_rt_qntls.rds")
saveRDS(apeestim_rt_qntls, "apeestim_rt_qntls.rds")

