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
      out <- tibble::rownames_to_column(out, var = "qntl")
      out
    }, .id = "state"
  )
  colnames(model_qntls)[3:ncol(model_qntls)] <- as.character(dates_pred)
  model_qntls
}

rti0_qntls <- collate_outputs(infiles, "rti0_model_outputs")
deca_qntls <- collate_outputs(infiles, "DeCa_latest")
apeestim_qntls <- collate_outputs(infiles, "apeestim_model_outputs")


model_files <- grep("ensemble_model_predictions", infiles, value = TRUE)
states <- gsub(pattern, "", model_files) %>%
  gsub("_", "", x = .) %>%
  gsub(".rds", "", x = .)
names(model_files) <- states
ensemble_qntls <- map_dfr(model_files, readRDS, .id = "state")


model_files <- grep("ensemble_model_rt_qntls", infiles, value = TRUE)
names(model_files) <- states
ensemble_rt_qntls <- map_dfr(model_files, readRDS, .id = "state")
