orderly::orderly_develop_start(use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
null_error <- map_dfr(
  grep("null", infiles, value = TRUE), readRDS
)

linear_error <- map_dfr(
  grep("linear", infiles, value = TRUE), readRDS
)


saveRDS(null_error, "null_model_error.rds")
saveRDS(linear_error, "linear_model_error.rds")
