run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
##infiles <- list.files(pattern = "*.csv")
unwtd <- grep("unwtd", infiles, value = TRUE)
unwtd_collated <- map_dfr(unwtd, readr::read_csv)
unwtd_collated$model_name <- "ensemble"

model_collated <- grep("model_predictions", infiles, value = TRUE)
model_collated <- map_dfr(model_collated, readr::read_csv)
##model_collated$model_name <- "individual"


write_csv(
  x = rbind(unwtd_collated, model_collated),
  path = "unwtd_pred_error.csv"
)
