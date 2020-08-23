run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
##infiles <- list.files(pattern = "*.csv")
unwtd <- grep("unwtd", infiles, value = TRUE)
unwtd_collated <- map_dfr(unwtd, readr::read_csv)

readr::write_csv(x = unwtd_collated, path = "unwtd_pred_error.csv")
