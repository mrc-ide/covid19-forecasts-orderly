model_input <- readRDS("model_input.rds")
deaths_tall <- tidyr::gather(model_input, country, deaths, -dates)


run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
infiles <- infiles[infiles != "model_input.rds"]

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)

## All unweighted ensemble outputs
unweighted_qntls <- purrr::map_dfr(
  infiles[grep("unwtd", infiles)], readRDS
)
unweighted_qntls$model <- "Unweighted Ensemble"
unweighted_qntls$date <- as.Date(unweighted_qntls$date)

## Weighted using weights from previous weeks forecasts only
wtd_prev_week <- purrr::map_dfr(
  grep("wtd_prev_week", infiles, value = TRUE), readRDS
)
wtd_prev_week$model <- "Weighted Ensemble (weights previous week)"
wtd_prev_week$date <- as.Date(wtd_prev_week$date)

## Weighted using weights from all previous forecasts
wtd_all_prev_weeks <- purrr::map_dfr(
  grep("wtd_all_prev_weeks", infiles, value = TRUE), readRDS
)
wtd_all_prev_weeks$model <- "Weighted Ensemble (weights all weeks)"
wtd_all_prev_weeks$date <- as.Date(wtd_all_prev_weeks$date)


unweighted_qntls <- dplyr::left_join(
  unweighted_qntls,
  deaths_tall,
  by = c("date" = "dates", "country" = "country")
)

wtd_prev_week <- dplyr::left_join(
  wtd_prev_week,
  deaths_tall,
  by = c("date" = "dates", "country" = "country")
)

wtd_all_prev_weeks <- dplyr::left_join(
  wtd_all_prev_weeks,
  deaths_tall,
  by = c("date" = "dates", "country" = "country")
)


saveRDS(
  object = unweighted_qntls, file = "unweighted_qntls.rds"
)

saveRDS(
  object = wtd_prev_week, file = "wtd_prev_week_qntls.rds"
)

saveRDS(
  object = wtd_all_prev_weeks, file = "wtd_all_prev_weeks_qntls.rds"
)
