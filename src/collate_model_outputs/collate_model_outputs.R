split_and_stick <- function(rt_qntls) {

  out <- split(rt_qntls, rt_qntls$si)
  purrr::map_dfr(
    out,
    function(df) {
      df <- tidyr::spread(df, quantile, out2)
      df <- rincewind::assign_epidemic_phase(df)
      df <- tidyr::gather(df, quantile, out2, `1%`:`99%`)
      df
    }
  )
}

## rt in wide form

model_input <- readRDS("model_input.rds")
deaths_tall <- tidyr::gather(model_input, country, deaths, -dates)

## This won't workx with orderly_develop_start.
## Do this instead: infiles <- list.files(pattern = "*.rds")
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
infiles <- infiles[infiles != "model_input.rds"]

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)
######################################################################
######################################################################
## Effective Reproduction Number Samples
######################################################################
######################################################################
sample_files <- grep(pattern = "samples", x = infiles, value = TRUE)

## Unweighted samples have a different list structure :-(
unwtd_rt_samples <- purrr::map_dfr(
  grep(pattern = "unwtd_rt", x = sample_files, value = TRUE),
  function(infile) {
    message("Processing ", infile)
    readRDS(infile)
  }
)


wtd_prev_week_rt_samples <- purrr::map_dfr(
  grep(pattern = "wtd_rt_prev_week", x = sample_files, value = TRUE),
  function(infile) {
    message("Processing ", infile)
    x <- readRDS(infile)
    purrr::map_dfr(
      x,
      ~ purrr::map_dfr(., dplyr::bind_rows, .id = "country"),
      .id = "forecast_date"
    )
  }
)

wtd_all_prev_week_rt_samples <- purrr::map_dfr(
  grep(pattern = "wtd_rt_all_prev_week", x = sample_files, value = TRUE),
  function(infile) {
    message("Processing ", infile)
    x <- readRDS(infile)
    purrr::map_dfr(
      x,
      ~ purrr::map_dfr(., dplyr::bind_rows, .id = "country"),
      .id = "forecast_date"
    )
  }
)

saveRDS(unwtd_rt_samples, "unwtd_rt_samples.rds")
saveRDS(wtd_prev_week_rt_samples, "wtd_prev_week_rt_samples.rds")
saveRDS(
  wtd_all_prev_week_rt_samples, "wtd_all_prev_week_rt_samples.rds"
)

infiles <- grep(
  pattern = "samples", x = infiles, value = TRUE, invert = TRUE
)

######################################################################
######################################################################
## Effective Reproduction Number Quantiles ###########################
######################################################################
######################################################################
unweighted_rt_qntls <- purrr::map_dfr(
  infiles[grep("unwtd_ensemble_model_rt", infiles)], readRDS
)

unweighted_rt_qntls <- split_and_stick(unweighted_rt_qntls)

unweighted_rt_qntls <- dplyr::rename(
  unweighted_rt_qntls, forecast_date = "model"
)
unweighted_rt_qntls$model <- "Unweighted Ensemble"

saveRDS(
  unweighted_rt_qntls, "unweighted_rt_qntls.rds"
)


wtd_prev_week_rt_qntls <- purrr::map_dfr(
  grep("wtd_rt_prev_week_qntls", infiles, value = TRUE), readRDS
)
wtd_prev_week_rt_qntls <- split_and_stick(wtd_prev_week_rt_qntls)
wtd_prev_week_rt_qntls <- dplyr::rename(
  wtd_prev_week_rt_qntls, forecast_date = "model"
)
wtd_prev_week_rt_qntls$model <- "Weighted Ensemble (weights previous week)"


saveRDS(wtd_prev_week_rt_qntls, "wtd_prev_week_rt_qntls.rds")


wtd_rt_all_prev_week_qntls <- purrr::map_dfr(
  grep("wtd_rt_all_prev_week_qntls", infiles, value = TRUE), readRDS
)
wtd_rt_all_prev_week_qntls <- split_and_stick(
  wtd_rt_all_prev_week_qntls
)
wtd_rt_all_prev_week_qntls <- dplyr::rename(
  wtd_rt_all_prev_week_qntls, forecast_date = "model"
)
wtd_rt_all_prev_week_qntls$model <- "Weighted Ensemble (weights all weeks)"

saveRDS(
  wtd_rt_all_prev_week_qntls,
  "wtd_rt_all_prev_week_qntls.rds"
)

## Remove rt files from infiles to make grepping easier
infiles <- grep(
  pattern = "rt", x = infiles, invert = TRUE, value = TRUE
)
######################################################################
######################################################################
######################################################################
######################################################################
## All unweighted ensemble outputs
######################################################################
######################################################################
unweighted_qntls <- purrr::map_dfr(
  infiles[grep("unwtd_ensemble_daily_qntls", infiles)], readRDS
)
unweighted_qntls$model <- "Unweighted Ensemble"
unweighted_qntls$date <- as.Date(unweighted_qntls$date)



## Weighted using weights from previous weeks forecasts only
wtd_prev_week <- purrr::map_dfr(
  grep("wtd_ensb_prev_week_daily_qntls", infiles, value = TRUE),
  readRDS
)
wtd_prev_week$model <- "Weighted Ensemble (weights previous week)"
wtd_prev_week$date <- as.Date(wtd_prev_week$date)

## Weighted using weights from all previous forecasts
wtd_all_prev_weeks <- purrr::map_dfr(
  grep("wtd_ensb_all_prev_weeks_daily_qntls", infiles, value = TRUE),
  readRDS
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
