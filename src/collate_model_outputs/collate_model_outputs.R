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



saveRDS(unwtd_rt_samples, "unwtd_rt_samples.rds")

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





saveRDS(
  object = unweighted_qntls, file = "unweighted_qntls.rds"
)
