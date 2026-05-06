# Workflow for US aggregation work

# This script runs the tasks necessary to reproduce the analyses in the manuscript:
# Forecasting COVID-19 cases in US states using reconstructed incidence data

# This work is set up as an orderly project with the following tasks:

# 1) download_jhu_data
#     Downloads incidence data from John Hopkins github
# 2) prepare_jhu_data
#     Task reads in, cleans and reformats US John Hopkins incidence data
# 3) produce_epi_params
#     Produces the SI distribution used
# 4) reconstruct_daily_incidence
#     Reconstructs daily incidence to use as an input to model
# 5) us_run_jointlyr
#     Runs the forecasting model
# 6) us_summarise_jointlyr
#     Produces the quantiles and CRPS values for the predictions
# 7) us_combine_crps
#     Produces the figures for publication


# Check if orderly is missing or if the version doesn't match
orderly_version <- "2.0.3"
if (!requireNamespace("orderly", quietly = TRUE) ||
    packageVersion("orderly") != orderly_version) {
  message(sprintf("Installing required version of orderly (%s).",
                  orderly_version))
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "http://cran.us.r-project.org")
  }
  remotes::install_version("orderly", version = orderly_version,
                           repos = "http://cran.us.r-project.org")
  }

library(orderly)
setwd("~/covid19-forecasts-orderly")

# Select whether reported or reconstructed data are used
reconstructed <- TRUE
#reconstructed <- FALSE

# Select the day of the week for the projections
week_ending <- "2022-02-21" # Monday
#week_ending <- "2022-02-22" # Tuesday 
#week_ending <- "2022-02-23" # Wednesday
#week_ending <- "2022-02-24" # Thursday
#week_ending <- "2022-02-25" # Friday
#week_ending <- "2022-02-26" # Saturday
#week_ending <- "2022-02-27" # Sunday

##########################
## Run each task in turn
##########################

orderly_run("download_jhu_data")

orderly_run("prepare_jhu_data",
            parameters = list(
              week_ending = week_ending
              )
            )

orderly_run("produce_epi_params")

orderly_run("reconstruct_daily_incidence",
            parameters = list(
              week_ending = week_ending,
              location = "subset", # selected US states
              short_run = FALSE
              )
            )

# Following tasks run for reconstructed and reported data:
orderly_run("us_run_jointlyr",
            parameters = list(
              week_ending = week_ending,
              location = "subset",
              short_run = FALSE,
              reconstructed = reconstructed,
              incidence_type = "cases" # cases or deaths
              ) 
            )

orderly_run("us_summarise_jointlyr",
            parameters = list(
              week_ending = week_ending,
              reconstructed = reconstructed,
              incidence_type = "cases"
              )
            )

# Plot results once the above tasks have been run for all combinations of
# week_ending and reported/reconstructed
orderly_run("us_combine_CRPS",
            parameters = list(
              incidence_type = "cases"
            )
)
