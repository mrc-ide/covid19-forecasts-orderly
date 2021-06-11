## For the server create an R script.
outfile <- "medium-term-forecasts.sh"
for (week in weeks){
  cat(
    sprintf("\n Rscript orderly-helper-scripts/dependencies_weights_combined_rt.R %s", week),
    file = outfile, append = TRUE
  )
  cat(
    sprintf("\n orderly run produce_weights_combined_rt use_si = si_2 week_ending=%s", week),
    file = outfile, append = TRUE
  )

  cat(
    sprintf("\n Rscript orderly-helper-scripts/dependencies_combined_rt.R %s", week),
    file = outfile, append = TRUE
  )
  cat(
    sprintf("\n orderly run produce_combined_rt use_si = si_2 week_ending=%s", week),
    file = outfile, append = TRUE
  )

  cat(
    sprintf("\n orderly run produce_longer_forecasts use_si = si_2 week_ending=%s", week),
    file = outfile, append = TRUE
  )

}

outfile <- "medium-term-forecasts-perf.sh"
for (week in weeks) {
  cat(
    sprintf("\n orderly run produce_longer_forecasts_metrics window=1 latest_week=2021-01-03 week_ending=%s", week),
    file = outfile, append = TRUE
  )
}
