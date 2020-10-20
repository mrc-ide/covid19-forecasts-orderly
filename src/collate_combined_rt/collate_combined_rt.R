## orderly::orderly_develop_start(use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

f <- function(infiles, pattern) {

  combined_rt_estimates <- map(
    infiles[grep(pattern, infiles)], readRDS
  )

  combined_rt_qntls <- map_depth(
    combined_rt_estimates, 2, combined_rt_quantiles
  )

  combined_rt_qntls <- map_dfr(
    combined_rt_qntls, ~ bind_rows(., .id = "country")
  )

  combined_rt_qntls

}

combined_rt_qntls <- f(infiles, "combined_rt_estimates")

saveRDS(combined_rt_qntls, "combined_rt_qntls.rds")


combined_wtd_rt_qntls <- f(
  infiles, "combined_weighted_estimates_across_countries"
)

names(combined_rt_estimates) <- gsub(
  pattern = "combined_rt_estimates_",
  replacement = "",
  x = infiles[grep("combined_rt_estimates", infiles)]
) %>% gsub(pattern = ".rds", replacement = "", x = .)

length_weeks_combined <- map_dfr(
  combined_rt_estimates,
  function(cntry) {
    map_dfr(
      cntry,
      function(x) data.frame(weeks_combined = length(x$weeks_combined)),
      .id = "country"
    )
  }, .id = "forecast_week"
)

saveRDS(
  length_weeks_combined, "length_weeks_combined.rds"
)
combined_rt_qntls <- map_depth(
  combined_rt_estimates,
  2,
  function(l) {
    l$weeks_combined <- as.Date(l$weeks_combined)
    qntls <- data.frame(val = l$combined_rt, check.names = FALSE)
    qntls <- tibble::rownames_to_column(qntls, "quantile")
    qntls <- spread(qntls, quantile, val)
    out <- data.frame(
      week_starting = min(l$weeks_combined),
      week_ending = max(l$weeks_combined)
    )
    cbind(out, qntls)
  }
)


combined_wtd2_rt_qntls <- f(
  infiles, "combined_weighted_estimates_per_country"
)


saveRDS(
  combined_wtd2_rt_qntls,
  "combined_weighted_estimates_per_country.rds"
)


weekly_iqr <- map(infiles[grep("weekly_iqr", infiles)], readRDS)
weekly_iqr <- map_dfr(
  weekly_iqr, ~ dplyr::bind_rows(., .id = "country")
)

saveRDS(weekly_iqr, "weekly_iqr.rds")
