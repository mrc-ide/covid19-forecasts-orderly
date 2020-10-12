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

saveRDS(
  combined_wtd_rt_qntls,
  "combined_weighted_estimates_across_countries.rds"
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
