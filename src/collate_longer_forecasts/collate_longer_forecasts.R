## orderly::orderly_develop_start(use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

named_infiles <- function(infiles, pattern) {
  x <- grep(pattern = pattern, x = infiles, value = TRUE)
  names(x) <- gsub(
  x = x, pattern = glue("{pattern}_"), replacement = ""
  ) %>% gsub(x = ., pattern = ".rds", replacement = "")
  x
}

unwtd <- named_infiles(infiles, pattern = "unweighted_pred_qntls")
wtd_across_all <- named_infiles(
  infiles, pattern = "weighted_across_countries_pred_qntls"
)
wtd_per_country <- named_infiles(
  infiles, "weighted_per_country_pred_qntls"
)


pred_qntls_infiles <- list(
  unwtd = unwtd,
  wtd_across_all = wtd_across_all,
  wtd_per_country = wtd_per_country
)

pred_qntls <- map_depth(pred_qntls_infiles, 2, readRDS)

iwalk(
  pred_qntls,
  function(qntls, strategy) {
    out <- map_dfr(
      qntls, ~ bind_rows(., .id = "country"), .id = "forecast_week"
    )
    saveRDS(out, glue("{strategy}_projections_qntls.rds"))
  }
)


unwtd <- named_infiles(infiles, pattern = "unweighted_ps_qntls")
wtd_across_all <- named_infiles(
  infiles, pattern = "weighted_across_countries_ps_qntls"
)
wtd_per_country <- named_infiles(
  infiles, "weighted_per_country_ps_qntls"
)


ps_qntls_infiles <- list(
  unwtd = unwtd,
  wtd_across_all = wtd_across_all,
  wtd_per_country = wtd_per_country
)

ps_qntls <- map_depth(ps_qntls_infiles, 2, readRDS)

iwalk(
  ps_qntls,
  function(qntls, strategy) {
    out <- map_dfr(
      qntls, ~ bind_rows(., .id = "country"), .id = "forecast_week"
    )
    saveRDS(out, glue("{strategy}_ps_qntls.rds"))
  }
)


unwtd <- named_infiles(infiles, pattern = "unweighted_reff_qntls")
wtd_across_all <- named_infiles(
  infiles, pattern = "weighted_across_countries_reff_qntls"
)
wtd_per_country <- named_infiles(
  infiles, "weighted_per_country_reff_qntls"
)


reff_qntls_infiles <- list(
  unwtd = unwtd,
  wtd_across_all = wtd_across_all,
  wtd_per_country = wtd_per_country
)

reff_qntls <- map_depth(reff_qntls_infiles, 2, readRDS)

iwalk(
  reff_qntls,
  function(qntls, strategy) {
    out <- map_dfr(
      qntls, ~ bind_rows(., .id = "country"), .id = "forecast_week"
    )
    saveRDS(out, glue("{strategy}_reff_qntls.rds"))
  }
)


unwtd <- named_infiles(
  infiles, pattern = "unweighted_reff_with_underreporting"
)
wtd_across_all <- named_infiles(
  infiles, pattern = "weighted_across_countries_reff_with_underreporting"
)
wtd_per_country <- named_infiles(
  infiles, "weighted_per_country_reff_with_underreporting"
)


reff_qntls_with_underreporting_infiles <- list(
  unwtd = unwtd,
  wtd_across_all = wtd_across_all,
  wtd_per_country = wtd_per_country
)

reff_qntls_with_underreporting <- map_depth(
  reff_qntls_with_underreporting_infiles, 2, readRDS
)

iwalk(
  reff_qntls_with_underreporting,
  function(qntls, strategy) {
    out <- bind_rows(qntls, .id = "forecast_week")
    saveRDS(out, glue("{strategy}_reff_qntls_with_underreporting.rds"))
  }
)



