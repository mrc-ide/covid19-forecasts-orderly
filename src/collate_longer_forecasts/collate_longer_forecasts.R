run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

names(infiles) <- gsub(
  x = infiles, pattern = "longer_projections_", replacement = ""
) %>% gsub(x = ., pattern = ".rds", replacement = "")

projections <- purrr::map(infiles, readRDS)

pred_qntls <- purrr::map_depth(
  projections, 2,
  function(pred) {
    pred <- data.frame(pred, check.names = FALSE)
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)

pred_qntls <- purrr::map_dfr(
  pred_qntls,
  ~ dplyr::bind_rows(., .id = "country"),
  .id = "forecast_week"
)

saveRDS(pred_qntls, "longer_projections_qntls.rds")
