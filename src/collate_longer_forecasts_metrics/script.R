run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

## orderly::orderly_develop_start(use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
exclude <- readRDS("exclude.rds")

infiles <- grep("exclude.rds", infiles, value = TRUE, invert = TRUE)


names(infiles) <- gsub(
  x = infiles, pattern = "long_projections_error_", replacement = ""
) %>% gsub(x = ., pattern = ".rds", replacement = "")

daily <- map_dfr(infiles, readRDS, .id = "forecast_week")
daily <- daily[!daily$country %in% exclude, ]

weekly <- group_by(
  daily, strategy, country, forecast_week, week_of_projection
) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()


saveRDS(daily, "long_projections_error_daily.rds")
saveRDS(weekly, "long_projections_error_weekly.rds")
