run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
source("R/utils.R")
short_term <- grep("short_term", infiles, value = TRUE) %>%
  map_dfr(readRDS)

short_term <- rename(short_term, c('forecast_date' = 'model'))
short_term$forecast_date <- as.Date(short_term$forecast_date)

short_term$phase_for_week <- phase_for_week(
  short_term$forecast_date - 6, short_term$forecast_date
)

medium_term <- grep("medium_term", infiles, value = TRUE) %>%
  map_dfr(readRDS)


saveRDS(short_term, "collated_short_term_phase.rds")
saveRDS(medium_term, "collated_medium_term_phase.rds")
