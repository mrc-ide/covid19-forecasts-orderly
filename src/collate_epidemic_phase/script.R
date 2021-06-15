run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

short_term <- grep("short_term", infiles, value = TRUE) %>%
  map_dfr(readRDS)

medium_term <- grep("medium_term", infiles, value = TRUE) %>%
  map_dfr(readRDS)


saveRDS(short_term, "collated_short_term_phase.rds")
saveRDS(medium_term, "collated_medium_term_phase.rds")
