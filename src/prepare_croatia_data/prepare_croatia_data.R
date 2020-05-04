clean <- readxl::read_xlsx("croatia_regions.xlsx", sheet = "clean")
clean <- janitor::clean_names(clean)
regions <- unique(clean$region)
clean <- dplyr::rename(clean, dates = "date")
clean$dates <- lubridate::ymd(clean$dates)
I_active_transmission <- clean[, c("dates", "region", "n_positive")]
I_active_transmission <- tidyr::spread(
  I_active_transmission, region, n_positive
)

D_active_transmission <- clean[, c("dates", "region", "n_deceased")]
D_active_transmission <- tidyr::spread(
  D_active_transmission, region, n_deceased
)

out <- list(
  date_week_finishing = max(clean$dates),
  Country = regions,
  Threshold_criterion_4weeks = 0,
  Threshold_criterion_7days = 0,
  I_active_transmission = I_active_transmission,
  D_active_transmission = D_active_transmission,
  si_mean = c(4.8032, 6.48),
  si_std = c(2.70201110286394, 3.83)
)

saveRDS(
  object = out,
  file = "data_croatia_week_ending_2020-04-20.rds"
)
