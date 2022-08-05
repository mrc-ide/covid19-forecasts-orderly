# orderly::orderly_develop_start(use_draft = "newer", parameters = list(
#                                week_ending = "2021-02-28",
#                                location = "Florida", short_run = TRUE))

model_input <- readRDS("model_input.rds")
deaths_to_use <- model_input$D_active_transmission
cases_to_use <- model_input$I_active_transmission

si_distrs <- readRDS("si_distrs.rds")

# Select incidence from time when Florida reported daily
# set this with week_ending parameter
# use March 2021 as the forecasted weeks
# i.e. forecast weeks will be c("2021-02-28", "2021-03-07", "2021-03-14", "2021-03-21")
# for each forecast recreate the daily incidence for the previous 6 weeks
# the first two weeks don't get used because they can vary in quality,
# then we use ~1 month of data in the forecast models

end <- as.Date(week_ending)
start <- end - 41 # select 6 weeks of data
analysis_period <- seq.Date(from = as.Date(start),
                              to = as.Date(end),
                              by = 1)

si_mean <- 4.80
si_std <- 2.70

recon_daily_deaths <- reconstruct_incid(incidence_data = deaths_to_use,
                                        time_window = analysis_period,
                                        location = location,
                                        si_mean = si_mean,
                                        si_sd = si_std)

recon_daily_cases <- reconstruct_incid(incidence_data = cases_to_use,
                                        time_window = analysis_period,
                                        location = location,
                                        si_mean = si_mean,
                                        si_sd = si_std)

saveRDS(recon_daily_deaths, "reconstructed_daily_deaths.rds")
saveRDS(recon_daily_cases, "reconstructed_daily_cases.rds")

# Create the model_input object that is used in the forecasting models

x <- list(
  date_week_ending = week_ending,
  I_active_transmission = recon_daily_cases,
  D_active_transmission = recon_daily_deaths,
  State = location,
  si_mean = si_mean,
  si_std = si_std
)

out <- saveRDS(object = x, file = "latest_model_input.rds")


# Compare reconstructed with the reported incidence

deaths <- deaths_to_use[deaths_to_use$dates %in% analysis_period, c("dates", location)]
colnames(deaths) <- c("dates", "reported_incid")

deaths$reconstructed_incid <- recon_daily_deaths


compare_incid <- deaths %>% 
  pivot_longer(cols = reported_incid:reconstructed_incid,
               names_to = "incid_type",
               values_to = "incid")

# simple plots to visualise comparison

p <- ggplot(compare_incid) +
  geom_point(aes(x = dates, y = incid, col = incid_type))

ggsave("incid_reported_vs_recon.png", p)
