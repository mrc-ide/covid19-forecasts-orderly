orderly::orderly_develop_start(use_draft = "newer", parameters = list(
                               week_ending = "2022-07-31",
                               location = "Florida", short_run = TRUE))

model_input <- readRDS("model_input.rds")
deaths_to_use <- model_input$D_active_transmission

si_distrs <- readRDS("si_distrs.rds")

# Select incidence from time when Florida reported daily
# use March 2021 as the forecasted weeks
# for each forecast recreate the daily incidence for the previous 6 weeks
# the first two weeks don't get used because they can vary in quality,
# then we use ~1 month of data in the forecast models

forecast_weeks <- c("2021-02-28", "2021-03-07", "2021-03-14",
                    "2021-03-21")

weekly_dat <- map(forecast_weeks, function(week) {
  
  end <- as.Date(week)
  start <- end - 41 # select 6 weeks of data
  analysis_period <- seq.Date(from = as.Date(start),
                              to = as.Date(end),
                              by = 1)
  
  incid <- deaths_to_use[deaths_to_use$dates %in% analysis_period, c("dates", location)]
  incid$week_starting <- cut(incid$dates, "week")
  incid <- incid %>% 
    group_by(week_starting) %>% 
    mutate(weekly_incidence = sum(Florida))
  
  aggr_incid <- incid %>%
    group_by(week_starting) %>% 
    summarise(weekly_incidence = mean(weekly_incidence))
  
  #convert to vector
  out <- pull(aggr_incid, weekly_incidence)
  out
  
})

names(weekly_dat) <- forecast_weeks

# define si mean and sd
config <- make_config(list(mean_si = 4.80,
                           std_si = 2.70))


# use EpiEstim to get the reconstructed incidence

recon_daily_inc <- map(weekly_dat, function(w) {
  
  res <- estimate_R_agg(incid = w,
                        dt = 7, # aggregation window
                        dt_out = 7, # length of sliding window used
                        iter = 10,
                        config = config, 
                        method = "parametric_si")
  
  res$I
  
  
})

# Compare reconstructed with the reported incidence

compare_incid <- imap(forecast_weeks, function(week, index) {
  
  end <- as.Date(week)
  start <- end - 41 # select 6 weeks of data
  analysis_period <- seq.Date(from = as.Date(start),
                              to = as.Date(end),
                              by = 1)
  
  incid <- deaths_to_use[deaths_to_use$dates %in% analysis_period, c("dates", location)]
  colnames(incid) <- c("dates", "reported_incid")

  incid$reconstructed_incid <- recon_daily_inc[[index]]
  
  incid
  
})

names(compare_incid) <- forecast_weeks
compare_incid <- bind_rows(compare_incid, .id = "week_ending") %>% 
  pivot_longer(cols = reported_incid:reconstructed_incid,
               names_to = "incid_type",
               values_to = "incid")

ggplot(compare_incid) +
  geom_point(aes(x = dates, y = incid, col = incid_type)) +
  facet_wrap(~week_ending)
