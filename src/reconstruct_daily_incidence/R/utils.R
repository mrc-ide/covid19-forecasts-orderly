# Function to reconstruct daily incidence using the prepare_jhu_data outputs

reconstruct_incid <- function(incidence_data, time_window, location,
                              si_mean, si_sd) {
  
  incid <- incidence_data[incidence_data$dates %in% time_window, c("dates", location)]
  incid$week_starting <- cut(incid$dates, "week")
  location_colname <- sym(location)
  
  incid <- incid %>% 
    rename(daily_incidence = all_of(location_colname)) %>% # change the column heading from state name to something more generic
    group_by(week_starting) %>%
    mutate(weekly_incidence = sum(daily_incidence))
  
  aggr_incid <- incid %>%
    group_by(week_starting) %>% 
    summarise(weekly_incidence = mean(weekly_incidence))
  
  #convert the weekly aggregations column to vector
  weekly_dat <- pull(aggr_incid, weekly_incidence)
  
  
  # define si mean and sd
  config <- make_config(list(mean_si = si_mean,
                             std_si = si_sd))
  
  
  # use EpiEstim to get the reconstructed incidence
  
  res <- estimate_R_agg(incid = weekly_dat,
                        dt = 7, # aggregation window
                        dt_out = 7, # length of sliding window used
                        iter = 10,
                        config = config, 
                        method = "parametric_si")
  
  res$I
  
}

