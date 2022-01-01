weekly_summary <- function(df) {
######################################################################
################## Weekly Summary for each country ###################
######################################################################
  weekly <- group_by(df, forecast_date, country) %>%
    summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each country ##########################
######################################################################
  by_country <- group_by(df, country) %>%
    summarise_if(is.numeric, list(c_mu = mean, c_sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each week ##########################
######################################################################
  by_week <- group_by(df, forecast_date) %>%
    summarise_if(is.numeric, list(d_mu = mean, d_sd = sd)) %>%
    ungroup()

  n_forecast <- count(weekly, country)

  weekly <- left_join(weekly, n_forecast)
  weekly <- left_join(weekly, by_country)
  weekly <- left_join(weekly, by_week)

  weekly
}
