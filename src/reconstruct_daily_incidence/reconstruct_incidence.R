orderly::orderly_develop_start(use_draft = "newer", parameters = list(
                               week_ending = "2022-07-31",
                               location = "Florida", short_run = TRUE))

model_input <- readRDS("model_input.rds")
deaths_to_use <- model_input$D_active_transmission

si_distrs <- readRDS("si_distrs.rds")

# Select incidence from time when Florida reported daily
# use Feb 2021 (plus the preceding two weeks)

start <- "2021-01-18"
end <- "2021-02-28"
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
weekly_dat <- pull(aggr_incid, weekly_incidence)

# define si mean and sd
config <- make_config(list(mean_si = 4.80,
                           std_si = 2.70))

res <- estimate_R_agg(incid = weekly_dat,
                      dt = 7, # aggregation window
                      dt_out = 7, # length of sliding window used
                      iter = 10,
                      config = config, 
                      method = "parametric_si")

res

incid$reconstructed_incid <- res$I
