## TO DO
# - set this up as proper orderly task (dependencies etc)

library(dplyr)
library(lubridate)
library(ggplot2)

# Load in US state and global data

state_cases <- readRDS("~/us_forecasts/covid19-forecasts-orderly/draft/prepare_jhu_data/20210920-204854-ee575aed/latest_cases_wide_no_filter.rds")

state_deaths <- readRDS("~/us_forecasts/covid19-forecasts-orderly/draft/prepare_jhu_data/20210920-204854-ee575aed/latest_deaths_wide_no_filter.rds")

global_cases <- readRDS("~/us_forecasts/covid19-forecasts-orderly/draft/prepare_ecdc_data/20210927-140714-fdb4c51e/latest_cases_wide_no_filter.rds")

global_deaths <- readRDS("~/us_forecasts/covid19-forecasts-orderly/draft/prepare_ecdc_data/20210927-140714-fdb4c51e/latest_deaths_wide_no_filter.rds")


# Check sums of cases
us_cases_state <- sum(colSums(state_cases[,-1]))

us_cases_global <- sum(global_cases[, "United_States_of_America"])

(us_cases_global - us_cases_state) / us_cases_global

# Check sums of deaths
us_deaths_state <- sum(colSums(state_deaths[,-1]))

us_deaths_global <- sum(global_deaths[, "United_States_of_America"])

(us_deaths_global - us_deaths_state) / us_deaths_global


# Look from when we started doing forecasts on server
# week ending 23 May 2021

state_cases$national_sum <- rowSums(state_cases[,2:ncol(state_cases)])

state_cases_weekly <- state_cases %>%
  select(dates, national_sum) %>% 
  mutate(week = week(ymd(dates)),
         year = year(ymd(dates))) %>% 
  group_by(year, week) %>% 
  summarise(state_cases = sum(national_sum))


global_cases_weekly <- select(global_cases, c("dates", "United_States_of_America")) %>% 
  mutate(week = week(ymd(dates)),
         year = year(ymd(dates))) %>% 
  group_by(year, week) %>% 
  summarise(country_cases = sum(`United_States_of_America`))

all_cases_weekly <- left_join(global_cases_weekly, state_cases_weekly, by = c("year", "week"))


### continue here. Make a plot showing how the national figure compares to aggregated states over time
# May need log scale.


all_cases_weekly <- all_cases_weekly %>% 
  mutate(week_axis = ifelse(year == 2021, week + 53, week),
         rel_diff = (state_cases - country_cases) / country_cases)

plot_cases_over_time <- ggplot(all_cases_weekly) +
  geom_point(aes(x = week_axis, y = rel_diff)) +
  ylab("(State total - country total) / country total") +
  scale_y_continuous(minor_breaks = seq(-1, 1, 0.1)) +
  ggtitle("Case reporting") +
  theme_bw()

plot_cases_over_time2 <-
  ggplot(all_cases_weekly) +
  geom_point(aes(x = week_axis, y = rel_diff)) +
  ylab("(State total - country total) / country total") +
  ylim(c(-0.2, 0.2)) +
  theme_bw()



## Same plots for deaths

state_deaths$national_sum <- rowSums(state_deaths[,2:ncol(state_deaths)])

state_deaths_weekly <- state_deaths %>%
  select(dates, national_sum) %>% 
  mutate(week = week(ymd(dates)),
         year = year(ymd(dates))) %>% 
  group_by(year, week) %>% 
  summarise(state_deaths = sum(national_sum))


global_deaths_weekly <- select(global_deaths, c("dates", "United_States_of_America")) %>% 
  mutate(week = week(ymd(dates)),
         year = year(ymd(dates))) %>% 
  group_by(year, week) %>% 
  summarise(country_deaths = sum(`United_States_of_America`))

all_deaths_weekly <- left_join(global_deaths_weekly, state_deaths_weekly, by = c("year", "week"))


all_deaths_weekly <- all_deaths_weekly %>% 
  mutate(week_axis = ifelse(year == 2021, week + 53, week),
         rel_diff = (state_deaths - country_deaths) / country_deaths,
         abs_diff = state_deaths - country_deaths)

plot_deaths_over_time <-
  all_deaths_weekly %>% 
  filter(rel_diff != Inf) %>% 
  ggplot() +
  geom_point(aes(x = week_axis, y = rel_diff)) +
  ylab("(State total - country total) / country total") +
  # scale_y_continuous(minor_breaks = seq(-1, 1, 0.1)) +
  ggtitle("Death reporting") +
  theme_bw()

plot_deaths_over_time


plot_deaths_over_time_abs <-
  all_deaths_weekly %>% 
  filter(rel_diff != Inf) %>% 
  ggplot() +
  geom_point(aes(x = week_axis, y = abs_diff)) +
  ylab("State total - country total") +
  # scale_y_continuous(minor_breaks = seq(-1, 1, 0.1)) +
  ggtitle("Death reporting (absolute diff)") +
  theme_bw()


plot_deaths_over_time
plot_deaths_over_time_abs

summary(all_deaths_weekly$rel_diff)

plot_cases_over_time
plot_cases_over_time2

summary(all_cases_weekly$rel_diff)


# NEXT TIME: LOOK AT WHAT HAPPENS IF WE CHANGE THE LEAD?LAG OF COUNTRY LEVEL VS STATE
# CAN WE SHIFT COUNTRY BY A DAY OR TWO TO GET BETTER MATCHES?

# ANOTHER THING TO LOOK AT: dataset changes over time
# How many overwrites occurred for past dates? Can we get an idea of how reliable the RT data was?
