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

ggplot(all_cases_weekly) +
  geom_point(aes(x = ))





