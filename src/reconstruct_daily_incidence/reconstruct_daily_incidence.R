# Reconstruct daily incidence to use as model input

orderly_parameters(week_ending = NULL,
                   location = NULL,
                   short_run = NULL)

packages <- c("purrr", "EpiEstim", "dplyr", "ggplot2", "lubridate")
lapply(packages, require, character.only = TRUE)

source("R/utils.R")

orderly_artefact(
  "Reconstructed daily incidence",
  c(
    "reconstructed_daily_cases.rds",
    "reconstructed_daily_deaths.rds",
    "latest_model_input.rds"
  )
)

orderly_dependency(
  "prepare_jhu_data",
  paste0("latest(parameter:week_ending == '", as.character(week_ending), "')"),
  c(
    "model_input.rds" = "latest_model_input.rds"
  )
)

orderly_dependency(
  "produce_epi_params",
  "latest",
  c(
    "si_distrs.rds"
  )
)


# ------------------------------------------------------------

model_input <- readRDS("model_input.rds")

if(location == "all") {
  location <- model_input$State
} else {
  location <- c(
    "California", "Colorado", "Delaware", "Hawaii", "Maryland", "Missouri",
    "New Jersey", "New York", "North Dakota", "Ohio", "Pennsylvania",
    "Tennessee", "Texas"
    )
  }

deaths_to_use <- model_input$D_active_transmission
cases_to_use <- model_input$I_active_transmission

si_distrs <- readRDS("si_distrs.rds")

# Aggregate by week before projection day e.g. if Sunday projections, aggregate Monday to Sunday
# 22nd Jan 2020 (Wednesday) is the earliest date in dataset
week_day_map <- list(
  "2022-02-21" = 2,  # Monday --> find first Tuesday (2) in dataset (28th Jan 2020)
  "2022-02-22" = 3,  # Tuesday --> find first Wednesday (3) in dataset (22nd Jan 2020)
  "2022-02-23" = 4,  # etc...
  "2022-02-24" = 5,  # 
  "2022-02-25" = 6,  # 
  "2022-02-26" = 7,  # 
  "2022-02-27" = 1   # 
)

# check that the input week_ending is valid
if (!week_ending %in% names(week_day_map)) {
  stop("Unrecognised week_ending date")
}

# Find first day to start aggregations
target_day <- week_day_map[[week_ending]]
start <- model_input$I_active_transmission$dates[
  which(wday(model_input$I_active_transmission$dates, week_start = 1) == target_day)[1]
]

# Date to stop aggregations
end <- as.Date(week_ending) # including final forecast

dates <- seq(from = start, to = end, by = 1)
analysis_period <- seq.Date(from = as.Date(start),
                              to = as.Date(end),
                              by = 1)

deaths_to_use <- model_input$D_active_transmission %>% filter(dates %in% analysis_period)
cases_to_use <- model_input$I_active_transmission %>% filter(dates %in% analysis_period)

si_mean <- 4.80
si_std <- 2.70

# Daily cases and deaths reconstructed from aggregated data
recon_daily_deaths <- lapply(location, function(loc) {
  print(loc)
  reconstruct_incid(
    incidence_data = deaths_to_use,
    time_window = analysis_period,
    location = loc,
    si_mean = si_mean,
    si_sd = si_std
  )
})

names(recon_daily_deaths) <- location


recon_daily_cases <- lapply(location, function(loc) {
  print(loc)
  reconstruct_incid(
    incidence_data = cases_to_use,
    time_window = analysis_period,
    location = loc,
    si_mean = si_mean,
    si_sd = si_std
  )
})

names(recon_daily_cases) <- location

# save result
saveRDS(recon_daily_deaths, "reconstructed_daily_deaths.rds")
saveRDS(recon_daily_cases, "reconstructed_daily_cases.rds")

# Create the model_input object that is used in the forecasting models  
deaths_tibble <- tibble(dates = analysis_period)
cases_tibble <- tibble(dates = analysis_period)

for (loc in names(recon_daily_deaths)) {
  deaths_tibble <- bind_cols(deaths_tibble, tibble(!!loc := recon_daily_deaths[[loc]]))
  cases_tibble <- bind_cols(cases_tibble, tibble(!!loc := recon_daily_cases[[loc]]))
}

x <- list(
  date_week_ending = week_ending,
  I_active_transmission = cases_tibble,
  D_active_transmission = deaths_tibble,
  State = location,
  si_mean = si_mean,
  si_std = si_std
)

out <- saveRDS(object = x, file = "latest_model_input.rds")

# Compare reconstructed with the reported incidence
report_daily_deaths <- deaths_to_use %>%
  select(dates, location) %>%
  filter(dates %in% analysis_period)

rep_deaths_long <- report_daily_deaths %>%
  pivot_longer(cols = -dates, names_to = "location", values_to = "reported")

recon_deaths_long <- deaths_tibble %>%
  pivot_longer(cols = -dates, names_to = "location", values_to = "reconstructed")

deaths <- inner_join(rep_deaths_long, recon_deaths_long)

p <- ggplot(deaths, aes(x = dates)) +
  geom_line(aes(y = reported, color = "Reported")) +
  geom_line(aes(y = reconstructed, color = "Reconstructed")) +
  scale_colour_manual("", 
                      breaks = c("Reported", "Reconstructed"),
                      values = c("darkgrey", "black"),
                      labels = c("Reported", "Reconstructed")) +
  facet_wrap(~ location, scales = "free_y") +
  labs(x = "Dates", y = "Values", color = "Data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("incid_reported_vs_recon.png", p)
