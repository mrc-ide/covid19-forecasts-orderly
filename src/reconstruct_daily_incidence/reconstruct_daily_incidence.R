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
  "latest",
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

# Find first Monday in dataset
start <- model_input$I_active_transmission$dates[
  which(wday(model_input$I_active_transmission$dates, week_start = 1) == 1)[1]
  ]

end <- as.Date(week_ending) # including final forecast week (22nd - 28th March)

dates <- seq(from = start, to = end, by = 1)
analysis_period <- seq.Date(from = as.Date(start),
                              to = as.Date(end),
                              by = 1)

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
  I_active_transmission = deaths_tibble,
  D_active_transmission = cases_tibble,
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
