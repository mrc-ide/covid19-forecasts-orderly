pars <- orderly_parameters(incidence_type = "cases")
list2env(pars, environment())

packages <- c("dplyr", "tidyr", "glue", "ggplot2", "gridExtra", "ggforce",
              "gdata", "tibble", "scoringutils", "knitr", "lubridate",
              "cowplot", "purrr")

suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))

orderly_artefact(description = "Manuscript figures",
                 c("figures/Fig2.tif", "figures/Fig3.tif",
                   "figures/Fig4.tif", "figures/Fig5.tif",
                   "figures/Fig6.tif", "figures/S1_fig.tif",
                   "figures/S2_fig.tif","figures/S3_fig.tif"))

# True reported incidence (entire period - including forecast period)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-21')",
  c("reported_model_input_MON.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-22')",
  c("reported_model_input_TUE.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-23')",
  c("reported_model_input_WED.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-24')",
  c("reported_model_input_THU.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-25')",
  c("reported_model_input_FRI.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-26')",
  c("reported_model_input_SAT.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "prepare_jhu_data",
  "latest(parameter:week_ending == '2022-02-27')",
  c("reported_model_input_SUN.rds" = "latest_model_input.rds")
)
# Reconstructed incidence (entire period - including forecast period)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-21')",
  c("reconstructed_model_input_MON.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-22')",
  c("reconstructed_model_input_TUE.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-23')",
  c("reconstructed_model_input_WED.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-24')",
  c("reconstructed_model_input_THU.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-25')",
  c("reconstructed_model_input_FRI.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-26')",
  c("reconstructed_model_input_SAT.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest(parameter:week_ending == '2022-02-27')",
  c("reconstructed_model_input_SUN.rds" = "latest_model_input.rds")
)

# CRPS values - reconstructed
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-21' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_MON.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_MON.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-22' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_TUE.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_TUE.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-23' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_WED.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_WED.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-24' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_THU.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_THU.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-25' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_FRI.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_FRI.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-26' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_SAT.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_SAT.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-27' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_SUN.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_SUN.rds" = "rti0_crps_weekly.rds"
  )
)
# CRPS values - reported
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-21' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_MON.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_MON.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-22' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_TUE.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_TUE.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-23' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_WED.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_WED.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-24' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_THU.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_THU.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-25' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_FRI.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_FRI.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-26' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_SAT.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_SAT.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_collate_weekly_outputs",
  paste0("latest(parameter:week_ending == '2022-02-27' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_SUN.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_SUN.rds" = "rti0_crps_weekly.rds"
  )
)

####################################

dir.create("figures", showWarnings = FALSE)

all_files <- list.files(pattern = "\\.rds$", full.names = TRUE)
for (x in all_files) {
  obj_name <- gsub("\\.rds$", "", basename(x))
  assign(obj_name, readRDS(x))
}

states <- unique(rep_rti0_crps_WED$state)

## True reported and true reconstructed

# True reported incidence
rep_inputs <- list(
  MON = reported_model_input_MON, TUE = reported_model_input_TUE,
  WED = reported_model_input_WED, THU = reported_model_input_THU,
  FRI = reported_model_input_FRI, SAT = reported_model_input_SAT,
  SUN = reported_model_input_SUN
)

start_dates <- as.Date(c(
  MON = "2020-03-09", TUE = "2020-03-10",
  WED = "2020-03-11", THU = "2020-03-12",
  FRI = "2020-03-13", SAT = "2020-03-14",
  SUN = "2020-03-15"
))

true_reported <- mapply(function(input, start_date) {
  input$I_active_transmission %>%
    filter(dates >= start_date) %>%
    select(dates, all_of(states)) %>%
    pivot_longer(cols = -dates, names_to = "state", values_to = "incidence")
}, rep_inputs, start_dates, SIMPLIFY = FALSE)


# True reconstructed incidence
recon_inputs <- list(
  MON = reconstructed_model_input_MON, TUE = reconstructed_model_input_TUE,
  WED = reconstructed_model_input_WED, THU = reconstructed_model_input_THU,
  FRI = reconstructed_model_input_FRI, SAT = reconstructed_model_input_SAT,
  SUN = reconstructed_model_input_SUN
)

true_reconstructed <- mapply(function(input, start_date) {
  input$I_active_transmission %>%
    filter(dates >= start_date) %>%
    select(dates, all_of(states)) %>%
    pivot_longer(cols = -dates, names_to = "state", values_to = "incidence")
}, recon_inputs, start_dates, SIMPLIFY = FALSE)


# Visualise reported data
true_reported$SUN$day <- factor(
  weekdays(as.Date(true_reported$SUN$dates, format="%d/%m/%Y")),
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
             "Friday", "Saturday", "Sunday"))

true_reported$SUN$week <- paste0(
  format(as.Date(true_reported$SUN$dates, format = "%d/%m/%Y"), "%Y"), "-",
  format(as.Date(true_reported$SUN$dates, format = "%d/%m/%Y"), "%V")
)

comp_weeks <- true_reported$SUN %>%
  group_by(week) %>%
  summarise(count = n(), .groups = "drop")

rm_weeks <- comp_weeks %>%
  filter(count != 91)

#####################################################################################
## Percentage of cases reported on each day of the week overall

dow_perc_inc_box <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  group_by(week) %>%
  mutate(total_weekly_cases = sum(incidence, na.rm = TRUE)) %>%
  group_by(week, day) %>%
  summarise(total_daily_cases = sum(incidence, na.rm = TRUE),
            total_weekly_cases = first(total_weekly_cases),
            .groups = "drop") %>%
  mutate(percentage = (total_daily_cases / total_weekly_cases) * 100,
         state = "All States")

ggplot(dow_perc_inc_box, aes(x = day, y = percentage)) +
  geom_boxplot(outliers = FALSE, fill = "dodgerblue", alpha = 0.3) +
  geom_hline(yintercept = 100 / 7, linetype = 2) +
  labs(x = "Day of the Week", y = "Percentage of Weekly Cases Reported") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 17),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(colour = "grey", fill = NA,
                                    linewidth = 0.5),
        axis.title.y = element_text(margin = margin(r = 10), size = 17),
        axis.title.x = element_text(margin = margin(t = 10), size = 17),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))


################################################################################
# Percentage of cases reported on each day of the week by state

dow_perc_inc_state <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  group_by(week, state) %>%
  mutate(total_cases = sum(incidence, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percentage = (incidence / total_cases) * 100,
         day = factor(substr(day, 1, 3), levels = c("Mon", "Tue", "Wed", "Thu",
                                                    "Fri", "Sat", "Sun")))

dow_perc_inc_state <- bind_rows(dow_perc_inc_box, dow_perc_inc_state) %>%
  mutate(day = factor(substr(day, 1, 3), levels = c("Mon", "Tue", "Wed", "Thu",
                                                    "Fri", "Sat", "Sun")))

fig2 <- ggplot(dow_perc_inc_state, aes(x = day, y = percentage)) +
  geom_boxplot(outliers = FALSE, alpha = 0.3, fill = "dodgerblue") +
  geom_hline(yintercept = 100 / 7, linetype = 2) +
  facet_wrap(~ state, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 17),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(colour = "grey", fill = NA,
                                    linewidth = 0.5),
        axis.title.y = element_text(margin = margin(r = 20), size = 19),
        axis.title.x = element_text(margin = margin(t = 20), size = 19),
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Day of the Week", y = "Percentage of Weekly Cases Reported")

ggsave("figures/Fig2.tif", fig2, width = 17, height = 15,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

################################################################################
# Percentage of cases reported on each day of the week by outbreak phase

dow_perc_inc_box <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  group_by(week) %>%
  mutate(total_weekly_cases = sum(incidence, na.rm = TRUE)) %>%
  group_by(week, day) %>%
  summarise(total_daily_cases = sum(incidence, na.rm = TRUE),
            total_weekly_cases = first(total_weekly_cases),
            .groups = "drop") %>%
  mutate(percentage = (total_daily_cases / total_weekly_cases) * 100,
         outbreak_phase = "All Phases")

dow_perc_inc_phase_box <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  mutate(outbreak_phase = case_when(
    dates >= as.Date("2020-03-01") & dates <=
      as.Date("2020-05-31") ~ "Early Pandemic",
    dates >= as.Date("2020-06-01") & dates <=
      as.Date("2020-09-30") ~ "Summer Surge",
    dates >= as.Date("2020-10-01") & dates <=
      as.Date("2020-12-31") ~ "Winter Surge",
    dates >= as.Date("2021-01-01") & dates <=
      as.Date("2021-06-30") ~ "Vaccine Rollout",
    dates >= as.Date("2021-07-01") & dates <=
      as.Date("2021-11-30") ~ "Delta Wave",
    dates >= as.Date("2021-12-01") & dates <=
      as.Date("2022-02-27") ~ "Omicron Wave",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(outbreak_phase)) %>%
  group_by(week, outbreak_phase) %>%
  mutate(total_weekly_cases = sum(incidence, na.rm = TRUE)) %>%
  group_by(week, day, outbreak_phase) %>%
  summarise(total_daily_cases = sum(incidence, na.rm = TRUE),
            total_weekly_cases = first(total_weekly_cases),
            percentage = (total_daily_cases / total_weekly_cases) * 100,
            .groups = "drop")

dow_perc_inc_phase_box <- bind_rows(dow_perc_inc_box,
                                    dow_perc_inc_phase_box) %>%
  mutate(day = factor(substr(day, 1, 3), levels = c("Mon", "Tue", "Wed", "Thu",
                                                    "Fri", "Sat", "Sun")),
         outbreak_phase = factor(outbreak_phase,
                                 levels = c("All Phases", "Early Pandemic",
                                            "Summer Surge", "Winter Surge",
                                            "Vaccine Rollout", "Delta Wave",
                                            "Omicron Wave")))

fig3 <- ggplot(dow_perc_inc_phase_box, aes(x = day, y = percentage)) +
  geom_boxplot(outliers = FALSE, alpha = 0.3, fill = "dodgerblue") +
  geom_hline(yintercept = 100 / 7, linetype = 2) +
  facet_wrap(~ outbreak_phase, ncol = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Day of the Week", y = "Percentage of Weekly Cases Reported") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 15),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(colour = "grey", fill = NA, linewidth = 0.5),
    axis.title.y = element_text(margin = margin(r = 20), size = 15),
    axis.title.x = element_text(margin = margin(t = 20), size = 15),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  )

ggsave("figures/Fig3.tif", fig3, width = 17, height = 8,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

####################################################################################
## Categorise

proj_days <- set_names(c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"))

# Find first projection week for each projection day
first_proj_week <- purrr::set_names(lapply(proj_days, function(dow) {
  min(get(paste0("rep_rti0_crps_", dow))$projection_week)
}), proj_days)

# Sequence of projection weeks for each projection day
projection_weeks <- purrr::set_names(lapply(proj_days, function(dow) {
  seq(from = as.Date(max(get(paste0("rep_rti0_crps_", dow))$date)) - (7 * 4),
      to = as.Date(first_proj_week[[dow]]), by = -7)
}), proj_days)

# Function to categorise incidence
categorise_incidence <- function(incidence) {
  case_when(
    incidence == 0 ~ "zero",
    incidence >= 1 & incidence <= 9 ~ "very low",
    incidence >= 10 & incidence <= 29 ~ "low",
    incidence >= 30 & incidence <= 99 ~ "medium",
    incidence >= 100 & incidence <= 999 ~ "high",
    incidence >= 1000 ~ "very high"
  )
}

# Calculate past week's incidence and categorise
categorised_data <- purrr::map_dfr(proj_days, function(dow) {
  projection_week <- projection_weeks[[dow]]
  
  purrr::map_dfr(projection_week, function(pw) {
    true_reported[[dow]] %>%
      filter(dates <= pw & dates > pw - 7) %>% # Filter last 7 days
      group_by(state) %>%
      summarise(
        total_incidence = sum(incidence, na.rm = TRUE),
        zero_days = sum(incidence == 0, na.rm = TRUE), # Count zero-incidence days
        max_daily_inc = max(incidence, na.rm = TRUE),
        .groups = "drop") %>% 
      mutate(projection_week = pw,
             inc_category = categorise_incidence(total_incidence))
  })
}, .id = "proj_dow")

# Filter for low or non-daily incidence
low_or_nonweekly <- purrr::map_dfr(proj_days, function(dow) {
  categorised_data %>%
    filter(proj_dow == dow) %>%
    filter(
      inc_category == "zero" |
        (inc_category %in% c("medium", "high", "very high") & zero_days >= 1)
    ) %>%
    mutate(projection_week = as.character(projection_week)) %>%
    select(projection_week, state)
}, .id = "proj_dow")

# Remove projection weeks with zero inc in past week and likely non-weekly reporting
remove_low_or_nonweekly <- function(dow) {
  rep_data <- get(paste0("rep_rti0_crps_weekly_", dow)) %>%
    anti_join(low_or_nonweekly %>% filter(proj_dow == dow),
              by = c("projection_week", "state")) %>%
    mutate(dataset = "Reported", proj_dow = dow, model = "jointlyr")
  
  recon_data <- get(paste0("recon_rti0_crps_weekly_", dow)) %>%
    anti_join(low_or_nonweekly %>% filter(proj_dow == dow),
              by = c("projection_week", "state")) %>%
    mutate(dataset = "Reconstructed", proj_dow = dow, model = "jointlyr")
  
  return(list(rep_data, recon_data))
}

jointlyr_crps_weekly <- purrr::map_dfr(proj_days, function(dow) {
  data <- remove_low_or_nonweekly(dow)
  bind_rows(data[[1]], data[[2]])
})

jointlyr_crps_weekly <- jointlyr_crps_weekly %>% select(-starts_with("true"))

################################################################################
## Data for CRPS violin plots
 
violin_data <- jointlyr_crps_weekly %>%
  mutate(log10_crps = log10(crps),
         proj_dow = stringr::str_to_title(tolower(proj_dow)),
         proj_dow = factor(proj_dow, levels = c("Mon", "Tue", "Wed", "Thu",
                                                "Fri", "Sat", "Sun")))

violin_allstates_jointlyr <- violin_data %>% mutate(state = "All States")

violin_jointlyr_combined <- bind_rows(violin_allstates_jointlyr, violin_data)

# Violin plots by phase
violin10_jointlyr_phase <- jointlyr_crps_weekly %>%
  mutate(
    outbreak_phase = case_when(
      week_ending >= as.Date("2020-03-01") &
        week_ending <= as.Date("2020-05-31") ~ "Early Pandemic",
      week_ending >= as.Date("2020-06-01") &
        week_ending <= as.Date("2020-09-30") ~ "Summer Surge",
      week_ending >= as.Date("2020-10-01") &
        week_ending <= as.Date("2020-12-31") ~ "Winter Surge",
      week_ending >= as.Date("2021-01-01") &
        week_ending <= as.Date("2021-06-30") ~ "Vaccine Rollout",
      week_ending >= as.Date("2021-07-01") &
        week_ending <= as.Date("2021-11-30") ~ "Delta Wave",
      week_ending >= as.Date("2021-12-01") &
        week_ending <= as.Date("2022-02-27") ~ "Omicron Wave",
      TRUE ~ NA_character_),
    outbreak_phase = factor(outbreak_phase,
                            levels = c("Early Pandemic", "Summer Surge",
                                       "Winter Surge", "Vaccine Rollout",
                                       "Delta Wave", "Omicron Wave")),
    log10_crps = log10(crps),
    proj_dow = stringr::str_to_title(tolower(proj_dow)),
    proj_dow = factor(proj_dow, levels = c("Mon", "Tue", "Wed", "Thu",
                                           "Fri", "Sat", "Sun"))
  )
 
violin10_jointlyr_median <- violin10_jointlyr_phase %>%
  group_by(outbreak_phase, forecast_week, dataset, model, proj_dow) %>%
  summarise(median_crps = median(log10_crps), .groups = "drop")

###############################################################################
## Supplementary figure 1 (compare reconstructed and reported incidence)
combined_inc_data <- bind_rows(
  mutate(true_reported$SUN, dataset = "Reported"),
  mutate(true_reconstructed$SUN, dataset = "Reconstructed")
  ) %>%
  mutate(dates = as.Date(dates))

combined_inc_data <- combined_inc_data %>%
  group_by(dates, dataset) %>%
  summarise(incidence = sum(incidence, na.rm = TRUE), .groups = "drop") %>%
  mutate(state = "All States") %>%
  bind_rows(combined_inc_data)

incidence_diff_data <- combined_inc_data %>%
  pivot_wider(id_cols = c(dates, state),
              names_from = dataset, 
              values_from = incidence) %>%
  mutate(inc_difference = Reported - Reconstructed)

dataset_cols_inc <- c("Reconstructed" = "#297a4d", "Reported" = "grey65")

state_list <- c("All States", sort(unique(combined_inc_data$state[combined_inc_data$state != "All States"])))

# stack incidence and reported-reconstructed panels for each state
build_state_plot <- function(target_state) {
  
  state_inc <- combined_inc_data %>% filter(state == target_state)
  state_diff <- incidence_diff_data %>% filter(state == target_state)
  
  # incidence panel
  p_inc <- ggplot(state_inc, aes(x = dates, y = incidence, colour = dataset)) +
    geom_line(data = state_inc %>% filter(dataset == "Reported"),
              linetype = "solid", linewidth = 0.5, alpha = 0.7) +
    geom_line(data = state_inc %>% filter(dataset == "Reconstructed"),
              linetype = "solid", linewidth = 0.5, alpha = 0.8) +
    scale_colour_manual(values = dataset_cols_inc) +
    facet_wrap(~ state) +
    labs(x = NULL, y = "Incidence") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          strip.background = element_rect(fill = "grey90", colour = "grey80"),
          strip.text = element_text(size = 12, face = "bold",
                                    margin = margin(t = 5, b = 5)),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 11, margin = margin(r = 5)),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey80", fill = NA),
          plot.margin = margin(t = 15, r = 5, b = 3, l = 5))
  
  # difference panel
  p_diff <- ggplot(state_diff, aes(x = dates, y = inc_difference)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "black", linewidth = 0.5) +
    geom_line(colour = "black", linewidth = 0.4, alpha = 0.6) +
    labs(x = NULL, y = "Reported\n - Reconstructed") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.title.y = element_text(size = 11, margin = margin(r = 5)),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "grey80", fill = NA),
      plot.margin = margin(t = 3, r = 5, b = 15, l = 5)
    )
  
  # combine incidence and difference panels vertically
  cowplot::plot_grid(
    p_inc, p_diff, 
    ncol = 1, align = "v",
    rel_heights = c(1.25, 1)
  )
}

# map function across all states to generate a list of combined plots
all_state_plots <- purrr::map(state_list, build_state_plot)

# extract single shared legend
legend_plot <- ggplot(combined_inc_data,
                      aes(x = dates, y = incidence, colour = dataset)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(name = NULL, values = dataset_cols_inc) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "vertical")

shared_legend <- cowplot::get_legend(legend_plot)

# arrange state blocks into a grid
master_grid <- cowplot::plot_grid(plotlist = all_state_plots, ncol = 3)

s1_fig <- cowplot::ggdraw(master_grid) +
  cowplot::draw_plot(shared_legend, 
                     x = 0.72, y = 0.05,
                     width = 0.2, height = 0.1)

ggsave("figures/S1_fig.tif", s1_fig, width = 16, height = 24,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

###############################################################################
## Supplementary figure 2 (all states and all forecast weeks)
violin_jointlyr_combined <- violin_jointlyr_combined %>%
  mutate(
    state = factor(state, levels = c("All States", unique(violin_data$state))),
    week_label = paste("Forecast Week", forecast_week)
  )

violin_medians_combined <- violin_jointlyr_combined %>%
  group_by(state, week_label, dataset, proj_dow) %>%
  summarise(median_crps = median(log10_crps), .groups = "drop")

panel_reference_lines <- violin_medians_combined %>%
  group_by(state, week_label) %>%
  summarise(week_state_median = median(median_crps), .groups = "drop")

dataset_fill <- c("Reconstructed" = "#297a4d", "Reported" = "darkgrey")
dataset_cols <- c("Reconstructed" = "#297a4d", "Reported" = "grey30")
dataset_line <- c("Reconstructed" = "solid", "Reported" = "dotted")

s2_fig <- ggplot(violin_jointlyr_combined,
                 aes(x = proj_dow, y = log10_crps, fill = dataset)) +
  geom_hline(data = panel_reference_lines,
             aes(yintercept = week_state_median),
             linetype = "dashed",
             colour = "black",
             linewidth = 0.4,
             alpha = 0.7) +
  geom_violin(aes(linetype = dataset, group = interaction(dataset, proj_dow)),
              position = position_dodge(width = 0.8),
              linewidth = 0.2,
              alpha = 0.4) +
  geom_point(data = violin_medians_combined,
             aes(y = median_crps, colour = dataset, shape = dataset,
                 group = interaction(dataset, proj_dow)),
             position = position_dodge(width = 0.8), 
             size = 1.2) +
  facet_grid(week_label ~ state, scales = "free_y") +
  scale_fill_manual(name = NULL, values = dataset_fill) +
  scale_colour_manual(name = NULL, values = dataset_cols) +
  scale_shape_manual(name = NULL, values = c("Reconstructed" = 16,
                                             "Reported" = 1)) +
  scale_linetype_manual(name = NULL, values = dataset_line) +
  labs(x = "Day of Projection", y = "log10(CRPS)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.border = element_rect(colour = "grey80", fill = NA),
        strip.background = element_rect(fill = "grey95", colour = NA),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.2, "lines"))

ggsave("figures/S2_fig.tif", s2_fig, width = 19, height = 10,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

###############################################################################
# Main paper figure 4 (all states combined faceted by forecast week)
main_fig_4_data <- violin_jointlyr_combined %>%
  filter(state == "All States")

main_medians_data <- violin_medians_combined %>%
  filter(state == "All States")

main_reference_lines <- panel_reference_lines %>%
  filter(state == "All States")

fig4 <- ggplot(main_fig_4_data, aes(x = proj_dow, y = log10_crps,
                                    fill = dataset)) +
  geom_hline(data = main_reference_lines, aes(yintercept = week_state_median),
             linetype = "dashed", colour = "black",
             linewidth = 0.4, alpha = 0.7) +
  geom_violin(aes(linetype = dataset, group = interaction(dataset, proj_dow)),
              position = position_dodge(width = 0.8),
              linewidth = 0.2, alpha = 0.4) +
  geom_point(data = main_medians_data,
             aes(y = median_crps, colour = dataset, shape = dataset,
                 group = interaction(dataset, proj_dow)),
             position = position_dodge(width = 0.8), size = 1.2) +
  facet_wrap(~ week_label, ncol = 4) +
  scale_fill_manual(name = NULL, values = dataset_fill) +
  scale_colour_manual(name = NULL, values = dataset_cols) +
  scale_shape_manual(name = NULL, values = c("Reconstructed" = 16,
                                             "Reported" = 1)) +
  scale_linetype_manual(name = NULL, values = dataset_line) +
  labs(x = "Day of Projection", y = "log10(CRPS)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    axis.title.y = element_text(size = 11, margin = margin(r = 15)),
    axis.title.x = element_text(size = 11, margin = margin(t = 15))
  )

ggsave("figures/Fig4.tif", fig4, width = 10, height = 4,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

###############################################################################
## Main paper Figure 5 (state by state for forecast week 1)
fig_week1_data <- violin_jointlyr_combined %>%
  filter(forecast_week == 1)

fig_week1_medians <- violin_medians_combined %>%
  filter(week_label == "Forecast Week 1")

fig_week1_ref_lines <- panel_reference_lines %>%
  filter(week_label == "Forecast Week 1")

fig5 <- ggplot(fig_week1_data,
               aes(x = proj_dow, y = log10_crps, fill = dataset)) +
  geom_hline(data = fig_week1_ref_lines, aes(yintercept = week_state_median),
             linetype = "dashed",
             colour = "black",
             linewidth = 0.4,
             alpha = 0.7) +
  geom_violin(aes(linetype = dataset, group = interaction(dataset, proj_dow)),
              position = position_dodge(width = 0.8), 
              linewidth = 0.2, alpha = 0.4) +
  geom_point(data = fig_week1_medians,
             aes(y = median_crps, colour = dataset, shape = dataset,
                 group = interaction(dataset, proj_dow)),
             position = position_dodge(width = 0.8), size = 1.5) +
  facet_wrap(~ state, nrow = 2) +
  scale_fill_manual(name = NULL, values = dataset_fill) +
  scale_colour_manual(name = NULL, values = dataset_cols) +
  scale_shape_manual(name = NULL, values = c("Reconstructed" = 16,
                                             "Reported" = 1)) +
  scale_linetype_manual(name = NULL, values = dataset_line) +
  labs(x = "Day of Projection", y = "log10(CRPS)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 11, margin = margin(r = 15)),
    axis.title.x = element_text(size = 11, margin = margin(t = 15)),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.3, "lines")
  )

ggsave("figures/Fig5.tif", fig5, width = 11, height = 7,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

###############################################################################
## Supplementary figure 3 (all phases faceted by forecast weeks 1-4)
violin_phase_subset <- violin10_jointlyr_phase %>%
  mutate(week_label = paste("Forecast Week", forecast_week))

panel_ref_s3 <- violin10_jointlyr_median %>%
  group_by(outbreak_phase, forecast_week) %>%
  summarise(week_state_median = median(median_crps), .groups = "drop") %>%
  mutate(week_label = paste("Forecast Week", forecast_week))

s3_fig <- ggplot(violin_phase_subset,
                 aes(x = proj_dow, y = log10_crps, fill = dataset)) +
  geom_hline(data = panel_ref_s3, aes(yintercept = week_state_median),
             linetype = "dashed", colour = "black",
             linewidth = 0.4, alpha = 0.7) +
  geom_violin(aes(linetype = dataset, group = interaction(dataset, proj_dow)),
              position = position_dodge(width = 0.8),
              linewidth = 0.2, alpha = 0.4) +
  geom_point(data = violin10_jointlyr_median %>%
               mutate(week_label = paste("Forecast Week", forecast_week)),
             aes(y = median_crps, colour = dataset, shape = dataset,
                 group = interaction(dataset, proj_dow)),
             position = position_dodge(width = 0.8), size = 1.5) +
  facet_grid(week_label ~ outbreak_phase, scales = "free_y") +
  scale_fill_manual(name = NULL, values = dataset_fill) +
  scale_colour_manual(name = NULL, values = dataset_cols) +
  scale_shape_manual(name = NULL, values = c("Reconstructed" = 16,
                                             "Reported" = 1)) +
  scale_linetype_manual(name = NULL, values = dataset_line) +
  labs(x = "Day of Projection", y = "log10(CRPS)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.3, "lines")
  )

ggsave("figures/S3_fig.tif", s3_fig, width = 13, height = 9,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

###############################################################################
# Main paper figure 6 (phases by forecast week 1 only)
violin_phase_week1 <- violin10_jointlyr_phase %>%
  filter(forecast_week == 1)

panel_ref_week1 <- violin10_jointlyr_median %>%
  filter(forecast_week == 1) %>%
  group_by(outbreak_phase) %>%
  summarise(week_state_median = median(median_crps), .groups = "drop")

median_points_week1 <- violin10_jointlyr_median %>%
  filter(forecast_week == 1)

fig_6 <- ggplot(violin_phase_week1,
                aes(x = proj_dow, y = log10_crps, fill = dataset)) +
  geom_hline(data = panel_ref_week1,
             aes(yintercept = week_state_median),
             linetype = "dashed", colour = "black",
             linewidth = 0.4, alpha = 0.7) +
  geom_violin(aes(linetype = dataset, group = interaction(dataset, proj_dow)),
              position = position_dodge(width = 0.8),
              linewidth = 0.2, alpha = 0.4) +
  geom_point(data = median_points_week1,
             aes(y = median_crps, colour = dataset, shape = dataset, 
                 group = interaction(dataset, proj_dow)),
             position = position_dodge(width = 0.8), size = 1.5) +
  facet_wrap(~ outbreak_phase, ncol = 6) +
  scale_fill_manual(name = NULL, values = dataset_fill) +
  scale_colour_manual(name = NULL, values = dataset_cols) +
  scale_shape_manual(name = NULL, values = c("Reconstructed" = 16,
                                             "Reported" = 1)) +
  scale_linetype_manual(name = NULL, values = dataset_line) +
  labs(x = "Day of Projection", y = "log10(CRPS)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    axis.title.y = element_text(size = 11, margin = margin(r = 15)),
    axis.title.x = element_text(size = 11, margin = margin(t = 15))
  )

ggsave("figures/Fig6.tif", fig_6, width = 11, height = 3.5,
       units = "in", dpi = 600, compression = "lzw", bg = "white")

