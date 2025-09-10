pars <- orderly_parameters(incidence_type = "cases")
list2env(pars, environment())

packages <- c("dplyr", "tidyr", "glue", "ggplot2", "gridExtra", "hrbrthemes",
              "ggforce", "gdata", "tibble", "scoringutils", "knitr", "lubridate",
              "cowplot", "purrr")

lapply(packages, require, character.only = TRUE)

orderly_artefact(
  "Manuscript figures",
  c(
    "figures/manuscript_figure_2.pdf",
    "figures/manuscript_figure_3.pdf",
    "figures/manuscript_figure_4.pdf",
    "figures/manuscript_figure_5.pdf"
  )
)

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
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-21' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_MON.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_MON.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-22' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_TUE.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_TUE.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-23' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_WED.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_WED.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-24' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_THU.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_THU.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-25' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_FRI.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_FRI.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-26' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_SAT.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_SAT.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-27' && parameter:reconstructed == TRUE) && parameter:incidence_type == '", incidence_type, "'"),
  c("recon_rti0_crps_SUN.rds" = "rti0_crps.rds",
    "recon_rti0_crps_weekly_SUN.rds" = "rti0_crps_weekly.rds"
  )
)
# CRPS values - reported
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-21' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_MON.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_MON.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-22' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_TUE.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_TUE.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-23' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_WED.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_WED.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-24' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_THU.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_THU.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-25' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_FRI.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_FRI.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-26' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_SAT.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_SAT.rds" = "rti0_crps_weekly.rds"
  )
)
orderly_dependency(
  "us_summarise_jointlyr",
  paste0("latest(parameter:week_ending == '2022-02-27' && parameter:reconstructed == FALSE) && parameter:incidence_type == '", incidence_type, "'"),
  c("rep_rti0_crps_SUN.rds" = "rti0_crps.rds",
    "rep_rti0_crps_weekly_SUN.rds" = "rti0_crps_weekly.rds"
  )
)

####################################

dir.create("figures")

all_files <- list.files(pattern = "\\.rds$", full.names = TRUE)
for (x in all_files) {
  obj_name <- gsub("\\.rds$", "", basename(x))
  assign(obj_name, readRDS(x))
}

states <- unique(rep_rti0_crps_WED$state)

## True reported and true reconstructed

# True reported incidence
rep_inputs <- list(
  MON = reported_model_input_MON,
  TUE = reported_model_input_TUE,
  WED = reported_model_input_WED,
  THU = reported_model_input_THU,
  FRI = reported_model_input_FRI,
  SAT = reported_model_input_SAT,
  SUN = reported_model_input_SUN
)

start_dates <- as.Date(c(
  MON = "2020-03-09",
  TUE = "2020-03-10",
  WED = "2020-03-11",
  THU = "2020-03-12",
  FRI = "2020-03-13",
  SAT = "2020-03-14",
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
  MON = reconstructed_model_input_MON,
  TUE = reconstructed_model_input_TUE,
  WED = reconstructed_model_input_WED,
  THU = reconstructed_model_input_THU,
  FRI = reconstructed_model_input_FRI,
  SAT = reconstructed_model_input_SAT,
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
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

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
## Percentage of cases overall

# Boxplot

dow_perc_inc_box <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  group_by(week) %>%
  mutate(total_weekly_cases = sum(incidence, na.rm = TRUE)) %>%
  group_by(week, day) %>%
  summarise(
    total_daily_cases = sum(incidence, na.rm = TRUE),
    total_weekly_cases = first(total_weekly_cases),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = (total_daily_cases / total_weekly_cases) * 100,
    state = "All States"
  )

ggplot(dow_perc_inc_box, aes(x = day, y = percentage, fill = day)) +
  geom_boxplot(outliers = FALSE, alpha = 0.6) +
  geom_hline(yintercept = 100 / 7, linetype = 2) +
  labs(
    x = "Day of the Week",
    y = "Percentage of Weekly Cases Reported"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 17),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.title.y = element_text(margin = margin(r = 10), size = 17),
        axis.title.x = element_text(margin = margin(t = 10), size = 17),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))


################################################################################
# By state

# Boxplots

dow_perc_inc_state <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  group_by(week, state) %>%
  mutate(total_cases = sum(incidence, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    percentage = (incidence / total_cases) * 100,
    day = factor(substr(day, 1, 3),
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

dow_perc_inc_state <- bind_rows(dow_perc_inc_box, dow_perc_inc_state) %>%
  mutate(
    day = factor(substr(day, 1, 3),
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

dow_perc_boxplot <- ggplot(dow_perc_inc_state,
                           aes(x = day, y = percentage, fill = day)) +
  geom_boxplot(outliers = FALSE, alpha = 0.6) +
  geom_hline(yintercept = 100 / 7, linetype = 2) +
  facet_wrap(~ state, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 17),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.title.y = element_text(margin = margin(r = 20), size = 19),
        axis.title.x = element_text(margin = margin(t = 20), size = 19),
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Day of the Week",
    y = "Percentage of Weekly Cases Reported"
  )

ggsave("figures/manuscript_figure_2_boxplot.pdf", dow_perc_boxplot,
       width = 17, height = 15)

################################################################################
# By outbreak phase

# Boxplot
dow_perc_inc_box <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  group_by(week) %>%
  mutate(total_weekly_cases = sum(incidence, na.rm = TRUE)) %>%
  group_by(week, day) %>%
  summarise(
    total_daily_cases = sum(incidence, na.rm = TRUE),
    total_weekly_cases = first(total_weekly_cases),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = (total_daily_cases / total_weekly_cases) * 100,
    outbreak_phase = "All Phases"
  )

dow_perc_inc_phase_box <- true_reported$SUN %>%
  anti_join(rm_weeks, by = "week") %>%
  mutate(
    outbreak_phase = case_when(
      dates >= as.Date("2020-03-01") & dates <= as.Date("2020-05-31") ~ "Early Pandemic",
      dates >= as.Date("2020-06-01") & dates <= as.Date("2020-09-30") ~ "Summer Surge",
      dates >= as.Date("2020-10-01") & dates <= as.Date("2020-12-31") ~ "Winter Surge",
      dates >= as.Date("2021-01-01") & dates <= as.Date("2021-06-30") ~ "Vaccine Rollout",
      dates >= as.Date("2021-07-01") & dates <= as.Date("2021-11-30") ~ "Delta Wave",
      dates >= as.Date("2021-12-01") & dates <= as.Date("2022-02-27") ~ "Omicron Wave",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(outbreak_phase)) %>%
  group_by(week, outbreak_phase) %>%
  mutate(total_weekly_cases = sum(incidence, na.rm = TRUE)) %>%
  group_by(week, day, outbreak_phase) %>%
  summarise(
    total_daily_cases = sum(incidence, na.rm = TRUE),
    total_weekly_cases = first(total_weekly_cases),
    percentage = (total_daily_cases / total_weekly_cases) * 100,
    .groups = "drop"
  )

dow_perc_inc_phase_box <- bind_rows(
  dow_perc_inc_box,
  dow_perc_inc_phase_box
) %>%
  mutate(
    day = factor(substr(day, 1, 3),
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    outbreak_phase = factor(
      outbreak_phase,
      levels = c("All Phases", "Early Pandemic", "Summer Surge", "Winter Surge",
                 "Vaccine Rollout", "Delta Wave", "Omicron Wave")
    )
  )

dow_perc_plot_phase_box <- ggplot(dow_perc_inc_phase_box, aes(x = day, y = percentage, fill = day)) +
  geom_boxplot(outliers = FALSE, alpha = 0.6) +
  geom_hline(yintercept = 100 / 7, linetype = 2) +
  facet_wrap(~ outbreak_phase, ncol = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = "Day of the Week",
    y = "Percentage of Weekly Cases Reported"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 15),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
    axis.title.y = element_text(margin = margin(r = 20), size = 15),
    axis.title.x = element_text(margin = margin(t = 20), size = 15),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  )

# Save plot
ggsave("figures/manuscript_figure_3_boxplot.pdf", dow_perc_plot_phase_box,
       width = 17, height = 8)


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
    anti_join(low_or_nonweekly %>% filter(proj_dow == dow), by = c("projection_week", "state")) %>%
    mutate(dataset = "Reported", proj_dow = dow, model = "jointlyr")
  
  recon_data <- get(paste0("recon_rti0_crps_weekly_", dow)) %>%
    anti_join(low_or_nonweekly %>% filter(proj_dow == dow), by = c("projection_week", "state")) %>%
    mutate(dataset = "Reconstructed", proj_dow = dow, model = "jointlyr")
  
  return(list(rep_data, recon_data))
}

jointlyr_crps_weekly <- purrr::map_dfr(proj_days, function(dow) {
  data <- remove_low_or_nonweekly(dow)
  bind_rows(data[[1]], data[[2]])
})

jointlyr_crps_weekly <- jointlyr_crps_weekly %>% select(-starts_with("true"))


################################################################################
## Violin plot of CRPS by state
 
violin_data <- jointlyr_crps_weekly %>%
  mutate(
    log10_crps = log10(crps),
    proj_dow = stringr::str_to_title(tolower(proj_dow)),
    proj_dow = factor(proj_dow,
                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

violin_allstates_jointlyr <- violin_data %>%
  mutate(state = "All States")

violin_jointlyr_combined <- bind_rows(violin_allstates_jointlyr, violin_data)

# Get medians
compute_medians <- function(data) {
  data %>%
    group_by(state, forecast_week, dataset, proj_dow) %>%
    summarise(
      median_crps = median(log10_crps),
      n = n(),
      sd_crps = sd(log10_crps),
      se_crps = sd_crps / sqrt(n),
      lower_ci = median_crps - qt(0.975, df = n - 1) * se_crps,
      upper_ci = median_crps + qt(0.975, df = n - 1) * se_crps,
      .groups = "drop"
    )
}

violin_allstates_median <- compute_medians(violin_allstates_jointlyr)
violin_groupedstates_median <- compute_medians(violin_data)
violin_medians_combined <- bind_rows(violin_allstates_median, violin_groupedstates_median)

week_line_data <- violin_medians_combined %>%
  group_by(forecast_week, state) %>%
  summarise(week_crps = median(median_crps), .groups = "drop")
 

violin_jointlyr_state_plot <- ggplot(
  violin_jointlyr_combined,
  aes(x = as.numeric(forecast_week), y = log10_crps)
  ) +
  geom_segment(
    data = week_line_data,
    aes(
      x = as.numeric(forecast_week) - 0.5,
      xend = as.numeric(forecast_week) + 0.5,
      y = week_crps,
      yend = week_crps
      ),
    col = "black"
    ) +
  geom_vline(
    data = week_line_data,
    aes(xintercept = as.numeric(forecast_week) + 0.5),
    linetype = "dashed",
    color = "black"
    ) +
  geom_violin(
    aes(fill = dataset,
        group = interaction(forecast_week, dataset, proj_dow)
    ),
    position = position_dodge(width = 1),
    alpha = 0.6,
    size = 0.5
    ) +
  geom_point(
    data = violin_medians_combined,
    aes(y = median_crps, shape = proj_dow, colour = dataset),
    size = 3.5,
    position = position_dodge(width = 1)
    ) +
  scale_color_manual(
    "",
    breaks = c("Reconstructed", "Reported"),
    values = c("#1f5b3a", "#363636")
  ) +
  scale_fill_manual(
    "",
    breaks = c("Reconstructed", "Reported"),
    values = c("#297a4d", "darkgrey")
  ) +
  scale_shape_manual(
    "",
    breaks = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    values = c(9, 8, 18, 16, 17, 15, 7)
  ) +
  scale_linetype_manual(
    "",
    breaks = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    values = c(rep(1, 7))
  ) +
  labs(
    x = "Forecast Week",
    y = "log10(CRPS)"
  ) +
  facet_wrap(~ state, ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, size = 17),
        axis.text.y = element_text(size = 17),
        strip.text = element_text(size = 21),
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        axis.title.y = element_text(margin = margin(r = 10), size = 19),
        axis.title.x = element_text(margin = margin(t = 10), size = 19),
        legend.title = element_blank(),
        legend.text = element_text(size = 21),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(expand = c(0, 0.01)) +
  scale_x_continuous(expand = c(0.005, 0)) +
  guides(shape = guide_legend(nrow = 1))
violin_jointlyr_state_plot

ggsave("figures/manuscript_figure_4.pdf", violin_jointlyr_state_plot,
       width = 14.5, height = 19)

################################################################################
# By Outbreak phase

# Violin plots by phase
violin10_jointlyr_phase <- jointlyr_crps_weekly %>%
  mutate(
    outbreak_phase = case_when(
      week_ending >= as.Date("2020-03-01") & week_ending <= as.Date("2020-05-31") ~ "Early Pandemic",
      week_ending >= as.Date("2020-06-01") & week_ending <= as.Date("2020-09-30") ~ "Summer Surge",
      week_ending >= as.Date("2020-10-01") & week_ending <= as.Date("2020-12-31") ~ "Winter Surge",
      week_ending >= as.Date("2021-01-01") & week_ending <= as.Date("2021-06-30") ~ "Vaccine Rollout",
      week_ending >= as.Date("2021-07-01") & week_ending <= as.Date("2021-11-30") ~ "Delta Wave",
      week_ending >= as.Date("2021-12-01") & week_ending <= as.Date("2022-02-27") ~ "Omicron Wave",
      TRUE ~ NA_character_
    ),
    outbreak_phase = factor(
      outbreak_phase,
      levels = c("Early Pandemic", "Summer Surge", "Winter Surge",
                 "Vaccine Rollout", "Delta Wave", "Omicron Wave")
    ),
    log10_crps = log10(crps),
    proj_dow = stringr::str_to_title(tolower(proj_dow)),
    proj_dow = factor(proj_dow, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )
 
violin10_jointlyr_median <- violin10_jointlyr_phase %>%
  group_by(outbreak_phase, forecast_week, dataset, model, proj_dow) %>%
  summarise(median_crps = median(log10_crps),
            n           = n(),
            sd_crps     = sd(log10_crps),
            se_crps     = sd_crps / sqrt(n),
            lower_ci    = median_crps - qt(0.975, df = n - 1) * se_crps,
            upper_ci    = median_crps + qt(0.975, df = n - 1) * se_crps
  ) %>%
  ungroup()

week_line_data <- violin10_jointlyr_median %>%
  group_by(forecast_week, outbreak_phase) %>%
  summarise(week_crps = median(median_crps))

violin10_jointlyr_phase_plot <- ggplot(
  violin10_jointlyr_phase,
  aes(x = as.numeric(forecast_week), y = log10_crps)
  ) +
  geom_segment(
    data = week_line_data,
    aes(
      x = as.numeric(forecast_week) - 0.5,
      xend = as.numeric(forecast_week) + 0.5,
      y = week_crps,
      yend = week_crps
      ),
    col = "black"
    ) +
  geom_vline(
    data = week_line_data,
    aes(xintercept = as.numeric(forecast_week) + 0.5),
    linetype = "dashed",
    color = "black"
    ) +
  geom_violin(
    aes(
      fill = dataset,
      group = interaction(forecast_week, dataset, proj_dow)
    ),
    position = position_dodge(width = 1),
    alpha = 0.6,
    size = 0.5
  ) +
  geom_point(
    data = violin10_jointlyr_median,
    aes(
      y = median_crps,
      shape = proj_dow,
      colour = dataset
      ),
    size = 3.5,
    position = position_dodge(width = 1)
    ) +
  labs(
    x = "Forecast Week",
    y = "log10(CRPS)"
  ) +
  facet_wrap(~ outbreak_phase, ncol = 2) +
  scale_color_manual(
    "",
    breaks = c("Reconstructed", "Reported"),
    values = c("#1f5b3a", "#363636")
  ) +
  scale_fill_manual(
    "",
    breaks = c("Reconstructed", "Reported"),
    values = c("#297a4d", "darkgrey")
  ) +
  scale_shape_manual(
    "",
    breaks = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    values = c(9, 8, 18, 16, 17, 15, 7)
  ) +
  scale_linetype_manual(
    "",
    breaks = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    values = c(1, 1, 1, 1, 1, 1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 17),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
    axis.title.y = element_text(margin = margin(r = 10), size = 16),
    axis.title.x = element_text(margin = margin(t = 10), size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 17),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    ) +
  scale_x_continuous(expand = c(0.005, 0)) +
  guides(shape = guide_legend(nrow = 1))
violin10_jointlyr_phase_plot

ggsave("figures/manuscript_figure_5.pdf", violin10_jointlyr_phase_plot,
       width = 14, height = 9)

