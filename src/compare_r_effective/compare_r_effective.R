## orderly::orderly_develop_start(use_draft = "newer",
## parameters = list(week_ending = "2020-10-04"))
dir.create("figures")

reff_qntls <- readRDS("weighted_per_country_reff_qntls.rds")
## Add day
reff_qntls <- map_dfr(reff_qntls, function(df) {
  df$day <- seq_len(nrow(df))
  df
}, .id = "country")


reff_qntls <- rincewind::assign_epidemic_phase(reff_qntls)
## We assume transmissibility remains constant for 1 week.
## Therefore, for each country, for each week, repeat the rows for 7
## days and increment dates
weekly_rt <- readRDS("weekly_rt.rds")
weekly_rt <- na.omit(weekly_rt)


######################################################################
######### Compare phase assigned by the two Rts ######################
######################################################################
reff_qntls$date <- as.Date(reff_qntls$date)
compare_phase <- dplyr::left_join(
  reff_qntls, weekly_rt,
  by = c("country", "date"),
  suffix = c("_eff", "_weekly")
  )

compare_phase$week_of_forecast <- case_when(
  compare_phase$day <= 7 ~ "Week 1",
  7 < compare_phase$day & compare_phase$day <= 14 ~ "Week 2",
  14 < compare_phase$day & compare_phase$day <= 21 ~ "Week 3",
  21 < compare_phase$day  ~ "Week 4"
)

######################################################################
####### Option 1: Compare the phase assigned #########################
######################################################################
x <- select(
  compare_phase, day, week_of_forecast, phase_eff, phase_weekly
)

out <- tabyl(x, phase_weekly, phase_eff, week_of_forecast) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  bind_rows(.id = "Week of forecast")

out <- gather(out, phase_eff, label, decline:unclear)
out$val <- readr::parse_number(out$label)

ggplot(out, aes(phase_weekly, phase_eff, fill = val)) +
  geom_tile() +
  geom_text(aes(phase_weekly, phase_eff, label = label)) +
  facet_wrap(~ `Week of forecast`, nrow = 2) +
  scale_fill_distiller(palette = "Greens", direction = -1) +
  xlab("Epidemic phase (weekly Rt)") +
  ylab("Epidemic phase (Rs)") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

######################################################################
####### Option 2: Compare the overlap ################################
######################################################################
x <- select(
  compare_phase, day, week_of_forecast,
  `2.5%_eff`, `97.5%_eff`, `25%_eff`, `75%_eff`,
  `2.5%_weekly`, `97.5%_weekly`, `25%_weekly`, `75%_weekly`
)

x$overlaps95 <- pmap_lgl(
  select(x, `2.5%_eff`, `97.5%_eff`, `2.5%_weekly`, `97.5%_weekly`),
  function(`2.5%_eff`, `97.5%_eff`, `2.5%_weekly`, `97.5%_weekly`) {
    x1 <- c(`2.5%_eff`, `97.5%_eff`)
    x2 <- c(`2.5%_weekly`, `97.5%_weekly`)
    overlaps(x1, x2, 2)
  }
)

x$overlaps50 <- pmap_lgl(
  select(x, `25%_eff`, `75%_eff`, `25%_weekly`, `75%_weekly`),
  function(`25%_eff`, `75%_eff`, `25%_weekly`, `75%_weekly`) {
    x1 <- c(`25%_eff`, `75%_eff`)
    x2 <- c(`25%_weekly`, `75%_weekly`)
    overlaps(x1, x2, 2)
  }
)

out50 <- tabyl(x, week_of_forecast, overlaps50) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2)

## |week_of_forecast |FALSE  |TRUE   |
## |:----------------|:------|:------|
## |Week 1           |4.29%  |95.71% |
## |Week 2           |27.35% |72.65% |
## |Week 3           |41.43% |58.57% |
## |Week 4           |40.00% |60.00% |

out95 <- tabyl(x, week_of_forecast, overlaps95) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2)

## |week_of_forecast |FALSE |TRUE    |
## |:----------------|:-----|:-------|
## |Week 1           |0.00% |100.00% |
## |Week 2           |2.86% |97.14%  |
## |Week 3           |4.29% |95.71%  |
## |Week 4           |7.14% |92.86%  |

######################################################################
####### Option 3: Compare correlation ################################
######################################################################
x <- split(compare_phase, compare_phase$week_of_forecast) %>%
  map_dfr(function(df) {
    Hmisc::rcorr(df$`50%_weekly`, df$`50%_eff`) %>% broom::tidy()
  }, .id = "week_of_forecast"
  )


## |week_of_forecast |column1 |column2 |  estimate|   n| p.value|
## |:----------------|:-------|:-------|---------:|---:|-------:|
## |Week 1           |y       |x       | 0.8561548| 490|       0|
## |Week 2           |y       |x       | 0.6358706| 490|       0|
## |Week 3           |y       |x       | 0.4090670| 490|       0|
## |Week 4           |y       |x       | 0.4094750| 490|       0|



