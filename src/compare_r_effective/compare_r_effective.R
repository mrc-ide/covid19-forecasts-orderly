## orderly::orderly_develop_start(use_draft = "newer")
## scales will multiply whatever you give it by a 100.
## whereas our values are already in (0, 100). We just want anice
## % sign
mypercent <- function(vec) scales::percent(vec/100, accuracy = 0.1)
dir.create("figures")
empirical <- readRDS("empirical_epidemic_phase.rds")
## phase is NA when SD is either 0 or NA
empirical <- na.omit(empirical)
##empirical$week_starting <- as.character(empirical$week_starting)

medium_term <- readRDS("collated_medium_term_phase.rds")
medium_term$day <- as.integer(medium_term$day)
medium_term <- medium_term[medium_term$day <= 28, ]
medium_term$week_starting <- as.Date(medium_term$model) - 1
medium_term$week_of_forecast <- case_when(
  medium_term$day <= 7 ~ "1-week ahead",
  medium_term$day > 7 & medium_term$day <= 14 ~ "2-weeks ahead",
  medium_term$day > 14 & medium_term$day <= 21 ~ "3-weeks ahead",
  medium_term$day > 21 & medium_term$day <= 28 ~ "4-weeks ahead",
)
### Assign phase to week rather than day, rule - the phase assigned
### to majority of days in the week is the weekly phase.
weekly_phase <- split(
  medium_term,
  list(medium_term$country, medium_term$week_starting,
       medium_term$week_of_forecast)
) %>% map_dfr(function(x) {
  freq <- tabyl(x$phase)
  if (nrow(freq) > 1) {
    message("More than 1 phase detected in ", x$country[1],
            " week ", x$week_starting[1])
  }
  most_freq <- which.max(freq$n)
  ## Return just the first row without the day, and the most
  ## frequent phase
  x$phase <- freq[[1]][most_freq]
  x[1, ]
})

## Now compare weekly phase with empirical phase
compare_phase <- left_join(
  weekly_phase, empirical,
  by = c("country", "week_starting"),
  suffix = c("_eff", "_empirical")
)
compare_phase <- distinct(compare_phase)
compare_phase <- na.omit(compare_phase)

########### Total number of country weeks for medium term
########### That is, n_countries X n_weeks
########### 82 * 2210 = 181220
country_weeks <- group_by(medium_term, country) %>%
  summarise(nweeks = length(unique(model))) %>%
  ungroup() %>%
  arrange(nweeks)

######################################################################
####### Option 1: Compare the phase assigned #########################
######################################################################
x <- select(
  compare_phase, week_of_forecast, phase_eff, phase_empirical
)

## Not counting indeterminate
wellclassified <- filter(x, phase_eff != "indeterminate")
wellclassified$overall_eff <- case_when(
  wellclassified$phase_eff %in% c("definitely growing", "likely growing") ~ "growing",
  wellclassified$phase_eff %in% c("definitely decreasing", "likely decreasing") ~ "not growing",
  )

wellclassified$overall_weekly <- case_when(
  wellclassified$phase_empirical %in% c("definitely growing", "likely growing") ~ "growing",
  wellclassified$phase_empirical %in% c("definitely decreasing", "likely decreasing") ~ "not growing",
)

ncorrect <- filter(wellclassified, phase_eff != "indeterminate") %>%
  filter(overall_eff == overall_weekly) %>%
  count(week_of_forecast, name = "n_correct")

misclassified <- filter(x, phase_empirical != phase_eff)

misclassified <- tabyl(
  misclassified, phase_empirical, phase_eff, week_of_forecast
) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  bind_rows(.id = "week_of_forecast")

misclassified <- select(
  misclassified, week_of_forecast, phase_empirical, `likely decreasing`,
  `definitely decreasing`, `likely stable`, `likely growing`,
  `definitely growing`
)

misclassified <- split(misclassified, misclassified$week_of_forecast) %>%
  map_dfr(function(x) {
    x[match(colnames(x)[c(-1, -2)], x$phase_empirical), ]
  })

saveRDS(misclassified, 'phase_misclassified.rds')

## opposite trend
opposite <- filter(x, phase_empirical != phase_eff)
opp_idx1 <- which(
  opposite$phase_eff %in% c('likely growing', 'definitely growing') &
  opposite$phase_empirical %in% c('likely decreasing', 'definitely decreasing')
)

opp_idx2 <- which(
  opposite$phase_eff %in% c('likely decreasing', 'definitely decreasing') &
  opposite$phase_empirical %in% c('likely growing', 'definitely growing')
)

## Summary across all weeks for which we have medium-term
## forecasts
y <- tabyl(x, phase_eff, phase_empirical) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

saveRDS(y, 'phase_eff_weekly_overall.rds')

out <- tabyl(x, phase_empirical, phase_eff, week_of_forecast) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>%
  bind_rows(.id = "Week of forecast")

saveRDS(out, 'phase_eff_weekly_week_of_forecast.rds')

out <- select(out, -Total)
out <- gather(out, phase_eff, label, `definitely decreasing`:`likely stable`)
out <- tidyr::separate(out, label, into = c("label", "total"), sep = "%")
out$val <- readr::parse_number(out$label)
out <- filter(out, phase_empirical != 'Total', phase_eff != 'Total')

out$phase_empirical <- factor(
  out$phase_empirical,
  levels = c("likely stable", "likely decreasing", "definitely decreasing",
             "likely growing","definitely growing",
             "indeterminate"),
  ordered = TRUE
)

out$phase_eff <- factor(
  out$phase_eff,
  levels = c("likely stable", "likely decreasing", "definitely decreasing",
             "likely growing","definitely growing",
             "indeterminate"),
  ordered = TRUE
)
out$perc_label <- mypercent(as.numeric(out$label))
out$perc_label <- stringr::str_remove_all(out$perc_label, "%")

p <- ggplot(
  out, aes(phase_empirical, phase_eff, fill = val),
  alpha = 0.7
) +
  geom_tile(width = 0.9, height = 0.9) +
  geom_text(
    aes(phase_empirical, phase_eff, label = perc_label),
    size = 8 /.pt
  ) +
  facet_wrap(~ `Week of forecast`, nrow = 2) +
  scale_fill_gradient(
    low = "#e5f2e5", high = "#66b266",
    labels = c("0.0%", "25.0%", "50.0%"),
    breaks = c(0, 25, 50),
    limits = c(0, 50),
    name = "% agreement",
    guide = guide_colorbar(title.hjust = 0.5, title.vjust = 0.95)
  ) +
  scale_x_discrete(
    breaks = c("likely stable", "likely decreasing", "definitely decreasing",
             "likely growing","definitely growing"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  scale_y_discrete(
    breaks = c("likely stable", "likely decreasing", "definitely decreasing",
             "likely growing","definitely growing",
             "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  xlab("Empirical epidemic phase (retrospective)") +
  ylab(expression(paste("Epidemic phase using ", R^S))) +
  theme_minimal() +
  theme(
    text = element_text(family = "CMU Sans Serif"),
    legend.position = "top", legend.title = element_text(size = 14),
    axis.text.x = element_text(
      angle = 90, size = 14, hjust = 0.95,vjust = 0.2
    ),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

save_multiple(p, "figures/percentage_phase_agree")
