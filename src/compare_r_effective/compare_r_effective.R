## orderly::orderly_develop_start(use_draft = "newer")
## scales will multiply whatever you give it by a 100.
## whereas our values are already in (0, 100). We just want anice
## % sign
mypercent <- function(vec) scales::percent(vec/100, accuracy = 0.01)


dir.create("figures")
empirical <- readRDS("empirical_epidemic_phase.rds")
## phase is NA when SD is either 0 or NA
empirical <- na.omit(empirical)
##empirical$week_starting <- as.character(empirical$week_starting)

medium_term <- readRDS("collated_medium_term_phase.rds")
medium_term$day <- as.integer(medium_term$day)
medium_term <- medium_term[medium_term$day <= 28, ]
medium_term$week_starting <- as.Date(medium_term$model) - 1
## How often does the phase vary within a week?
medium_term$week_of_forecast <- case_when(
  medium_term$day <= 7 ~ "1-week ahead",
  7 < medium_term$day & medium_term$day <= 14 ~ "2-weeks ahead",
  14 < medium_term$day & medium_term$day <= 21 ~ "3-weeks ahead",
  21 < medium_term$day & medium_term$day <= 28  ~ "4-weeks ahead",
  28 < medium_term$day & medium_term$day <= 35  ~ "5-weeks ahead",
  35 < medium_term$day & medium_term$day <= 42  ~ "6-weeks ahead"
)

x <- group_by(medium_term, model, country, week_of_forecast) %>%
  summarise(ndiff = length(unique(phase))) %>%
  ungroup()

compare_phase <- left_join(
  medium_term, empirical,
  by = c("country", "week_starting"),
  suffix = c("_eff", "_empirical")
)
compare_phase <- distinct(compare_phase)
## The only NAs should now be from the first three weeks
## for which we did not produce medium-term forecasts.
## x <- compare_phase[! complete.cases(compare_phase), ]
## unique(x$week_starting)
## [1] "2020-03-22" "2020-03-08" "2020-03-15"]
## Therefore we can safely omit these
compare_phase <- na.omit(compare_phase)


compare_phase$week_of_forecast <- case_when(
  compare_phase$day <= 7 ~ "1-week ahead",
  7 < compare_phase$day & compare_phase$day <= 14 ~ "2-weeks ahead",
  14 < compare_phase$day & compare_phase$day <= 21 ~ "3-weeks ahead",
  21 < compare_phase$day & compare_phase$day <= 28  ~ "4-weeks ahead",
  28 < compare_phase$day & compare_phase$day <= 35  ~ "5-weeks ahead",
  35 < compare_phase$day & compare_phase$day <= 42  ~ "6-weeks ahead"
)

x <- tabyl(compare_phase, phase_empirical, phase_eff, day) %>%
  adorn_percentages("row") %>%
  bind_rows(.id = "day_of_forecast")

x <- select(x, day_of_forecast, phase_empirical, `likely stable`,`likely decreasing` ,`definitely decreasing`, `likely growing` ,`definitely growing`, `indeterminate`)
xtall <- gather(x, phase_eff, val, -day_of_forecast, -phase_empirical)
xtall$phase_empirical <-
  factor(
    xtall$phase_empirical,
    levels = c(
      "likely stable",
      "likely decreasing",
      "definitely decreasing",
      "likely growing",
      "definitely growing"
    ), ordered = TRUE
  )

xtall$phase_eff <-
  factor(
    xtall$phase_eff,
    levels = c(
      "likely stable",
      "likely decreasing",
      "definitely decreasing",
      "likely growing",
      "definitely growing",
      "indeterminate"
    ), ordered = TRUE
  )

ggplot(xtall) +
  geom_tile(
    aes(phase_empirical, phase_eff, fill = val),
    width = 0.9, height = 0.9
  ) +
  facet_wrap(~day_of_forecast, ncol = 7) +
  scale_fill_distiller(
    palette = "RdYlBu", limits = c(0, 1)
  ) +
  theme(axis.text.x = element_text(angle = 90))


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
  compare_phase, day, week_of_forecast, phase_eff, phase_empirical
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


## When the
## phase definitions using $\rtsat{t}$ and $\rtcurr{t}$ (reproduction
## number estimated weekly for short-term forecasts) were different,
## the medium-term were most frequently
## misclassified as stable/growing slowly (46.8\%) or unclear
## (18.4\%).
misclassified <- filter(x, phase_empirical != phase_eff)
misclassified <- tabyl(
  misclassified, phase_empirical, phase_eff
) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
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
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  bind_rows(.id = "Week of forecast")

saveRDS(out, 'phase_eff_weekly_week_of_forecast.rds')
out <- select(out, -Total)
out <- gather(out, phase_eff, label, `definitely decreasing`:`likely growing`)
out <- tidyr::separate(out, label, into = c("label", "total"), sep = "%")
out$val <- readr::parse_number(out$label)
out <- filter(out, phase_empirical != 'Total', phase_eff != 'Total')

out$phase_empirical <- factor(
  out$phase_empirical,
  levels = c("definitely growing",  "likely growing",
             "definitely decreasing", "likely decreasing",
             "indeterminate"),
  ordered = TRUE
)

out$phase_eff <- factor(
  out$phase_eff,
  levels = c("definitely growing",  "likely growing",
             "definitely decreasing", "likely decreasing",
             "indeterminate"),
  ordered = TRUE
)
out$perc_label <- mypercent(as.numeric(out$label))

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
    labels = c("0.00%", "50.00%", "100%"),
    breaks = c(0, 50, 100),
    limits = c(0, 100),
    name = "% agreement",
    guide = guide_colorbar(title.hjust = 0.5, title.vjust = 0.95)
  ) +
  scale_x_discrete(
    breaks = c("definitely growing",  "likely growing",
               "definitely decreasing", "likely decreasing",
               "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  scale_y_discrete(
    breaks = c("definitely growing",  "likely growing",
               "definitely decreasing", "likely decreasing",
               "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  xlab(expression(paste("Epidemic phase using ", R^{curr}))) +
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

## phase_eff is estimated on a daily scale. Before aggregating it to
## a weekly metric, check if there are instances where it different
## within a week
## group_by(compare_phase, forecast_week, country, week_of_forecast) %>%
##   summarise(n = length(unique(phase_eff))) %>%
##   filter(n > 1) %>%
##   arrange(desc(n))
## A maximum of 2 different phases have been assigned within a week
out <- tabyl(x, phase_empirical, phase_eff, day) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  bind_rows(.id = "day")

out <- gather(out, phase_eff, label, `definitely decreasing`:`likely growing`)
out$val <- readr::parse_number(out$label)

out$week_of_forecast <- case_when(
  out$day <= 7 ~ "Week 1",
  7 < out$day & out$day <= 14 ~ "Week 2",
  14 < out$day & out$day <= 21 ~ "Week 3",
  21 < out$day  ~ "Week 4"
)

out$day <- factor(out$day, levels = 1:28, ordered = TRUE)

saveRDS(out, "phase_compare_daily.rds")


daily_phase_compare <- function(x, phase) {
  p <- ggplot() +
    geom_tile(
      data = x[x$phase_eff == phase, ], aes(day, phase_eff, fill = val),
      width = 0.5, height = 0.5
  ) +
  scale_fill_distiller(
    palette = "Greens", direction = 1, breaks = c(0, 50, 100),
    limits = c(0, 100), label = mypercent
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = x[x$phase_eff != phase, ], aes(day, phase_eff, fill = val),
      width = 0.5, height = 0.5
  ) +
  scale_fill_distiller(
    palette = "OrRd", direction = 1, breaks = c(0, 50, 100),
    limits = c(0, 100), label = mypercent
  )
  p
}

plots <- split(out, out$phase_empirical) %>%
  imap(function(y, phase) daily_phase_compare(y, phase))

## Top left
unclear <- plots[["indeterminate"]] +
  scale_y_discrete(
    breaks = c("definitely growing",  "likely growing",
               "definitely decreasing", "likely decreasing",
               "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  ) +
  ggtitle(
    label = NULL,
    subtitle = expression(
      paste("Epidemic phase using ", R[t], ": Indeterminate")
    )
  )
## Top right
growing1 <- plots[["definitely growing"]] +
  theme_minimal() +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank()
  ) +
  ggtitle(label = NULL, subtitle = "Definitely growing")

growing2 <- plots[["likely growing"]] +
  theme_minimal() +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank()
    ) + ggtitle(label = NULL, subtitle = "Likely growing")

## Bottom left
decline1 <- plots[["definitely decreasing"]] +
  theme_minimal() +
  scale_y_discrete(
    breaks = c("definitely growing",  "likely growing",
               "definitely decreasing", "likely decreasing",
               "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  theme(axis.text.x = element_text(size = 6, angle = 90)) +
  ggtitle(label = NULL, subtitle = "Definitely decreasing")


decline2<- plots[["likely decreasing"]] +
  theme_minimal() +
  scale_y_discrete(
    breaks = c("definitely growing",  "likely growing",
               "definitely decreasing", "likely decreasing",
               "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  theme(axis.text.x = element_text(size = 6, angle = 90)) +
  ggtitle(label = NULL, subtitle = "Likely decreasing")


## Bottom right

final <- unclear + growing1 + growing2 +
  decline1 + decline2 +
  ## Horizontal legend is placed *below* the subtitles for some reason
  plot_layout(ncol = 2, nrow = 3, byrow = TRUE, guides = "collect") &
  theme(
    legend.title = element_blank(),
    axis.title = element_blank(),
    plot.subtitle = element_text(size = 8)
  )

label1 <- textGrob(
  expression(paste("Epidemic phase using ", R^S)), rot = 90,
  gp = gpar(fontsize = 8)
)

label2 <- textGrob("Day of forecast", gp = gpar(fontsize = 8))

with_ylabel <- wrap_elements(label1) + wrap_elements(final) +
  plot_layout(ncol = 2, widths = c(0.03, 1))

with_xlabel <- wrap_elements(with_ylabel) + wrap_elements(label2) +
  plot_layout(ncol = 1, heights = c(1, 0.03))

save_multiple(with_xlabel, "figures/si_compare_phase_daily")
