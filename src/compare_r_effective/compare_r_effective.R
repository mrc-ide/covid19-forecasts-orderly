## orderly::orderly_develop_start(use_draft = "newer")
dir.create("figures")

reff_qntls <- readRDS("reff_qntls.rds")
reff_qntls <- rincewind::assign_epidemic_phase(reff_qntls)
## We assume transmissibility remains constant for 1 week.
## Therefore, for each country, for each week, repeat the rows for 7
## days and increment dates
weekly_rt <- readRDS("weekly_rt.rds")
weekly_rt <- na.omit(weekly_rt)


######################################################################
######### Compare phase assigned by the two Rts ######################
######################################################################
## There will be instances (countries and weeks) where we will have
## Rt from medium-term forecasts, but not from weekly Rt
## Therefore it is better to join weekly_rt to reff_qntls
## rather than the other way around
compare_phase <- left_join(
  weekly_rt, reff_qntls, by = c("country", "date"),
  suffix = c("_weekly", "_eff")
)
compare_phase <- distinct(compare_phase)
## The only NAs should now be from the first three weeks
## for which we did not produce medium-term forecasts.
## x <- compare_phase[! complete.cases(compare_phase), ]
## unique(x$forecast_date)
## [1] "2020-03-22" "2020-03-08" "2020-03-15"]
## Therefore we can safely omit these
compare_phase <- na.omit(compare_phase)

compare_phase$week_of_forecast <- case_when(
  compare_phase$day <= 7 ~ "Week 1",
  7 < compare_phase$day & compare_phase$day <= 14 ~ "Week 2",
  14 < compare_phase$day & compare_phase$day <= 21 ~ "Week 3",
  21 < compare_phase$day  ~ "Week 4"
)
######################################################################
########### Total number of country weeks
########### That is, n_countries X n_weeks
########### 78 * 2177 = 169806
country_weeks <- group_by(weekly_rt, country) %>%
  summarise(nweeks = length(unique(forecast_date))) %>%
  ungroup() %>%
  arrange(nweeks)

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

## Summary across all weeks for which we have medium-term
## forecasts
y <- tabyl(x, phase_eff, phase_weekly) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


misclassified <- filter(x, phase_weekly != phase_eff)

misclassified <- tabyl(
  misclassified, phase_eff, phase_weekly
) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

### stargazer::stargazer(misclassified, summary=FALSE, rownames = FALSE)

out <- gather(out, phase_eff, label, decline:unclear)
out$val <- readr::parse_number(out$label)

p <- ggplot(out, aes(phase_weekly, phase_eff, fill = val)) +
  geom_tile(width = 0.8, height = 0.8) +
  geom_text(
    aes(phase_weekly, phase_eff, label = label),
    size = 7 /.pt
  ) +
  facet_wrap(~ `Week of forecast`, nrow = 2) +
  scale_fill_distiller(
    palette = "Greens", direction = 1,
    labels = c("0.00%", "50.00%", "100.00%"),
    breaks = c(0, 50, 100),
    limits = c(0, 100)
  ) +
  scale_x_discrete(
    breaks = c("decline", "growing", "stable/growing slowly",
               "unclear"),
    labels = c("Declining", "Growing", "Stable \n growing slowly",
               "Unclear")
  ) +
  scale_y_discrete(
    breaks = c("decline", "growing", "stable/growing slowly",
               "unclear"),
    labels = c("Declining", "Growing", "Stable \n growing slowly",
               "Unclear")
  ) +
  xlab(expression(paste("Epidemic phase using ", R[t]))) +
  ylab(expression(paste("Epidemic phase using ", R^S))) +
  theme_minimal() +
  theme(
    legend.position = "top", legend.title = element_blank()
  )

save_multiple(p, "figures/percentage_phase_agree.tiff")


## phase_eff is estimated on a daily scale. Before aggregating it to
## a weekly metric, check if there are instances where it different
## within a week
## group_by(compare_phase, forecast_week, country, week_of_forecast) %>%
##   summarise(n = length(unique(phase_eff))) %>%
##   filter(n > 1) %>%
##   arrange(desc(n))
## A maximum of 2 different phases have been assigned within a week
out <- tabyl(x, phase_weekly, phase_eff, day) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  bind_rows(.id = "day")

out <- gather(out, phase_eff, label, decline:unclear)
out$val <- readr::parse_number(out$label)

out$week_of_forecast <- case_when(
  out$day <= 7 ~ "Week 1",
  7 < out$day & out$day <= 14 ~ "Week 2",
  14 < out$day & out$day <= 21 ~ "Week 3",
  21 < out$day  ~ "Week 4"
)

out$day <- factor(out$day, levels = 1:28, ordered = TRUE)

saveRDS(out, "phase_compare_daily.rds")

## scales will multiply whatever you give it by a 100.
## whereas our values are already in (0, 100). We just want anice
## % sign
mypercent <- function(vec) scales::percent(vec/100, accuracy = 0.1)

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

plots <- split(out, out$phase_weekly) %>%
  imap(function(y, phase) daily_phase_compare(y, phase))

## Top left
unclear <- plots[["unclear"]] +
  scale_y_discrete(
    breaks = c("decline", "growing", "stable/growing slowly",
               "unclear"),
    labels = c("Declining", "Growing", "Stable \n growing slowly",
               "Unclear")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  ) +
  ggtitle(
    label = NULL,
    subtitle = expression(
      paste("Epidemic phase using ", R[t], ": Unclear")
    )
  )
## Top right
growing <- plots[["growing"]] +
  theme_minimal() +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank()
    ) + ggtitle(label = NULL, subtitle = "Growing")

## Bottom left
stable <- plots[["stable/growing slowly"]] +
  theme_minimal() +
  scale_y_discrete(
    breaks = c("decline", "growing", "stable/growing slowly",
               "unclear"),
    labels = c("Declining", "Growing", "Stable \n growing slowly",
               "Unclear")
  ) +
  theme(axis.text.x = element_text(size = 6, angle = 90)) +
  ggtitle(label = NULL, subtitle = "Stable/growing slowly")

## Bottom right
decline <- plots[["decline"]] +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank()
  ) +
  theme(
    axis.text.x = element_text(size = 6, angle = 90)
  ) +
  ggtitle(label = NULL, subtitle = "Declining")

final <- unclear + growing + stable + decline +
  ## Horizontal legend is placed *below* the subtitles for some reason
  plot_layout(ncol = 2, nrow = 2, byrow = TRUE, guides = "collect") &
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

save_multiple(with_xlabel, "figures/si_compare_phase_daily.tiff")

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

## knitr::kable(out50)
## week_of_forecast |FALSE  |TRUE   |
## |:----------------|:------|:------|
## |Week 1           |1.80%  |98.20% |
## |Week 2           |33.01% |66.99% |
## |Week 3           |38.80% |61.20% |
## |Week 4           |42.68% |57.32% |

out95 <- tabyl(x, week_of_forecast, overlaps95) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2)

## knitr::kable(out95)
## |week_of_forecast |FALSE  |TRUE   |
## |:----------------|:------|:------|
## |Week 1           |0.28%  |99.72% |
## |Week 2           |4.06%  |95.94% |
## |Week 3           |7.44%  |92.56% |
## |Week 4           |10.36% |89.64% |

overlap <- left_join(
  out50, out95, by = "week_of_forecast", suffix = c('_50', '_95')
)
saveRDS(overlap, "overlap.rds")
