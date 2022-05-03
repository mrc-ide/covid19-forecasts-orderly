## orderly::orderly_develop_start(use_draft = "newer")
## scales will multiply whatever you give it by a 100.
## whereas our values are already in (0, 100). We just want anice
## % sign
mypercent <- function(vec) scales::percent(vec/100, accuracy = 0.01)
source("R/utils.R")
dir.create("figures")
######################################################################
######### Compare phase assigned by the two Rts ######################
######################################################################
## There will be instances (countries and weeks) where we will have
## Rt from medium-term forecasts, but not from weekly Rt
## Therefore it is better to join weekly_rt to reff_qntls
## rather than the other way around
short_term <- readRDS("collated_short_term_phase.rds")
short_term <- short_term[short_term$model_name == "ensemble", ]

y <- short_term[short_term$phase %in% c("likely stable", "indeterminate"), ]
p1 <- ggplot(y, aes(rt_cv)) +
  geom_histogram() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  xlab("Width of 95% CrI") +
  theme_bw()

ggsave("cri_width_hist.pdf", p1)

p2 <- ggplot(short_term, aes(less_than_1, rt_cv, col = phase)) +
  geom_point() +
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 1.1) +
  xlab("% samples less than 1") +
  ylab("Width of 95% CrI") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank())
ggsave("cri_width_lessthan1.pdf", p2)

## deaths <- readRDS("model_input.rds")
## deaths <- deaths[deaths$dates <= as.Date("2020-12-31"), ]

## x <- split(short_term, short_term$country)
## iwalk(
##   x, function(df, country) {
##     df$start <- as.Date(df$forecast_date) - 6
##     df$end <- as.Date(df$forecast_date)
##     y <- deaths[, c("dates", country)]
##     y[["incid"]] <- y[[country]]
##     ymax <- max(y$incid)
##     p <- ggplot() +
##       geom_rect(
##         data = df,
##         aes(xmin = start, xmax = end,
##             ymin = 0, ymax = ymax, fill = phase),
##         alpha = 0.3
##       ) +
##       geom_point(data = y, aes(dates, incid)) +
##       scale_x_date(
##         limits = c(as.Date("2020-03-01"), as.Date("2020-12-31"))
##       ) +
##       ylab("Daily Incidence") +
##       theme_minimal() +
##       theme(legend.position = "top",
##             legend.title = element_blank(),
##             axis.title.x = element_blank())

##     ggsave(glue("figures/{country}.pdf"), p)


##   }
## )



## Medium-term phase is define prospectively
medium_term <- readRDS("collated_medium_term_phase.rds")
compare_phase <- inner_join(
  short_term, medium_term, by = c("country", "phase_for_week"),
  suffix = c("_weekly", "_eff")
)
            ## > rows only in x  (  103)
            ## > rows only in y  (1,050)
            ## > matched rows     7,790    (includes duplicates)
            ## >                 =======
            ## > rows total       7,790
check1 <- anti_join(
  short_term, medium_term, by = c("country", "phase_for_week"),
  suffix = c("_weekly", "_eff")
)
## Medium-term phase only starts from 30 March
## Short-term from 23rd March. so there will be no matches where
## phase for week is 23 March-29 March 2020.
## The second reason for non-matching rows is that because short-term phase is
## defined retrospecively and medium-term prospectively
## we may not have the same countries included in the analysis.

######################################################################
########### Total number of country weeks for short term
########### That is, n_countries X n_weeks
########### 82 * 2210 = 181220
country_weeks <- group_by(short_term, country) %>%
  summarise(nweeks = length(unique(forecast_date))) %>%
  ungroup() %>%
  arrange(nweeks)

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
  compare_phase, day, week_of_forecast, phase_eff, phase_weekly
)

## The number of 1-, 2- , 3- and 4-week ahead forecasts is not the same
## because (a) for a given week we will make multiple (at most 4 forecasts),
## (b) but not exactly 4 becase the countries included in the analysis chane
## from one week to another.
## Across all countries and weeks for which we
## produced forecasts, the phase definition using the reproduction
## number estimates from
## medium-term forecasts (RtS ) was consistent with that using the
## estimates from the short-term forecasts (Rcurr)
## in xx.x% of country-weeks in 1-week ahead forecasts and in xx.x% t
## of country-weeks in 4 week ahead forecasts
nforecasts <- count(x, week_of_forecast, name = "n_forecasts")
wellclassified <- filter(x, phase_weekly == phase_eff)
ncorrect <- count(wellclassified, week_of_forecast, name = "n_correct")
wellclassified <- left_join(nforecasts, ncorrect)
wellclassified$prop <- wellclassified$n_correct / wellclassified$n_forecasts
saveRDS(wellclassified, 'phase_wellclassified.rds')
### stargazer::stargazer(misclassified, summary=FALSE, rownames = FALSE)

## Not counting indeterminate
wellclassified <- filter(x, phase_eff != "indeterminate")
wellclassified$overall_eff <- case_when(
  wellclassified$phase_eff %in% c("definitely growing", "likely growing") ~ "growing",
  wellclassified$phase_eff %in% c("definitely decreasing", "likely decreasing") ~ "not growing",
  )

wellclassified$overall_weekly <- case_when(
  wellclassified$phase_weekly %in% c("definitely growing", "likely growing") ~ "growing",
  wellclassified$phase_weekly %in% c("definitely decreasing", "likely decreasing") ~ "not growing",
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
misclassified <- filter(x, phase_weekly != phase_eff)
misclassified <- tabyl(
  misclassified, phase_weekly, phase_eff
) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
saveRDS(misclassified, 'phase_misclassified.rds')

## opposite trend
opposite <- filter(x, phase_weekly != phase_eff)
opp_idx1 <- which(
  opposite$phase_eff %in% c('likely growing', 'definitely growing') &
  opposite$phase_weekly %in% c('likely decreasing', 'definitely decreasing')
)

opp_idx2 <- which(
  opposite$phase_eff %in% c('likely decreasing', 'definitely decreasing') &
  opposite$phase_weekly %in% c('likely growing', 'definitely growing')
)

## Summary across all weeks for which we have medium-term
## forecasts
y <- tabyl(x, phase_eff, phase_weekly) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "all") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

saveRDS(y, 'phase_eff_weekly_overall.rds')

out <- tabyl(x, phase_weekly, phase_eff, week_of_forecast) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  bind_rows(.id = "Week of forecast")

saveRDS(out, 'phase_eff_weekly_week_of_forecast.rds')
out <- select(out, -Total)
out <- gather(out, phase_eff, label, `definitely decreasing`:`likely stable`)
out <- separate(out, label, into = c("label", "total"), sep = "%")
out$val <- parse_number(out$label)
out <- filter(out, phase_weekly != 'Total', phase_eff != 'Total')

out$phase_weekly <- factor(
  out$phase_weekly,
  levels = c("definitely growing",  "likely growing",
             "definitely decreasing", "likely decreasing",
             "likely stable",
             "indeterminate"),
  ordered = TRUE
)

out$phase_eff <- factor(
  out$phase_eff,
  levels = c("definitely growing",  "likely growing",
             "definitely decreasing", "likely decreasing",
             "likely stable",
             "indeterminate"),
  ordered = TRUE
)
out$perc_label <- mypercent(as.numeric(out$label))

p <- ggplot(
  out, aes(phase_weekly, phase_eff, fill = val),
  alpha = 0.7
) +
  geom_tile(width = 0.9, height = 0.9) +
  geom_text(
    aes(phase_weekly, phase_eff, label = label),
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
               "likely stable",
               "indeterminate"),
    labels = function(x) nice_country_name(x),
    drop = FALSE
  ) +
  scale_y_discrete(
    breaks = c("definitely growing",  "likely growing",
               "definitely decreasing", "likely decreasing",
               "likely stable",
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
