## orderly::orderly_develop_start(
## use_draft = FALSE, parameters = list(use_si = "si_2", strategy = "unwtd"))
######### Performance metrics
observed <- readRDS("model_input.rds")

main_text_countries <- c(
  "Brazil", "India", "Italy", "Mexico", "United_States_of_America"
)

labels <- c(
  "rel_mae" = "Relative mean error",
  "rel_mse" = "Relative mean squared error",
  "rel_sharpness" = "Relative sharpness",
  "bias" = "Bias",
  "prop_in_50" = "Proportion in 50% CrI",
  "prop_in_975" = "Proportion in 97.5% CrI",
  "empirical_p" = "Probability(obs|predictions)"
)

observed_tall <- tidyr::gather(observed, country, deaths, -dates)

observed_tall <- split(
  observed_tall, observed_tall$country
) %>%
  purrr::map_dfr(
    function(df) {
      df$deaths_scaled <- df$deaths / max(df$deaths)
      cum_deaths <- cumsum(df$deaths)
      idx <- which(cum_deaths >= 100)
      if (length(idx) == 0) {
        days_since_100_deaths <- rep(0, length(cum_deaths))
      } else {
        idx <- idx[1]
        days_since_100_deaths <- c(
          seq(to = -1, length.out = idx - 1, by = 1),
          seq(
            from = 1, length.out = length(cum_deaths) - idx + 1, by = 1
          )
        )
      }
      df$days_since_100_deaths <- days_since_100_deaths
      df
   }
)

weekly_incidence <- readRDS("weekly_incidence.rds")
weekly_incidence$forecast_date <- as.Date(weekly_incidence$week_starting)

##weekly_incidence$forecast_date[weekly_incidence$forecast_date == as.Date("2020-03-09")] <- as.Date("2020-03-08")

######################################################################
######################################################################
################## Unweighted Ensemble ###############################
######################################################################
######################################################################

unwtd_pred_error <- readr::read_csv("unwtd_pred_error.csv") %>%
  dplyr::filter(si == use_si)
unwtd_pred_error$country[unwtd_pred_error$country == "Czech_Republic"] <- "Czechia"
unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- rename(unwtd_pred_error, "forecast_date" = "model")
unwtd_pred_error_daily <- unwtd_pred_error
unwtd_pred_error <- dplyr::group_by(
  unwtd_pred_error, forecast_date, country, si
) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
dplyr::ungroup()

## This has one row for each quantile.
unweighted_rt_qntls <- readRDS("unweighted_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)
unweighted_rt_qntls$country[unweighted_rt_qntls$country == "Czech_Republic"] <- "Czechia"

unwtd_pred_error$forecast_date <- as.Date(unwtd_pred_error$forecast_date)
unwtd_pred_error <- dplyr::left_join(unwtd_pred_error, weekly_incidence)

unweighted_rt_qntls$forecast_date <- as.Date(unweighted_rt_qntls$forecast_date)
unwtd_pred_error <- dplyr::left_join(
  unwtd_pred_error, unweighted_rt_qntls
)

unwtd_pred_error_daily$forecast_date <- as.Date(unwtd_pred_error_daily$forecast_date)
unwtd_pred_error_daily <- dplyr::left_join(
  unwtd_pred_error_daily, unweighted_rt_qntls
)


w_use_strategy <- list(
  ##wtd_prev_week = wtd_prev_week_error,
  ##wtd_all_prev_weeks = wtd_all_prev_weeks_error,
  unwtd = unwtd_pred_error
)

d_use_strategy <- list(
  ##wtd_prev_week = wtd_prev_week_error_daily,
  ##wtd_all_prev_weeks = wtd_all_prev_weeks_error_daily,
  unwtd = unwtd_pred_error_daily
)

weekly <- w_use_strategy[[strategy]]
daily <- d_use_strategy[[strategy]]




#####################################################################
#####################################################################
########## Observed vs Median Predicitons Density ###################
#####################################################################
#####################################################################
x50_all <- select(unwtd_pred_error_daily, country, obs, median_pred)
x50_all <- distinct(x50_all)
x50_all <- filter(x50_all, obs >= 0)

##x50_all$log_obs <- log(x50_all$obs, 10)
##x50_all$log_median_pred <- log(x50_all$median_pred, 10)
##ymax <- max(x50_all$log_median_pred, x50_all$log_obs, na.rm = TRUE)
##ymax <- ceiling(ymax)

bin_start <- seq(0, 4000, by = 100)
bin_end  <- bin_start + 100
##bin_start <- c(-Inf, bin_start)
##bin_end <- c(0, bin_end)

x50_all$obs_category <- apply(
  x50_all,
  1,
  function(row) {
    idx <- map2_lgl(
      bin_start, bin_end, function(x, y) between(row[["obs"]], x, y)
    )
    idx <- Position(isTRUE, idx)
    glue::glue("[{bin_start[idx]}, {bin_end[idx]})")
  }
)


x50_all$pred_category <- apply(
  x50_all,
  1,
  function(row) {
    idx <- map2_lgl(
      bin_start, bin_end, function(x, y) between(row[["median_pred"]], x, y)
    )
    idx <- Position(isTRUE, idx)
    glue::glue("[{bin_start[idx]}, {bin_end[idx]})")
  }
)

x50_all$pred_category[x50_all$median_pred >= 4100] <- "[4100, Inf)"

categories <- glue::glue("[{bin_start}, {bin_end})")
categories <- c(categories, "[4100, Inf)")

y <- dplyr::count(x50_all, pred_category, obs_category)

normalised <- map_dfr(
  categories,
  function(category) {
    x <- y[y$obs_category == category, ]
    x$proportion <- x$n / sum(x$n)
    x
  }
)

normalised$pred_category <- factor(
  normalised$pred_category, levels = categories, ordered = TRUE
)

normalised$obs_category <- factor(
  normalised$obs_category, levels = categories, ordered = TRUE
)


pdensity <- ggplot(
  normalised, aes(obs_category, pred_category, fill = proportion)
) +
  geom_tile(width = 0.9, height = 0.9) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_distiller(
    palette = "YlOrRd", direction = 1,
    name = "Proportion in bin"
  ) +
  theme_minimal() +
  xlab("Observed daily deaths") +
  ylab("Median predictied daily deaths") +
  theme(
    legend.position = "top",
    ##legend.title = element_blank(),
    axis.text.x = element_text(angle = -90, hjust = 0),
    legend.key.width = unit(2, "lines")
  )

ggsave("obs_predicted_2d_density.png", pdensity)

normalised <- select(normalised, -n) %>%
  spread(pred_category, proportion, fill = 0)

readr::write_csv(normalised, "obs_predicted_2d_density.csv")



######################################################################
######################################################################
#######################  Daily Figures ###############################
######################################################################
######################################################################

pdaily <- ggplot(daily, aes(obs, rel_mae, col = phase)) +
  geom_point() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  xlab("(log) Deaths") +
  ylab("(log) Relative Error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("daily_obs_error_all_countries.png", pdaily)


pdaily <- ggplot() +
  geom_point(
    data = daily[daily$country %in% main_text_countries, ],
    aes(obs, rel_mae, col = phase)
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  xlab("(log) Deaths") +
  ylab("(log) Relative Error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("daily_obs_error_main_countries.png", pdaily)

######################################################################
######################################################################
#######################  Weekly Figures ##############################
######################################################################
######################################################################

## All countries, Relative mean error on log scale and weekly incidence
pall <- ggplot(
  weekly, aes(weekly_incid, rel_mae, col = phase)
) + geom_point() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  xlab("(log) Weekly Incidence") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_incid_all_countries.png", pall)

## Main text countries,
## Relative mean error on log scale and weekly incidence
pmain <- ggplot() +
  geom_point(
    data = weekly[weekly$country %in% main_text_countries, ],
    aes(weekly_incid, rel_mae, col = phase)
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +

  theme_minimal() +
  xlab("Weekly Incidence") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_incid_main_countries.png", pmain)

######################################################################


pcv_all <- ggplot(
  weekly, aes(weekly_cv, rel_mae, col = phase)
) + geom_point() +
    scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  xlab("(log) Weekly coefficient of variation") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_cv_all_countries.png", pcv_all)


pcv_main <- ggplot() +
  geom_point(
    data = weekly[weekly$country %in% main_text_countries, ],
    aes(weekly_cv, rel_mae, col = phase),
    size = 2
  ) +
    scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  xlab("(log) Weekly coefficient of variation") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_cv_main_countries.png", pcv_main)

######################################################################
######################################################################
######################################################################
########### Comparison with baseline error ###########################
######################################################################
######################################################################
######################################################################

null_error <- readRDS("null_model_error.rds")
x <- left_join(daily, null_error, by = c("date" = "dates", "country"))
weekly_compare <- group_by(x, forecast_date, country, si) %>%
  summarise(
    weekly_rel_err = mean(mae), weekly_null_err = mean(null_error)
  ) %>%
  ungroup()
weekly_compare$ratio <- weekly_compare$weekly_rel_err / weekly_compare$weekly_null_err

weekly_compare$forecast_date <- factor(weekly_compare$forecast_date)
x <- dplyr::count(weekly_compare, country)
countries <- x$country
weekly_compare$err_level <- ifelse(
  weekly_compare$ratio >= 1, "greater_than_1", "less_than_1"
)

x <- group_by(weekly_compare, country) %>%
  summarise(
    n_forecasts = n(), n_less_than_1 = sum(ratio < 1, na.rm = TRUE)
  ) %>% ungroup()

x$percent_less_than_1 <-  x$n_less_than_1 / x$n_forecasts

x <- dplyr::arrange(x, desc(percent_less_than_1))

weekly_compare <- left_join(weekly_compare, x)
weekly_compare$country <-  factor(
  weekly_compare$country, levels = x$country, ordered = TRUE
)

weekly_compare$country[weekly_compare$country == "Czech Republic"] <- "Czechia"

labels <- unique(weekly_compare$forecast_date)

weekly_compare$weekly_rel_err <- signif(weekly_compare$weekly_rel_err, 3)
weekly_compare$weekly_null_err <- signif(weekly_compare$weekly_null_err, 3)
weekly_compare$error_values <- glue(
  "{weekly_compare$weekly_rel_err}/{weekly_compare$weekly_null_err}"
)

weekly_compare$percent_less_than_1 <-
  scales::percent(weekly_compare$percent_less_than_1, accuracy = 0.1)

more_forecasts <- weekly_compare[weekly_compare$n_forecasts >= 15, ]
more_forecasts$country <- droplevels(more_forecasts$country)
more_forecasts$country <- forcats::fct_reorder(
  more_forecasts$country, more_forecasts$percent_less_than_1, max
)



## rel_error <- group_by(daily, forecast_date, country) %>%
##   summarise(rel_error = mean(rel_mae, na.rm = TRUE)) %>%
##   ungroup()
## rel_error$forecast_date <- as.factor(rel_error$forecast_date)
## more_forecasts <- left_join(more_forecasts, rel_error)

p2 <- ggplot(
  more_forecasts, aes(x = 0.01, y = country, label = percent_less_than_1)
) + geom_text(size = 3) +
  scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_discrete(limits = levels(more_forecasts$country)) +
  theme_void() +
  coord_cartesian(clip = "off")
  ##theme(plot.margin = unit(c(0, 0, 0, 1), "pt"))

p1 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = more_forecasts[more_forecasts$ratio <= 1, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = -1
  ) +
  ggnewscale::new_scale_fill() +

  geom_tile(
    data = more_forecasts[more_forecasts$ratio > 1 & more_forecasts$ratio <= 5, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
scale_fill_distiller(
  palette = "YlOrRd", na.value = "white", direction = 1
) +
ggnewscale::new_scale_fill() +
  geom_tile(
    data = more_forecasts[more_forecasts$ratio > 5, ],
    aes(forecast_date, country),
    fill = "#515bdc",
    width = 0.9,
    height = 0.8
  ) +
  xlab("") + ylab("") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(2, "lines")
  ) +
  geom_text(
    data = more_forecasts,
    aes(x = forecast_date, y = country, label = error_values),
    size = 2,
    fontface = "bold"
  ) +
scale_y_discrete(
  limits = levels(more_forecasts$country),
  labels = snakecase::to_title_case(rev(levels(more_forecasts$country)))
) +
coord_cartesian(clip = "off")

p <- p1 + p2 + plot_layout(ncol = 2, widths = c(3, 1))

ggsave("comparison_with_baseline_error.png", p)


######################################################################
######################################################################
##################### Relative Error #################################
######################################################################
######################################################################

x <- left_join(daily, null_error, by = c("date" = "dates", "country"))
x <- select(x, forecast_date, country, rel_null_error, rel_mae)
x <- gather(x, var, val, -forecast_date, -country)
x <- dplyr::distinct(x)

continent <- readr::read_csv("country_continent.csv") %>%
  janitor::clean_names()

by_country <- group_by(x, country, var) %>%
  summarise(
    mu = mean(val, na.rm = TRUE),
    sigma = sd(val, na.rm = TRUE),
    median = quantile(val, probs = 0.5, na.rm = TRUE),
    low = quantile(val, probs = 0.25, na.rm = TRUE),
    high = quantile(val, probs = 0.75, na.rm = TRUE)
  ) %>% ungroup()

by_country <- left_join(
  by_country, continent, by = c("country" = "countries_and_territories")
)
by_country$country <- forcats::fct_reorder(by_country$country, by_country$continent, min)


by_week <- group_by(x, forecast_date, var) %>%
  summarise(
    mu = mean(val, na.rm = TRUE),
    sigma = sd(val, na.rm = TRUE),
    median = quantile(val, probs = 0.5, na.rm = TRUE),
    low = quantile(val, probs = 0.25, na.rm = TRUE),
    high = quantile(val, probs = 0.75, na.rm = TRUE)
  ) %>% ungroup()

p1 <- ggplot(by_week) +
  geom_point(
    aes(forecast_date, median, col = var),
    position = position_dodge(width = 0.5)
  ) +
  geom_linerange(
    aes(forecast_date, ymin = low, ymax = high, col = var),
    position = position_dodge(width = 0.5)
  ) +
  ##facet_wrap(~continent, ncol = 1, scales = "free") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 week") +
  xlab("") + ylab("Relative error") +
  theme(legend.position = "top", legend.title = element_blank())


out <- select(by_country, country, var, high)
out <- spread(out, var, high)
countries <- droplevels(
  out$country[out$rel_mae < 10 & out$rel_null_error < 10]
)

x1 <- by_country[by_country$country %in% countries, ]
x1$country <- factor(x1$country)

p1 <- ggplot(by_country) +
  geom_point(
    aes(country, median, col = var),
    position = position_dodge(width = 0.3)
  ) +
  geom_linerange(
    aes(country, ymin = low, ymax = high, col = var),
    position = position_dodge(width = 0.3)
  ) +
  facet_wrap(~continent, ncol = 1, scales = "free") +
  theme_minimal() +
  xlab("") + ylab("Relative error") +
  theme(legend.position = "top", legend.title = element_blank())


x2 <- by_country[by_country$high >= 10, ]
x2 <- by_country[by_country$country %in% x2$country, ]
ggplot(x2) +
  geom_point(
    aes(country, median, col = var),
    position = position_dodge(width = 0.1)
  ) +
  geom_linerange(
    aes(country, ymin = low, ymax = high, col = var),
    position = position_dodge(width = 0.1)
  ) +
  facet_wrap(~continent, ncol = 1, scales = "free")


by_country <- tidyr::gather(by_country, var, val, -country)
vars <- c("null_050", "null_025", "null_975")
x <- by_country[by_country$var %in% ]
ggplot(x) +
  geom_point(aes(country, ), col = "red") +
  geom_linerange(
    aes(country, ymin = null_025, ymax = weeklly_rel_null_975),
    col = "red"
  ) +
  geom_point(aes(country, weeklly_rel_pred_050), col = "blue") +
  geom_linerange(
    aes(country, ymin = weeklly_rel_pred_025, ymax = weeklly_rel_pred_975),
    col = "blue"
  ) + ylim(0, 50)


weekly_rel_err$forecast_date <- factor(weekly_rel_err$forecast_date)



## Across all countries
weekly_rel_err$forecast_date <- as.Date(weekly_rel_err$forecast_date)

x <- group_by(weekly_rel_err, country) %>%
  summarise_if(is.numeric, list(mean, median, quantile), na.rm = TRUE) %>%
  ungroup()

x <- gather(weekly_rel_err, var, val, -country, -forecast_date)

x1 <- x[x$country %in% x$country[1:50], ]
x2 <- x[x$country %in% x$country[51:99], ]

p1 <- ggplot(x, aes(as.factor(forecast_date), val, fill = var)) +
  geom_boxplot(position = "dodge") +
  ##scale_y_log10() +
  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top", legend.title = element_blank()
  ) +
  ylim(0, 5)


ggplot() +
  geom_half_violin(
    data = x[x$var == "weekly_rel_pred", ],
    aes(as.factor(forecast_date), val),
    side = "l", alpha = 0.3, fill = "blue"
  ) +
  geom_half_violin(
    data = x[x$var == "weekly_rel_null", ],
    aes(as.factor(forecast_date), val),
    side = "r", alpha = 0.3, fill = "red"
  ) +

  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top", legend.title = element_blank()
  )

p2 <- ggplot(x2, aes(country, val, col = var)) +
  geom_point() +
  ##scale_y_log10() +
  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "none", legend.title = element_blank()
  )


p <- p1 + p2 + plot_layout(ncol = 1)

## Across all weeks
x <- group_by(weekly_rel_err, forecast_date) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

x <- gather(x, var, val, -forecast_date)


p1 <- ggplot(x, aes(forecast_date, val, col = var)) +
  geom_point() +
  ##scale_y_log10() +
  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top", legend.title = element_blank()
  ) +
  scale_x_date(date_breaks = "1 week")
