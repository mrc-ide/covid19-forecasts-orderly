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

#####################################################################
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
x <- dplyr::left_join(daily, null_error, by = c("date" = "dates", "country"))
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

x <- dplyr::arrange(x, desc(n_forecasts), desc(percent_less_than_1))

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

p2 <- ggplot(
  more_forecasts, aes(x = 0.1, y = country, label = percent_less_than_1)
) + geom_text(size = 3) +
  scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_discrete(limits = rev(levels(more_forecasts$country))) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))

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
  limits = rev(levels(more_forecasts$country)),
  labels = snakecase::to_title_case(rev(levels(more_forecasts$country)))
) +
coord_cartesian(clip = "off")

p <- p1 + p2 + plot_layout(ncol = 2, widths = c(3, 1))

ggsave("comparison_with_baseline_error.png", p)


######################################################################
######################################################################
########### Version 2 with one scale #################################
######################################################################
######################################################################
p3 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = more_forecasts[more_forecasts$ratio <=5, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
scale_fill_distiller(
  palette = "YlOrRd", na.value = "white", direction = 1
) +
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
  limits = rev(levels(more_forecasts$country)),
  labels = snakecase::to_title_case(rev(levels(more_forecasts$country)))
) +
coord_cartesian(clip = "off")

p <- p3 + p2 + plot_layout(ncol = 2, widths = c(3, 1))

ggsave("comparison_with_baseline_error_single_scale.png", p)

######################################################################
######################################################################
########## Version 3 with borders ####################################
######################################################################
######################################################################

p4 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = more_forecasts[more_forecasts$ratio <=5, ],
    aes(forecast_date, country, fill = ratio, col = err_level),
    width = 0.9,
    height = 0.8,
    size = 1
  ) +
  scale_fill_gradient2(midpoint = 1, mid = "#cce5cc") +
  scale_colour_manual(
    values = c(less_than_1 = "#004c00", greater_than_1 = "#400040")
  ) +
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
  limits = rev(levels(more_forecasts$country)),
  labels = snakecase::to_title_case(rev(levels(more_forecasts$country)))
) +
  guides(colour = "none") +
coord_cartesian(clip = "off")

p <- p4 + p2 + plot_layout(ncol = 2, widths = c(3, 1))

ggsave("comparison_with_baseline_error_with_border.png", p)

######################################################################
######################################################################
######################################################################
