## orderly::orderly_develop_start(
## use_draft = FALSE, parameters = list(use_si = "si_2"))
null_error <- readRDS("null_model_error.rds")
weekly_incidence <- readRDS("weekly_incidence.rds")
weekly_incidence$forecast_date <- as.Date(weekly_incidence$week_starting)

weekly_delta <- split(
  weekly_incidence, weekly_incidence$country
) %>%
  map_dfr(
    function(df) {
      df <- arrange(df, forecast_date)
      df$delta <- c(NA, diff(df$weekly_incid))
      df
    }
  )


unwtd_pred_error <- readr::read_csv("unwtd_pred_error.csv") %>%
  dplyr::filter(si == use_si)
unwtd_pred_error$country[unwtd_pred_error$country == "Czech_Republic"] <- "Czechia"
unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- rename(unwtd_pred_error, "forecast_date" = "model")

null_error$country[null_error$country == "Czech Republic"] <- "Czechia"
unwtd_pred_error$country[unwtd_pred_error$country == "Czech Republic"] <- "Czechia"
######################################################################
######################################################################
######################################################################
########### Comparison with baseline error ###########################
######################################################################
######################################################################
######################################################################

weekly_compare <- left_join(
 unwtd_pred_error, null_error, by = c("date" = "dates", "country")
) %>%
  group_by(forecast_date, country, si) %>%
  summarise(
    weekly_rel_err = mean(mae),
    weekly_null_err = mean(null_error)
  ) %>%
  ungroup()

weekly_compare$ratio <- weekly_compare$weekly_rel_err /
  weekly_compare$weekly_null_err


weekly_compare$err_level <- ifelse(
  weekly_compare$ratio >= 1, "greater_than_1", "less_than_1"
)

better_than_null <- group_by(weekly_compare, country) %>%
  summarise(
    n_forecasts = n(), n_less_than_1 = sum(ratio < 1, na.rm = TRUE)
  ) %>% ungroup()

better_than_null$percent_less_than_1 <-  better_than_null$n_less_than_1 /
  better_than_null$n_forecasts

better_than_null <- arrange(better_than_null, desc(percent_less_than_1))

weekly_compare <- left_join(weekly_compare, better_than_null)

weekly_compare$country <- factor(
  weekly_compare$country, levels = better_than_null$country, ordered = TRUE
)

weekly_compare$forecast_date <- factor(weekly_compare$forecast_date)
weekly_compare$weekly_rel_err <- signif(weekly_compare$weekly_rel_err, 3)
weekly_compare$weekly_null_err <- signif(weekly_compare$weekly_null_err, 3)
weekly_compare$error_values <- glue(
  "{weekly_compare$weekly_rel_err}/{weekly_compare$weekly_null_err}"
)

weekly_compare$percent_less_than_1 <-
  scales::percent(weekly_compare$percent_less_than_1, accuracy = 0.1)


more_forecasts <- weekly_compare[weekly_compare$n_forecasts >= 15, ]
more_forecasts$country <- droplevels(more_forecasts$country)


less_forecasts <- weekly_compare[weekly_compare$n_forecasts < 15 & weekly_compare$n_forecasts > 3, ]
less_forecasts$country <- droplevels(less_forecasts$country)

######################################################################
######################################################################
############### Main Text Figure
############### 15 or more forecasts
######################################################################
######################################################################
p1 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = more_forecasts[more_forecasts$ratio <= 1, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Model Error/Baseline Error < 1",
      title.position = "left",
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +

  geom_tile(
    data = more_forecasts[more_forecasts$ratio > 1 & more_forecasts$ratio <= 5, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
scale_fill_distiller(
  palette = "YlOrRd", na.value = "white", direction = 1,
  guide = guide_colourbar(
    title = "Model Error/Baseline Error > 1",
    title.position = "right",
    order = 2
  )
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

p2 <- ggplot(
  more_forecasts, aes(x = 0.01, y = country, label = percent_less_than_1)
) + geom_text(size = 3) +
  scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_discrete(limits = rev(levels(more_forecasts$country))) +
  theme_void() +
  coord_cartesian(clip = "off")

p <- p1 + p2 + plot_layout(ncol = 2, widths = c(3, 1))

ggsave("comparison_with_baseline_error.png", p)

######################################################################
######################################################################
######################################################################
######################################################################
############### SI Text Figure
############### More than 3 but less than 15 forecasts
######################################################################
######################################################################
######################################################################

p1 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = less_forecasts[less_forecasts$ratio <= 1, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Model Error/Baseline Error < 1",
      title.position = "left",
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +

  geom_tile(
    data = less_forecasts[less_forecasts$ratio > 1 & less_forecasts$ratio <= 5, ],
    aes(forecast_date, country, fill = ratio),
    width = 0.9,
    height = 0.8
  ) +
scale_fill_distiller(
  palette = "YlOrRd", na.value = "white", direction = 1,
  guide = guide_colourbar(
    title = "Model Error/Baseline Error > 1",
    title.position = "right",
    order = 2
  )
) +
ggnewscale::new_scale_fill() +
  geom_tile(
    data = less_forecasts[less_forecasts$ratio > 5, ],
    aes(forecast_date, country),
    fill = "#515bdc",
    width = 0.9,
    height = 0.8
  ) +
  xlab("") + ylab("") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    legend.position = "top",
    legend.key.width = unit(2, "lines")
  ) +
  geom_text(
    data = less_forecasts,
    aes(x = forecast_date, y = country, label = error_values),
    size = 2,
    fontface = "bold"
  ) +
scale_y_discrete(
  limits = rev(levels(less_forecasts$country)),
  labels = snakecase::to_title_case(rev(levels(less_forecasts$country)))
) +
coord_cartesian(clip = "off")

p2 <- ggplot(
  less_forecasts, aes(x = 0.01, y = country, label = percent_less_than_1)
) + geom_text(size = 3) +
  scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_discrete(limits = rev(levels(less_forecasts$country))) +
  theme_void() +
  coord_cartesian(clip = "off")


p <- p1 + p2 + plot_layout(ncol = 2, widths = c(3, 1))

ggsave("si_comparison_with_baseline_error.png", p)

######################################################################
######################################################################
######################################################################
############## SI Text Figure
############## When the rate of change of deaths is small, model error
############## is large compared to null model error.
######################################################################
######################################################################
######################################################################
weekly_compare$forecast_date <- as.Date(weekly_compare$forecast_date)
df <- left_join(weekly_compare, weekly_delta)
df <- df[df$weekly_incid > 0, ]

p <- ggplot(df, aes(ratio, delta)) +
  geom_point() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_minimal() +
  xlab("Model Error/Baseline Error") +
  ylab("Weekly change in deaths")

ggsave("ratio_vs_delta.png", p)
##x <- dplyr::count(weekly_compare, country)
##countries <- x$country
