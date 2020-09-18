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
  unwtd_pred_error, null_error,
  by = c("date" = "dates", "country")
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
  ) %>%
  ungroup()

better_than_null$percent_less_than_1 <- better_than_null$n_less_than_1 /
  better_than_null$n_forecasts

better_than_null <- arrange(better_than_null, desc(percent_less_than_1))

## Save this so that all other figures can use this order of countries
saveRDS(better_than_null, "better_than_null.rds")

weekly_compare <- left_join(weekly_compare, better_than_null)

weekly_compare$country <- factor(
  weekly_compare$country,
  levels = better_than_null$country, ordered = TRUE
)

weekly_compare$forecast_date <- factor(weekly_compare$forecast_date)

weekly_compare$weekly_rel_err <- signif(
  weekly_compare$weekly_rel_err, 2
)

weekly_compare$weekly_null_err <- signif(weekly_compare$weekly_null_err, 2)

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

compare_with_baseline <- function(df) {
  xmax <- max(as.numeric(df$forecast_date)) + 1
  p1 <- ggplot() +
    theme_classic() +
    geom_tile(
      data = df[df$ratio <= 1, ],
      aes(forecast_date, country, fill = ratio),
      width = 0.9,
      height = 0.8
    ) +
    scale_fill_distiller(
      palette = "Greens", na.value = "white", direction = -1,
      guide = guide_colourbar(
        title = "Model Error/Baseline Error < 1",
        title.position = "top",
        title.hjust = 0.5,
        order = 1
      )
    ) +
    ggnewscale::new_scale_fill() +
    geom_tile(
      data = df[df$ratio > 1 & df$ratio <= 5, ],
      aes(forecast_date, country, fill = ratio),
      width = 0.9,
      height = 0.8
    ) +
    scale_fill_distiller(
      palette = "YlOrRd", na.value = "white", direction = 1,
      guide = guide_colourbar(
        title = "Model Error/Baseline Error > 1",
        title.position = "top",
        title.hjust = 0.5,
        order = 2
      )
    ) +
    ggnewscale::new_scale_fill() +
    geom_tile(
      data = df[df$ratio > 5, ],
      aes(forecast_date, country),
      fill = "#515bdc",
      width = 0.9,
      height = 0.8
    ) +
    xlab("") +
    ylab("") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0.5),
      legend.position = "top",
      legend.title = element_text(size = 8),
##      legend.title.align = 0.5,
      legend.key.width = unit(2, "lines"),
      plot.margin = margin(t = 8, r = 15, b = 0, l = 0, unit = "pt")
    ) +
    geom_text(
      data = df,
      aes(x = forecast_date, y = country, label = error_values),
      size = 1.9,
      fontface = "bold"
    ) +
    geom_text(
      data = df,
      aes(x = xmax, y = country, label = percent_less_than_1),
      size = 2,
      fontface = "bold"
    ) +
    scale_y_discrete(
      limits = rev(levels(df$country)),
      labels = snakecase::to_title_case(rev(levels(df$country)))
    ) +
    coord_cartesian(clip = "off")
  p1
}

p1 <- compare_with_baseline(more_forecasts)
ggsave(
  filename = "comparison_with_baseline_error.tiff",
  plot = p1,
  ##device = agg_tiff,
  width = 8,
  height = 7.5,
  units = "in"
  #scaling = 1.3
)

######################################################################
######################################################################
######################################################################
######################################################################
############### SI Text Figure
############### More than 3 but less than 15 forecasts
######################################################################
######################################################################
######################################################################
p2 <- compare_with_baseline(less_forecasts)

ggsave(
  filename = "si_comparison_with_baseline_error.tiff",
  plot = p2,
  width = 8,
  height = 7.5,
  units = "in"
)

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
df$log_ratio <- log(df$ratio, 10)

p <- ggplot(df, aes(delta, ratio)) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  ylab("log Model Error/Baseline Error") +
  xlab("Weekly change in deaths") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )

ggsave("log_ratio_vs_delta.tiff", p, width = 5, height = 5, unit = "in")
## x <- dplyr::count(weekly_compare, country)
## countries <- x$country


