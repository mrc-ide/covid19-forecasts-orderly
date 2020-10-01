data_prep <- function(pred_err_df, null_err_df) {

  weekly_compare <- left_join(
    pred_err_df, null_err_df,
    by = c("date" = "dates", "country")
  ) %>%
    group_by(forecast_date, country, si) %>%
    summarise(
      weekly_rel_err = mean(mae),
      weekly_null_err = mean(null_error)
    ) %>% ungroup()

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

  list(
    weekly_compare = weekly_compare,
    better_than_null = better_than_null
  )

}

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

