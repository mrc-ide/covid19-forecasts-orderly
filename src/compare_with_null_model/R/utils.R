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

  x <- data.frame(
    forecast_date = unique(df$forecast_date)
  )
  x$x <- seq(from = 1, by = 2, length.out = nrow(x))

  df <- left_join(df, x)

  df$x_labels <- strftime(
    as.Date(df$forecast_date), format = "%d-%b"
  )

  xmax <- max(as.numeric(df$x)) + 1.5

  p1 <- ggplot() +
    geom_label(
      data = df[df$ratio <= 1, ],
      aes(x, country, label = error_values, fill = ratio),
      size = 1.5, label.padding = unit(0.05, "line"),
      label.r = unit(0.01, "lines"), parse = TRUE
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
    geom_label(
      data = df[df$ratio > 1 & df$ratio <= 5, ],
      aes(x, country, label = error_values, fill = ratio),
      size = 1.5, label.padding = unit(0.05, "line"),
      label.r = unit(0.01, "lines"), parse = TRUE
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
    geom_label(
      data = df[df$ratio > 5, ],
      aes(x, country, label = error_values),
      fill = "#515bdc",
      size = 1.5, label.padding = unit(0.05, "line"),
      label.r = unit(0.01, "lines"), parse = TRUE
    ) +
    geom_text(
      data = df,
      aes(x = xmax, y = country, label = percent_less_than_1),
      size = 2,
      fontface = "bold"
    ) +
    scale_x_discrete(
      breaks = unique(df$x),
      labels = unique(df$x_labels)
    ) +
    scale_y_discrete(
      limits = rev(levels(df$country)),
      labels = snakecase::to_title_case(rev(levels(df$country)))
    ) +
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0.5),
      legend.position = "top",
      legend.title = element_text(size = 8),
      legend.key.width = unit(2, "lines"),
      plot.margin = margin(t = 8, r = 15, b = 0, l = 0, unit = "pt")
    ) +
    coord_cartesian(clip = "off")
  p1
}

