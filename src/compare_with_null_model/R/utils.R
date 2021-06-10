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
    "{weekly_compare$weekly_rel_err}<br>{weekly_compare$weekly_null_err}"
  )

  weekly_compare$percent_less_than_1 <-
    scales::percent(weekly_compare$percent_less_than_1, accuracy = 0.1)

  list(
    weekly_compare = weekly_compare,
    better_than_null = better_than_null
  )
}

## Assign x and y levels to forecast weeks and countries respectively
## so that they can be forced further apart
augment_data <- function(df, width = 1.5) {

  x <- data.frame(forecast_date = unique(df$forecast_date))
  x$x <- seq(from = 1, by = width, length.out = nrow(x))
  x_labels <- strftime(
    as.Date(x$forecast_date), format = "%d-%b"
  )
  ## Every 4th date on the x-axis
  idx <- seq(1, length(x$x), by = 3)
  x_labels <- setNames(x_labels[idx], x$x[idx])

  y <- data.frame(country = rev(levels(df$country)))
  y$y <- seq(from = 1, by = width, length.out = nrow(y))

  y_labels <- rincewind::nice_country_name(y$country)
  y_labels[y_labels == "United States of America"] <- "USA"
  y_labels <- setNames(y_labels, y$y)

  df <- left_join(df, x) %>% left_join(y)

  list(
    df = df, x_labels = x_labels, y_labels = y_labels
  )

}

compare_with_baseline <- function(df, x_labels, y_labels) {


  xmax <- max(as.numeric(df$x)) + 2
  ##idx <- seq(1, length.out = length(x_labels), by = 3)
  p1 <- ggplot() +
    geom_tile(
      data = df[df$ratio <= 1, ],
      aes(x = x, y = y, fill = ratio),
      width = 1.4,
      height = 1.2
    ) +
    scale_fill_distiller(
      palette = "Greens", na.value = "white", direction = -1,
      guide = guide_colourbar(
        title = "Model Error/Baseline Error",
        title.position = "left",
        title.vjust = 0.8,
        order = 1
      )
    ) +
    ggnewscale::new_scale_fill() +
    geom_tile(
      data = df[df$ratio > 1 & df$ratio < 2, ],
      aes(x, y, fill = ratio), alpha = 0.7,
      width = 1.4,
      height = 1.2
    ) +
    scale_fill_distiller(
      palette = "YlOrRd", na.value = "white", direction = 1,
      guide = guide_colourbar(
        title = NULL,
        ##title.position = "top",
        ##title.hjust = 0.5,
        order = 2
      ),
      breaks = c(1.2, 1.4, 1.6, 1.8)
    ) +
    ggnewscale::new_scale_fill() +
    geom_tile(
      data = df[df$ratio >= 2, ], aes(x, y, fill = "#0000ff"),
      alpha = 0.7, width = 1.4, height = 1.2
    ) +
    scale_fill_identity(
      breaks = "#0000ff",
      labels = " >= 2",
      guide = guide_legend(
        order = 3, label = TRUE, title = NULL,
        label.position = "bottom"
      )
    ) +
    ## geom_richtext(
    ##   data = df, aes(x = x, y = y, label = error_values), size = 1.7,
    ##   fontface = "bold", fill = NA, label.color = NA, # remove background and outline
    ##   label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ## ) +
    ## geom_richtext(
    ##   data = df,
    ##   aes(x = xmax, y = y, label = percent_less_than_1), size = 2,
    ##   fontface = "bold", fill = NA, label.color = NA, # remove background and outline
    ##   label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ## ) +
    scale_y_continuous(
      breaks = sort(unique(df$y)),
      labels = y_labels,
      minor_breaks = NULL
    ) +
    scale_x_continuous(
      breaks = as.numeric(names(x_labels)),
      labels = x_labels,
      minor_breaks = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_blank(),
      legend.position = "top",
      legend.justification = "right",
      legend.title = element_text(size = 12),
      legend.key.width = unit(1, "lines"),
      legend.key.height = unit(0.8, "lines"),
      legend.margin = margin(0, 0, 0, -0.4, unit = "cm"),
      axis.line = element_blank()
    ) +
    coord_cartesian(clip = "off")
  p1
}


