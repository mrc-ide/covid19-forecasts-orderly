prop_in_cri_heatmap <- function(df, CrI = "50%") {

  df$x_labels <- strftime(
    as.Date(df$forecast_date), format = "%d-%b"
  )
  ##sorted <- sort(unique(df$forecast_date))
  df$forecast_date <- factor(df$forecast_date)
  xmax <- max(as.numeric(df$forecast_date)) + 2.5
  ymax <- max(as.numeric(factor(df$country))) + 1

  p <- ggplot(df) +
  geom_tile(
    aes(forecast_date, country, fill = prop_in_CrI),
    width = 0.9, height = 0.8
  ) +
  geom_text(
    aes(x = xmax, y = country, label = right_label),
    parse = TRUE, size = 2
  ) +
  geom_text(
    aes(x = forecast_date, y = ymax, label = top_label),
    parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 2
  ) +
    scale_y_discrete(
      limits = rev(levels(df$country)),
      labels = nice_country_name
    ) +
    scale_x_discrete(
      breaks = unique(df$forecast_date),
      labels = unique(df$x_labels)
    ) +
    theme_minimal() +
    scale_fill_distiller(
      palette = "Greens",
      direction = 1,
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0, 1),
      name = glue("Proportion in {CrI} CrI")
    ) +
    theme(
      axis.line = element_blank(),
      axis.text.x.bottom = element_text(
        angle = 90, hjust = 0.5, vjust = 0.5, size = 6
      ),
      axis.text.y = element_text(size = 6),
      plot.margin = margin(t = 30, r = 20, b = 0, l = 0),
      legend.position = "bottom",
      legend.key.width = unit(2, "lines"),
      legend.key.height = unit(1, "lines")
    ) +
    xlab("") +
    ylab("") +
    coord_cartesian(clip = "off")

  p
}

augment_data <- function(df, width = 1.5) {

  x <- data.frame(forecast_date = unique(df$forecast_date))
  x$x <- seq(from = 1, by = width, length.out = nrow(x))
  x_labels <- strftime(
    as.Date(x$forecast_date), format = "%d-%b"
  )
  x_labels <- setNames(x_labels, x$x)

  y <- data.frame(country = rev(levels(df$country)))
  y$y <- seq(from = 1, by = width, length.out = nrow(y))

  y_labels <- as.character(y$country) %>%
    snakecase::to_title_case()
  y_labels <- setNames(y_labels, y$y)

  df <- left_join(df, x) %>% left_join(y)

  list(
    df = df, x_labels = x_labels, y_labels = y_labels
  )

}

relative_error_heatmap <- function(df, x_labels, y_labels) {

  ymax <- max(as.integer(df$y)) + 2
  xmax <- max(as.integer(df$x)) + 3.5

  p <- ggplot() +
    geom_tile(
      data = df[df$rel_mae_mu < 2, ],
      aes(x, y, fill = rel_mae_mu),
      width = 1.8,
      height = 1.8
    ) +
  scale_fill_distiller(
    palette = "Spectral", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Relative Error",
      title.position = "left",
      title.vjust = 0.5,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae_mu >= 2, ],
    aes(x, y),
    fill = "#b2b2ff", width = 1.8, height = 1.8
  ) +
  geom_text(
    data = df,
    aes(x = xmax, y = y, label = right_label),
    parse = TRUE, size = 2
  ) +
  geom_text(
    data = df,
    aes(x = x, y = ymax, label = top_label),
    parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 2
  ) +
  geom_richtext(
    data = df,
    aes(x = x, y = y, label = cell_label), size = 1.7,
      fontface = "bold", fill = NA, label.color = NA, # remove background and outline
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  ) +
  scale_y_continuous(
    breaks = sort(unique(df$y)),
    labels = y_labels,
    minor_breaks = NULL
  ) +
  scale_x_continuous(
    breaks = sort(unique(df$x)),
    labels = x_labels,
    minor_breaks = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x.bottom = element_text(
      angle = 90, hjust = 0.5, vjust = 0.5, size = 6
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = margin(t = 30, r = 20, b = 0, l = 0),
    legend.position = "bottom",
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(1, "lines"),
    axis.line = element_blank(),
    legend.title = element_text(margin = margin(0, 0, 5, 0)),
    legend.margin = margin(0, 0, 5, 0),
    legend.box.margin = margin(-5, -5, 5, -5)
  ) +
  xlab("") +
  ylab("") +
  coord_cartesian(clip = "off")

  p
}


long_relative_error_heatmap <- function(df) {

  df$week_of_projection <- factor(
    df$week_of_projection,
    ordered = TRUE,
    levels = 1:max(df$week_of_projection)
  )

  ymax <- max(as.integer(df$country)) + 1
  xmax <- max(as.integer(df$week_of_projection)) + 1


 p <- ggplot() +
    theme_classic() +
    geom_tile(
      data = df[df$rel_mae <= 2, ],
      aes(week_of_projection, country, fill = rel_mae),
      width = 0.9,
      height = 0.8
    ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "Model Error <= 2",
      title.position = "top",
      title.vjust = 0.5,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > 2 & df$rel_mae <= 5, ],
    aes(week_of_projection, country, fill = rel_mae),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "YlOrRd", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "2 < Model Error <= 5",
      title.position = "top",
      title.hjust = 0.5,
      order = 2
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > 5, ],
    aes(week_of_projection, country),
    fill = "#b2b2ff",
    width = 0.9,
    height = 0.8
  ) +
  xlab("") +
  ylab("") +
  geom_text(
    data = df,
    aes(x = xmax, y = country, label = right_label),
    parse = TRUE, size = 2
  ) +
  geom_text(
    data = df,
    aes(x = week_of_projection, y = ymax, label = top_label),
    parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 2
  ) +
  geom_text(
    data = df,
    aes(x = week_of_projection, y = country, label = cell_label),
    size = 1.8, parse = TRUE
  ) +
  scale_y_discrete(
    limits = rev(levels(df$country)),
    labels = nice_country_name
  ) +
  scale_x_discrete(limits = levels(df$week_of_projection)) +
  theme(
    axis.text.x.bottom = element_text(
      angle = 90, hjust = 0.5, vjust = 0.5, size = 6
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = margin(t = 30, r = 20, b = 0, l = 0),
    legend.position = "bottom",
    axis.line.x = element_blank()
  ) +
  coord_cartesian(clip = "off")

  p
}
