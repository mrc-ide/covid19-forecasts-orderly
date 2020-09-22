nice_country_name <- function(x) snakecase::to_title_case(as.character(x))

round_and_format <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}


prop_in_cri_heatmap <- function(df, CrI = "50%") {

  df$forecast_date <- factor(df$forecast_date)
  xmax <- max(as.numeric(df$forecast_date)) + 2
  ymax <- max(as.numeric(factor(df$country))) + 1

  p <- ggplot(df) +
  geom_tile(
    aes(forecast_date, country, fill = prop_in_CrI),
    width = 0.8, height = 0.8
  ) +
    theme_classic() +
    scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0, 1),
      name = glue("Proportion in {CrI} CrI")
    ) +
  geom_text(
    aes(x = xmax, y = country, label = right_label),
    parse = TRUE, size = 2
  ) +
  geom_text(
    aes(x = forecast_date, y = ymax, label = top_label),
    parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 2
  ) +
  geom_text(
    aes(x = forecast_date, y = country, label = cell_label),
    size = 1.8, parse = TRUE
  ) +
  theme(
    axis.text.x.bottom = element_text(
      angle = 90, hjust = 0.5, vjust = 0.5, size = 6
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = margin(t = 30, r = 20, b = 0, l = 0),
    legend.position = "bottom",
    axis.line.x = element_blank()
  ) +
    scale_y_discrete(
    limits = rev(levels(df$country)),
    labels = nice_country_name
  ) +
  xlab("") +
  ylab("") +
  coord_cartesian(clip = "off")

  p
}


relative_error_heatmap <- function(df) {

  df$forecast_date <- factor(df$forecast_date)
  ymax <- max(as.integer(df$country)) + 1
  xmax <- max(as.integer(df$forecast_date)) + 2

  p <- ggplot() +
    theme_classic() +
    geom_tile(
      data = df[df$rel_mae_mu < 2, ],
      aes(forecast_date, country, fill = rel_mae_mu),
      width = 0.9,
      height = 0.8
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
    aes(forecast_date, country),
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
    aes(x = forecast_date, y = ymax, label = top_label),
    parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 2
  ) +
  geom_text(
    data = df,
    aes(x = forecast_date, y = country, label = cell_label),
    size = 1.8, parse = TRUE
  ) +
  scale_y_discrete(
    limits = rev(levels(df$country)),
    labels = nice_country_name
  ) +
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
      data = df[df$rel_mae <= 1, ],
      aes(week_of_projection, country, fill = rel_mae),
      width = 0.9,
      height = 0.8
    ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "Model Error < 1",
      title.position = "top",
      title.vjust = 0.5,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > 1 & df$rel_mae <= 5, ],
    aes(week_of_projection, country, fill = rel_mae),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "YlOrRd", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "1 < Model Error < 5",
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
