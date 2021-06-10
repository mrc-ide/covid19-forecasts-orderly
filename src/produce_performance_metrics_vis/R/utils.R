prop_in_cri_heatmap <- function(df, weeks, CrI = "50%") {

  x_labels <- strftime(weeks, format = "%d-%b")
  ##sorted <- sort(unique(df$forecast_date))
  weeks <- factor(weeks)
  idx <- seq(1, length(weeks), 3)
  xmax <- max(as.numeric(weeks)) + 2.5
  ymax <- max(as.numeric(factor(df$country))) + 1

  p <- ggplot(df) +
  geom_tile(
    aes(factor(forecast_date), country, fill = prop_in_CrI),
    width = 0.9, height = 0.8
  ) +
  ## geom_text(
  ##   aes(x = xmax, y = country, label = right_label),
  ##   parse = TRUE, size = 6 / .pt
  ## ) +
  ## geom_text(
  ##   aes(x = forecast_date, y = ymax, label = top_label),
  ##   parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 6 / .pt
  ## ) +
    scale_y_discrete(
      limits = rev(levels(df$country)),
      labels = nice_country_name
    ) +
    scale_x_discrete(breaks = weeks[idx], labels = x_labels[idx]) +
    theme_minimal() +
    scale_fill_distiller(
      palette = "Greens", direction = 1, breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1), limits = c(0, 1),
      name = glue("Proportion in {CrI} CrI")
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 10),
      legend.key.width = unit(2, "lines"),
      legend.key.height = unit(1, "lines"),
      ##legend.margin = margin(0, 0, 2, 0),
      ##legend.box.margin=margin(0, -10, -10, -10),
      axis.line = element_blank()
    ) +
    coord_cartesian(clip = "off")
  p
}



augment_data <- function(df, weeks, width = 1.5) {

  x <- data.frame(forecast_date = weeks)
  x$x <- seq(from = 1, by = width, length.out = nrow(x))
  x_labels <- strftime(
    as.Date(x$forecast_date), format = "%d-%b"
  )
  ## Every 4th date on the x-axis
  idx <- seq(1, length(x$x), by = 3)
  x_labels <- setNames(x_labels[idx], x$x[idx])


  y <- data.frame(country = rev(levels(df$country)))
  y$y <- seq(from = 1, by = width, length.out = nrow(y))

  y_labels <- nice_country_name(y$country)
  y_labels[y_labels == "United States of America"] <- "USA"
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
      width = 1.8, height = 1.8, alpha = 0.8
    ) +
  scale_fill_distiller(
    palette = "Spectral", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Relative Error",
      title.position = "left",
      title.vjust = 0.8,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae_mu >= 2, ],
    aes(x, y, fill = "#0000ff"), width = 1.8, height = 1.8
  ) +
  scale_fill_identity(
    guide = guide_legend(
      order = 2,
      title = NULL,
      label.position = "bottom",
      label = TRUE
      ##title.position = "top",
      ##title.vjust = 0.5, label = FALSE
    ),
    breaks = "#0000ff", labels = " >= 2"
  ) +
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
    legend.title = element_text(size = 12),
    legend.key.width = unit(1, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.margin = margin(0, 0, 2, 0),
    legend.box.margin = margin(0, -10, -10, -10),
    axis.line = element_blank()
  ) +
  coord_cartesian(clip = "off")

  p
}
