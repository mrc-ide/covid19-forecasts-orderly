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
      data = df[df$rel_mae <= 5, ],
      aes(week_of_projection, country, fill = rel_mae),
      width = 0.9,
      height = 0.8
    ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "Model Error <= 5",
      title.position = "top",
      title.vjust = 0.5,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > 5 & df$rel_mae <= 10, ],
    aes(week_of_projection, country, fill = rel_mae),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "YlOrRd", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "5 < Model Error <= 10",
      title.position = "top",
      title.hjust = 0.5,
      order = 2
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > 10, ],
    aes(week_of_projection, country),
    fill = "#b2b2ff",
    width = 0.9,
    height = 0.8
  ) +
  xlab("") +
  ylab("") +
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
