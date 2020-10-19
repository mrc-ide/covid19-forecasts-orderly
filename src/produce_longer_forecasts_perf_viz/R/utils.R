augment_data <- function(df, width = 1.5) {

  x <- data.frame(week_of_projection = unique(df$week_of_projection))
  x$x <- seq(from = 1, by = width, length.out = nrow(x))
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

long_relative_error_heatmap <- function(df, high1, high2, x_labels, y_labels) {

  df$week_of_projection <- factor(
    df$week_of_projection,
    ordered = TRUE,
    levels = 1:max(df$week_of_projection)
  )

  ymax <- max(as.integer(df$y)) + 2
  xmax <- max(as.integer(df$x)) + 3.5


 p <- ggplot() +
    geom_tile(
      data = df[df$rel_mae <= high1, ],
      aes(week_of_projection, country, fill = rel_mae),
      width = 1.8, height = 1.8
    ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = glue::glue("Model Error <= {high1}"),
      title.position = "top",
      title.vjust = 0.5,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > high1 & df$rel_mae <= high2, ],
    aes(week_of_projection, country, fill = rel_mae),
    width = 1.8, height = 1.8
  ) +
  scale_fill_distiller(
    palette = "YlOrRd", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = glue::glue("{high1} < Model Error <= {high2}"),
      title.position = "top",
      title.hjust = 0.5,
      order = 2
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = df[df$rel_mae > high2, ],
    aes(week_of_projection, country),
    fill = "#b2b2ff",
    width = 0.9,
    height = 0.8
  ) +
  geom_text(
    data = df, aes(x = xmax, y = y, label = right_label),
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
  xlab("") +
  ylab("") +
  scale_y_discrete(
    limits = rev(levels(df$country)), labels = nice_country_name
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
