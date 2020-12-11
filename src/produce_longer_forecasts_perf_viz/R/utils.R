augment_data <- function(df, width = 1.5) {

  x <- data.frame(week_of_projection = unique(df$week_of_projection))
  x$x <- seq(from = 1, by = width, length.out = nrow(x))
  x_labels <- setNames(x$week_of_projection, x$x)

  y <- data.frame(country = rev(levels(df$country)))
  y$y <- seq(from = 1, by = width, length.out = nrow(y))

  y_labels <- nice_country_name(y$country)
  y_labels <- setNames(y_labels, y$y)

  df <- left_join(df, x) %>% left_join(y)

  list(
    df = df, y_labels = y_labels, x_labels = x_labels
  )
}

long_relative_error_heatmap <- function(df, high1, high2, x_labels, y_labels) {


  ymax <- max(as.integer(df$y)) + 2
  xmax <- max(as.integer(df$x)) + 3.5

 p <- ggplot() +
    geom_tile(
      data = df[df$rel_mae_mu <= high1, ],
      aes(x, y, fill = rel_mae_mu),
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
    data = df[df$rel_mae_mu > high1 & df$rel_mae_mu <= high2, ],
    aes(x, y, fill = rel_mae_mu),
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
    data = df[df$rel_mae_mu > high2, ],
    aes(x, y), fill = "#b2b2ff", width = 1.8, height = 1.8
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
  scale_y_continuous(
    breaks = sort(unique(df$y)), labels = y_labels, minor_breaks = NULL
  ) +
  scale_x_continuous(
    breaks = sort(unique(df$x)), labels = x_labels, minor_breaks = NULL
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


weekly_summary <- function(df, col = "rel_mae") {
######################################################################
################## Weekly Summary for each country ###################
######################################################################
  weekly <- group_by(df, week_of_projection, country) %>%
    summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each country ##########################
######################################################################
  by_country <- group_by(df, country) %>%
    summarise_if(is.numeric, list(c_mu = mean, c_sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each week ##########################
######################################################################
  by_week <- group_by(df, week_of_projection) %>%
    summarise_if(is.numeric, list(d_mu = mean, d_sd = sd)) %>%
    ungroup()

  weekly <- left_join(weekly, by_country)
  weekly <- left_join(weekly, by_week)

  col1 <- glue("{col}_d_mu")
  col2 <- glue("{col}_d_sd")
  weekly$top_label <- glue(
    "{round_and_format(weekly[[col1]])}",
    " %+-% {round_and_format(weekly[[col2]])}"
  )

  col1 <- glue("{col}_c_mu")
  col2 <- glue("{col}_c_sd")

  weekly$right_label <- glue(
    "{round_and_format(weekly[[col1]])}",
    " %+-% {round_and_format(weekly[[col2]])}"
  )

  col <- glue("{col}_mu")
  weekly$cell_label <- glue(
    "{round_and_format(weekly[[col]])}"
    ##" %+-% {round_and_format(weekly$rel_mae_sd)}"
  )

  weekly$country <- factor(
    weekly$country,
    levels = better_than_null$country, ordered = TRUE
  )
  weekly$country <- droplevels(weekly$country)
  weekly
}
