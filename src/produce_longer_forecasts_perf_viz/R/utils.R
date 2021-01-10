augment_data <- function(df, weeks, width = 1.5) {

  x <- data.frame(forecast_week = weeks)
  x$x <- seq(from = 1, by = width, length.out = nrow(x))
  x_labels <- strftime(
    as.Date(x$forecast_week), format = "%d-%b"
  )
  ## Every 4th date on the x-axis
  idx <- seq(1, length(x$x), by = 3)
  x_labels <- setNames(x_labels[idx], x$x[idx])
  x$forecast_week <- factor(x$forecast_week)
  ##y <- data.frame(country = rev(levels(df$country)))
  y <- data.frame(country = rev(levels(df$country)))
  y$y <- seq(from = 1, by = width, length.out = nrow(y))

  y_labels <- nice_country_name(y$country)
  y_labels <- setNames(y_labels, y$y)

  df <- left_join(df, x) %>% left_join(y)

  list(
    df = df, x_labels = x_labels, y_labels = y_labels
  )

}

long_relative_error_heatmap <- function(df, high1, high2, x_labels, y_labels) {


  ymax <- max(as.integer(df$y)) + 2
  xmax <- max(as.integer(df$x)) + 3.5

 p <- ggplot() +
    geom_tile(
      data = df[df$rel_mae <= high1, ],
      aes(x, y, fill = rel_mae),
      width = 1.8, height = 1.8, alpha = 0.8
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
    aes(x, y, fill = rel_mae),
    width = 1.8, height = 1.8, alpha = 0.8
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
    aes(x, y), fill = "#4c0000", width = 1.8, height = 1.8
  ) +
  scale_y_continuous(
    breaks = sort(unique(df$y)), labels = y_labels, minor_breaks = NULL
  ) +
  scale_x_continuous(
    breaks = as.numeric(names(x_labels)), labels = x_labels,
    minor_breaks = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    axis.title = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(1, "lines"),
    legend.margin = margin(0, 0, 2, 0),
    legend.box.margin=margin(0, -10, -10, -10),
    axis.line = element_blank()
    ) +
  coord_cartesian(clip = "off")

  p
}


weekly_summary <- function(df, col = "rel_mae") {
######################################################################
################## Weekly Summary for each country ###################
######################################################################
  weekly <- group_by(df, forecast_week, country, week_of_projection) %>%
    summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each country ##########################
######################################################################
  by_country <- group_by(df, country, week_of_projection) %>%
    summarise_if(is.numeric, list(c_mu = mean, c_sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each week ##########################
######################################################################
  by_week <- group_by(df, forecast_week, week_of_projection) %>%
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

  ## weekly$country <- factor(
  ##   weekly$country,
  ##   levels = better_than_null$country, ordered = TRUE
  ## )
  ## weekly$country <- droplevels(weekly$country)
  weekly
}


prop_in_ci_heatmap <- function(df, x_labels, y_labels, CrI = "50%") {

 p <- ggplot(df) +
    geom_tile(
      aes(x, y, fill = fill),
      width = 1.8, height = 1.8, alpha = 0.8
    ) +
  scale_fill_distiller(
    palette = "Greens", na.value = "white", direction = 1,
    name = glue("Proportion in {CrI} CrI"),
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = scales::percent
  ) +
  scale_y_continuous(
    breaks = sort(unique(df$y)), labels = y_labels, minor_breaks = NULL
  ) +
  scale_x_continuous(
    breaks = as.numeric(names(x_labels)), labels = x_labels,
    minor_breaks = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    axis.title = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(1, "lines"),
    legend.margin = margin(0, 0, 2, 0),
    legend.box.margin=margin(0, -10, -10, -10),
    axis.line = element_blank()
    ) +
  coord_cartesian(clip = "off")

  p
}
