metrics_over_time <- function(df, si, countries, var, labels) {

  df <- df[df$si == si, ]
  df <- df[df$country %in% countries, ]
  df <- tidyr::gather(df, var, val, `rel_mae`:`poisson_p`)
  df <- df[df$var %in% var, ]

  p <- ggplot(df, aes(date, val)) +
    geom_point() +
    facet_wrap(
      ~var, scales = "free_y", ncol = 1, labeller = labeller(var = labels)
    ) +
    scale_x_date(date_breaks = "2 weeks") +
    theme_classic() +
    xlab("") +
    ylab("")

  p
}
