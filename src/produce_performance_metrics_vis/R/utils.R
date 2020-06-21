metrics_over_time <- function(df, si, countries, var, labels) {

  df <- df[df$si == si, ]
  df <- df[df$country %in% countries, ]
  df <- tidyr::gather(df, var, val, `rel_mae`:`poisson_p`)
  df <- df[df$var %in% var, ]

  p <- ggplot(df, aes(days_since_100_deaths, val)) +
    geom_point() +
    facet_wrap(
      ~var, scales = "free_y", ncol = 1, labeller = labeller(var = labels)
    ) +
    ##scale_x_date(date_breaks = "2 weeks") +
    theme_classic() +
    xlab("Days since 100 deaths") +
    ylab("")

  p
}

scaled_incid_and_metric <- function(incid,
                                    metrics,
                                    legend = TRUE,
                                    xlab,
                                    xticks) {
  plot(
    x = incid$days_since_100_deaths,
    y = incid$deaths_scaled,
    col = "blue",
    type = "l",
    axes = FALSE,
    xlab = xlab,
    ylab = "",
    cex.axis = 1.5,
    main = snakecase::to_title_case(incid$country[1])
  )
  axis(side = 1, lwd = 1.5, tick = xticks, labels = xticks)
  axis(
    side = 2, col = "blue", col.axis = "blue",
    lwd = 1.5, at = c(0, 0.5, 1)
  )
  par(new = TRUE)
  plot(
    x = metrics$days_since_100_deaths,
    y = metrics$val,
    xaxt = "n", yaxt = "n",
    ylab = "", xlab = "", col = "red", axes = FALSE
  )
  ## organise yticks which are otherwise all over the place
  ymax <- ceiling(max(metrics$val))
  message("ymax = ", ymax)
  message(paste(seq(0, ymax, by = 1), collapse = " "))
  axis(
    side = 4,
    at = seq(0, ymax, by = 1),
    labels = seq(0, ymax, by = 1),
    col = "red", col.axis = "red", lwd = 1.5
  )

  if (legend) {
    legend(
        "topleft",
      c("Scaled Deaths", "Relative Error"),
      col = c("blue", "red"), lty = c(1, 2)
    )
  }

}
