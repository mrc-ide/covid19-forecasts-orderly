
all_forecasts <- function(obs, pred) {

  ggplot() +
      geom_point(
        data = obs,
        aes(date, deaths),
        col = "black"
      ) +
      geom_line(
        data = pred,
        aes(x = date, `50%`, group = proj, col = "#0072B2"),
        size = 1
      ) +
      geom_ribbon(
        data = pred,
        aes(
          x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          group = proj,
          fill = "#0072B2"
        ),
        alpha = 0.3
      ) +
      geom_ribbon(
        data = pred,
        aes(
          x = date,
          ymin = `25%`,
          ymax = `75%`,
          group = proj,
          fill = "#0072B2"
        ),
        alpha = 0.7
      ) +
      theme_classic() +
      theme(legend.position = "none", legend.title = element_blank()) +
      xlab("") +
      ylab("") +
    scale_x_date(
      limits = c(as.Date("2020-03-01"), NA), date_breaks = "2 weeks"
    ) +
    facet_wrap(
      ~country,
      ncol = 1,
      scales = "free_y",
      labeller = labeller(country = snakecase::to_title_case)
    )

}


cap_predictions <- function(pred) {

  x <- split(pred, pred$country)
  purrr::map_dfr(x, function(y) {
    ymax <- 2 * ceiling(max(y$deaths) / 10) * 10
    y$`50%`[y$`50%` > ymax] <- NA
    dplyr::mutate_if(y, is.numeric, ~ ifelse(.x > ymax, ymax, .x))
  }
 )
}


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
