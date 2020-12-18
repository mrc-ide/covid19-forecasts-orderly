ps_plot <- function(ps) {

  p <- ggplot(ps) +
    geom_ribbon(
      aes(
        x = date, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
      ),
      alpha = 0.4
    ) +
    geom_line(aes(date, `50%`, col = forecast_week)) +
    scale_y_continuous(limits = c(0, 1)) +
    ylab("Proportion of population susceptible")

   p
}


reff_weekly_plot <- function(reff, weekly) {
  ##ymax <- ceiling(max(reff$`97.5%`))
  ymax <- 5
  p <- restimates_linegraph(weekly, forecast_date) +
    geom_line(
      data = reff, aes(date, `50%`, group = forecast_week),
      col = "#4d647e", size = 1.1
    ) +
    geom_ribbon(
      data = reff,
      aes(
        x = date, ymin = `2.5%`, ymax = `97.5%`,
        group = forecast_week
      ), fill = "#9ac8fc", alpha = 0.4
    ) +
    ylim(0, ymax) +
    ylab("Reproduction Number")

  p

}

pred_plot <- function(pred, obs) {

  p <- ggplot() +
    geom_point(
      data = obs, aes(dates, deaths), alpha = 0.5, size = 1
    ) +
    geom_ribbon(
      data = pred,
      aes(
        x = date, ymin = `2.5%`, ymax = `97.5%`,
        fill = forecast_week
      ),
      alpha = 0.4
    ) +
    geom_line(
      data = pred, aes(date, `50%`, col = forecast_week)
    ) +
    ylab("Daily Incidence")

  p

}
