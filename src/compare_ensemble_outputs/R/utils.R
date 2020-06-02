
all_forecasts <- function(obs, pred) {

  ggplot() +
      geom_point(
        data = obs,
        aes(dates, deaths),
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
      scale_x_date(limits = c(as.Date("2020-03-01"), NA)) +
    facet_wrap(
      ~country,
      ncol = 1,
      scales = "free_y",
      labeller = labeller(snakecase::to_title_case)
    )

}
