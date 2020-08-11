all_restimates_violin <- function(out) {

  p <- ggplot(
    out, aes(forecast_date, val)
  ) +
  geom_half_violin(scale = "width", width = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
  facet_wrap(~country, ncol = 1, scales = "free_y") +
  theme_minimal() +
  xlab("") +
  ylab("Effective Reproduction Number") +
  facet_wrap(
    ~country,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(country = snakecase::to_title_case)
  )
  p
}

all_restimates_line <- function(out) {

  ## vals <- interaction(out$forecast_date, out$phase)
  ## palette <- rep(NA, length(vals))
  ## palette[grep("decline", vals)] <- "#009E73"
  ## palette[grep("growing", vals)] <- "#E69F00"
  ## palette[grep("stable/growing slowly", vals)] <- "#56B4E9"
  ## palette[grep("unclear", vals)] <- "#999999"
  ## names(palette) <- vals

  p <- ggplot(out) +
    geom_ribbon(
      aes(
        x = days_since_100_deaths,
        ymin = `2.5%`,
        ymax = `97.5%`,
        group = forecast_date
      ),
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        x = days_since_100_deaths,
        ymin = `25%`,
        ymax = `75%`,
        group = forecast_date
      ),
      alpha = 0.5
    ) +
    geom_line(
      aes(days_since_100_deaths, `50%`, group = forecast_date)) +
    geom_hline(yintercept = 1, linetype = "dashed", col = "red") +
    facet_wrap(~country, ncol = 1, scales = "free_y") +
    theme_minimal() +
    xlab("") +
    ylab("Effective Reproduction Number") +
  facet_wrap(
    ~country,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(country = snakecase::to_title_case)
  ) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1))

  p
}


all_forecasts <- function(obs, pred) {

  ggplot() +
      geom_point(
        data = obs,
        aes(days_since_100_deaths, deaths),
        col = "black"
      ) +
      geom_line(
        data = pred,
        aes(x = days_since_100_deaths, `50%`, group = proj, col = "#0072B2"),
        size = 1
      ) +
      geom_ribbon(
        data = pred,
        aes(
          x = days_since_100_deaths,
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
          x = days_since_100_deaths,
          ymin = `25%`,
          ymax = `75%`,
          group = proj,
          fill = "#0072B2"
        ),
        alpha = 0.7
      ) +
      theme_minimal() +
      theme(legend.position = "none", legend.title = element_blank()) +
      xlab("") +
      ylab("") +
    ## scale_x_date(
    ##   limits = c(as.Date("2020-03-01"), NA),
    ##   date_breaks = "2 weeks"
    ## ) +
    facet_wrap(
      ~country,
      ncol = 1,
      scales = "free_y",
      labeller = labeller(country = snakecase::to_title_case)
    ) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1))

}


