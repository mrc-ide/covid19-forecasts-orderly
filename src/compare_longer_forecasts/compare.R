## orderly::orderly_develop_start(use_draft = "newer")
with_ps <- readRDS("longer_projections_qntls.rds")
without_ps <- readRDS("no_ps_projections_qntls.rds")
obs <- readRDS("latest_deaths_wide_no_filter.rds")

countries <- unique(without_ps$country)

walk(
  countries, function(country) {
    x <- with_ps[with_ps$country == country, ]
    y <- without_ps[without_ps$country == country, ]
    z <- obs[, c("dates", country)]
    z$deaths <- z[[country]]
    weeks <- unique(x$forecast_week)
    df <- data.frame(forecast_week = weeks,
                     panel = rep(1:4, length.out = length(weeks)))
    x <- left_join(x, df)
    y <- left_join(y, df)
    x$date <- as.Date(x$date)
    y$date <- as.Date(y$date)

    p <- ggplot() +
      geom_point(data = z,  aes(dates, deaths), alpha = 0.2) +
      geom_line(
        data = x,
        aes(x = date, y = `50%`, group = forecast_week), color = "red"
      ) +
      geom_line(
        data = y,
        aes(x = date, y = `50%`, group = forecast_week), color = "blue"
      ) +
      geom_ribbon(
        data = x,
        aes(x = date, ymin = `2.5%`, ymax = `97.5%`, fill = "red", group = forecast_week),
        alpha = 0.1
      ) +
      geom_ribbon(
        data = y,
        aes(x = date, ymin = `2.5%`, ymax = `97.5%`, fill = "blue", group = forecast_week),
        alpha = 0.1
      ) +
      facet_wrap(~panel, scales = "free_y", ncol = 1) +
      labs(fill = "Adjusted for declining p_s") +
      scale_fill_identity(
        breaks = c("red", "blue"), labels = c("Yes", "No"),
        guide = "legend"
      ) +
      scale_x_date(limits = c(as.Date("2020-11-01"), as.Date("2021-01-15"))) +
      theme_classic() +
      theme(legend.position = "top", axis.title.x = element_blank())

    ggsave(glue("{country}.pdf"), p)

  }
)
