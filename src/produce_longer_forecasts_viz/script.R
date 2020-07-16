dir.create("figures")

pred_qntls <- readRDS("longer_projections_qntls.rds")
pred_qntls$forecast_week <- as.Date(pred_qntls$forecast_week)

all_deaths <- readRDS("latest_deaths_wide_no_filter.rds")

x <- split(
  pred_qntls,
  list(pred_qntls$forecast_week, pred_qntls$country),
  sep = ":"
)

x <- purrr::keep(x, ~ nrow(.) > 0)
x <- purrr::map(x, ~ .[.$`.width` == 0.95, ])
purrr::iwalk(
  x,
  function(pred, cntry_week) {
    message(cntry_week)
    cntry <- strsplit(cntry_week, split = ":")[[1]][2]
    obs <- all_deaths[, c("dates", cntry)]
    obs <- obs[obs$dates <= max(pred$dates) + 7, ]
    obs$deaths <- obs[[cntry]]
    week_ending <- max(pred$forecast_week)
    ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
    pred$val[pred$val > ymax] <- NA
    pred <- dplyr::mutate_if(
      pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
    )
    p <- ggplot() +
      geom_point(data = obs, aes(dates, deaths), col = "blue") +
      geom_ribbon(
        data = pred, aes(x = dates, ymin = .lower, ymax = .upper),
        alpha = 0.3
      ) +
      geom_line(
        data = pred, aes(dates, val), size = 1.2
      ) +
      theme_minimal() +
      scale_x_date(
        date_breaks = "2 weeks",
        limits = c(as.Date("2020-03-01"), NA),
        ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      ) +
      xlab("") +
      ylab("Daily Incidence")

    label <- glue::glue(
      "Projections for {cntry} from {week_ending} to {max(pred$dates)}"
    )
    p <- p + ggtitle(label)
    ggsave(glue::glue("figures/projections_{cntry_week}.png"), p)
  }
)


## For a given country, overlapping projections
x <- split(pred_qntls, pred_qntls$country)
##x <- purrr::map(x, ~ .[.$`.width` == 0.95, ])
purrr::iwalk(
  x,
  function(pred, cntry) {
    message(cntry)
    pred <- pred[order(pred$forecast_week), ]
    forecast_weeks <- unique(pred$forecast_week)
    palette <- rep(c("#efa7ff", "#9c1f4c"), 2 * length(forecast_weeks))
    palette <- palette[1:length(forecast_weeks)]
    names(palette) <- forecast_weeks

    obs <- all_deaths[, c("dates", cntry)]
    obs <- obs[obs$dates <= max(pred$dates) + 7, ]
    obs$deaths <- obs[[cntry]]
    week_ending <- max(pred$forecast_week)
    ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
    pred$val[pred$val > ymax] <- NA
    pred <- dplyr::mutate_if(
      pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
    )
    pred$forecast_week <- factor(pred$forecast_week)
    idx <- which(cumsum(obs$deaths) >= 100)[1]
    xmin <- obs$dates[idx]
    p <- ggplot() +
      geom_point(data = obs, aes(dates, deaths), col = "blue") +
      geom_ribbon(
        data = pred[pred$`.width` == 0.95, ],
        aes(x = dates, ymin = .lower, ymax = .upper, fill = forecast_week),
        alpha = 0.3
      ) +
      geom_line(
        data = pred, aes(dates, val, col = forecast_week), size = 1.2
      ) +
      scale_fill_manual(values = palette, aesthetics = c("col", "fill")) +
      theme_minimal() +
      scale_x_date(
        date_breaks = "2 weeks", limits = c(as.Date(xmin), NA)
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      ) +
      xlab("") +
      ylab("Daily Incidence") +
      theme(legend.position = "none")
    label <- glue::glue("Projections for {cntry}")
    p <- p + ggtitle(label)

    p2 <- p +
      facet_wrap(~forecast_week, ncol = 1, scales = "free_y") +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )

    ggsave(
      glue::glue("figures/all_projections_facetted_{cntry}.png"), p2
    )
    ggsave(glue::glue("figures/all_projections_{cntry}.png"), p)
  }
)
