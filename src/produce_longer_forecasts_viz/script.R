dir.create("figures")

pred_qntls <- readRDS("longer_projections_qntls.rds")
pred_qntls$forecast_week <- as.Date(pred_qntls$forecast_week)

exclude <- readRDS("exclude.rds")
pred_qntls <- pred_qntls[!pred_qntls$country %in% exclude, ]
all_deaths <- readRDS("latest_deaths_wide_no_filter.rds")
all_deaths$Czech_Republic <- all_deaths$Czechia

x <- split(
  pred_qntls,
  list(pred_qntls$forecast_week, pred_qntls$country),
  sep = ":"
)

x <- purrr::keep(x, ~ nrow(.) > 0)



x <- split(pred_qntls, pred_qntls$country)

npanels <- 6
nrows <- 3
ncols <- 2

purrr::iwalk(
  x,
  function(pred, cntry) {
    message(cntry)
    pred <- pred[order(pred$forecast_week), ]
    pred$date <- as.Date(pred$date)
    forecast_weeks <- unique(pred$forecast_week)

    obs <- all_deaths[, c("dates", cntry)]
    obs <- obs[obs$dates <= max(pred$date) + 7, ]
    obs$deaths <- obs[[cntry]]
    week_ending <- max(pred$forecast_week)
    ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
    ##pred$val[pred$val > ymax] <- NA
    pred <- dplyr::mutate_if(
      pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
    )

    idx <- which(cumsum(obs$deaths) >= 100)[1]
    xmin <- obs$dates[idx]


    palette <- rep(c("#3d2115", "#8e4d31"), 2 * length(forecast_weeks))
    palette <- palette[1:length(forecast_weeks)]

    pred$forecast_week <- factor(pred$forecast_week)
    npages <- ceiling(length(forecast_weeks) / npanels)

    p <- ggplot() +
      geom_point(
        data = obs,
        aes(dates, deaths), col = "#663723", alpha = 0.5, size = 1
      ) +
      scale_fill_manual(values = palette, aesthetics = c("col", "fill")) +
      theme_minimal() +
      scale_x_date(
        date_breaks = "2 weeks", limits = c(as.Date(xmin), NA)
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      xlab("") +
      ylab("Daily Incidence") +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 6)
      )


    label <- glue(
      "Projections for {snakecase::to_title_case(cntry)}"
    )
    p <- p + ggtitle(label)

    for (page in 1:npages) {
      idx <- seq(to = page * npanels, length.out = npanels)
      idx <- idx[idx <= length(forecast_weeks)]
      weeks <- factor(forecast_weeks[idx])
      df <- pred[pred$forecast_week %in% weeks, ]
      p2 <- p +
        geom_ribbon(
        data = df,
        aes(
          x = date, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
        ),
        alpha = 0.4
      ) +
      geom_line(
        data = df,
        aes(date, `50%`, col = forecast_week), size = 1.2
      ) +
      facet_wrap(
        ~forecast_week, ncol = ncols, nrow = nrows##, scales = "free_y"
      )
      outfile <- glue::glue("figures/{cntry}_{page}.png")
      message(outfile)
      ggsave(outfile , p2)
    }
  }
)


