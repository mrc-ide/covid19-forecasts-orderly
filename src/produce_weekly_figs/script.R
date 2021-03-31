## orderly::orderly_develop_start(parameters = list(week_ending = "2021-03-28"), use_draft = "newer")

projection_plot <- function(obs, pred) {
  ## Plot only the latest projections.
  palette <- c("#56B4E9", "#009E73", "#D55E00", "#CC79A7")
  names(palette) <- c(
    "Model 2", "Model 1", "Model 3", "Ensemble"
  )
  date_min <- as.Date("2021-01-01")
  date_max <- max(pred$date) + 2
  dates_to_mark <- seq(
    from = date_min,
    to = date_max,
    by = "1 day"
  )
  dates_to_mark <- dates_to_mark[weekdays(dates_to_mark) == "Monday"]
  idx <- seq(from = length(dates_to_mark), to = 1, by = -3)
  dates_to_mark <- dates_to_mark[rev(idx)]
  ## Get dates of adding vlines.
  window_eps <- dplyr::group_by(pred, proj) %>%
    dplyr::summarise(date = min(date)) %>%
    dplyr::ungroup()

  window_eps$xintercepts <- as.numeric(window_eps$date - 1) + 0.5
  ## To get nice labels
  ## https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }

  p <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
    geom_line(
      data = pred,
      aes(date, `50%`, col = proj, group = proj),
      size = 1.1
    ) +
    geom_ribbon(
      data = pred,
      aes(x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          fill = proj,
          group = proj),
      alpha = 0.4) +
    scale_color_manual(
      values = palette,
      aesthetics = c("color", "fill"),
    ) +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank()) +
    scale_x_date(breaks = dates_to_mark, limits = c(date_min, date_max)) +
    scale_y_continuous(breaks = integer_breaks()) +
    geom_vline(
      xintercept = c(
        window_eps$xintercepts
      ),
      linetype = "dashed"
    ) + xlab("") +
    ylab("Deaths") +
    theme(
      axis.text.x = element_text(angle = -90)
    )

  p
}



## ensemble projections
ensemble_forecasts_qntls <- readRDS("us_ensemble_forecasts_qntls.rds")
ensemble_forecasts_qntls <- ensemble_forecasts_qntls[ensemble_forecasts_qntls$si == "si_2", ]
model_inputs <- readRDS("latest_model_input.rds")
tall <- tidyr::gather(model_inputs$D_active_transmission, state, deaths, -dates)
ensemble_forecasts_qntls$date <- as.Date(ensemble_forecasts_qntls$date)
ensemble_forecasts_qntls$proj <- "Ensemble"

p <- projection_plot(tall, ensemble_forecasts_qntls) +
  ggforce::facet_wrap_paginate(~state, ncol = 1, nrow = 3, page = 5, scales = "free_y")
