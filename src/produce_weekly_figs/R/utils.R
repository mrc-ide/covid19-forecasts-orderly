rt_boxplot <- function(rt, nice_names) {

  rt$state <- reorder(rt$state, -rt$`50%`)
  p <- ggplot(rt) +
  geom_boxplot(
    aes(
      y = state,
      xmin = `2.5%`,
      xmax = `97.5%`,
      xmiddle = `50%`,
      xlower = `25%`,
      xupper = `75%`,
      fill = proj
    ),
    alpha = 0.3,
    stat = "identity"
  ) +
    xlab("Effective Reproduction Number") +
    ylab("") +
    scale_y_discrete(labels = nice_names) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed"
    ) + theme_minimal() +
    scale_fill_manual(values = palette) +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    ) +
    expand_limits(x = 0)

  p
}

projection_plot <- function(obs, pred, date_min = "2021-01-01") {

  date_min <- as.Date(date_min)
  date_max <- max(pred$date) + 2
  dates_to_mark <- seq(
    from = date_min,
    to = date_max,
    by = "1 day"
  )
  dates_to_mark <- dates_to_mark[weekdays(dates_to_mark) == "Monday"]
  idx <- seq(from = length(dates_to_mark), to = 1, by = -3)
  dates_to_mark <- dates_to_mark[rev(idx)]
  
  obs_to_plot <- obs[obs$dates >= date_min,]

  ## Get dates of adding vlines.
  window_eps <- group_by(pred, proj) %>%
    summarise(date = min(date)) %>%
    ungroup()

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
    geom_point(data = obs_to_plot, aes(dates, deaths)) +
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

rt_lineplot <- function(rt, nice_names) {

  if (length(unique(rt$model)) == 1) width <- 0.1
  else width <- 0.7

  p <- ggplot() +
    geom_errorbar(
      data = rt,
      aes(x = state, ymin = `2.5%`, ymax = `97.5%`, col = proj),
      position = position_dodge(width = width),
      size = 1.1
    ) +
    geom_point(
      data = rt,
      aes(x = state, y = `50%`, col = proj),
      position = position_dodge(width = width),
      size = 4
    ) +
    xlab("") +
    ylab("Effective Reproduction Number") +
    scale_x_discrete(labels = nice_names) +
    geom_hline(
      yintercept = 1, linetype = "dashed"
    ) +
    scale_color_manual(values = palette) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "top", legend.title = element_blank()
    )  +
    expand_limits(y = 0)

    p
}
