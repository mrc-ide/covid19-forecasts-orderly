projection_plot <- function(obs, pred) {

    ## Number of projections
  nprojs <- length(unique(pred$week_ending))

  ## Latest projections get a different color
  if (nprojs == 1) {
    palette <- c("#b3669e")
  } else {
    palette <- c(
      rep("#98984d", nprojs - 1),
      "#b3669e"
    )
  }
  names(palette) <- unique(pred$week_ending)

  ## Plot only the latest projections.
  ##pred <- pred[pred$week_ending == max(as.Date(pred$week_ending)), ]

  date_min <- max(
    as.Date(pred$week_ending) - 28, as.Date("2020-03-01")
  )
  date_max <- max(pred$date) + 2
  dates_to_mark <- seq(
    from = date_min,
    to = date_max,
    by = "1 day"
  )
  dates_to_mark <- dates_to_mark[weekdays(dates_to_mark) == "Monday"]
  ## Get dates of adding vlines.
  window_eps <- dplyr::group_by(pred, proj) %>%
    dplyr::summarise(date = min(date)) %>%
    dplyr::ungroup()

  window_eps$xintercepts <- as.numeric(
    window_eps$date - 1
  ) + 0.5
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
      aes(date, `50%`, col = week_ending, group = week_ending)
    ) +
    geom_ribbon(
      data = pred,
      aes(x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          fill = week_ending,
          group = week_ending),
      alpha = 0.4) +
    scale_color_manual(
      values = palette,
      aesthetics = c("color", "fill")
    ) +
    theme_project() +
    theme(legend.position = "none") +
    scale_x_date(breaks = dates_to_mark, limits = c(date_min, date_max)) +
    scale_y_continuous(breaks = integer_breaks()) +
    geom_vline(
      xintercept = c(
        window_eps$xintercepts
      ),
      linetype = "dashed"
    ) + xlab("") +
    ylab("Deaths")

  p
}


rt_plot <- function(rt) {

    palette <- c("#E69F00", "#0072B2", "#D55E00")
    names(palette) <- unique(rt$model)


    nice_names <- snakecase::to_any_case(
        rt$country,
        "title"
    )
    names(nice_names) <- rt$country
    rt$country <- reorder(rt$country, -rt$`50%`)
    if (length(unique(rt$model)) == 1) width <- 0.1
    else width <- 0.7

    p <- ggplot() +
        geom_errorbar(
            data = rt,
            aes(x = country, ymin = `2.5%`, ymax = `97.5%`, col = model),
            position = position_dodge(width = width),
            size = 1.1
        ) +
        geom_point(
            data = rt,
            aes(x = country, y = `50%`, col = model),
            position = position_dodge(width = 0.5),
            size = 4
            ) +
      ##theme_pubr() +
        xlab("") +
        ylab("Effective Reproduction Number") +
        scale_x_discrete(labels = nice_names) +
        geom_hline(
            yintercept = 1,
            linetype = "dashed"
        ) + theme_project() +
          scale_color_manual(
            values = palette,
            labels = c("Ensemble", "Model 1", "Model 2")
          ) + theme(
                legend.position = "bottom",
                legend.title = element_blank()
              )

    p
}


rt_boxplot <- function(rt) {

  nice_names <- snakecase::to_title_case(rt$country)
  names(nice_names) <- rt$country
  ##rt$country <- reorder(rt$country, -rt$`50%`)

  rt$country <- reorder(rt$country, -rt$`50%`)
  p <- ggplot(rt) +
  geom_boxplot(
    aes(
      y = country,
      xmin = `2.5%`,
      xmax = `97.5%`,
      xmiddle = `50%`,
      xlower = `25%`,
      xupper = `75%`
    ),
    stat = "identity"
  ) +
    xlab("Effective Reproduction Number") +
    ylab("") +
    scale_y_discrete(labels = nice_names) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed"
    ) + theme_project() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )

  p
}

