dir.create("figures")
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as


latest <- readRDS(grep(week_ending, infiles, value = TRUE))
countries <- unique(latest$country)
names(countries) <- countries

rt_samples <- purrr::map_dfr(infiles, readRDS)
week_ending <- as.Date(week_ending)
rt_samples$model <- as.Date(rt_samples$model)


country_weeks <- purrr::map(
  countries,
  function(country) {
    weeks <- unique(
      rt_samples[rt_samples$country == country, "model"]
    )
    consecutive_weeks <- list()
    prev_week <- week_ending
    while (prev_week %in% weeks) {
      message(country, " in ", prev_week)
      consecutive_weeks <- append(consecutive_weeks, prev_week)
      prev_week <- prev_week - 7
    }
    consecutive_weeks
  }
)


##country_weeks <- purrr::keep(country_weeks, ~ length(.) > 1)

week_iqr <- purrr::imap(
  country_weeks,
  function(weeks, country) {
    rt <- rt_samples[rt_samples$model %in% weeks & rt_samples$country == country, ]
    x <- dplyr::group_by(rt, model) %>%
      dplyr::summarise_if(
        is.numeric,
        ~ list(
  `2.5%` = quantile(., prob = 0.025),
  `25%` = quantile(., prob = 0.25),
  `50%` = quantile(., prob = 0.50),
  `75%` = quantile(., prob = 0.75),
  `97.5%` = quantile(., prob = 0.975)
  )) %>% dplyr::ungroup()

    x <- tidyr::unnest(x, cols = c(si_1, si_2))
    x <- x[, c("model", use_si)]
    colnames(x) <- c("forecast_week", "val")
    x$qntl <- rep(c("2.5%", "25%", "50%", "75%", "97.5%"), nrow(x) / 5)
    tidyr::spread(x, qntl, val)
  }
)

saveRDS(week_iqr, "weekly_iqr.rds")


combined_estimates <- purrr::imap(
  week_iqr, ~ combine_with_previous(.x, .y)
)

saveRDS(combined_estimates, "combined_rt_estimates.rds")

combined2 <- purrr::keep(
  combined_estimates, ~ length(.$weeks_combined) > 1
)

plots <- purrr::imap(
  combined2,
  function(y, country) {
    x <- week_iqr[[country]]
    x <- split(x, x$forecast_week) %>%
      purrr::map_dfr(
        function(df) {
          df <- df[rep(seq_len(nrow(df)), each = 7), ]
          df$week_starting <- df$forecast_week
          df$forecast_week <- seq(
            from = df$forecast_week[1],
            length.out = 7,
            by = "1 day"
          )
          df
        }
    )
    df <- data.frame(
      week_starting1 = min(y$weeks_combined),
      week_starting2 = max(y$weeks_combined),
      `2.5%` = y$combined_rt[["2.5%"]],
      `50%` = y$combined_rt[["50%"]],
      `97.5%` = y$combined_rt[["97.5%"]],
      check.names = FALSE
    )
    ## Extend this for the days between week_starting2 and
    ## week_starting1 + another 7 days because you would have continued
    ## with Rt estimate for another 7 days
    ndays <- as.numeric(df$week_starting2 - df$week_starting1) + 7

    df <- df[rep(seq_len(nrow(df)), each = ndays), ]
    df$forecast_week <- seq(
      from = df$week_starting1[1],
      length.out = ndays,
      by = "1 day"
    )

    ymax <- ceiling(max(x[["97.5%"]]))

    p1 <- plot_weekly_iqr(x) +
      coord_cartesian(clip = "off") +
      ylim(0, ymax)

    xmin <- min(x$forecast_week)
    xmax <-max(x$forecast_week)

    p2 <- plot_combined_iqr(df) +
      coord_cartesian(clip = "off") +
      ylim(0, ymax) +
      scale_x_date(date_breaks = "1 week", limits = c(xmin, xmax))

    p <- cowplot::plot_grid(p1, p2, ncol = 1, align = "hv")
    p
  }
)

purrr::iwalk(
  plots,
  function(p, country) ggsave(glue::glue("figures/{country}.png"), p)
)


