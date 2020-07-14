infiles <- list.files(pattern = "*.rds")
rt_samples <- purrr::map_dfr(infiles, readRDS)
week_ending <- as.Date(week_ending)
rt_samples$model <- as.Date(rt_samples$model)

countries <- unique(
  rt_samples[rt_samples$model == week_ending, "country"]
)
names(countries) <- countries

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


country_weeks <- purrr::keep(country_weeks, ~ length(.) > 1)

week_iqr <- purrr::imap(
  country_weeks,
  function(weeks, country) {
    rt <- rt_samples[rt_samples$model %in% weeks & rt_samples$country == country, ]
    x <- dplyr::group_by(rt, model) %>%
      dplyr::summarise_if(
        is.numeric,
        ~ list(`25%` = quantile(., prob = 0.25),
               `75%` = quantile(., prob = 0.75))
        ) %>% dplyr::ungroup()

    x <- tidyr::unnest(x, cols = c(si_1, si_2))
    x <- x[, c("model", use_si)]
    colnames(x) <- c("forecast_week", "val")
    x$qntl <- rep(c("25%", "75%"), nrow(x) / 2)
    tidyr::spread(x, qntl, val)
  }
)


combine_with_previous <- function(df, country) {
  df <- df[order(df$forecast_week, decreasing = TRUE), ]
  combined_iqr <- c(df$`25%`[1], df$`75%`[1])
  prev <- 2
  prev_iqr <- c(df$`25%`[prev], df$`75%`[prev])
  overlap <- rincewind::overlaps(combined_iqr, prev_iqr)

  while (overlap & prev < nrow(df)) {
    message("prev = ", prev)
    weeks <- head(df$forecast_week, prev)
    combined_rt <- rt_samples[rt_samples$model %in% weeks & rt_samples$country == country, use_si]
    combined_iqr <- quantile(combined_rt, probs = c(0.25, 0.75))
    combined_iqr <- c(combined_iqr[["25%"]], combined_iqr[["75%"]])
    prev <- prev + 1
    prev_iqr <- c(df$`25%`[prev], df$`75%`[prev])
    overlap <- rincewind::overlaps(combined_iqr, prev_iqr)
  }

  list(
    combined_rt = combined_iqr,
    weeks_combined = head(df$forecast_week, prev),
    rt_samples = sample(x = combined_rt, size = 1000)
  )
}

combined_estimates <- purrr::imap(
  week_iqr, ~ combine_with_previous(.x, .y)
)
