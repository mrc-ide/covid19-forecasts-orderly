combine_with_previous <- function(df, country) {

  df <- df[order(df$forecast_week, decreasing = TRUE), ]
  combined_rt <- rt_samples[rt_samples$model %in% df$forecast_week[1] &
                            rt_samples$country == country, use_si]
  combined_qntls <- c(
    `25%` = df$`25%`[1], `50%` = df$`50%`[1], `75%` = df$`75%`[1]
  )
  combined_iqr <- c(df$`25%`[1], df$`75%`[1])
  prev <- 1
  overlap <- TRUE
  out <- list(
    combined_rt = combined_qntls,
    weeks_combined = head(df$forecast_week, prev),
    rt_samples = sample(x = combined_rt, size = 1000)
  )
  while (overlap & prev < nrow(df)) {
    prev <- prev + 1
    prev_iqr <- c(df$`25%`[prev], df$`75%`[prev])
    overlap <- rincewind::overlaps(combined_iqr, prev_iqr)
    if (overlap) {
      weeks <- head(df$forecast_week, prev)
      combined_rt <- rt_samples[rt_samples$model %in% weeks & rt_samples$country == country, use_si]
      combined_qntls <- quantile(combined_rt, probs = c(0.25, 0.50, 0.75))
      combined_iqr <- c(combined_qntls[["25%"]], combined_qntls[["75%"]])

      out <- list(
        combined_rt = combined_qntls,
        weeks_combined = head(df$forecast_week, prev),
        rt_samples = sample(x = combined_rt, size = 1000)
      )
    }
  }

  out

}


plot_combined_iqr <- function(df) {

  p <- ggplot() +
    geom_ribbon(
      data = df,
      aes(x = forecast_week, ymin = `25%`, ymax = `75%`),
    alpha = 0.3
  ) + geom_line(
    data = df,
    aes(
      x = forecast_week, y = `50%`
    )
    ) + theme_minimal() +
    scale_x_date(date_breaks = "1 week") +
    xlab("Forecast Week") +
    ylab("Combined Effective Reproduction Number") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme(
      axis.text.x = element_text(hjust = 0.8)
    )

  p

}


plot_weekly_iqr <- function(df) {

  p <- ggplot() +
  geom_ribbon(
    data = df,
    aes(
      x = forecast_week, ymin = `25%`, ymax = `75%`, group = week_starting
    ),
    alpha = 0.3
  ) + geom_line(
    data = df,
    aes(
      x = forecast_week, y = `50%`, group = week_starting
    )
  ) + theme_minimal() +
    scale_x_date(date_breaks = "1 week") +
    xlab("Forecast Week") +
    ylab("Weekly Effective Reproduction Number") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme(
      axis.text.x = element_text(hjust = 0.8)
    )


  p

}
