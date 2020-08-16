combine_with_previous <- function(df, country) {
  message(country)
  df <- df[order(df$forecast_week, decreasing = TRUE), ]
  combined_rt <- rt_samples[rt_samples$model %in% df$forecast_week[1] &
                            rt_samples$country == country, use_si]
  combined_qntls <- c(
    `2.5%` = df$`2.5%`[1],
    `25%` = df$`25%`[1],
    `50%` = df$`50%`[1],
    `75%` = df$`75%`[1],
    `97.5%` = df$`97.5%`[1]
  )
  combined_iqr <- c(df$`2.5%`[1], df$`97.5%`[1])
  latest_iqr <- combined_iqr
  prev <- 1
  overlap <- TRUE
  out <- list(
    combined_rt = combined_qntls,
    weeks_combined = head(df$forecast_week, prev),
    rt_samples = sample(x = combined_rt, size = 1000)
  )
  while (overlap & prev < nrow(df)) {
    prev <- prev + 1
    prev_iqr <- c(df$`2.5%`[prev], df$`97.5%`[prev])
    overlap <- rincewind::overlaps(latest_iqr, prev_iqr, digits = 2)
    message(prev_iqr, " overlaps ", latest_iqr)
    if (overlap) {
      weeks <- head(df$forecast_week, prev)
      combined_rt <- rt_samples[rt_samples$model %in% weeks & rt_samples$country == country, use_si]
      combined_qntls <- quantile(
        combined_rt, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)
      )
      combined_iqr <- c(
        combined_qntls[["2.5%"]], combined_qntls[["97.5%"]]
      )

      out <- list(
        combined_rt = combined_qntls,
        weeks_combined = head(df$forecast_week, prev),
        rt_samples = sample(x = combined_rt, size = 1000)
      )
    }
  }

  out

}


## df is the rt_samples filtered to the country of interest and
## the weeks combined obtained from combine_with_previous
## weights is a weighted list where the names are the weeks combined
## for example:
## weights <- c(`2020-03-29` = 0.950330211697379, `2020-03-22` = 0.047314155221824,
## `2020-03-15` = 0.00235563308079668)
combine_with_previous_weighted <- function(df, weights, size = 10000) {
  message(country)
  out <- split(df, df$model)
  idx <- sample(names(out), size = size, replace = TRUE, prob = weights)
  nsamples <- data.frame(table(idx))
  rt_samples <- imap_dfr(
    out,
    function(rt, week) {
      choose <- nsamples$Freq[nsamples$idx == week]
      if (length(choose) == 0) return(NULL)
      rows <- sample(1:nrow(rt), choose, replace = TRUE)
      rt[rows, ]
    }
  )
  combined_qntls <- quantile(
    rt_samples[[use_si]], probs = c(0.025, 0.25, 0.50, 0.75, 0.975)
  )
  list(
    combined_rt = combined_qntls,
    weeks_combined = names(out),
    weights = weights,
    frequency = nsamples$Freq,
    rt_samples = sample(x = combined_rt[[use_si]], size = 1000)
  )
}








plot_combined_iqr <- function(df) {

  p <- ggplot() +
    geom_ribbon(
      data = df,
      aes(x = forecast_week, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) + geom_line(
        data = df,
        aes(x = forecast_week, y = `50%`)
      ) +
    theme_minimal() +
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
      x = forecast_week, ymin = `2.5%`, ymax = `97.5%`, group = week_starting
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
