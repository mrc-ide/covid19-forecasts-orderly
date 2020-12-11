plot_combined_iqr <- function(df) {

  weeks <- unique(df$forecast_week)
  palette <- rep(c("#cc6f47", "#512c1c"), length(weeks))
  palette <- palette[seq_len(length(weeks))]
  names(palette) <- unique(weeks)

  weeks <- weeks[order(weeks)]
  df$forecast_week <- factor(
    df$forecast_week, levels = as.character(weeks), ordered = TRUE
  )

  p <- ggplot() +
    geom_ribbon(
      data = df,
      aes(
        x = dates, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
      ),
      alpha = 0.2
    ) + geom_line(
        data = df,
        aes(
          x = dates, y = `50%`, col = forecast_week
        )
        ) +
    theme_minimal() +
    xlab("Forecast Week") +
    ylab("Combined Effective Reproduction Number") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
      legend.position = "none"
    ) +
    scale_fill_manual(
      values = palette, aesthetics = c("col", "fill")
    )

  p

}


plot_weekly_iqr <- function(df) {

  p <- ggplot() +
  geom_ribbon(
    data = df,
    aes(
      x = dates, ymin = `2.5%`, ymax = `97.5%`, group = forecast_week
    ),
    alpha = 0.3
  ) + geom_line(
    data = df,
    aes(
      x = dates, y = `50%`, group = forecast_week
    )
  ) + theme_minimal() +

    ##xlab("Forecast Week") +
  ylab("Weekly Effective Reproduction Number") +
  xlab("") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0)
  )
  p

}

combined_rt_quantiles <-   function(l) {

  l$weeks_combined <- as.Date(l$weeks_combined)
  qntls <- data.frame(val = l$combined_rt, check.names = FALSE)
  qntls <- tibble::rownames_to_column(qntls, "quantile")
  qntls <- spread(qntls, quantile, val)
  out <- data.frame(
    week_starting = min(l$weeks_combined),
    week_ending = max(l$weeks_combined),
    weeks_combined = length(l$weeks_combined)
  )

  cbind(out, qntls)
}
