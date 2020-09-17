## orderly::orderly_develop_start(use_draft = "newer")
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

dir.create("figures")

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

combined_rt_estimates <- map(
  infiles[grep("combined_rt_estimates", infiles)], readRDS
)

combined_rt_qntls <- map_depth(
  combined_rt_estimates,
  2,
  function(l) {
    l$weeks_combined <- as.Date(l$weeks_combined)
    qntls <- data.frame(val = l$combined_rt, check.names = FALSE)
    qntls <- tibble::rownames_to_column(qntls, "quantile")
    qntls <- spread(qntls, quantile, val)
    out <- data.frame(
      week_starting = min(l$weeks_combined),
      week_ending = max(l$weeks_combined)
    )
    cbind(out, qntls)
  }
)
combined_rt_qntls <- map_dfr(
  combined_rt_qntls, ~ bind_rows(., .id = "country")
)
saveRDS(combined_rt_qntls, "combined_rt_qntls.rds")

weekly_iqr <- map(infiles[grep("weekly_iqr", infiles)], readRDS)
weekly_iqr <- map_dfr(weekly_iqr, ~ dplyr::bind_rows(., .id = "country"))

saveRDS(weekly_iqr, "weekly_iqr.rds")

weekly_iqr <- split(weekly_iqr, weekly_iqr$country)

plots <- imap(
  weekly_iqr,
  function(y, country) {
    message(country)
    x <- dplyr::distinct(weekly_iqr[[country]])
    x <- split(x, x$forecast_week) %>%
      map_dfr(
        function(df) {
          df <- df[rep(seq_len(nrow(df)), each = 7), ]
          df$dates <- seq(
            from = df$forecast_week[1],
            length.out = 7,
            by = "1 day"
          )
          df
        }
    )
    cntry <- map(combined_rt_estimates, ~ .[[country]])
    cntry <- keep(cntry, ~ ! is.null(.))
    cntry_combined <- map_dfr(
      cntry, function(y) {
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
    z <- as.Date(y$weeks_combined)
    ndays <- as.numeric(max(z) - min(z))

    df <- df[rep(seq_len(nrow(df)), each = ndays), ]
    df$dates <- seq(
      from = df$week_starting1[1],
      length.out = ndays,
      by = "1 day"
    )
        df

      }
    )

    ymax <- ceiling(max(x[["97.5%"]]))

    p1 <- plot_weekly_iqr(x) +
      coord_cartesian(clip = "off") +
      scale_x_date(date_breaks = "2 week") +
      ylim(0, ymax)

    xmin <- min(x$forecast_week)
    xmax <-max(x$forecast_week)
    cntry_combined$forecast_week <- cntry_combined$week_starting2
    p2 <- plot_combined_iqr(cntry_combined) +
      coord_cartesian(clip = "off") +
      ylim(0, ymax) +
      scale_x_date(date_breaks = "2 week", limits = c(xmin, xmax))

    p <- cowplot::plot_grid(
      p1, p2, ncol = 1, align = "hv", rel_heights = c(0.7, 1)
    )
    p
  }
)

iwalk(
  plots,
  function(p, country) ggsave(glue::glue("figures/{country}.png"), p)
)
