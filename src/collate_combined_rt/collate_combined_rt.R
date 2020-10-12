## orderly::orderly_develop_start(use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

f <- function(infiles, pattern) {

  combined_rt_estimates <- map(
    infiles[grep(pattern, infiles)], readRDS
  )

  combined_rt_qntls <- map_depth(
    combined_rt_estimates, 2, combined_rt_quantiles
  )

  combined_rt_qntls <- map_dfr(
    combined_rt_qntls, ~ bind_rows(., .id = "country")
  )

  combined_rt_qntls

}

combined_rt_qntls <- f(infiles, "combined_rt_estimates")

saveRDS(combined_rt_qntls, "combined_rt_qntls.rds")


combined_wtd_rt_estimates <- f(
  infiles, "combined_weighted_estimates_across_countries"
)

saveRDS(
  combined_wtd_rt_qntls,
  "combined_weighted_estimates_across_countries.rds"
)


combined_wtd2_rt_estimates <- f(
  infiles, "combined_weighted_estimates_per_country"
)


saveRDS(
  combined_wtd2_rt_qntls,
  "combined_weighted_estimates_per_country.rds"
)


weekly_iqr <- map(infiles[grep("weekly_iqr", infiles)], readRDS)
weekly_iqr <- map_dfr(
  weekly_iqr, ~ dplyr::bind_rows(., .id = "country")
)

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
