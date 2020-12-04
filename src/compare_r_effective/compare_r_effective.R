## orderly::orderly_develop_start(use_draft = "newer",
## parameters = list(week_ending = "2020-10-04"))
dir.create("figures")

reff_overlaps_weekly <- function(reff, weekly) {
  x <- left_join(
    reff, weekly, by = c("date" = "dates"),
    suffix = c("_reff", "_weekly")
  )
  x <- na.omit(x)
  x$day <- seq_len(nrow(x))
  ## We have now decided that we will not forecast beyond 4 weeks
  x <- x[x$day <= 28, ]
  x$reff_overlaps_weekly <-
    (x$`2.5%_weekly` > x$`2.5%_reff`) &
    (x$`97.5%_weekly` < x$`97.5%_reff`)

  x
}

infiles <- list(
  unweighted = "unweighted_reff_qntls.rds",
  weighted_across_countries = "weighted_across_countries_reff_qntls.rds",
  weighted_per_country = "weighted_per_country_reff_qntls.rds"
)

reff_by_strategy <- map(infiles, readRDS)

## We assume transmissibility remains constant for 1 week.
## Therefore, for each country, for each week, repeat the rows for 7
## days and increment dates
weekly_iqr <- readRDS("weekly_iqr.rds")

weekly_iqr_augm <- split(weekly_iqr, weekly_iqr$country) %>%
  map(function(x) {
    message(x$country[1])
    x <- dplyr::distinct(x)
    x <- split(x, x$forecast_week) %>%
      map_dfr(
        function(df) {
          df <- df[rep(seq_len(nrow(df)), each = 7), ]
          df$dates <- seq(
            from = df$forecast_week[1] + 1,
            length.out = 7,
            by = "1 day"
          )
        df
      }
     )
    x
  }
)


overlap <- map(
  reff_by_strategy,
  function(strategy) {
    countries <- setNames(names(strategy), names(strategy))
    map_dfr(
      countries,
      function(country) {
        weekly <- weekly_iqr_augm[[country]]
        reff <- strategy[[country]]
        reff$date <- as.Date(reff$date)
        weekly <- weekly[weekly$dates %in% reff$date, ]
        reff_overlaps_weekly(reff, weekly)
      }, .id = "country"
    )
  }
)

saveRDS(overlap, "overlap.rds")

iwalk(
  reff_by_strategy,
  function(strategy, nm) {
    countries <- names(strategy)
    walk(
      countries,
      function(country) {
        weekly <- weekly_iqr_augm[[country]]
        reff <- strategy[[country]]
        reff$date <- as.Date(reff$date)
        p <- ggplot() +
          geom_ribbon(
            data = reff, aes(date, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2
          ) +
          geom_line(data = reff, aes(date, `50%`)) +
          geom_ribbon(
            data = weekly,
            aes(dates, ymin = `2.5%`, ymax = `97.5%`, group = forecast_week),
            alpha = 0.2, fill = "blue"
          ) +
          geom_line(
            data = weekly, aes(dates, `50%`, group = forecast_week)
          ) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          scale_x_date(
            date_breaks = "2 weeks", limits = range(reff$date)
          ) +
          scale_y_continuous(expand = c(0, NA)) +
          ylab("Reproduction number") +
          xlab("") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 0.5)
          ) +
          ggtitle(
            glue("Effective and weekly reproduction number for {country}")
          )
        outfile <- glue("figures/{nm}_{country}.png")
        ggsave(filename = outfile, p)
      }
    )
  }
)

iwalk(
  overlap,
  function(df, nm) {
    ntrue <- filter(df, reff_overlaps_weekly) %>%
      count(country, reff_overlaps_weekly, sort = TRUE)

    df$country <- factor(
      df$country, levels = ntrue$country, ordered = TRUE
    )
    ## We forecast ahead for 56 days, hence x axis goes from 1 to 56
    df$day <- factor(df$day, levels = 1:56, ordered = TRUE)
    nice_country_name <- function(x) snakecase::to_title_case(as.character(x))

    p <- ggplot(df) +
      geom_tile(
        aes(day, country, fill = reff_overlaps_weekly),
        width = 0.9, height = 0.9
      ) +
      theme_minimal() +
      scale_y_discrete(
        limits = rev(levels(df$country)),
        labels = nice_country_name
      ) +
      theme(
        legend.position = "bottom", legend.title = element_blank()
      ) +
      ylab("") +
      xlab("")
    outfile <- glue("figures/{nm}_overall.png")
    ggsave(filename = outfile, p)
  }
)

