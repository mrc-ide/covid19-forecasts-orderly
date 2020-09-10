## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"), use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

infiles <- grep("beta", infiles, invert = TRUE, value = TRUE)
latest <- readRDS(grep(week_ending, infiles, value = TRUE))
per_country_beta <- readRDS("per_country_beta.rds")
across_countries_beta <- readRDS("across_countries_beta.rds")

countries <- unique(latest$country)
names(countries) <- countries
infiles <- grep("country_weeks", infiles, value = TRUE, invert = TRUE)
rt_samples <- map_dfr(infiles, readRDS)
rt_samples$model <- as.Date(rt_samples$model)

week_ending <- as.Date(week_ending)



## country_weeks <- purrr::map(
##   countries,
##   function(country) {
##     weeks <- unique(
##       rt_samples[rt_samples$country == country, "model"]
##     )
##     consecutive_weeks <- list()
##     prev_week <- week_ending
##     while (prev_week %in% weeks) {
##       message(country, " in ", prev_week)
##       consecutive_weeks <- append(consecutive_weeks, prev_week)
##       prev_week <- prev_week - 7
##     }
##     consecutive_weeks
##   }
## )

country_weeks <- readRDS("country_weeks.rds")
##country_weeks <- purrr::keep(country_weeks, ~ length(.) > 1)

week_iqr <- imap(
  country_weeks,
  function(weeks, country) {
    rt <- rt_samples[rt_samples$model %in% as.Date(weeks) & rt_samples$country == country, ]
    x <- group_by(rt, model) %>%
      summarise_if(
        is.numeric,
        ~ list(
  `2.5%` = quantile(., prob = 0.025),
  `25%` = quantile(., prob = 0.25),
  `50%` = quantile(., prob = 0.50),
  `75%` = quantile(., prob = 0.75),
  `97.5%` = quantile(., prob = 0.975)
  )) %>% ungroup()

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

combined_weighted_estimates <- imap(
  combined_estimates,
  function(combined, country) {
    message(country)
    if (length(combined$weeks_combined) == 1) {
      return(combined)
    }
    df <- rt_samples[rt_samples$model %in% combined$weeks_combined & rt_samples$country == country, ]
    beta <- per_country_beta$beta[per_country_beta$country == country]
    if (length(beta) == 0) beta <- 0
    weights <- exp(-beta * seq(0, length(combined$weeks_combined) - 1))
    weights <- weights/ sum(weights)
    names(weights) <- combined$weeks_combined
    combine_with_previous_weighted(df, weights)
  }
)

across_countries_beta <- across_countries_beta$beta

combined_weighted_estimates2 <- imap(
  combined_estimates,
  function(combined, country) {
    message(country)
    if (length(combined$weeks_combined) == 1) {
      return(combined)
    }
    df <- rt_samples[rt_samples$model %in% combined$weeks_combined & rt_samples$country == country, ]
    beta <- across_countries_beta
    weights <- exp(-beta * seq(0, length(combined$weeks_combined) - 1))
    weights <- weights/ sum(weights)
    names(weights) <- combined$weeks_combined
    combine_with_previous_weighted(df, weights)
  }
)

saveRDS(
  combined_weighted_estimates2, "combined_weighted_estimates_across_countries.rds"
)

saveRDS(
  combined_weighted_estimates, "combined_weighted_estimates_per_country.rds"
)

combined2 <- purrr::keep(
  combined_estimates, ~ length(.$weeks_combined) > 1
)

## Extend this for the days between week_starting2 and
## week_starting1 + another 7 days because you would have continued
## with Rt estimate for another 7 days
f <- function(y) {
  df <- data.frame(
      week_starting1 = min(y$weeks_combined),
      week_starting2 = max(y$weeks_combined),
      `2.5%` = y$combined_rt[["2.5%"]],
      `50%` = y$combined_rt[["50%"]],
      `97.5%` = y$combined_rt[["97.5%"]],
      check.names = FALSE
    )
  df$week_starting1 <- as.Date(df$week_starting1)
  df$week_starting2 <- as.Date(df$week_starting2)
  ndays <- as.numeric(df$week_starting2 - df$week_starting1) + 7
  df <- df[rep(seq_len(nrow(df)), each = ndays), ]
  df$forecast_week <- seq(
    from = df$week_starting1[1] + 1,
    length.out = ndays,
    by = "1 day"
  )
  df
}

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
            from = df$forecast_week[1] + 1,
            length.out = 7,
            by = "1 day"
          )
          df
        }
    )
    unwtd <- f(y)
    weighted_all <- f(combined_weighted_estimates2[[country]])
    weighted_per  <- f(combined_weighted_estimates[[country]])

    weights_all <- data.frame(
      weights = combined_weighted_estimates2[[country]][["frequency"]]
    )
    weights_all <- tibble::rownames_to_column(weights_all, var = "Week Starting")

    weights_per <- data.frame(
      weights = combined_weighted_estimates[[country]][["frequency"]]
    )
    weights_per <- tibble::rownames_to_column(weights_per, var = "Week Starting")
    weights <- left_join(
      weights_per, weights_all, by = "Week Starting"
    )

    colnames(weights)[2:3] <- c(
        "beta across countries", "beta per country"
    )

    ##weights <- mutate_if(weights, is.numeric, ~ round(., 3))
    weights <- gather(
      weights, beta, weight, `beta across countries`:`beta per country`
    ) %>%
      filter(weight > 0) %>%
      spread(`Week Starting`, weight, fill = 0)

    ymax <- ceiling(max(x[["97.5%"]]))

    p1 <- plot_weekly_iqr(x) +
      ##scale_x_date(breaks = unique(x$forecast_week)) +
      coord_cartesian(clip = "off") +
      ylim(0, ymax)

    xmin <- min(x$forecast_week)
    xmax <-max(x$forecast_week)

    unwtd$category <- "Unweighted"
    weighted_per$category <- "Weighted (beta per country)"
    weighted_all$category <- "Weighted (beta across countries)"

    both <- rbind(unwtd, weighted_per, weighted_all)

    ## Where to place the table.
    xtable <- as.Date(xmin) + 2
    ytable <- ymax
    label <- tibble(x = xtable, y = ytable, tb = list(weights))

    ##weeks <- df$week_starting1
    ##weeks <- df$week_starting1[seq(1, length(weeks), by = 2)]

    p2 <- plot_combined_iqr(both) +
      coord_cartesian(clip = "off") +
      ylim(0, ymax) +
      theme(legend.position = "top", legend.title = element_blank()) +
      scale_x_date(date_breaks = "2 weeks", limits = c(xmin, xmax)) +
      geom_table(
        data = label, aes(x = x, y = y, label = tb), size = 1.5
      ) +
      theme(
        axis.text.x.bottom = element_text(
          angle = 90, vjust = 0, hjust = 0.5
        )
      )
    p <- cowplot::plot_grid(p1, p2, ncol = 1, align = "hv")
    p
  }
)

purrr::iwalk(
  plots,
  function(p, country) ggsave(glue::glue("figures/{country}.png"), p)
)


