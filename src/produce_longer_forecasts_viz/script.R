## orderly::orderly_develop_start(use_draft = "newer")
dir.create("figures")

ndays_projected <- 28
nweeks_projected <- ndays_projected / 7
### common plot propoerties
####
date_labels <- "%d - %b"
date_breaks <- "4 weeks"
date_limits <- c(as.Date("2020-03-01"), NA)
exclude <- readRDS("exclude.rds")

all_deaths <- readRDS("latest_deaths_wide_no_filter.rds")
all_deaths$Czech_Republic <- all_deaths$Czechia

## First time the number of deaths exceeeded 100.
first_100th <- apply(
  all_deaths[, -1], 2, function(deaths) which(cumsum(deaths) > 100)[1]
)

ps_qntls <- readRDS("ps_qntls.rds")
ps_qntls <- ps_qntls[!ps_qntls$country %in% exclude, ]
forecast_weeks <- unique(ps_qntls$forecast_week)

ps_qntls <- split(
  ps_qntls,
  list(ps_qntls$country, ps_qntls$forecast_week),
  drop = TRUE
) %>%
  map_dfr(
    function(df) {
      df$date <- as.Date(df$date)
      df <- df[order(df$date), ]
      df$day <- seq_len(nrow(df))
      df <- df[df$day <= ndays_projected, ]
      df
    }
  )

ps_bycountry <- split(ps_qntls, ps_qntls$country, drop = TRUE)

weekly_rt <- readRDS("weekly_rt_qntls.rds")
weekly_rt <- weekly_rt[weekly_rt$si == "si_2", ]
weekly_rt <- weekly_rt[!weekly_rt$country %in% exclude, ]

weekly_rt <- split(weekly_rt, weekly_rt$country) %>%
  map_dfr(function(weekly) {
    split(weekly, weekly$forecast_date) %>%
      map_dfr(
        function(df) {
          df <- spread(df, quantile, out2)
          df <- df[rep(seq_len(nrow(weekly)), each = 7), ]
          df$date <- seq(
            from = as.Date(df$forecast_date[1]) + 1,
            length.out = 7,
            by = "1 day"
          )
          df
        }
      )
  }
)

saveRDS(weekly_rt, "weekly_rt_augmented.rds")

weekly_rt_bycountry <- split(weekly_rt, weekly_rt$country)

reff_qntls <- readRDS("reff_qntls.rds")
reff_qntls <- reff_qntls[!reff_qntls$country %in% exclude, ]

reff_qntls <- split(
  reff_qntls,
  list(reff_qntls$country, reff_qntls$forecast_week),
  drop = TRUE
) %>%
  map_dfr(
    function(df) {
      df$date <- as.Date(df$date)
      df <- df[order(df$date), ]
      df$day <- seq_len(nrow(df))
      df <- df[df$day <= ndays_projected, ]
      df
    }
  )

reff_bycountry <- split(reff_qntls, reff_qntls$country, drop = TRUE)

pred_qntls <- readRDS("longer_projections_qntls.rds")
pred_qntls$forecast_week <- as.Date(pred_qntls$forecast_week)
pred_qntls <- pred_qntls[!pred_qntls$country %in% exclude, ]

pred_qntls <- split(
  pred_qntls,
  list(pred_qntls$country, pred_qntls$forecast_week),
  drop = TRUE
) %>%
  map_dfr(
    function(df) {
      df$date <- as.Date(df$date)
      df <- df[order(df$date), ]
      df$day <- seq_len(nrow(df))
      df <- df[df$day <= ndays_projected, ]
      df
    }
  )


pred_qntls_bycountry <- split(pred_qntls, pred_qntls$country)



saveRDS(ps_qntls, "ps_qntls_filtered.rds")
saveRDS(pred_qntls, "pred_qntls_filtered.rds")
saveRDS(reff_qntls, "reff_qntls_filtered.rds")



## Split all forecast weeks into 4 groups
week_groups <- list()

for (i in seq_len(nweeks_projected)) {
  idx <- seq(from = i, by = nweeks_projected, to = length(forecast_weeks))
  week_groups[[i]] <- forecast_weeks[idx]
}

names(week_groups) <- seq_along(week_groups)

countries <- setNames(names(ps_bycountry), names(ps_bycountry))
xmin <- min(as.Date("2020-03-01"))

augm_data <- map(
  countries,
  function(country) {
    message(country)
    pred <- pred_qntls_bycountry[[country]]
    reff <- reff_bycountry[[country]]
    weekly <- weekly_rt_bycountry[[country]]
    weekly$dates <- weekly$date
    obs <- all_deaths[, c("dates", country)]
    obs$deaths <- obs[[country]]
    ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
    imap(
      week_groups,
      function(weeks, index) {
        weeks <- as.Date(weeks)
        pred <- pred[pred$forecast_week %in% weeks, ]
        reff <- reff[as.Date(reff$forecast_week) %in% weeks, ]

        obs_local <- obs[obs$dates >= xmin, ]
        obs_local <- obs_local[obs_local$dates <= max(pred$date) + 7, ]

        pred <- dplyr::mutate_if(
          pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
        )

        weekly <- weekly[weekly$date >= min(as.Date(pred$date)), ]
        weekly <- weekly[weekly$date <= max(pred$date) + 7, ]

        pred$forecast_week <- as.factor(pred$forecast_week)
        reff$forecast_week <- as.factor(reff$forecast_week)

        list(
          pred = pred, reff = reff, weekly_rt = weekly,
          obs_local = obs_local
        )
      }
    )
  }
)


main_text_countries <- c(
  "Brazil", "India", "Italy", "South_Africa",
  "Turkey", "United_States_of_America"
)
names(main_text_countries) <- c(
  "Brazil", "India", "Italy", "South Africa",
  "Turkey", "USA"
)

stacked_vars <- map(
  main_text_countries, function(country) {
    pred <- augm_data[[country]][[1]][["pred"]]
    rsat <- augm_data[[country]][[1]][["reff"]]
    rweekly <- augm_data[[country]][[1]][["weekly_rt"]]
    rsat <- select(
      rsat, forecast_date = forecast_week, date,
      ymin = `2.5%`, y = `50%`, ymax = `97.5%`
    )

    rweekly <- select(
      rweekly, forecast_date,
      date, ymin = `2.5%`, y = `50%`, ymax = `97.5%`
    )

    pred <- select(
      pred, forecast_date = forecast_week, date,
      ymin = `2.5%`, y = `50%`, ymax = `97.5%`
    )


    rsat$var <- "rt"
    rweekly$var <- "rt"
    pred$var <- "forecasts"

    rsat$fill <- "#009E73"
    rsat$color <- rsat$fill

    pred$fill <- "#009E73"
    pred$color <- pred$fill

    rweekly$fill <- "#000000"
    rweekly$color <- rweekly$fill


    list(x = rbind(rsat, pred), rweekly = rweekly)
  }
)



stacked_plots <- imap(
  main_text_countries,
  function(country, name) {
    obs <- augm_data[[country]][[1]][["obs_local"]]
    obs <- select(obs, date = dates, y = deaths)
    obs$var <- "forecasts"
    obs$color <- "#666666"

    x <- stacked_vars[[name]][["x"]]
    rweekly <- stacked_vars[[name]][["rweekly"]]

    p <- ggplot(x) +
      geom_line(aes(date, y, group = forecast_date, col = color)) +
      geom_ribbon(
        aes(
          date, ymin = ymin, ymax = ymax,
          group = forecast_date, fill = fill
        ), alpha = 0.3
      ) +
      geom_line(
        data = rweekly,
        aes(date, y, group = forecast_date, col = color)
      ) +
      geom_ribbon(
        data = rweekly,
        aes(
          date, ymin = ymin, ymax = ymax,
          group = forecast_date, fill = fill
        ), alpha = 0.3
      ) +
      geom_point(
        data = obs, aes(date, y, col = color),
        alpha = 0.1, size = 0.8
      ) +
      scale_fill_identity(
        breaks = c("#009E73", "#000000"),
        labels = scales::parse_format()(c(deparse(bquote("Forecasts/"~R^S)), bquote(R^{curr}))),
        guide = guide_legend(order = 2)
      ) +
      scale_color_identity(
        breaks = '#666666',
        labels = "Observed deaths",
        guide = guide_legend(order = 1)
      ) +
      geom_hline(
        data = data.frame(var = "rt", y = 1), aes(yintercept = y),
        linetype = "dashed", col = "red"
      ) +
      facet_wrap(
        ~var, nrow = 2, scales = "free_y",
        strip.position = "left",
        labeller = as_labeller(
          c(forecasts = "Daily Deaths", rt = "Rt")
        )
      )  +
      scale_x_date(
        date_breaks = date_breaks, date_labels = date_labels,
        limits = date_limits
      ) +
        expand_limits(y = 0) +
      ggtitle(name) +
      theme_manuscript() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    rincewind::save_multiple(p, glue::glue("figures/{name}"))
    p
 }
)

legend <- cowplot::get_legend(stacked_plots[[1]] + theme(legend.box = "horizontal"))

nolegend_plots <- imap(
  stacked_plots, function(p, index) {
    p <- p + theme(legend.position = "none")
    if (index %in% c("Brazil", "India", "Italy")) {
      p <- p +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(
            t = 0, r = 10, b = -10, l = 0, unit = "pt"
          )
        )
    } else {
      p <- p + theme(axis.text.x = element_text(angle = 90))
    }
    if (!index %in% c("Brazil", "South Africa")) {
      message("Getting rid of axis title ", index)
      p <- p +
        theme(
          strip.text = element_blank(),
          plot.margin = margin(
            t = -10, r = 10, b = 0, l = 0, unit = "pt"
          )
        )
    }
    p
  }
)

pbottom <- cowplot::plot_grid(plotlist = nolegend_plots, nrow = 2, align = "hv", axis = "l")
final <- cowplot::plot_grid(legend, pbottom, nrow = 2, rel_heights = c(0.1, 1))
rincewind::save_multiple(final, "figures/main_long_forecasts")






