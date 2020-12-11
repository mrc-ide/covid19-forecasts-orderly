## orderly::orderly_develop_start(use_draft = "newer")
dir.create("figures")

ndays_projected <- 28
nweeks_projected <- 4
### common plot propoerties
####
date_labels <- "%d - %b"
date_breaks <- "4 weeks"
exclude <- readRDS("exclude.rds")

all_deaths <- readRDS("latest_deaths_wide_no_filter.rds")
all_deaths$Czech_Republic <- all_deaths$Czechia

## First time the number of deaths exceeeded 100.
first_100th <- apply(
  all_deaths[, -1], 2, function(deaths) which(cumsum(deaths) > 100)[1]
)

ps_qntls <- readRDS("ps_qntls.rds")
ps_qntls <- ps_qntls[! ps_qntls$country %in% exclude, ]

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
weekly_rt <- weekly_rt[! weekly_rt$country %in% exclude, ]
weekly_rt <-  split(weekly_rt, weekly_rt$country) %>%
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

weekly_rt_bycountry <- split(weekly_rt, weekly_rt$country)

reff_qntls <- readRDS("reff_qntls.rds")
reff_qntls <- reff_qntls[! reff_qntls$country %in% exclude, ]

reff_qntls <- split(
  reff_qntls,
  list(reff_qntls$country, reff_qntls$forecast_week), drop = TRUE
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

pred_qntls$forecast_week <- as.Date(pred_qntls$forecast_week)
pred_qntls <- pred_qntls[!pred_qntls$country %in% exclude, ]

pred_qntls_bycountry <- split(pred_qntls, pred_qntls$country)

forecast_weeks <- unique(ps_qntls$forecast_week)
palette <- alternating_palette(
  forecast_weeks, col1 = "#8a7972", col2 = "#3d2115"
)

saveRDS(ps_qntls, "ps_qntls_filtered.rds")
saveRDS(pred_qntls, "pred_qntls_filtered.rds")
saveRDS(reff_qntls, "reff_qntls_filtered.rds")



## Split all forecast weeks into 4 groups
week_groups <- list()

for (i in seq_len(nweeks_projected)) {
  idx <- seq(from = i, by = nweeks_projected, to = length(forecast_weeks))
  week_groups[[i]] <- forecast_weeks[idx]
}

ps_plots <- imap(
  ps_bycountry,
  function(ps, cntry) {
    ps$date <- as.Date(ps$date)

    ps$forecast_week <- factor(ps$forecast_week)
    plots <- map(
      week_groups,
      function(weeks) {
        ## slightly longer than the largest date
        xmax <- max(as.Date(weeks)) + ndays_projected + 2

        ps_local <- ps[ps$forecast_week %in% weeks, ]
        p <- ggplot(ps_local) +
          geom_ribbon(
            aes(
              x = date, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
            ),
            alpha = 0.4
          ) +
          geom_line(
            aes(date, `50%`, col = forecast_week), size = 1.2
          ) +
          scale_fill_manual(
            values = palette, aesthetics = c("col", "fill")
          ) +
          scale_y_continuous(limits = c(0, 1)) +
          scale_x_date(
            date_breaks = date_breaks, ##, limits = c(xmin, NA),
            date_labels = date_labels
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.text = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 6)
          ) +
          xlab("") +
          ylab("Proportion of population susceptible")

        p
      }
    )
    plots
  }
)

reff_plots <- imap(
  reff_bycountry,
  function(reff, cntry) {
    reff$date <- as.Date(reff$date)
    reff$forecast_week <- factor(reff$forecast_week)
    xmin <- all_deaths$dates[first_100th[[cntry]]]
    ymax <- ceiling(max(reff$`97.5%`))
    plots <- map(
      week_groups,
      function(weeks) {
        reff_local <- reff[reff$forecast_week %in% weeks, ]
        p <- ggplot(reff_local) +
          geom_ribbon(
            aes(
              x = date, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
            ),
            alpha = 0.4
          ) +
          geom_line(
            aes(date, `50%`, col = forecast_week), size = 1.2
          ) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          ylim(0, ymax) +
          scale_fill_manual(
            values = palette, aesthetics = c("col", "fill")
          ) +
          xlab("") +
          ylab("Effective Reproduction Number") +
          scale_x_date(
            date_breaks = date_breaks, date_labels = date_labels
          ) +
          theme_minimal() +
          theme(
            legend.position = "none",
            axis.text.y = element_text(size = 6),
            strip.text = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
        p
      }
    )
    plots
  }
)


reff_weekly_plots <- imap(
  reff_bycountry,
  function(reff, cntry) {
    message(cntry)
    reff$date <- as.Date(reff$date)
    reff$forecast_week <- factor(reff$forecast_week)
    weekly <- weekly_rt_bycountry[[cntry]]
    weekly <- weekly[weekly$si == "si_2", ]
    ymax <- ceiling(max(reff$`97.5%`))
    plots <- map(
      week_groups,
      function(weeks) {
        reff_local <- reff[reff$forecast_week %in% weeks, ]
        weekly_local <- weekly[weekly$date %in% reff_local$date, ]

        p1 <- ggplot() +
          geom_line(
            data = weekly_local,
            aes(date, `50%`, group = forecast_date), col = "#000000"
          ) +
          geom_ribbon(
            data = weekly_local,
            aes(
              x = date, ymin = `2.5%`, ymax = `97.5%`, group = forecast_date
            ), fill = "#000000", alpha = 0.3
          ) +
          geom_line(
            data = reff_local,
            aes(date, `50%`, group = forecast_week),
            col = "#009E73") +
        geom_ribbon(
          data = reff_local,
          aes(
            x = date, ymin = `2.5%`, ymax = `97.5%`,
            group = forecast_week
          ),
          fill = "#009E73", alpha = 0.3
        ) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          scale_x_date(
            date_breaks = date_breaks, date_labels = date_labels
          ) +
        ylim(0, ymax) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.text = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 6)
          ) +
          xlab("") +
          ylab("Effective Reproduction Number")

        p1
      }
    )
    plots
  }
)


pred_plots <- imap(
  pred_qntls_bycountry,
  function(pred, cntry) {
    message(cntry)
    pred$forecast_week <- factor(pred$forecast_week)
    pred$date <- as.Date(pred$date)
    obs <- all_deaths[, c("dates", cntry)]
    obs <- obs[obs$dates <= max(pred$date) + 7, ]
    obs$deaths <- obs[[cntry]]
    ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
    pred <- dplyr::mutate_if(
      pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
    )
    xmin <- min(pred$date)
    plots <- map(
      week_groups, function(weeks) {
        pred_local <- pred[pred$forecast_week %in% weeks, ]
        xmax <- max(pred_local$date)
        p <- ggplot() +
          geom_point(
            data = obs, aes(dates, deaths), alpha = 0.5, size = 1
          ) +
          geom_ribbon(
            data = pred_local,
            aes(
              x = date, ymin = `2.5%`, ymax = `97.5%`,
              fill = forecast_week
            ),
            alpha = 0.4
          ) +
          geom_line(
            data = pred_local,
            aes(date, `50%`, col = forecast_week), size = 1.2
          ) +
          scale_fill_manual(
            values = palette, aesthetics = c("col", "fill")
          ) +
          scale_x_date(
            date_breaks = date_breaks, date_labels = date_labels,
            limits = c(xmin, xmax)
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            strip.text = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(size = 6)
          ) +
          xlab("") +
          ylab("Daily Incidence")

        p
      }
    )
    plots
  }
)


### Now put them together

together <- pmap(
  list(top = pred_plots, middle = ps_plots, bottom = reff_weekly_plots),
  function(top, middle, bottom) {
    map(
      seq_along(week_groups), function(index) {
        p1 <- top[[index]]
        p2 <- middle[[index]]
        p3 <- bottom[[index]]
        cowplot::plot_grid(p1, p2, p3, ncol = 1,
                           rel_heights = c(1, 0.5, 0.5))
      }
    )
  }
)


reff_qntls <- rincewind::assign_epidemic_phase(reff_qntls)
compare_phase <- left_join(
  reff_qntls, weekly_rt, by = c("country", "date"),
  suffix = c("_eff", "_weekly")
)
compare_phase <- na.omit(compare_phase)

saveRDS(compare_phase, "compare_phase_weekly_effevtive.rds")

compare_phase$flag <- case_when(
  compare_phase$phase_weekly == compare_phase$phase_eff ~ "same",
  TRUE ~ "different"
)

country_groups <- readRDS("country_groups.rds")
x <- compare_phase[compare_phase$country %in% country_groups[[1]], ]
x$country <- rincewind::nice_country_name(x$country)

ggplot(x, aes(date, country, fill = flag)) +
  geom_tile() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("") +
  scale_x_date(
    date_breaks = date_breaks, date_labels = "%b - %Y"
  )
