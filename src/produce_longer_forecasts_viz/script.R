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
ps_qntls <- ps_qntls[!ps_qntls$country %in% exclude, ]

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
  })

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
  forecast_weeks,
  col1 = "#8a7972", col2 = "#3d2115"
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


countries <- setNames(names(ps_bycountry), names(ps_bycountry))

plots <- map(
  countries,
  function(country) {
    pred <- pred_qntls_bycountry[[country]]
    reff <- reff_bycountry[[country]]
    ps <- ps_bycountry[[country]]
    weekly <- weekly_rt_bycountry[[country]]
    map(
      week_groups,
      function(weeks) {
        weeks <- as.Date(weeks)
        pred <- pred[pred$forecast_week %in% weeks, ]
        reff <- reff[as.Date(reff$forecast_week) %in% weeks, ]
        ps <- ps[as.Date(ps$forecast_week) %in% weeks, ]

        obs <- all_deaths[, c("dates", country)]
        obs <- obs[obs$dates >= min(as.Date(pred$date)), ]
        obs <- obs[obs$dates <= max(pred$date) + 7, ]
        obs$deaths <- obs[[cntry]]
        ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
        pred <- dplyr::mutate_if(
          pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
        )

        weekly <- weekly[weekly$date >= min(as.Date(pred$date)), ]
        weekly <- weekly[weekly$date <= max(pred$date) + 7, ]

        pred$forecast_week <- as.factor(pred$forecast_week)
        ps$forecast_week <- as.factor(ps$forecast_week)
        reff$forecast_week <- as.factor(reff$forecast_week)

        p1 <- pred_plot(pred, obs)
        p2 <- reff_weekly_plot(reff, weekly)
        p3 <- ps_plot(ps)

        p <- (p1 + p2 + p3 + plot_layout(ncol = 1))

        p <- p & scale_fill_manual(
          values = palette, aesthetics = c("col", "fill")
        ) &
          scale_x_date(
            date_breaks = date_breaks, date_labels = date_labels
          ) &
          theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 6),
              legend.position = "none",
              axis.text.y = element_text(size = 6)
            )
        ## This is only to check that the dates are correctly aligned
        outfile <- glue("figures/{country}_check.png")
        ggsave(outfile, p)
        ## Get rid of the dates on the top 2 panels
        p1_final <- p1 + theme(axis.text.x = element_blank())
        p2_final <- p2 + theme(axis.text.x = element_blank())

        p_final <- (p1_final + p2_final + p3 + plot_layout(ncol = 1))
        p_final <- p_final & scale_fill_manual(
          values = palette, aesthetics = c("col", "fill")
        ) &
          scale_x_date(
            date_breaks = date_breaks, date_labels = date_labels
          ) &
          theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 6),
              legend.position = "none",
              axis.text.y = element_text(size = 6)
            )

        p_final
      }
    )
  }
)



reff_qntls <- rincewind::assign_epidemic_phase(reff_qntls)
compare_phase <- left_join(
  reff_qntls, weekly_rt,
  by = c("country", "date"),
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
