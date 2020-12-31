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

pred_plots <- map_depth(
  augm_data, 2, function(df_list) {
    if (nrow(df_list[["pred"]]) == 0) return(NULL)
    all_forecasts_calendar(
      df_list[["obs_local"]], df_list[["pred"]], date_breaks,
      date_labels, forecast_week, xmin
    ) + theme(legend.title = element_blank())
  }
)

reff_plots <- map_depth(
  augm_data, 2, function(df_list) {
    if (nrow(df_list[["reff"]]) == 0) return(NULL)
    reff_weekly_plot(df_list[["reff"]], df_list[["weekly_rt"]])
  }
)


with_dates <- map(
  countries,
  function(country) {
    p1_list <- pred_plots[[country]]
    p2_list <- reff_plots[[country]]
    ##p3_list <- ps_plots[[country]]
    map(
      seq_along(p1_list),
      function(index) {
        message(country)
        message(index)
        p1 <- p1_list[[index]]
        p2 <- p2_list[[index]]
        if (is.null(p1) | is.null(p2)) return(NULL)
        ##p3 <- p3_list[[index]]
        p <- (p1 + p2 + plot_layout(ncol = 1))

        p <- p &
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 6),
            legend.position = "none",
        axis.text.y = element_text(size = 6)
        )
        ## This is only to check that the dates are correctly aligned
        outfile <- glue("figures/{country}_{index}_check.png")
        ggsave(outfile, p)
        p
      }
    )
  }
)


file_format <- ".tiff"

main_text_countries <- c(
  "Brazil", "India", "Italy", "South_Africa", "Turkey",
  "United_States_of_America"
)

### Intricate assempbly of main text plot
### column 1
legend <- get_legend(
  pred_plots[["Brazil"]][[1]] + theme(legend.box = "horizontal")
)

p11 <- pred_plots[["Brazil"]][[1]] +
  ggtitle("Brazil") +
  theme(axis.text.x = element_blank())

p21 <- reff_plots[["Brazil"]][[1]]

p12 <- pred_plots[["India"]][[1]] + ggtitle("India") +
    theme(axis.text.x = element_blank())
p22 <- reff_plots[["India"]][[1]]

p13 <- pred_plots[["Italy"]][[1]] + ggtitle("Italy") +
    theme(axis.text.x = element_blank())
p23 <- reff_plots[["Italy"]][[1]]

p14 <- pred_plots[["South_Africa"]][[1]] + ggtitle("South Africa") +
    theme(axis.text.x = element_blank())

p24 <- reff_plots[["South_Africa"]][[1]]

p15 <- pred_plots[["Turkey"]][[1]] + ggtitle("Turkey") +
    theme(axis.text.x = element_blank())
p25 <- reff_plots[["Turkey"]][[1]]

p16 <- pred_plots[["United_States_of_America"]][[1]] +
  ggtitle("USA") +   theme(axis.text.x = element_blank())
p26 <- reff_plots[["United_States_of_America"]][[1]]

label1 <- textGrob("Daily deaths", rot = 90, gp = gpar(fontsize = 7))
label2 <- textGrob(
  "Reproduction Number", rot = 90, gp = gpar(fontsize = 7)
)

top <- p11 + p12 + p13 +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() &
  theme(
    text = element_text(size = 6), axis.text.x = element_blank(),
    legend.position = "none", axis.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

bottom <- (p21 + p22 + p23) +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() +
  theme(
    text = element_text(size = 6),
    axis.title = element_blank(), legend.position = "none",
    axis.text.x = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

top <- wrap_elements(label1) + wrap_elements(top) +
  plot_layout(ncol = 2, widths = c(0.03, 1))

bottom <- wrap_elements(label2) + wrap_elements(bottom) +
  plot_layout(ncol = 2, widths = c(0.03, 1))

ptop <- cowplot::plot_grid(legend, top, bottom,
                        rel_heights = c(0.1, 1, 0.6), nrow = 3)

top <- p14 + p15 + p16 +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() &
  theme(
    text = element_text(size = 6),
    axis.text.x = element_blank(), legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

bottom <- (p24 + p25 + p26) +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() +
  theme(
    text = element_text(size = 6),
    axis.title = element_blank(), legend.position = "none",
    axis.text.x = element_text(angle = 90),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

top <- wrap_elements(label1) + wrap_elements(top) +
  plot_layout(ncol = 2, widths = c(0.03, 1))

bottom <- wrap_elements(label2) + wrap_elements(bottom) +
  plot_layout(ncol = 2, widths = c(0.03, 1))

pbottom <- cowplot::plot_grid(top, bottom, rel_heights = c(1, 0.6),
                              nrow = 2)

final <- cowplot::plot_grid(ptop, pbottom, nrow = 2)

rincewind::save_multiple(final, "figures/main_long_forecasts.tiff")



######################################################################
######### Compare phase assigned by the two Rts ######################
######################################################################
reff_qntls <- rincewind::assign_epidemic_phase(reff_qntls)

compare_phase <- dplyr::left_join(
  reff_qntls, weekly_rt,
  by = c("country", "date"),
  suffix = c("_eff", "_weekly")
)

##compare_phase <- na.omit(compare_phase)

saveRDS(compare_phase, "compare_phase_weekly_effevtive.rds")

compare_phase$flag <- case_when(
  compare_phase$phase_weekly == compare_phase$phase_eff ~ "same",
  TRUE ~ "different"
)

country_groups <- readRDS("country_groups.rds")

palette <- c(
  decline = "018571", growing = "#a6611a", unclear = "#dfc27d",
  `stable/growing slowly` = "#80cdc1"
)


plots <- map(
  country_groups,
  function(countries) {
    x <- compare_phase[compare_phase$country %in% countries, ]
    x$country <- rincewind::nice_country_name(x$country)
    ##x <- select(x, country, date, phase_eff, phase_weekly)
    x$week_of_forecast <- case_when(
      x$day <= 7 ~ "Week 1",
      7 < x$day & x$day <= 14 ~ "Week 2",
      14 < x$day & x$day <= 21 ~ "Week 3",
      21 < x$day  ~ "Week 4"
    )

    ggplot(x) +
      geom_line(
        aes(date, country, col = phase_eff), size = 1.2,
        position = position_nudge(x = 0, y = -0.2)
      ) +
      geom_line(
        aes(date, country, col = phase_weekly),
        size = 1.2, linetype = "dashed",
        position = position_nudge(x = 0, y = 0.2)
      ) +
      scale_color_manual(values = palette) +
      scale_x_date(
        date_breaks = date_breaks, date_labels = date_labels
      ) +
      facet_wrap(~week_of_forecast, ncol = 2) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank()
      )
  }
)

iwalk(plots, function(p, index) {
  outfile <- glue("compare_phase_{index}.tiff")
  rincewind::save_multiple(p, outfile)
})

