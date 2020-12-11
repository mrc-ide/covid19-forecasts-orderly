## orderly::orderly_develop_start(use_draft = "newer")
dir.create("figures")

ndays_projected <- 28
nweeks_projected <- 4

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


saveRDS(ps_qntls, "ps_qntls_filtered.rds")

ps_bycountry <- split(ps_qntls, ps_qntls$country, drop = TRUE)

##ps_bycountry <- split(pred_qntls, pred_qntls$country)

iwalk(
  ps_bycountry,
  function(ps, cntry) {
    ps$date <- as.Date(ps$date)
    forecast_weeks <- unique(ps$forecast_week)
    palette <- alternating_palette(forecast_weeks)
    ps$forecast_week <- factor(ps$forecast_week)
    ps$flag <- rep(
      seq_len(nweeks_projected), each = ndays_projected,
      length.out = nrow(ps)
    )
    xmin <- all_deaths$dates[first_100th[[cntry]]]

    palette <- alternating_palette(
      forecast_weeks, col1 = "#8a7972", col2 = "#3d2115"
    )

    p <- ggplot(ps) +
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
        date_breaks = "4 weeks", limits = c(as.Date(xmin), NA),
        date_labels = "%b - %Y"
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

    outfile <- glue("figures/{cntry}_ps.png")
    message(outfile)
    ggsave(outfile , p)
  }
)

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

saveRDS(reff_qntls, "reff_qntls_filtered.rds")

reff_bycountry <- split(reff_qntls, reff_qntls$country, drop = TRUE)


iwalk(
  reff_bycountry,
  function(reff, cntry) {
    reff$date <- as.Date(reff$date)
    forecast_weeks <- unique(reff$forecast_week)
    palette <- alternating_palette(forecast_weeks)
    reff$forecast_week <- factor(reff$forecast_week)
    reff$flag <- rep(seq_len(nweeks_projected), each = ndays_projected, length.out = nrow(reff))
    xmin <- all_deaths$dates[first_100th[[cntry]]]
    ymax <- ceiling(max(reff$`97.5%`))

    p <- ggplot(reff) +
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
        date_breaks = "4 weeks", limits = c(as.Date(xmin), NA),
        date_labels = "%b-%Y"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 6),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )

    outfile <- glue("figures/{cntry}_reff_unfacetted.png")
    message(outfile)
    ggsave(outfile , p)

    p2 <- p + facet_wrap(~flag, ncol = 1)
    outfile <- glue("figures/{cntry}_reff.png")
    message(outfile)
    ggsave(outfile , p2)

    p3 <- p +
      ggforce::facet_wrap_paginate(~flag, nrow = 1, ncol = 1, page = 1)

    pages <- ggforce::n_pages(p3)

    for (page in seq_len(pages)) {
      p3 <- p +
        ggforce::facet_wrap_paginate(~flag, nrow = 1, ncol = 1, page = page)

      outfile <- glue("figures/{cntry}_reff_{page}.png")
      message(outfile)
      ggsave(outfile , p3)
    }
  }
)


iwalk(
  reff_bycountry,
  function(reff, cntry) {
    message(cntry)
    reff$date <- as.Date(reff$date)
    forecast_weeks <- unique(reff$forecast_week)
    palette <- alternating_palette(forecast_weeks)
    reff$forecast_week <- factor(reff$forecast_week)
    reff$flag <- rep(seq_len(nweeks_projected), each = ndays_projected, length.out = nrow(reff))
    weekly <- weekly_rt_bycountry[[cntry]]
    weekly <- weekly[weekly$si == "si_2", ]
    ymax <- ceiling(max(reff$`97.5%`))
    p1 <- ggplot() +
      geom_line(
        data = weekly, aes(date, `50%`, group = forecast_date),
        col = "#000000"
      ) +
    geom_ribbon(
      data = weekly,
      aes(
        x = date, ymin = `2.5%`, ymax = `97.5%`, group = forecast_date
      ),
      fill = "#000000", alpha = 0.3
    ) +
      geom_line(
        data = reff, aes(date, `50%`, group = forecast_week),
        col = "#009E73") +
      geom_ribbon(
        data = reff,
        aes(x = date, ymin = `2.5%`, ymax = `97.5%`, group = forecast_week),
        fill = "#009E73", alpha = 0.3
      ) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        ylim(0, ymax) +
        theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 6)
      ) +
      xlab("") +
        ylab("Effective Reproduction Number") +
        facet_wrap(~flag, ncol = 1)

    outfile <- glue("figures/{cntry}_short_and_long_rt.png")
    ggsave(outfile , p1)
  }
)


## x <- split(
##   pred_qntls,
##   list(pred_qntls$forecast_week, pred_qntls$country),
##   sep = ":"
## )
## x <- purrr::keep(x, ~ nrow(.) > 0)
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

saveRDS(pred_qntls, "pred_qntls_filtered.rds")

pred_qntls$forecast_week <- as.Date(pred_qntls$forecast_week)
pred_qntls <- pred_qntls[!pred_qntls$country %in% exclude, ]

x <- split(pred_qntls, pred_qntls$country)


purrr::iwalk(
  x,
  function(pred, cntry) {
    message(cntry)
    pred <- pred[order(pred$forecast_week), ]
    pred$date <- as.Date(pred$date)
    forecast_weeks <- unique(pred$forecast_week)
    pred$flag <- rep(seq_len(nweeks_projected), each = ndays_projected, length.out = nrow(pred))

    obs <- all_deaths[, c("dates", cntry)]
    obs <- obs[obs$dates <= max(pred$date) + 7, ]
    obs$deaths <- obs[[cntry]]
    week_ending <- max(pred$forecast_week)
    ymax <- 2 * ceiling(max(obs$deaths, na.rm = TRUE) / 10) * 10
    ##pred$val[pred$val > ymax] <- NA
    pred <- dplyr::mutate_if(
      pred, is.numeric, ~ ifelse(.x > ymax, ymax, .x)
    )

    xmin <- obs$dates[first_100th[[cntry]]]



    pred$forecast_week <- factor(pred$forecast_week)

    palette <- alternating_palette(forecast_weeks)

    p <- ggplot() +
      geom_point(
        data = obs,
        aes(dates, deaths), col = "#663723", alpha = 0.5, size = 1
      ) +
      geom_ribbon(
        data = pred,
        aes(
          x = date, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
        ),
        alpha = 0.4
      ) +
      geom_line(
        data = pred, aes(date, `50%`, col = forecast_week), size = 1.2
      ) +
      scale_fill_manual(values = palette, aesthetics = c("col", "fill")) +
      scale_x_date(
        date_breaks = "4 weeks", limits = c(as.Date(xmin), NA),
        date_labels = "%b - %Y"
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

    label <- glue(
      "Projections for {snakecase::to_title_case(cntry)}"
    )
    ##p <- p + ggtitle(label)
    p2 <- p + facet_wrap(~flag, ncol = 1, scales = "free_y")
    outfile <- glue("figures/{cntry}.png")
    message(outfile)
    ggsave(outfile , p)

    p3 <- p +
      ggforce::facet_wrap_paginate(~flag, nrow = 1, ncol = 1, page = 1)

    pages <- ggforce::n_pages(p3)

    for (page in seq_len(pages)) {
      p3 <- p +
        ggforce::facet_wrap_paginate(~flag, nrow = 1, ncol = 1, page = page)

      outfile <- glue("figures/{cntry}_projections_{page}.png")
      message(outfile)
      ggsave(outfile , p3)
    }
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
    date_breaks = "4 weeks", date_labels = "%b - %Y"
  )
