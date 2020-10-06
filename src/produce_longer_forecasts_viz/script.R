## orderly::orderly_develop_start(use_draft = "newer")
dir.create("figures")


## ggforce::facet_wrap_paginate fails on R 4.0 when the plot
## has less than nrow * ncol facets. Hence having to write my own
## very specific pagination
paginate <- function(p, df, npanels, nrows, ncols) {

  forecast_weeks <- unique(df$forecast_week)
  npages <- ceiling(length(forecast_weeks) / npanels)
  out <- list()
  for (page in 1:npages) {
    message(page)
    idx <- seq(to = page * npanels, length.out = npanels, by = 1)
    idx <- idx[idx <= length(forecast_weeks)]
    weeks <- factor(forecast_weeks[idx])
    df_page <- df[df$forecast_week %in% weeks, ]
    p2 <- p +
      geom_ribbon(
        data = df_page,
        aes(
          x = date, ymin = `2.5%`, ymax = `97.5%`, fill = forecast_week
        ),
        alpha = 0.4
      ) +
      geom_line(
        data = df_page,
        aes(date, `50%`, col = forecast_week), size = 1.2
      ) +
      facet_wrap(
        ~forecast_week, ncol = ncols, nrow = nrows##, scales = "free_y"
      )
    out[[page]] <- p2
  }
  out
}

npanels <- 6
nrows <- 3
ncols <- 2

exclude <- readRDS("exclude.rds")

all_deaths <- readRDS("latest_deaths_wide_no_filter.rds")
all_deaths$Czech_Republic <- all_deaths$Czechia

## First time the number of deaths exceeeded 100.
first_100th <- apply(
  all_deaths[, -1], 2, function(deaths) which(cumsum(deaths) > 100)[1]
)


ps_qntls <- readRDS("ps_qntls.rds")
ps_qntls <- ps_qntls[! ps_qntls$country %in% exclude, ]

ps_bycountry <- split(ps_qntls, ps_qntls$country)
ps_bycountry <- keep(ps_bycountry, ~ nrow(.) > 0)
##ps_bycountry <- split(pred_qntls, pred_qntls$country)

iwalk(
  ps_bycountry,
  function(ps, cntry) {
    ps$date <- as.Date(ps$date)
    forecast_weeks <- unique(ps$forecast_week)
    palette <- alternating_palette(forecast_weeks)
    ps$forecast_week <- factor(ps$forecast_week)

    xmin <- all_deaths$dates[first_100th[[cntry]]]

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
      theme_minimal() +
      scale_y_continuous(limits = c(0, 1)) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      xlab("") +
      ylab("Proportion of population susceptible") +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 6)
      )

    if (length(forecast_weeks) >= 2) {
      p <- p +
        scale_x_date(
          date_breaks = "2 weeks", limits = c(as.Date(xmin), NA)
      )
    }
    outfile <- glue("figures/{cntry}_ps.png")
    message(outfile)
    ggsave(outfile , p)
  }
)

weekly_rt <- readRDS("weekly_rt_qntls.rds")
weekly_rt <- weekly_rt[! weekly_rt$country %in% exclude, ]
weekly_rt_bycountry <- split(weekly_rt, weekly_rt$country)

reff_qntls <- readRDS("reff_qntls.rds")
reff_qntls <- reff_qntls[! reff_qntls$country %in% exclude, ]

reff_bycountry <- split(reff_qntls, reff_qntls$country)
reff_bycountry <- keep(reff_bycountry, ~ nrow(.) > 0)
##reff_bycountry <- split(pred_qntls, pred_qntls$country)
iwalk(
  reff_bycountry,
  function(reff, cntry) {
    reff$date <- as.Date(reff$date)
    forecast_weeks <- unique(reff$forecast_week)
    palette <- alternating_palette(forecast_weeks)
    reff$forecast_week <- factor(reff$forecast_week)

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
      scale_fill_manual(
        values = palette, aesthetics = c("col", "fill")
      ) +
      xlab("") +
      ylab("Effective Reproduction Number") +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 6)
      ) +
      ylim(0, ymax) +
      geom_hline(yintercept = 1, linetype = "dashed")

    if (length(forecast_weeks) >= 2) {
      p <- p +
        scale_x_date(
          date_breaks = "2 weeks", limits = c(as.Date(xmin), NA)
      )
    }
    outfile <- glue("figures/{cntry}_reff.png")
    message(outfile)
    ggsave(outfile , p)
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

    weekly <- weekly_rt_bycountry[[cntry]]
    ## Augment data.frame by adding a row for each day of the week
    weekly <- weekly[weekly$si == "si_2", ]
    weekly <- split(weekly, weekly$forecast_date) %>%
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
    ## Scatter plot
    x <- dplyr::left_join(
      reff, weekly,
      by = c("date"),
      suffix = c("_short", "_long")
    )

    p <- ggplot(x, aes(`50%_short`, `50%_long`)) +
      geom_point() +
      ##ylim(0, ymax) +
      ##xlim(0, ymax) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_minimal() +
      xlab("Weekly Effective Reproduction Number") +
      ylab("Long-term Effective Reproduction Number")  +
      coord_cartesian() +
      expand_limits(x = 0, y = 0)

    outfile <- glue("figures/{cntry}_short_and_long_rt_scatter.png")
    ggsave(outfile , p)


    plots <- map(
      forecast_weeks,
      function(week) {
        long <- reff[reff$forecast_week == week, ]
        short <- weekly[weekly$date %in% long$date, ]
        ymax <- ceiling(max(short$`97.5%`, long$`97.5%`))

        p1 <- ggplot() +
          geom_line(
            data = short, aes(date, `50%`, group = forecast_date),
            col = "#000000"
          ) +
        geom_line(data = long, aes(date, `50%`), col = "#009E73") +
        geom_ribbon(
          data = short,
          aes(
            x = date, ymin = `2.5%`, ymax = `97.5%`, group = forecast_date
          ),
          fill = "#000000", alpha = 0.3
        ) +
        geom_ribbon(
          data = long, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
          fill = "#009E73", alpha = 0.3
        ) +
          theme_minimal() +
          ylim(0, ymax) +
          geom_hline(yintercept = 1, linetype = "dashed") +
          xlab("") +
          ylab("")

        if (nrow(long == 7)) {
          dates <- seq(
            from = min(long$date), length.out = 3, to = max(long$date)
          )
          p1 <- p1 + scale_x_date(breaks = dates)
        } else {
          p1 <- p1 + scale_x_date(date_breaks = "1 week")
        }
        p1
      }
    )
    label <- grid::textGrob("Effective Reproduction Number", rot = 90)
    npages <- ceiling(length(forecast_weeks) / npanels)
    for (page in 1:npages) {
      message(page)
      idx <- seq(to = page * npanels, length.out = npanels, by = 1)
      idx <- idx[idx <= length(forecast_weeks)]
      plist <- plots[idx]
      p <- grid.arrange(grobs = plist, ncol = ncols, left = label)
      outfile <- glue("figures/{cntry}_short_and_long_rt_{page}.png")
      ggsave(outfile , p)
    }
  }
)


## x <- split(
##   pred_qntls,
##   list(pred_qntls$forecast_week, pred_qntls$country),
##   sep = ":"
## )
## x <- purrr::keep(x, ~ nrow(.) > 0)
pred_qntls <- readRDS("longer_projections_qntls.rds")
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

    palette <- alternating_palette(forecast_weeks)

    pred$forecast_week <- factor(pred$forecast_week)
    npages <- ceiling(length(forecast_weeks) / npanels)

    p <- ggplot() +
      geom_point(
        data = obs,
        aes(dates, deaths), col = "#663723", alpha = 0.5, size = 1
      ) +
      scale_fill_manual(values = palette, aesthetics = c("col", "fill")) +
      theme_minimal() +
      scale_x_date(
        date_breaks = "2 weeks", limits = c(as.Date(xmin), NA)
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      xlab("") +
      ylab("Daily Incidence") +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 6)
      )


    label <- glue(
      "Projections for {snakecase::to_title_case(cntry)}"
    )
    p <- p + ggtitle(label)
    plots <- paginate(p, pred, npanels, nrows, ncols)

    iwalk(
      plots,
      function(p2, page) {
        outfile <- glue("figures/{cntry}_{page}.png")
        message(outfile)
        ggsave(outfile , p2)
      }
    )
  }
)
