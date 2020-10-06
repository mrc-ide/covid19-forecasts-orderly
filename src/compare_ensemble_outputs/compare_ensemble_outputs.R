## orderly::orderly_develop_start(parameters = list(use_si = "si_2", latest_week = "2020-09-27"), use_draft = "newer")
### This task produces the following visualtions:
### comparison of unweighted and weighted ensembles for each country
### all forecasts from unweighted ensemble
### all forecasts from weighted ensemble with weights coming from
### last week
### all forecasts from weighted ensemble with weights coming from
###all previous weeks
file_format <- ".tiff"

main_text_countries <- c(
  "Brazil", "India", "Italy", "South_Africa", "United_States_of_America"
)

unweighted_qntls <- readRDS("unweighted_qntls.rds") %>%
  dplyr::filter(si == use_si) %>%
  na.omit()

unweighted_qntls$date <- as.Date(unweighted_qntls$date)

## Days since 100 deaths
model_input <- readRDS("model_input.rds")
model_input <- gather(model_input, country, deaths, -dates)
model_input <- split(
  model_input, model_input$country
) %>%
  map_dfr(
    function(x) {
      message(x$country[1])
      idx <- which(cumsum(x$deaths) >= 100)[1]
      if (is.na(idx)) return(NULL)
      message(idx)
      before_100 <- seq(to = -1, length.out = idx - 1, by = 1)
      since_100 <- seq(from = 0, length.out = nrow(x) - idx + 1, by = 1)
      x$days_since_100_deaths <- c(before_100, since_100)
      x
  }
)
model_input$dates <- as.Date(model_input$dates)
## Performance Metrics
## deaths_tall <- deaths_tall[deaths_tall$country %in% unweighted_qntls$country, ]
## levels <- unique(interaction(both$proj, both$model))
## unwtd <- grep(pattern = "Unweighted", x = levels, value = TRUE)
## wtd <- grep(pattern = "Unweighted", x = levels, value = TRUE, invert TRUE)
## palette <- c(rep("#b067a3", nlevels(levels) / 2),
##              rep("#9c954d", nlevels(levels) / 2))
## names(palette) <- c(unwtd, wtd)

countries <- setNames(
  unique(unweighted_qntls$country), unique(unweighted_qntls$country)
)
## si_countries <- countries[! countries %in% main_text_countries]
## names(si_countries) <- si_countries
## ## Put all main text countries in one list,
## ## all others by themselves.
## countries <- list(main = main_text_countries)
## countries <- append(x = countries, values = unlist(si_countries))
countries$Czech_Republic <- "Czechia"
exclude <- readRDS("exclude.rds")
countries <- countries[!countries %in% exclude]
######################################################################
############## Rt ####################################################
######################################################################
unweighted_rt_qntls <- readRDS("unweighted_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)


purrr::iwalk(
  countries,
  function(country, name) {
    message(paste(country, collapse = " "))
    ## obs <-  unweighted_qntls[unweighted_qntls$country %in% country,
    ##                          c("date", "country" ,"deaths")]
    obs <- model_input[model_input$country %in% country, ]
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    if (nrow(pred) == 0) {
      message("No data for ", country)
      continue
    }
    pred <- rincewind:::cap_predictions(pred)
    pred <- left_join(pred, obs, by = c("date" = "dates"))


    x <- unweighted_rt_qntls[unweighted_rt_qntls$country %in% country, ]
    x <- tidyr::spread(x, quantile, out2)
    out <- split(x, list(x$forecast_date, x$country)) %>%
      purrr::keep(~ nrow(.) > 0) %>%
      purrr::map_dfr(
        function(y) {
          ## repeat each row 7 times (projection horizon)
          y <- y[rep(seq_len(nrow(y)), 7), ]
          y$dates <- seq(
            from = as.Date(y$forecast_date[1]) + 1,
            length.out = 7,
            by = "1 day"
          )
          y
      }
    )
    out <- left_join(out, obs)
    xmax <- max(out$days_since_100_deaths, na.rm = TRUE)
    message(xmax)
    if (xmax < 0) return(NULL)

    p1 <- all_forecasts(obs, pred)
    p1 <- p1 +
      scale_x_continuous(
        breaks = seq(0, xmax, 7),
        limits = c(0, xmax),
        minor_breaks = NULL
    )

    ## Remove x-axis ticks to have them on the bottom panel only
    p1 <- p1 +
      ylab("Daily Deaths") +
      theme_manuscript() +
      theme(
        strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(angle = 90),
        legend.position = "none"
      )

    p2 <- all_restimates_line(out) +
      ylab("Effective Reproduction Number") +
      xlab("Days since 100 deaths")

    p2 <- p2 +
      scale_x_continuous(
        breaks = seq(0, xmax, 7), limits = c(0, xmax), minor_breaks = NULL
      )

    p2 <- p2 +
      theme_manuscript() +
      theme(
        strip.text = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(angle = 90),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )

    p <- cowplot::plot_grid(
      p1, p2, align  = "hv", rel_heights = c(1, 0.6), ncol = 1
    )
    outfile <- glue::glue("{name}_forecasts{file_format}")
    save_multiple(plot = p, filename = outfile)
  }
)








### compare observed with predicted
p <- ggplot(unweighted_qntls, aes(deaths, `50%`)) +
  geom_bin2d(binwidth = 0.1) +
  ## geom_abline(
  ##   slope = 1,
  ##   intercept = c(0, -0.30, 0.17),
  ##   linetype = "dashed",
  ##   col = "red"
  ## ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_fill_continuous(low="lavenderblush", high="red") +
  xlab("(log) Observed deaths") +
  ylab("(log) Median predicted deaths") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off') +
  theme(
    strip.text = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )



plow <- ggplot(unweighted_qntls, aes(deaths, `25%`)) +
  geom_bin2d(binwidth = 0.1) +
  ## geom_abline(
  ##   slope = 1,
  ##   intercept = c(0, -0.30, 0.17),
  ##   linetype = "dashed",
  ##   col = "red"
  ## ) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("(log) Observed Deaths") +
  ylab("(log) Median predicted Deaths") +
  scale_fill_continuous(low="lavenderblush", high="red") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off') +
  theme(
    strip.text = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )


phigh <- ggplot(unweighted_qntls, aes(deaths, `75%`)) +
  geom_bin2d(binwidth = 0.1) +
  ## geom_abline(
  ##   slope = 1,
  ##   intercept = c(0, -0.30, 0.17),
  ##   linetype = "dashed",
  ##   col = "red"
  ## ) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("(log) Observed Deaths") +
  ylab("(log) Median predicted Deaths") +
  scale_fill_continuous(low="lavenderblush", high="red") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off') +
  theme(
    strip.text = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )



ggsave("obs_vs_pred.png", p)
ggsave("obs_vs_pred_25.png", plow)
ggsave("obs_vs_pred_75.png", phigh)
