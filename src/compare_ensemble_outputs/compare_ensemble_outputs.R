## orderly::orderly_develop_start(parameters = list(use_si = "si_2"))
### This task produces the following visualtions:
### comparison of unweighted and weighted ensembles for each country
### all forecasts from unweighted ensemble
### all forecasts from weighted ensemble with weights coming from
### last week
### all forecasts from weighted ensemble with weights coming from
###all previous weeks
file_format <- ".png"

main_text_countries <- c(
  "Brazil", "India", "Italy", "South_Africa", "United_States_of_America"
)

unweighted_qntls <- readRDS("unweighted_qntls.rds") %>%
  dplyr::filter(si == use_si) %>%
  na.omit()

wtd_prev_week <- readRDS("wtd_prev_week_qntls.rds") %>%
  dplyr::filter(si == use_si) %>%
  na.omit()

wtd_all_prev_weeks <- readRDS("wtd_all_prev_weeks_qntls.rds") %>%
  dplyr::filter(si == use_si) %>%
  na.omit()

unweighted_qntls$date <- as.Date(unweighted_qntls$date)
wtd_prev_week$date <- as.Date(wtd_prev_week$date)
wtd_all_prev_weeks$date <- as.Date(wtd_all_prev_weeks$date)


## Performance Metrics



## deaths_tall <- deaths_tall[deaths_tall$country %in% unweighted_qntls$country, ]
## levels <- unique(interaction(both$proj, both$model))
## unwtd <- grep(pattern = "Unweighted", x = levels, value = TRUE)
## wtd <- grep(pattern = "Unweighted", x = levels, value = TRUE, invert TRUE)
## palette <- c(rep("#b067a3", nlevels(levels) / 2),
##              rep("#9c954d", nlevels(levels) / 2))
## names(palette) <- c(unwtd, wtd)

countries <- unique(unweighted_qntls$country)
si_countries <- countries[! countries %in% main_text_countries]
names(si_countries) <- si_countries
## Put all main text countries in one list,
## all others by themselves.
countries <- list(main = main_text_countries)
countries <- append(x = countries, values = unlist(si_countries))
countries$Czech_Republic <- "Czechia"

######################################################################
############## Rt ####################################################
######################################################################
unweighted_rt_qntls <- readRDS("unweighted_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)

wtd_prev_week_rt_qntls <- readRDS("wtd_prev_week_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)

wtd_rt_all_prev_week_qntls <- readRDS("wtd_rt_all_prev_week_qntls.rds") %>%
    dplyr::filter(si == use_si)

purrr::iwalk(
  countries,
  function(cntry, name) {
    x <- unweighted_rt_qntls[unweighted_rt_qntls$country %in% cntry, ]
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
    p <- all_restimates_line(out)
    outfile <- glue::glue("{name}_restimates_line_unwtd{file_format}")
    ggsave(outfile, p)
  }
)


purrr::iwalk(
  countries,
  function(cntry, name) {
    message(paste(cntry, collapse = " "))
    x <- wtd_prev_week_rt_qntls[wtd_prev_week_rt_qntls$country %in% cntry, ]
    if (nrow(x) == 0) {
      message("No data for ", cntry)
      return()
    }
    x <- tidyr::spread(x, quantile, out2)
    out <- split(x, list(x$forecast_date, x$country)) %>%
      purrr::keep(~ nrow(.) > 0) %>%
      purrr::map_dfr(
        function(y) {
          ## repeat each row 7 times (projection horizon)
          y <- y[rep(seq_len(nrow(y)), 7), ]
          y$dates <- seq(
            from = as.Date(y$forecast_date[1]),
            length.out = 7,
            by = "1 day"
          )
          y
      }
    )
    p <- all_restimates_line(out)
    outfile <- glue::glue("{name}_restimates_line_wtd_prev_week{file_format}")
    ggsave(outfile, p)
  }
)

purrr::iwalk(
  countries,
  function(cntry, name) {
    x <- wtd_rt_all_prev_week_qntls[wtd_rt_all_prev_week_qntls$country %in% cntry, ]
    if (nrow(x) == 0) {
      message("No data for ", cntry)
      return()
    }
    x <- tidyr::spread(x, quantile, out2)
    out <- split(x, list(x$forecast_date, x$country)) %>%
      purrr::keep(~ nrow(.) > 0) %>%
      purrr::map_dfr(
        function(y) {
          ## repeat each row 7 times (projection horizon)
          y <- y[rep(seq_len(nrow(y)), 7), ]
          y$dates <- seq(
            from = as.Date(y$forecast_date[1]),
            length.out = 7,
            by = "1 day"
          )
          y
        }
        )
    p <- all_restimates_line(out)
    outfile <- glue::glue("{name}_restimates_line_wtd_rt_all_prev_week{file_format}")
    ggsave(outfile, p)
  }
)

purrr::iwalk(
  countries,
  function(country, name) {
    message(paste(country, collapse = " "))
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country,
                             c("date", "country" ,"deaths")]
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    if (nrow(pred) == 0) {
      message("No data for ", country)
      continue
    }
    pred <- rincewind::cap_predictions(pred)

    p <- all_forecasts(obs, pred)
    outfile <- glue::glue("{name}_forecasts{file_format}")
    ggsave(outfile, p)
  }
)

purrr::iwalk(
  countries,
  function(country, name) {
    message(paste(country, collapse = " "))
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- wtd_prev_week[wtd_prev_week$country %in% country, ]
    if (nrow(pred) == 0) {
      message("No data for ", country)
      return()
    }
    pred <- rincewind::cap_predictions(pred)
    p <- all_forecasts(obs, pred)
    outfile <- glue::glue("{name}_forecasts_wtd_prev_week{file_format}")
    ggsave(outfile, p)
  }
)

purrr::iwalk(
  countries,
  function(country, name) {
    message(paste(country, collapse = " "))
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- wtd_all_prev_weeks[wtd_all_prev_weeks$country %in% country, ]
    if (nrow(pred) == 0) {
      message("No data for ", country)
      return()
    }
    pred <- rincewind::cap_predictions(pred)
    p <- all_forecasts(obs, pred)
    outfile <- glue::glue("{name}_forecasts_wtd_all_prev_weeks{file_format}")
    ggsave(outfile, p)
  }
)



countries <- unique(wtd_prev_week$country)

purrr::walk(
  countries,
  function(country) {
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    if (nrow(pred) == 0) {
      message("No data for ", country)
      return()
    }
    pred <- rincewind::cap_predictions(pred)

    p <- ggplot() +
      geom_point(
        data = obs,
        aes(date, deaths),
        col = "black"
      ) +
      geom_line(
        data = pred,
        aes(x = date, `50%`, group = proj, col = "#0072B2"),
        size = 1
      ) +
      geom_ribbon(
        data = pred,
        aes(
          x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          group = proj,
          fill = "#0072B2"
        ),
        alpha = 0.3
      ) +
      theme_classic() +
      theme(legend.position = "none", legend.title = element_blank()) +
      xlab("") +
      ylab("") +
      scale_x_date(limits = c(as.Date("2020-03-01"), NA)) +
      facet_wrap(~country)

    ### Unweighted and weighted comparison
    wtd1 <- wtd_prev_week[wtd_prev_week$country %in% country, ] %>%
      rincewind::cap_predictions()

    wtd2 <- wtd_all_prev_weeks[wtd_all_prev_weeks$country %in% country, ] %>%
      rincewind::cap_predictions()

    message(country)
    p1 <- p +
      geom_ribbon(
        data = wtd1,
        aes(
          x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          group = proj,
          fill = "#D55E00"
        ),
        alpha = 0.3
      ) +
      ## geom_ribbon(
      ##   data = wtd1,
      ##   aes(x = date, ymin = `25%`, ymax = `75%`, group = proj),
      ##   fill = "#D55E00",
      ##   alpha = 0.5
      ## ) +
      geom_line(
        data = wtd1,
        aes(x = date, `50%`, group = proj, col = "#D55E00"),
        size = 1
      ) +

      geom_ribbon(
        data = wtd2,
        aes(
          x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          group = proj,
          fill = "#CC79A7"
        ),
        alpha = 0.3
      ) +
      ## geom_ribbon(
      ##   data = wtd2,
      ##   aes(x = date, ymin = `25%`, ymax = `75%`, group = proj),
      ##   fill = "#CC79A7",
      ##   alpha = 0.5
      ## ) +
      geom_line(
        data = wtd2,
        aes(x = date, `50%`, group = proj, col = "#CC79A7"),
        size = 1
      ) +
      scale_fill_identity(
        breaks = c("#0072B2", "#D55E00", "#CC79A7"),
        labels = c("Unweighted", "Previous Week", "All previous weeks"),
        guide = "legend"
      ) +
      scale_color_identity(
        breaks = c("#0072B2", "#D55E00", "#CC79A7"),
        labels = c("Unweighted", "Previous Week", "All previous weeks"),
        guide = "legend"
      ) +
      theme(legend.position = "top", legend.title = element_blank())

    ggsave(glue::glue("{country}_forecasts_comparison{file_format}"), p1)
  }
)


### compare observed with predicted
p <- ggplot(wtd_prev_week, aes(deaths, `50%`)) +
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
  xlab("(log) Observed Deaths") +
  ylab("(log) Median predicted Deaths") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off')


plow <- ggplot(wtd_prev_week, aes(deaths, `25%`)) +
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
  coord_cartesian(clip = 'off')

phigh <- ggplot(wtd_prev_week, aes(deaths, `75%`)) +
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
  coord_cartesian(clip = 'off')


ggsave("wtd_prev_week_obs_vs_pred.png", p)
ggsave("wtd_prev_week_obs_vs_pred_25.png", plow)
ggsave("wtd_prev_week_obs_vs_pred_75.png", phigh)


p <- ggplot(wtd_all_prev_weeks, aes(deaths, `50%`)) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = c(0, -0.30, 0.17),
    linetype = "dashed",
    col = "red"
  ) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("(log) Observed Deaths") +
  ylab("(log) Median predicted Deaths") +
  theme_minimal() +
  coord_cartesian(clip = 'off')


ggsave("wtd_all_prev_weeks_obs_vs_pred.png", p)

p <- ggplot(unweighted_qntls, aes(deaths, `50%`)) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = c(0, -0.30, 0.17),
    linetype = "dashed",
    col = "red"
  ) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("(log) Observed Deaths") +
  ylab("(log) Median predicted Deaths") +
  theme_minimal() +
  coord_cartesian(clip = 'off')


ggsave("unwtd_obs_vs_pred.png", p)
