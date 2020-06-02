### This task produces the following visualtions:
### comparison of unweighted and weighted ensembles for each country
### all forecasts from unweighted ensemble
### all forecasts from weighted ensemble with weights coming from
### last week
### all forecasts from weighted ensemble with weights coming from
###all previous weeks

main_text_countries <- c(
  "Brazil", "India", "Italy", "Mexico", "United_States_of_America"
)

unweighted_qntls <- readRDS("unweighted_qntls.rds") %>%
  dplyr::filter(si == use_si)

wtd_prev_week <- readRDS("wtd_prev_week_qntls.rds") %>%
  dplyr::filter(si == use_si)

wtd_all_prev_weeks <- readRDS("wtd_all_prev_weeks_qntls.rds") %>%
    dplyr::filter(si == use_si)

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

purrr::iwalk(
  countries,
  function(country, name) {
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    pred <- cap_predictions(pred)

    p <- all_forecasts(obs, pred)
    outfile <- glue::glue("{name}_forecasts.tiff")
    ggsave(outfile, p)
  }
)

purrr::iwalk(
  countries,
  function(country, name) {
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- wtd_prev_week[wtd_prev_week$country %in% country, ]
    pred <- cap_predictions(pred)
    p <- all_forecasts(obs, pred)
    outfile <- glue::glue("{name}_forecasts_wtd_prev_week.tiff")
    ggsave(outfile, p)
  }
)



purrr::iwalk(
  countries,
  function(country, name) {
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- wtd_all_prev_weeks[wtd_all_prev_weeks$country %in% country, ]
    pred <- cap_predictions(pred)
    p <- all_forecasts(obs, pred)
    outfile <- glue::glue("{name}_forecasts_wtd_all_prev_weeks.tiff")
    ggsave(outfile, p)
  }
)



countries <- unique(wtd_prev_week$country)

purrr::walk(
  countries,
  function(country) {
    obs <-  unweighted_qntls[unweighted_qntls$country %in% country, c("date", "country" ,"deaths")]
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    pred <- cap_predictions(pred)

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
      cap_predictions()

    wtd2 <- wtd_all_prev_weeks[wtd_all_prev_weeks$country %in% country, ] %>%
      cap_predictions()

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

    ggsave(glue::glue("{country}_forecasts_comparison.tiff"), p1)
  }
)
