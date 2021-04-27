### orderly::orderly_develop_start(parameters = list(use_si = "si_2", latest_week = "2020-12-06"), use_draft = "newer")
### This task produces the following visualtions:
### comparison of unweighted and weighted ensembles for each country
### all forecasts from unweighted ensemble
### all forecasts from weighted ensemble with weights coming from
### last week
### all forecasts from weighted ensemble with weights coming from
###all previous weeks
date_labels <- "%d - %b"
date_breaks <- "4 weeks"
date_limits <- c(as.Date("2020-03-01"), NA)

file_format <- ".tiff"

main_text_countries <- c(
  "Brazil", "India", "Italy", "South_Africa",
  "Turkey", "United_States_of_America"
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

###### Put them together
stacked_vars <- map(
  countries,
  function(country) {
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    pred <- rincewind:::cap_predictions(pred)
    rt <- unweighted_rt_qntls[unweighted_rt_qntls$country %in% country, ]
    rt <- tidyr::spread(rt, quantile, out2)
    out <- split(rt, list(rt$forecast_date, rt$country), drop = TRUE) %>%
      map_dfr(
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
    ### Stack Rt and predictions together
    pred_qntls <- select(
      pred, date, ymin = `2.5%`, y = `50%`, ymax = `97.5%`,
      forecast_date = proj
    )
    rt_qntls <- select(
      out, date = dates, ymin = `2.5%`, y = `50%`, ymax = `97.5%`,
      forecast_date
    )
    pred_qntls$var <- "forecasts"
    rt_qntls$var <- "rt"

    rbind(pred_qntls, rt_qntls)
  }
)


stacked_plots <- purrr::map(
  main_text_countries,
  function(country) {
    obs <- model_input[model_input$country %in% country, ]
    obs <- select(obs, date = dates, y = deaths)
    obs$var <- "forecasts"
    obs$color <- "#000000"

    x <- stacked_vars[[country]]
    x$fill <- ifelse(x$var == 'forecasts', '#009E73', '#56B4E9')
    x$color <- ifelse(x$var == 'forecasts', '#009E73', '#56B4E9')

    p <- ggplot(x) +
      geom_line(aes(date, y, group = forecast_date, col = color)) +
      geom_ribbon(
        aes(
          date, ymin = ymin, ymax = ymax,
          group = forecast_date, fill = fill
        ), alpha = 0.3
      ) +
      geom_point(data = obs, aes(date, y, col = color)) +
      scale_fill_identity(breaks = c("#009E73", "#56B4E9")) +
      scale_color_identity(
        breaks = c('#000000', '#009E73', '#56B4E9'),
        labels = c("Observed deaths", "Forecasts (median and 95% CrI)",
                   "Rt (median and 95% CrI)"),
        guide = "legend"
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
      ggtitle(country) +
      theme_manuscript() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title = element_blank()
      )
    p
 }
)








