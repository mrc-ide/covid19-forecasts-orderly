### orderly::orderly_develop_start(parameters = list(use_si = "si_2", latest_week = "2020-12-06"), use_draft = "newer")
### This task produces the following visualtions:
### comparison of unweighted and weighted ensembles for each country
### all forecasts from unweighted ensemble
### all forecasts from weighted ensemble with weights coming from
### last week
### all forecasts from weighted ensemble with weights coming from
###all previous weeks
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

proj_plots <- map(
  countries,
  function(country) {
    obs <- model_input[model_input$country %in% country, ]
    pred <- unweighted_qntls[unweighted_qntls$country %in% country, ]
    if (nrow(pred) == 0) {
      message("No data for ", country)
      continue
    }
    pred <- rincewind:::cap_predictions(pred)
    pred <- left_join(pred, obs, by = c("date" = "dates"))
    obs$rolling_mean <- slider::slide_dbl(
      obs$deaths, mean, .before = 3, .after = 3
    )
    p1 <- all_forecasts_calendar(obs, pred)
    ## Remove x-axis ticks to have them on the bottom panel only
    p1 <- p1 +
      ylab("Daily Deaths") +
      theme_manuscript() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"),
        panel.spacing = unit(c(0, 0, 0, 0), "cm")
      )
    p1

  }
)

rt_plots <- map(
  countries,
  function(country) {
    message(paste(country, collapse = " "))

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
    p2 <- all_restimates_line(out) +
      ylab("Effective Reproduction Number") +
      theme_manuscript() +
      theme(
        strip.text = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8, angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        panel.spacing = unit(c(0, 0, 0, 0), "cm")
      )

    p2
  }
)

both <- map(
  countries,
  function(country) {
    message(paste(country, collapse = " "))
    p1 <- proj_plots[[country]] +
      theme(legend.position = "none")
    p2 <- rt_plots[[country]]
    p <- plot_grid(
      p1, p2, align  = "hv", rel_heights = c(1, 0.6), ncol = 1
    )
    outfile <- glue::glue("{country}_forecasts{file_format}")
    ##save_multiple(plot = p, filename = outfile, one_col = FALSE)
    p
  }
)



## main_text_countries <- c(
##  "Brazil", "India", "Italy", "South_Africa",
##  "Turkey", "United_States_of_America"
##)

p1 <- both[["Brazil"]]

legend <- get_legend(
  proj_plots[[1]] + theme(legend.box = "horizontal")
)

p2 <- plot_grid(
  proj_plots[["India"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  rt_plots[["India"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  align  = "hv", rel_heights = c(1, 0.6), ncol = 1, axis = "l"
)

p3 <- plot_grid(
  proj_plots[["Italy"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  rt_plots[["Italy"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  align  = "hv", rel_heights = c(1, 0.6), ncol = 1, axis = "l"
)

p4 <- both[["South_Africa"]]

p5 <- plot_grid(
  proj_plots[["Turkey"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  rt_plots[["Turkey"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  align  = "hv", rel_heights = c(1, 0.6), ncol = 1, axis = "l"
)

p6 <- plot_grid(
  proj_plots[["United_States_of_America"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  rt_plots[["United_States_of_America"]] +
  theme(axis.title.y = element_blank(), legend.position = "none"),
  align  = "hv", rel_heights = c(1, 0.6), ncol = 1, axis = "l"
)

p <- plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)

plegend <- plot_grid(
  legend, p, ncol = 1, rel_heights = c(0.1, 1),
  rel_widths = c(0.25, 1), align = "hv", axis = "l"
)

save_multiple(plegend, "test.tiff")


## For SI, png is fine.
## infiles <- list.files(pattern = "*.tiff")
## walk(
##   infiles,
##   function(infile) {
##     img <- magick::image_read(infile)
##     outfile <- stringr::str_replace(infile, "tiff", "png")
##     magick::image_write(img, path = outfile, format = "png")
##   }
## )
