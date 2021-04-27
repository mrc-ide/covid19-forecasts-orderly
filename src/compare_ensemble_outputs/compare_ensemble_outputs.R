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
    ##pred <- left_join(pred, obs, by = c("date" = "dates"))
    p1 <- all_forecasts_calendar(
      obs, pred, date_breaks, date_labels, proj, xmin = "2020-11-01"
    ) +
      ylab("Daily Deaths") +
      theme_manuscript() +
      theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, angle = 90),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"),
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
    out <- split(x, list(x$forecast_date, x$country), drop = TRUE) %>%
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
    p2 <- restimates_linegraph(
      out, forecast_date, xmin = "2020-11-01"
    ) +
      ylab("Reproduction Number") +
      theme_manuscript() +
      theme(
        strip.text = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8, angle = 90),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"),
        panel.spacing = unit(c(0, 0, 0, 0), "cm")
      ) + coord_cartesian(clip = "off")


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
    ##outfile <- glue::glue("{country}_forecasts{file_format}")
    ##save_multiple(plot = p, filename = outfile)
    p
  }
)



## main_text_countries <- c(
##  "Brazil", "India", "Italy", "South_Africa",
##  "Turkey", "United_States_of_America"
##)


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

legend <- get_legend(
  proj_plots[["Brazil"]] + theme(legend.box = "horizontal")
)

p11 <- proj_plots[["Brazil"]] +
  ggtitle("Brazil") +
  theme(axis.text.x = element_blank())

p21 <- rt_plots[["Brazil"]]

p12 <- proj_plots[["India"]] + ggtitle("India") +
    theme(axis.text.x = element_blank())
p22 <- rt_plots[["India"]]

p13 <- proj_plots[["Italy"]] + ggtitle("Italy") +
    theme(axis.text.x = element_blank())
p23 <- rt_plots[["Italy"]]

p14 <- proj_plots[["South_Africa"]] + ggtitle("South Africa") +
    theme(axis.text.x = element_blank())

p24 <- rt_plots[["South_Africa"]]

p15 <- proj_plots[["Turkey"]] + ggtitle("Turkey") +
    theme(axis.text.x = element_blank())
p25 <- rt_plots[["Turkey"]]

p16 <- proj_plots[["United_States_of_America"]] +
  ggtitle("USA") +   theme(axis.text.x = element_blank())
p26 <- rt_plots[["United_States_of_America"]]

label1 <- textGrob("Daily deaths", rot = 90, gp = gpar(fontsize = 7))
label2 <- textGrob(
  "Reproduction Number", rot = 90, gp = gpar(fontsize = 7)
)

top <- p11 + p12 + p13 +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() &
  theme(
    text = element_text(size = 7), axis.text.x = element_blank(),
    legend.position = "none", axis.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

bottom <- (p21 + p22 + p23) +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() +
  theme(
    text = element_text(size = 7),
    axis.title = element_blank(), legend.position = "none",
    ##axis.text.x = element_text(angle = 90),
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
    text = element_text(size = 7),
    axis.text.x = element_blank(), legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

bottom <- (p24 + p25 + p26) +
  plot_layout(ncol = 3, nrow = 1) &
  theme_minimal() +
  theme(
    text = element_text(size = 7),
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

##rincewind::save_multiple(final, "main_short_forecasts.tiff")
ggsave("main_short_forecasts.tiff", final)
