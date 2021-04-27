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


stacked_plots <- map(
   countries,
  function(country) {
    obs <- model_input[model_input$country %in% country, ]
    obs <- select(obs, date = dates, y = deaths)

    obs$var <- "forecasts"

    obs$color <- "#000000"

    x <- stacked_vars[[country]]

    x$fill <- case_when(
      x$var == "forecasts" ~ "#009E73",
      x$var == "rt" ~ "#56B4E9"
    )

    x$color <- case_when(
      x$var == "forecasts" ~ "#009E73",
      x$var == "rt" ~ "#56B4E9"
    )

    p <- ggplot(x) +
      geom_line(
        aes(date, y, group = forecast_date, col = color)
      ) +
      geom_ribbon(
        aes(
          date, ymin = ymin, ymax = ymax,
          group = forecast_date, fill = fill
        ), alpha = 0.3
      ) +
      geom_point(data = obs, aes(date, y, col = color)) +
      scale_fill_identity(
        breaks = c("#009E73", "#56B4E9"),
        labels = c("95% CrI (forecasts)", "95% CrI (Rt)")
      ) +
      scale_color_identity(
        breaks = c("#000000","#009E73", "#56B4E9"),
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

top <-


p11 <- proj_plots[["Brazil"]] +
  ggtitle("Brazil") +
  theme(axis.text.x = element_blank())

p21 <- rt_plots[["Brazil"]]

pbrazil <- plot_grid(
  p11 + p21, nrow = 2, align = "v", axis = "l"
)


p12 <- proj_plots[["India"]] + ggtitle("India") +
    theme(axis.text.x = element_blank())
p22 <- rt_plots[["India"]]

pindia <- plot_grid(
  p12 + p22, nrow = 2, align = "v", axis = "l"
)


p13 <- proj_plots[["Italy"]] + ggtitle("Italy") +
    theme(axis.text.x = element_blank())
p23 <- rt_plots[["Italy"]]

pitaly <- plot_grid(
  p13 + p23, nrow = 2, align = "v", axis = "l"
)

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

ggsave("main_short_forecasts.png", final)
