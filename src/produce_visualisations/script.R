
## Observations in tall format
model_input <- readr::read_rds("model_input.rds")
obs_deaths <- model_input [["D_active_transmission"]]
obs_deaths <- tidyr::gather(
  obs_deaths,
  country,
  deaths,
  -dates
)

## List of continents-coutnry mapping
continents <- readr::read_csv(
  "country-and-continent-codes-list-csv_csv.csv"
)
continents <- janitor::clean_names(continents)
## Some are dups, not that it matters..
dups <- duplicated(continents$three_letter_country_code)
continents <- continents[!dups, ]
continents <- continents[, c(
  "continent_name",
  "three_letter_country_code"
)]

obs_deaths <- add_continents(obs_deaths, continents)


ensb_pred <- readr::read_rds("ensemble_daily_qntls.rds")
ensb_pred <- na.omit(ensb_pred)
ensb_pred$week_ending <- ensb_pred$proj
ensb_pred$proj <- "Ensemble"

## Read in the model specific outputs here so that we can construct
## nice names
daily_predictions_qntls <- readRDS("daily_predictions_qntls.rds")

daily_predictions_qntls <- tidyr::separate(
  daily_predictions_qntls,
  col = "model",
  into = c("proj", NA, NA, NA, NA, "week_ending"),
  sep = "_"
)

sbsm_countries <- dplyr::filter(
  daily_predictions_qntls, proj == "sbsm"
) %>% pull(country) %>% unique
##ensb_pred$proj[ensb_pred$country == "United_Kingdom"] <- "sbsm"

## Make plots for only the latest week.
ensb_pred <- ensb_pred[ensb_pred$week_ending == as.Date(ensb_pred$week_ending), ]
ensb_pred <- add_continents(ensb_pred, continents)

by_continent_si <- split(
  ensb_pred, list(ensb_pred$continent_name, ensb_pred$si),
  sep = "_"
)
plots <- purrr::map(
  by_continent_si,
  function(pred) {
    pred$date <- as.Date(pred$date)
    obs <- obs_deaths[obs_deaths$country %in% pred$country, ]
    projection_plot(obs, pred) + theme(legend.position = "none")
  }
)

nice_names <- snakecase::to_title_case(unique(ensb_pred$country))
names(nice_names) <- unique(ensb_pred$country)
nice_names[names(nice_names) %in% sbsm_countries] <-
  glue::glue("{nice_names[names(nice_names) %in% sbsm_countries]}**")

nice_names[! names(nice_names) %in% sbsm_countries] <-
  glue::glue("{nice_names[! names(nice_names) %in% sbsm_countries]}*")


## 6 pages in each plot so.
n_cntrs <- purrr::map_int(by_continent_si, ~ length(unique(.$country)))
n_pages <- ceiling(n_cntrs / 6)
purrr::iwalk(
  plots,
  function(p, continent_si) {
    here_pages <- n_pages[[continent_si]]
    if (here_pages == 1) {
      nrow <- NULL
    } else {
      nrow <- 3
    }
    purrr::walk(
      seq_len(here_pages),
      function(page_num) {
        p <- p +
          ggforce::facet_wrap_paginate(
            ~country,
            scales = "free_y",
            labeller = as_labeller(
              nice_names
            ),
            ncol = 2,
            nrow = nrow,
            page = page_num
          )

        outfile <- glue::glue("ensmbl_pred_{continent_si}_page_{page_num}.png")
        message("Saving ", outfile)
        ggsave(
          filename = outfile,
          plot = p,
          width = fig_size$fig.width,
          height = fig_size$fig.height,
          unit = fig_size$units
        )
      }
    )
  }
)

##### Individual Model Projection Plots

daily_predictions_qntls <- add_continents(
  daily_predictions_qntls, continents
)

daily_predictions_qntls$proj <- dplyr::case_when(

  daily_predictions_qntls$proj == "RtI0" ~ "Model 1",
  daily_predictions_qntls$proj == "sbkp" ~ "Model 2",
  daily_predictions_qntls$proj == "DeCa" ~ "Model 3",
  daily_predictions_qntls$proj == "sbsm" ~ "Model 4"
)

by_model_si <- split(
  daily_predictions_qntls,
  list(
    ##daily_predictions_qntls$proj,
    daily_predictions_qntls$continent_name,
    daily_predictions_qntls$si
  ),
  sep = "_"
)

by_model_si <- purrr::keep(by_model_si, ~ nrow(.) >= 1)

plots <- purrr::map(
  by_model_si,
  function(pred) {
    pred$date <- as.Date(pred$date)
    obs <- obs_deaths[obs_deaths$country %in% pred$country, ]
    projection_plot(obs, pred)
  }
)

#nice_names <- snakecase::to_title_case(unique(daily_predictions_qntls$country))
#names(nice_names) <- unique(daily_predictions_qntls$country)
## 6 pages in each plot so.
n_cntrs <- purrr::map_int(by_model_si, ~ length(unique(.$country)))
n_pages <- ceiling(n_cntrs / 6)
purrr::iwalk(
  plots,
  function(p, model_si) {
    here_pages <- n_pages[[model_si]]

    if (here_pages == 1) {
      nrow <- NULL
    } else {
      nrow <- 3
    }

    purrr::walk(
      seq_len(here_pages),
      function(page_num) {
        p <- p +
          ggforce::facet_wrap_paginate(
            ~country,
            scales = "free_y",
            labeller = as_labeller(
              nice_names
            ),
            ncol = 2,
            nrow = nrow,
            page = page_num
          )

        outfile <- glue::glue("{model_si}_page_{page_num}.png")
        message("Saving ", outfile)
        ggsave(
          filename = outfile,
          plot = p,
          width = fig_size$fig.width,
          height = fig_size$fig.height,
          unit = fig_size$units
        )
      }
    )
  }
)

## Model Rt quantiles
ensemble_rt <- readRDS("ensemble_model_rt.rds")
### For ensemble plots, need to split by date
###
ensemble_rt_wide <- tidyr::spread(
  ensemble_rt, quantile, out2
)
ensemble_rt_wide <- add_continents(ensemble_rt_wide, continents)
ensemble_rt_wide$proj <- "Ensemble"
##ensemble_rt_wide$proj[ensemble_rt_wide$country == "United_Kingdom"] <- "Model 4"
## nice_names <- c(
##   Algeria = "Algeria *",
##   Austria = "Austria**",
##   Belgium = "Belgium*",
##   Brazil = "Brazil*",
##   Canada = "Canada*",
##   China = "China*",
##   Colombia = "Colombia*",
##   Czechia = "Czechia*",
##   Denmark = "Denmark**",
##   Dominican_Republic = "Dominican Republic*",
##   Ecuador = "Ecuador*",
##   Egypt = "Egypt*",
##   France = "France**",
##   Germany = "Germany**",
##   India = "India*",
##   Indonesia = "Indonesia*",
##   Iran = "Iran*",
##   Ireland = "Ireland*",
##   Israel = "Israel*",
##   Italy = "Italy**",
##   Mexico = "Mexico*",
##   Morocco = "Morocco*",
##   Netherlands = "Netherlands**",
##   Peru = "Peru*",
##   Philippines = "Philippines*",
##   Poland = "Poland*",
##   Portugal = "Portugal**",
##   Romania = "Romania*",
##   Russia = "Russia*",
##   South_Korea = "South Korea*",
##   Spain = "Spain**",
##   Sweden = "Sweden**",
##   Switzerland = "Switzerland**",
##   Turkey = "Turkey*",
##   United_Kingdom = "United Kingdom***",
##   United_States_of_America = "United States of America*"
## )

plots <- split(
  ensemble_rt_wide,
  list(
    ensemble_rt_wide$continent_name,
    ensemble_rt_wide$si
  ),
  sep = "_"
) %>% purrr::map(~ rt_boxplot(., nice_names) + theme(legend.position = "none"))

purrr::iwalk(
  plots,
  function(p, date_si) {
    outfile <- glue::glue("ensemble_rt_{date_si}_boxplot.png")
    message(outfile)
    ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)

## nice_names <- c(
##   Algeria = "Algeria *",
##   Austria = "Austria**",
##   Belgium = "Belgium*",
##   Brazil = "Brazil*",
##   Canada = "Canada*",
##   China = "China*",
##   Colombia = "Colombia*",
##   Czechia = "Czechia*",
##   Denmark = "Denmark**",
##   Dominican_Republic = "Dominican Republic*",
##   Ecuador = "Ecuador*",
##   Egypt = "Egypt*",
##   France = "France**",
##   Germany = "Germany**",
##   India = "India*",
##   Indonesia = "Indonesia*",
##   Iran = "Iran*",
##   Ireland = "Ireland*",
##   Israel = "Israel*",
##   Italy = "Italy**",
##   Mexico = "Mexico*",
##   Morocco = "Morocco*",
##   Netherlands = "Netherlands**",
##   Peru = "Peru*",
##   Philippines = "Philippines*",
##   Poland = "Poland*",
##   Portugal = "Portugal**",
##   Romania = "Romania*",
##   Russia = "Russia*",
##   South_Korea = "South Korea*",
##   Spain = "Spain**",
##   Sweden = "Sweden**",
##   Switzerland = "Switzerland**",
##   Turkey = "Turkey*",
##   United_Kingdom = "United Kingdom**",
##   United_States_of_America = "United States of America*"
## )


##ensemble_rt$model <- paste0("Ensemble_", ensemble_rt$model)
model_rt <- readRDS("model_rt_qntls.rds")
model_rt$model <- gsub(
  x = model_rt$model,
  pattern = "Std_results_week_end_",
  replacement = ""
)
## Re-shuffle columns so that we can rbind
model_rt <- model_rt[, colnames(ensemble_rt)]
##rt_both <- rbind(ensemble_rt, model_rt)
rt_both <- model_rt
rt_both <- tidyr::separate(
  rt_both,
  col = "model",
  into = c("proj", "date"),
  sep = "_"
)
rt_both <- tidyr::spread(rt_both, key = quantile, value = out2)

rt_both$proj <- dplyr::case_when(

  rt_both$proj == "RtI0" ~ "Model 1",
  rt_both$proj == "sbkp" ~ "Model 2",
  rt_both$proj == "DeCa" ~ "Model 3",
  rt_both$proj == "sbsm" ~ "Model 4",
  rt_both$proj == "Ensemble" ~ "Ensemble"
)

rt_both <- add_continents(rt_both, continents)

plots <- split(
  rt_both,
  list(rt_both$si, rt_both$date, rt_both$continent_name),
  sep = "_"
) %>% purrr::map(~ rt_lineplot(., nice_names))

purrr::iwalk(
  plots,
  function(p, model_si) {
    outfile <- glue::glue("rt_{model_si}.png")
    ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)


### Ensemble Produced without SBSM Model and
### SBSM Model overlaid
## sbsm <- daily_predictions_qntls[daily_predictions_qntls$proj == "sbsm", ]
## sbsm <- sbsm[sbsm$si == "si_2", ]

## ensb_pred <- ensb_pred[ensb_pred$si == "si_2", ]
## ensb_pred <- ensb_pred[ensb_pred$proj == "2020-04-12", ]
## ensb_pred <- dplyr::filter(ensb_pred, country %in% sbsm$country)
## ensb_pred$proj <- paste0("ensemble_without_sbsm_", ensb_pred$proj)

## ensb_with_sbsm <- readr::read_rds("ensemble_with_sbsm_qntls.rds")
## ensb_with_sbsm$proj <- "ensemble_with_sbsm"
## ensb_with_sbsm <- dplyr::filter(ensb_with_sbsm, country %in% sbsm$country)


## cols <- c("proj", "country", "date", "2.5%", "50%", "97.5%")
## sbsm <- sbsm[, cols]
## ensb_pred <- ensb_pred[, cols]
## ensb_with_sbsm <- ensb_with_sbsm [, cols]

## compare <- rbind(ensb_pred, sbsm, ensb_with_sbsm)

## obs <- dplyr::filter(obs_deaths, country %in% sbsm$country)



## obs$dates <- as.Date(obs$dates)
## compare$date <- as.Date(compare$date)

## p <- ggplot() +
##   geom_point(data = obs, aes(dates, deaths)) +
##   geom_ribbon(
##     data = compare,
##     aes(x = date, ymin = `2.5%`, ymax = `97.5%`, fill = proj),
##     alpha = 0.3
##   ) +
##   geom_line(data = compare, aes(date, `50%`, col = proj)) +
##   xlab("") +
##   ylab("Deaths") +
##   scale_x_date(
##     limits = c(
##       as.Date("2020-03-01"),
##       as.Date("2020-04-20")
##     )
##   ) +
##   theme_project(font_size = 18) +
##   theme(legend.position = "top")

## p1 <- p +
##   ggforce::facet_wrap_paginate(~country, ncol = 2, nrow = 3, scales = "free_y", page = 1)


## p2 <- p +
##   ggforce::facet_wrap_paginate(~country, ncol = 2, nrow = 3, scales = "free_y", page = 2)

## ggsave("comparison_sbsm_unwtd_ensb_page_1.png", p1)
## ggsave("comparison_sbsm_unwtd_ensb_page_2.png", p2)



###################
##### Reporting Trends
### col -> column to scale
x <- readr::read_rds("DeCa_Std_Ratio_plot_2020-04-19.rds")
max_deaths <- purrr::map_dfr(x, ~ max(.[["D_t"]]), .id = "country")
max_deaths <- tidyr::gather(max_deaths, country, max_deaths)

max_cases <- purrr::map_dfr(
  x, ~ max(.[["I_t_minus_meanDelay"]]),
  .id = "country"
)
max_cases <- tidyr::gather(max_cases, country, max_cases)

x <- purrr::map_dfr(
  x,
  function(cntry) {
    scale_by <- max(
      max(cntry$I_t_minus_meanDelay),
      max(cntry$D_t)
    )
    cases_scaled <- cntry$I_t_minus_meanDelay / scale_by


    deaths_scaled <- cntry$D_t / scale_by
    out <- data.frame(
      date = cntry$dates,
      cases_scaled = cases_scaled,
      deaths_scaled = deaths_scaled,
      median_ratio = cntry$median_ratio,
      low_ratio = cntry$low_ratio,
      up_ratio = cntry$up_ratio
    )
    out
  },
  .id = "country"
)



x <- add_continents(x, continents)
x$date <- as.Date(x$date)
x$country <- snakecase::to_title_case(x$country)
plots <- split(x, x$continent_name) %>%
  purrr::imap(
    function(df, continent) {
      npages <- ceiling(length(unique(df$country)) / 6)
      ncol <- 2
      if (npages == 1) {
        ## from ggforce::facet_wrap_paginate:
        ## If either ncol or nrow is NULL this function
        ## will fall back to the standard facet_wrap functionality.
        nrow <- NULL
      } else {
        nrow <- 3
      }
      for (page_num in seq_len(npages)) {
        p <- ggplot(df) +
          geom_point(aes(date, cases_scaled, color = "black")) +
          geom_point(aes(date, deaths_scaled, color = "red")) +
          geom_ribbon(
            aes(x = date, ymin = low_ratio, ymax = up_ratio),
            fill = "blue",
            alpha = 0.3
          ) +
          geom_line(aes(date, median_ratio, color = "blue")) +
          ## scale_y_continuous(sec.axis = sec_axis(~ . * max_y$max_cases)) +
          ggforce::facet_wrap_paginate(
            ~country,
            ncol = ncol, nrow = nrow, page = page_num
          ) +
          theme_project() +
          xlab("") +
          ylab("ratio D to C") +
          scale_color_identity(
            name = element_blank(),
            breaks = c("black", "red", "blue"),
            labels = c("Cases (scaled)", "Deaths (scaled)", "Reporting Ratio"),
            guide = "legend",
            aesthetics = "color"
          ) +
          theme(
            legend.text = element_text(size = 20),
            legend.key.width = unit(3, "line"),
            legend.key.height = unit(3, "line")
          )

        outfile <- glue::glue("reporting_ratio_{continent}_page_{page_num}.png")
        message("Saving ", outfile)
        ggsave(
          filename = outfile,
          plot = p,
          width = fig_size$fig.width,
          height = fig_size$fig.height,
          unit = fig_size$units
        )
      }
    }
  )
