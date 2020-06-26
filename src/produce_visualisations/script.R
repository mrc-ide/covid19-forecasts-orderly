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


## Observations in tall format
model_input <- readRDS(
  glue::glue(
  "{dirname(covid_19_path)}/model_inputs/data_{week_ending_vis}.rds"
  )
)
obs_deaths <- model_input [["D_active_transmission"]]
obs_deaths <- tidyr::gather(
  obs_deaths,
  country,
  deaths,
  -dates
)

obs_deaths <- add_continents(obs_deaths, continents)


ensb_pred <- readr::read_rds("ensemble_daily_qntls.rds")
ensb_pred <- na.omit(ensb_pred)
ensb_pred$week_ending <- ensb_pred$proj
ensb_pred$proj <- "Ensemble"
exclude <- c(
  "Ecuador", "Cameroon", "United_States_of_America",
  "Sudan", "Yemen", "Democratic_Republic_of_the_Congo", "Mauritania"
)

ensb_pred <- ensb_pred[! ensb_pred$country %in% exclude, ]

## Read in the model specific outputs here so that we can construct
## nice names
daily_predictions_qntls <- readRDS("daily_predictions_qntls.rds")
daily_predictions_qntls <- daily_predictions_qntls[! daily_predictions_qntls$country %in% exclude, ]
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
ensb_pred <- ensb_pred[ensb_pred$week_ending == max(as.Date(ensb_pred$week_ending)), ]
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
ensemble_rt <- ensemble_rt[! ensemble_rt$country %in% exclude,]
### For ensemble plots, need to split by date
###
ensemble_rt_wide <- tidyr::spread(
  ensemble_rt, quantile, out2
)
ensemble_rt_wide <- add_continents(ensemble_rt_wide, continents)
ensemble_rt_wide <- ensemble_rt_wide[ensemble_rt_wide$model == max(as.Date(ensemble_rt_wide$model)), ]
ensemble_rt_wide$proj <- "Ensemble"


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


model_rt <- readRDS("model_rt_qntls.rds")
model_rt <- model_rt[! model_rt$country %in% exclude,]
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
  list(rt_both$si, rt_both$continent_name),
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



###################
##### Reporting Trends
### col -> column to scale
x <- readr::read_rds("deaths_to_cases_qntls.rds")
x <- x[! names(x) %in% exclude]
## This scaling is happening in DeCa model
## max_deaths <- purrr::map_dfr(x, ~ max(.[["D_t"]]), .id = "country")
## max_deaths <- tidyr::gather(max_deaths, country, max_deaths)

## max_cases <- purrr::map_dfr(
##   x, ~ max(.[["I_t_minus_meanDelay"]]),
##   .id = "country"
## )
## max_cases <- tidyr::gather(max_cases, country, max_cases)

## x <- purrr::map_dfr(
##   x,
##   function(cntry) {
##     scale_by <- max(
##       max(cntry$I_t_minus_meanDelay),
##       max(cntry$D_t)
##     )
##     cases_scaled <- cntry$I_t_minus_meanDelay / scale_by


##     deaths_scaled <- cntry$D_t / scale_by
##     out <- data.frame(
##       date = cntry$dates,
##       cases_scaled = cases_scaled,
##       deaths_scaled = deaths_scaled,
##       median_ratio = cntry$median_ratio,
##       low_ratio = cntry$low_ratio,
##       up_ratio = cntry$up_ratio
##     )
##     out
##   },
##   .id = "country"
## )


x <- purrr::map_dfr(x, ~ ., .id = "country")
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
          geom_point(aes(date, I_t_minus_meanDelay, color = "black")) +
          geom_point(aes(date, D_t, color = "red")) +
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



## plots <- purrr::imap(
##   by_continent_si,
##   function(pred, y) {
##     pred$date <- as.Date(pred$date)
##     obs <- obs_deaths[obs_deaths$country %in% pred$country, ]
##     obs <- dplyr::rename(obs, date = "dates")
##     df <- dplyr::left_join(pred, obs)
##     df <- dplyr::filter(df, date >= "2020-03-01")
##     p <- ggplot(df) +
##       geom_point(aes(date, deaths), col = "black") +
##       geom_ribbon(
##         aes(x = date, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3
##       ) +
##       geom_ribbon(
##         aes(x = date, ymin = `25%`, ymax = `75%`), alpha = 0.5
##       ) + geom_line(
##             aes(x = date, y = `50%`)
##           ) +
##       facet_wrap(~country, ncol = 2, scales = "free_y")
##     outfile <- glue::glue("ensemble_predictions_{y}.html")
##     widget <- ggplotly(p)
##     saveWidget(
##       widget = widget,
##       file = outfile,
##       selfcontained = FALSE,
##       libdir = "lib"
##     )

##   }
## )
