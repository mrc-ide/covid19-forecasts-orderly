add_continents <- function(df, mapping) {

  df$iso3c <- countrycode::countrycode(
    snakecase::to_title_case(df$country),
      "country.name",
      "iso3c"
   )

  df <- dplyr::left_join(
    df,
    mapping,
    by = c("iso3c" = "three_letter_country_code")
  )
  df
}

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
continents <- continents[ , c(
  "continent_name",
  "three_letter_country_code"
)]

obs_deaths <- add_continents(obs_deaths, continents)


ensb_pred <- readr::read_rds("ensemble_daily_qntls.rds")
ensb_pred <- na.omit(ensb_pred)
ensb_pred$week_ending <- ensb_pred$proj

ensb_pred <- add_continents(ensb_pred, continents)

by_continent_si <- split(
  ensb_pred, list(ensb_pred$continent_name, ensb_pred$si), sep = "_"
)
plots <-  purrr::map(
  by_continent_si,
  function(pred) {
      pred$date <- as.Date(pred$date)
      obs <- obs_deaths[obs_deaths$country %in% pred$country, ]
      projection_plot(obs, pred)
    }
  )

nice_names <- snakecase::to_title_case(unique(ensb_pred$country))
names(nice_names) <- unique(ensb_pred$country)
## 6 pages in each plot so.
n_cntrs <- purrr::map_int(by_continent_si, ~ length(unique(.$country)))
n_pages <- ceiling(n_cntrs / 6)
purrr::iwalk(
  plots,
  function(p, continent_si) {
    here_pages <- n_pages[[continent_si]]
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
            nrow = 3,
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
daily_predictions_qntls <- readRDS("daily_predictions_qntls.rds")

daily_predictions_qntls <- tidyr::separate(
  daily_predictions_qntls,
  col = "model",
  into = c("proj", NA, NA, NA, NA, "week_ending"),
  sep = "_"
)

daily_predictions_qntls <- add_continents(
  daily_predictions_qntls, continents
)

by_model_si <- split(
  daily_predictions_qntls,
  list(
    daily_predictions_qntls$proj,
    daily_predictions_qntls$continent_name,
    daily_predictions_qntls$si),
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

nice_names <- snakecase::to_title_case(unique(daily_predictions_qntls$country))
names(nice_names) <- unique(daily_predictions_qntls$country)
## 6 pages in each plot so.
n_cntrs <- purrr::map_int(by_model_si, ~ length(unique(.$country)))
n_pages <- ceiling(n_cntrs / 6)
purrr::iwalk(
  plots,
  function(p, model_si) {
    here_pages <- n_pages[[model_si]]
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
            nrow = 3,
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


plots <- split(
  ensemble_rt_wide,
  list(
    ensemble_rt_wide$model,
    ensemble_rt_wide$continent_name,
    ensemble_rt_wide$si
  ), sep = "_"
) %>% purrr::map(rt_boxplot)

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


ensemble_rt$model <- paste0("ensemble_", ensemble_rt$model)
model_rt <- readRDS("model_rt_qntls.rds")
model_rt$model <- gsub(
  x = model_rt$model,
  pattern = "Std_results_week_end_",
  replacement = ""

)
## Re-shuffle columns so that we can rbind
model_rt <- model_rt[, colnames(ensemble_rt)]
rt_both <- rbind(ensemble_rt, model_rt)
rt_both <- tidyr::separate(
  rt_both,
  col = "model",
  into = c("model", "date"),
  sep = "_"
)
rt_both <- tidyr::spread(rt_both, key = quantile, value = out2)

plots <- split(
  rt_both,
  list(rt_both$model, rt_both$si, rt_both$date), sep = "_"
) %>% purrr::map(rt_plot)

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
## ensb_pred <- dplyr::filter(ensb_pred, proj == "2020-04-12")
## sbsm <- dplyr::filter(daily_predictions_qntls, model == "sbsm_Std_results_week_end_2020-04-12")
## sbsm <- dplyr::filter(sbsm, si == "si_2")
## ensb_pred <- dplyr::filter(ensb_pred, si == "si_2")
## ensb_pred <- dplyr::filter(ensb_pred, country %in% sbsm$country)
## obs <- dplyr::filter(obs_deaths, country %in% sbsm$country)

## obs$dates <- as.Date(obs$dates)
## ensb_pred$date <- as.Date(ensb_pred$date)
## sbsm$date <- as.Date(sbsm$date)

## p <- ggplot() +
##   geom_point(data = obs, aes(dates, deaths)) +
##   geom_ribbon(
##     data = ensb_pred,
##     aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
##     fill = "red", alpha = 0.3
##   ) +
##   geom_ribbon(
##     data = sbsm,
##     aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
##     fill = "blue",
##     alpha = 0.3
##   ) +
##   geom_line(data = ensb_pred, aes(date, `50%`), color = "red") +
##   geom_line(data = sbsm, aes(date, `50%`), color = "blue") +
##   xlab("") +
##   ylab("Deaths") +
##   scale_x_date(
##     limits = c(as.Date("2020-03-01"),
##                as.Date("2020-04-20")
##                )
##   ) +
##   theme_project(font_size = 18)

## p1 <- p +
##   ggforce::facet_wrap_paginate(~country, ncol = 2, nrow = 3, scales = "free_y", page = 1)


## p2 <- p +
##   ggforce::facet_wrap_paginate(~country, ncol = 2, nrow = 3, scales = "free_y", page = 2)

## ggsave("comparison_sbsm_unwtd_ensm_with_sbsm_page_1.png", p1)
## ggsave("comparison_sbsm_unwtd_ensm_with_sbsm_page_2.png", p2)




