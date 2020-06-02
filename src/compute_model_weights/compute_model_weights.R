model_metrics <- readr::read_csv("model_predictions_error.csv")
model_metrics <- model_metrics[model_metrics$model != "sbsm", ]

grpd <- dplyr::group_by(model_metrics, model, forecast_date, country, si)

likelihood <- dplyr::summarise(
  grpd, poisson_ll = prod(poisson_p), empirical_ll = prod(empirical_p)
  ) %>% dplyr::ungroup()

x <- split(likelihood, likelihood$forecast_date)

forecast_dates <- names(x)
names(forecast_dates) <- forecast_dates
### Strategy 1. For each week, use only the last week's foreecasts
### to compute weights
wts_curr_week <- purrr::map(
  forecast_dates,
  function(forecast_date) {
    message(forecast_date)
    ##prev_week <- as.Date(forecast_date) - 7
    ##message(prev_week)
    ##df <- x[[as.character(prev_week)]]
    df <- x[[forecast_date]]
    split(
      df, df$si
    ) %>%
      purrr::map(
        function(df_si) {
          ## First get weights for each country, and then
          ## average across countries
          cntry_wt <- split(df_si, df_si$country) %>%
            purrr::map_dfr(function(y) {
              data.frame(
                model = y$model,
                wt_poisson = y$poisson_ll / (sum(y$poisson_ll) + 1),
                wt_empirical = y$empirical_ll / (sum(y$empirical_ll) + 1)
              )
            }, .id = "country"
           )

          ## Now average across countries
          model_weight <- dplyr::group_by(cntry_wt, model) %>%
            dplyr::summarise(
              wt_poisson = mean(wt_poisson),
              wt_empirical = mean(wt_empirical)
              ) %>% dplyr::ungroup()
          ## And normalise
          model_weight$wt_poisson <- model_weight$wt_poisson /
            sum(model_weight$wt_poisson)

          model_weight$wt_empirical <- model_weight$wt_empirical /
            sum(model_weight$wt_empirical)

          list(
            DeCa_RtI0_sbkp = model_weight
          )
        }
     )
  }
)

## Strategy 2: Use all previous forecasts to compute weights
wts_all_prev_weeks <- purrr::map(
  forecast_dates,
  function(forecast_date) {
    message(forecast_date)
    prev_weeks <- which(
      as.Date(names(wts_curr_week)) <= as.Date(forecast_date)
    )
    message(paste(names(x)[prev_weeks], collapse = " "))
    dflist <- wts_curr_week[prev_weeks]
    f <- function(df) {
      dplyr::group_by(df, model) %>%
        dplyr::summarise_if(is.numeric, mean) %>%
          dplyr::ungroup()
    }
    wts_si_1 <- purrr::map_dfr(
      dflist, ~ .$si_1[[1]], .id = "forecast_date"
      )

    wts_si_2 <- purrr::map_dfr(
      dflist, ~ .$si_2[[1]], .id = "forecast_date"
    )

    list(
      si_1 = list(DeCa_RtI0_sbkp = f(wts_si_1)),
      si_2 = list(DeCa_RtI0_sbkp = f(wts_si_2))
    )
    }
  )



## For each country, extract a list of models that were run for it.
## Then group countries that have the same set of models
## The SI doesn't matter here, because presumably a model that was run
## using one serial interval was also run using the 2nd.
countries_per_model <- split(
  model_metrics, model_metrics$forecast_date
) %>% purrr::map(~ weighter::groupvar_to_model(., "country"))


saveRDS(
  object = wts_curr_week,
  file = "unnormalised_model_weights_using_prev_week.rds"
)

saveRDS(
  object = wts_all_prev_weeks,
  file = "unnormalised_model_weights_using_all_prev_week.rds"
)

saveRDS(
  object = countries_per_model,
  file = "countries_grouped_by_models.rds"
)


########### Compare weights ###################

## wts_curr_week <- purrr::map_dfr(
##   wts_curr_week, ~ .$si_2$DeCa_RtI0_sbkp, .id = "forecast_date"
## )
## wts_curr_week$strategy <- "Previous week"

## wts_all_prev_weeks <- purrr::map_dfr(
##   wts_all_prev_weeks, ~ .$si_2$DeCa_RtI0_sbkp, .id = "forecast_date"
##   )
## wts_all_prev_weeks$strategy <- "All previous weeks"

## df <- rbind(wts_curr_week, wts_all_prev_weeks)
## df <- tidyr::gather(df, key = var, value = val, wt_poisson:wt_empirical)


## ggplot(
##   df, aes(forecast_date, val, shape = var)
## ) + geom_point() +
##   facet_grid(strategy ~ model) +
##   ggthemes::theme_base() +
##   ylab("Weight") +
##   xlab("Date of forecast") +
##   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
