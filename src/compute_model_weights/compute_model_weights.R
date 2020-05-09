model_metrics <- readr::read_csv("model_predictions_error.csv")
x <- split(model_metrics, model_metrics$forecast_date)

forecast_dates <- names(x)[-1]
### Strategy 1. For each week, use only the last week's foreecasts
### to compute weights
wts_prev_week <- purrr::map(
  forecast_dates,
  function(forecast_date) {
    message(forecast_date)
    prev_week <- as.Date(forecast_date) - 7
    message(prev_week)
    df <- x[[as.character(prev_week)]]
    split(
      df, df$si
    ) %>%
      purrr::map(
        function(df_si) {
          df_si <- dplyr::select(df_si, -forecast_date, -si)
          ## Sum the errors across the whole projection horizon
          df_si <- dplyr::group_by(
            df_si, model, country
            ) %>%
            dplyr::summarise_if(is.numeric, sum) %>%
            dplyr::ungroup()

          df_si <- df_si[, c("model", "country", "avg_likelhd")]
          df_si$avg_likelhd <- -df_si$avg_likelhd
          weighter::model_weights(df_si, "country", "avg_likelhd")
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
      as.Date(names(x)) < as.Date(forecast_date)
    )
    message(paste(names(x)[prev_weeks], collapse = " "))
    df <- dplyr::bind_rows(x[prev_weeks])
    split(df, df$si) %>%
      purrr::map(
        function(df_si) {
          df_si <- dplyr::select(df_si, -forecast_date, -si)
          ## Sum the errors across the whole projection horizon
          ## and across all forecast periods
          df_si <- dplyr::group_by(
            df_si, model, country
            ) %>%
            dplyr::summarise_if(is.numeric, sum) %>%
            dplyr::ungroup()

          df_si <- df_si[, c("model", "country", "avg_likelhd")]
          df_si$avg_likelhd <- -df_si$avg_likelhd
          weighter::model_weights(df_si, "country", "avg_likelhd")
        }
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
  object = wts_prev_week,
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
