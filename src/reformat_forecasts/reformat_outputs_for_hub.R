##orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-03-14"))

## Use ISO2c codes from the hub
## https://github.com/epiforecasts/covid19-forecast-hub-europe/blob/main/data-locations/locations_eu.csv
countrycode <- readr::read_csv("locations_eu.csv")

forecast_date <- as.Date(week_ending) + 1
prefix <- glue::glue("{forecast_date}-Imperial")
forecasts <- readRDS("weekly_predictions_qntls.rds")
forecasts <- forecasts[forecasts$si == "si_2", ]

forecasts$country <- rincewind::nice_country_name(forecasts$country)
forecasts <- left_join(forecasts, countrycode, by = c("country" = "location_name"))
forecasts <- forecasts[complete.cases(forecasts), ]

## Date as YYYY-MM-DD, last day (Monday) of submission window
forecasts$forecast_date <- forecast_date
## target" # wk ahead inc case" or "# wk ahead inc death" where # is
## usually between 1 and 4
forecasts$target <- "1 wk ahead inc death"

## target_end_date
## Date as YYYY-MM-DD, the last day (Saturday) of the target week
forecasts$target_end_date <- as.Date(week_ending) + 6


## type One of "point" or "quantile"
forecasts$type <- "quantile"

## quantile and value; from wide to long
forecasts <- gather(forecasts, key = "quantile", value = "value", `1%`:`99%`)
forecasts$quantile <- stringr::str_remove_all(forecasts$quantile, "%")
forecasts$quantile <- as.numeric(forecasts$quantile) / 100

## Add the 50th percentile as a point estimate
percentile50 <- forecasts[forecasts$quantile == 0.50, ]
percentile50$type <- "point"

forecasts <- rbind(forecasts, percentile50)
forecasts <- arrange(forecasts, location)

## Only keep the columns needed for submission
forecasts$value <- round(forecasts$value)
by_model <- split(forecasts, forecasts$model) %>%
  map(function(x) {
    select(
      x, forecast_date, target, target_end_date, location, type,
      quantile, value
    )
  })

## Copy directly into target folder
outdir <- "/Users/sbhatia/GitWorkArea/covid19-forecast-hub-europe/data-processed/Imperial"
iwalk(
  by_model, function(x, model) {
    model <- stringr::str_remove(model, "_Std_results")
    readr::write_csv(x, glue::glue("{prefix}-{model}.csv"))
    readr::write_csv(x, glue::glue("{outdir}-{model}/{prefix}-{model}.csv"))
  }
)

## To keep orderly happy
iwalk(
  by_model, function(x, model) {
    model <- stringr::str_remove(model, "_Std_results")
    readr::write_csv(x, glue::glue("{model}.csv"))
  }
)

