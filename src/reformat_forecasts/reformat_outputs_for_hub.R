orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-03-14"))


forecast_date <- as.Date(week_ending) + 1
prefix <- glue::glue("{forecast_date}-Imperial")
forecasts <- readRDS("weekly_predictions_qntls.rds")
forecasts <- forecasts[forecasts$si == "si_2", ]


forecasts$continent <- countrycode(
  forecasts$country, origin = "country.name", destination = "continent"
)
forecasts <- forecasts[forecasts$continent == "Europe", ]
forecasts <- forecasts[complete.cases(forecasts), ]

## Date as YYYY-MM-DD, last day (Monday) of submission window
forecasts$forecast_date <- forecast_date
## target" # wk ahead inc case" or "# wk ahead inc death" where # is
## usually between 1 and 4
forecasts$target <- "1 wk ahead inc death"

## target_end_date
## Date as YYYY-MM-DD, the last day (Saturday) of the target week
forecasts$target_end_date <- as.Date(week_ending) + 6

## ISO-2 country code
forecasts$location <- countrycode(
  forecasts$country, origin = "country.name", destination = "iso2c"
)

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
by_model <- split(forecasts, forecasts$model) %>%
  map(function(x) {
    select(
      x, forecast_date, target, target_end_date, location, type,
      quantile, value
    )
  })


iwalk(
  by_model, function(x, model) {
    model <- stringr::str_remove(model, "_Std_results")
    readr::write_csv(x, glue::glue("{prefix}-{model}.csv"))
  }
)

## To keep orderly happy
iwalk(
  by_model, function(x, model) {
    model <- stringr::str_remove(model, "_Std_results")
    readr::write_csv(x, glue::glue("{model}.csv"))
  }
)
