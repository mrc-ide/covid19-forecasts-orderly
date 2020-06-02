model_qntls <- readRDS("ensemble_daily_qntls.rds")
usa <- model_qntls[model_qntls$country == "United_States_of_America", ]
usa$proj <- as.Date(usa$proj)
usa$country <- "US"
usa <- tidyr::gather(usa, quantile, value, `1%`:`99%`)
usa$quantile <- readr::parse_number(usa$quantile) / 100
usa$type <- "quantile"
usa$target <- as.Date(usa$date) - as.Date(usa$proj)
usa$target <- glue::glue("{usa$target} day ahead inc death")

colnames(usa) <- c(
  "forecast_date", "location", "si",
  "target_end_date", "quantile", "values",
  "type", "target"
)

forecast_date <- usa$forecast_date[1]
x <- split(usa, usa$si)
readr::write_csv(x[[1]], "formatted_si_1.csv")
readr::write_csv(x[[2]], "formatted_si_2.csv")
readr::write_csv(x[[1]], glue::glue("{forecast_date}-Imperial-ensemble1.csv"))
readr::write_csv(x[[2]], glue::glue("{forecast_date}-Imperial-ensemble2.csv"))
