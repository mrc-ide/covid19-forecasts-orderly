## orderly::orderly_develop_start(use_draft = "newer", parameters = list(use_si = "si_2"))
source("R/weekly_error_summary.R")
better_than_null <- readRDS("better_than_null.rds")
country_groups <- readRDS("country_groups.rds")

indv_perf <- readr::read_csv("model_predictions_error.csv")
indv_perf <- filter(indv_perf, si == "si_2")
indv_perf <- rename(indv_perf, "forecast_date" = "model")

weekly_summaries <- map(
  country_groups,
  function(countries) {
    df <- indv_perf[indv_perf$country %in% countries, ]
    df$country <- factor(df$country, levels = countries, ordered = TRUE)
    out <- split(df, df$model_name) %>%
      map_dfr(weekly_summary, .id = "model_name")
    out
  }
)

ensb_weekly <- readr::read_csv("unwtd_pred_weekly_summary.csv")


