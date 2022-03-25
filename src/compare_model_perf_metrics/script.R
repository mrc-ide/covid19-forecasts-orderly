## orderly::orderly_develop_start(use_draft = "newer", parameters = list(use_si = "si_2"))
source("R/weekly_error_summary.R")
better_than_null <- readRDS("better_than_null.rds")
country_groups <- readRDS("country_groups.rds")

model_perf <- read_csv("unwtd_pred_error.csv")
model_perf <- filter(model_perf, si == "si_2")
model_perf <- rename(model_perf, "forecast_date" = "model")

weekly_summaries <- group_by(
  model_perf, model_name, country
) %>%
  summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
  ungroup()

y <- tidyr::gather(weekly_summaries, var, val, mae_mu:obs_sd)
y <- tidyr::spread(y, model_name, val)
colnames(y)[3:6] <- c("ApeEstim", "DeCa", "Ensemble", "RtI0")
y_metrics <- split(y, y$var)
y_metrics <- map(y_metrics, function(x) {
  x$best <- apply(x[, c(-1, -2)], 1, which.min)
  x
})


out <- map_dfr(y_metrics, function(x) count(x, best), .id = "var")
out <- tidyr::spread(out, best, n)
