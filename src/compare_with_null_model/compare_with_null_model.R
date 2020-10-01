## orderly::orderly_develop_start(
## use_draft = "newer", parameters = list(use_si = "si_2", latest_week = "2020-09-27"))
null_error <- readRDS("null_model_error.rds")
linear_error <- readRDS("linear_model_error.rds")
weekly_incidence <- readRDS("weekly_incidence.rds")
weekly_incidence$forecast_date <- as.Date(weekly_incidence$week_starting)

weekly_delta <- split(
  weekly_incidence, weekly_incidence$country
) %>%
  map_dfr(
    function(df) {
      df <- arrange(df, forecast_date)
      df$delta <- c(NA, diff(df$weekly_incid))
      df
    }
  )


unwtd_pred_error <- readr::read_csv("unwtd_pred_error.csv") %>%
  dplyr::filter(si == use_si)
unwtd_pred_error$country[unwtd_pred_error$country == "Czech_Republic"] <- "Czechia"
unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- rename(unwtd_pred_error, "forecast_date" = "model")

null_error$country[null_error$country == "Czech Republic"] <- "Czechia"
linear_error$country[linear_error$country == "Czech Republic"] <- "Czechia"

unwtd_pred_error$country[unwtd_pred_error$country == "Czech Republic"] <- "Czechia"
######################################################################
######################################################################
######################################################################
########### Comparison with baseline error ###########################
######################################################################
######################################################################
######################################################################
out <- data_prep(unwtd_pred_error, null_error)
null_compare <- out[["weekly_compare"]]
better_than_null <- out[["better_than_null"]]


more_forecasts <- null_compare[null_compare$n_forecasts >= 15, ]
more_forecasts$country <- droplevels(more_forecasts$country)


less_forecasts <- null_compare[null_compare$n_forecasts < 15 & null_compare$n_forecasts > 3, ]
less_forecasts$country <- droplevels(less_forecasts$country)

######################################################################
######################################################################
############### Main Text Figure
############### 15 or more forecasts
######################################################################
######################################################################


p1 <- compare_with_baseline(more_forecasts)
ggsave(
  filename = "comparison_with_baseline_error.tiff",
  plot = p1,
  ##device = agg_tiff,
  width = 8,
  height = 7.5,
  units = "in"
  #scaling = 1.3
)

######################################################################
######################################################################
######################################################################
######################################################################
############### SI Text Figure
############### More than 3 but less than 15 forecasts
######################################################################
######################################################################
######################################################################
p2 <- compare_with_baseline(less_forecasts)

ggsave(
  filename = "si_comparison_with_baseline_error.tiff",
  plot = p2,
  width = 8,
  height = 7.5,
  units = "in"
)

######################################################################
######################################################################
######################################################################
############## SI Text Figure
############## When the rate of change of deaths is small, model error
############## is large compared to null model error.
######################################################################
######################################################################
######################################################################
null_compare$forecast_date <- as.Date(null_compare$forecast_date)
df <- left_join(null_compare, weekly_delta)
df <- df[df$weekly_incid > 0, ]
df$log_ratio <- log(df$ratio, 10)

p <- ggplot(df, aes(delta, ratio)) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  ylab("log Model Error/Baseline Error") +
  xlab("Weekly change in deaths") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )

ggsave("log_ratio_vs_delta.tiff", p, width = 5, height = 5, unit = "in")
## x <- dplyr::count(null_compare, country)
## countries <- x$country




######################################################################
######################################################################
######################################################################
########### Comparison with linear model error #######################
######################################################################
######################################################################
######################################################################
out <- data_prep(unwtd_pred_error, linear_error)
null_compare <- out[["weekly_compare"]]
better_than_null <- out[["better_than_null"]]


more_forecasts <- null_compare[null_compare$n_forecasts >= 15, ]
more_forecasts$country <- droplevels(more_forecasts$country)


less_forecasts <- null_compare[null_compare$n_forecasts < 15 & null_compare$n_forecasts > 3, ]
less_forecasts$country <- droplevels(less_forecasts$country)

######################################################################
######################################################################
############### Main Text Figure
############### 15 or more forecasts
######################################################################
######################################################################


p1 <- compare_with_baseline(more_forecasts)
ggsave(
  filename = "comparison_with_linear_error.tiff",
  plot = p1,
  ##device = agg_tiff,
  width = 8,
  height = 7.5,
  units = "in"
  #scaling = 1.3
)

######################################################################
######################################################################
######################################################################
######################################################################
############### SI Text Figure
############### More than 3 but less than 15 forecasts
######################################################################
######################################################################
######################################################################
p2 <- compare_with_baseline(less_forecasts)

ggsave(
  filename = "si_comparison_with_linear_error.tiff",
  plot = p2,
  width = 8,
  height = 7.5,
  units = "in"
)
