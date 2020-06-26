## orderly::orderly_develop_start(
## use_draft = TRUE, parameters = list(use_si = "si_2", strategy = "unwtd"))
######### Performance metrics
observed <- readRDS("model_input.rds")

main_text_countries <- c(
  "Brazil", "India", "Italy", "Mexico", "United_States_of_America"
)

labels <- c(
  "rel_mae" = "Relative mean error",
  "rel_mse" = "Relative mean squared error",
  "rel_sharpness" = "Relative sharpness",
  "bias" = "Bias",
  "prop_in_50" = "Proportion in 50% CrI",
  "prop_in_975" = "Proportion in 97.5% CrI",
  "empirical_p" = "Probability(obs|predictions)"
)

observed_tall <- tidyr::gather(observed, country, deaths, -dates)

observed_tall <- split(
  observed_tall, observed_tall$country
) %>%
  purrr::map_dfr(
    function(df) {
      df$deaths_scaled <- df$deaths / max(df$deaths)
      df$moving_avg <- slider::slide_dbl(
        df$deaths, ~ mean(.x), .before = 3, .after = 3
      )
      cum_deaths <- cumsum(df$deaths)
      idx <- which(cum_deaths >= 100)
      if (length(idx) == 0) {
        days_since_100_deaths <- rep(0, length(cum_deaths))
      } else {
        idx <- idx[1]
        days_since_100_deaths <- c(
          rep(0, idx - 1),
          seq(
            from = 1, length.out = length(cum_deaths) - idx + 1, by = 1
          )
        )
      }
      df$days_since_100_deaths <- days_since_100_deaths
      df
   }
)
######################################################################
######################################################################
############# Using previou weeks ####################################
######################################################################
######################################################################
wtd_prev_week_error <- readr::read_csv(
  "wtd_prev_week_error.csv"
) %>% dplyr::filter(si == use_si)

wtd_prev_week_error$strategy <- "Weighted (previous week)"
wtd_prev_week_error <- tidyr::separate(
  wtd_prev_week_error,
  col = "model",
  into = c(NA, NA, NA, NA, "forecast_date"),
  sep = "_"
)

## Weekly metrics
wtd_prev_week_error <- dplyr::group_by(
  wtd_prev_week_error, forecast_date, country, si
) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
dplyr::ungroup()

wtd_prev_week_rt_qntls <- readRDS("wtd_prev_week_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)

## Combine with phase information
wtd_prev_week_error <- dplyr::left_join(
  wtd_prev_week_error, wtd_prev_week_rt_qntls
)
## Combine with observed deaths
forecast_sundays <- as.Date(unique(wtd_prev_week_error$forecast_date))
names(forecast_sundays) <- forecast_sundays

forecast_dates <- purrr::map(
  forecast_sundays,
  function(x) seq(x + 1, length.out = 7, by = "1 day")
)

weekly_incidence <- purrr::map_dfr(
  forecast_dates,
  function(dates) {
    x <- observed[observed$dates %in% dates, ]
    weekly <- data.frame(
      weekly_incid = colSums(x[, -1]),
      weekly_mean = colMeans(x[, -1]),
      weekly_sd = apply(x[, -1], 2, sd)
    )
    weekly$cv <- weekly$weekly_mean / weekly$weekly_sd
    weekly$incid_level <- case_when(
      weekly$weekly_incid <= 100 ~ "Weekly deaths <= 100",
      weekly$weekly_incid > 100 ~ "Weekly deaths > 100"
    )
    tibble::rownames_to_column(weekly, var = "country")
  }, .id = "forecast_date"
)

wtd_prev_week_error <- dplyr::left_join(
  wtd_prev_week_error, weekly_incidence
)

######################################################################
######################################################################
############# Using all previous weeks ###############################
######################################################################
######################################################################

wtd_all_prev_weeks_error <- readr::read_csv(
  "wtd_all_prev_weeks_error.csv"
) %>% dplyr::filter(si == use_si)

wtd_all_prev_weeks_error$strategy <- "Weighted (all previous weeks)"

wtd_all_prev_weeks_error <- tidyr::separate(
  wtd_all_prev_weeks_error,
  col = "model",
  into = c(NA, NA, NA, NA, NA, "forecast_date"),
  sep = "_"
)

wtd_all_prev_weeks_error <- dplyr::group_by(
  wtd_all_prev_weeks_error, forecast_date, country, si
) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
dplyr::ungroup()

wtd_all_prev_weeks_rt_qntls <- readRDS(
  "wtd_rt_all_prev_week_qntls.rds"
) %>% dplyr::filter(si == use_si)


wtd_all_prev_weeks_error <- dplyr::left_join(
  wtd_all_prev_weeks_error, weekly_incidence
)

wtd_all_prev_weeks_error <- dplyr::left_join(
  wtd_all_prev_weeks_error, wtd_all_prev_weeks_rt_qntls
)

######################################################################
######################################################################
################## Unweighted Ensemble ###############################
######################################################################
######################################################################

unwtd_pred_error <- readr::read_csv("unwtd_pred_error.csv") %>%
  dplyr::filter(si == use_si)

unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- tidyr::separate(
  unwtd_pred_error,
  col = "model",
  into = c(NA, NA, NA, NA, "forecast_date"),
  sep = "_"
)

unwtd_pred_error <- dplyr::group_by(
  unwtd_pred_error, forecast_date, country, si
) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
dplyr::ungroup()

## This has one row for each quantile.
unweighted_rt_qntls <- readRDS("unweighted_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)


unwtd_pred_error <- dplyr::left_join(
  unwtd_pred_error, weekly_incidence
)

unwtd_pred_error <- dplyr::left_join(
  unwtd_pred_error, unweighted_rt_qntls
)

#####################################################################
use_strategy <- list(
  wtd_prev_week = wtd_prev_week_error,
  wtd_all_prev_weeks = wtd_all_prev_weeks_error,
  unwtd = unwtd_pred_error
)
metrics <- use_strategy[[strategy]]

ggplot(
  metrics, aes(weekly_incid, rel_mae, col = phase)
) + geom_point() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) + facet_wrap(~phase, ncol = 1, scales = "free_y")


ggplot(
  metrics, aes(cv, rel_mae, col = phase)
) + geom_point() +
 facet_wrap(~phase, ncol = 1, scales = "free_y")


x <- metrics[metrics$country %in% main_text_countries, ]
x <- x[, c("forecast_date", "rel_mae", "cv", "country")]
x <- tidyr::gather(x, var, val, rel_mae:cv)

ggplot(
  x, aes(forecast_date, val, col = var)
) + facet_wrap(~country, ncol = 1, scales = "free_y") + geom_point()


ggplot(x, aes(cv, rel_mae, col = phase)) +
  geom_point() +
    scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
