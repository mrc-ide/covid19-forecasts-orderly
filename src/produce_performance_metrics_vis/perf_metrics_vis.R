## orderly::orderly_develop_start(
## use_draft = FALSE, parameters = list(use_si = "si_2", strategy = "unwtd"))
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
wtd_prev_week_error_daily <- wtd_prev_week_error

## Weekly metrics
wtd_prev_week_error <- dplyr::group_by(
  wtd_prev_week_error, forecast_date, country, si
) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
dplyr::ungroup()

wtd_prev_week_rt_qntls <- readRDS("wtd_prev_week_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)

## Combine with phase information
wtd_prev_week_error_daily <- dplyr::left_join(
  wtd_prev_week_error_daily , wtd_prev_week_rt_qntls
)

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

wtd_all_prev_weeks_error_daily <- wtd_all_prev_weeks_error

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

wtd_all_prev_weeks_error_daily <- dplyr::left_join(
  wtd_all_prev_weeks_error_daily, wtd_all_prev_weeks_rt_qntls
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
unwtd_pred_error_daily <- unwtd_pred_error
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

unwtd_pred_error_daily <- dplyr::left_join(
  unwtd_pred_error_daily, unweighted_rt_qntls
)

#####################################################################
w_use_strategy <- list(
  wtd_prev_week = wtd_prev_week_error,
  wtd_all_prev_weeks = wtd_all_prev_weeks_error,
  unwtd = unwtd_pred_error
)

d_use_strategy <- list(
  wtd_prev_week = wtd_prev_week_error_daily,
  wtd_all_prev_weeks = wtd_all_prev_weeks_error_daily,
  unwtd = unwtd_pred_error_daily
)

weekly <- w_use_strategy[[strategy]]
daily <- d_use_strategy[[strategy]]
######################################################################
######################################################################
#######################  Daily Figures ###############################
######################################################################
######################################################################

pdaily <- ggplot(daily, aes(obs, rel_mae, col = phase)) +
  geom_point() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  xlab("(log) Deaths") +
  ylab("(log) Relative Error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("daily_obs_error_all_countries.png", pdaily)


pdaily <- ggplot() +
  geom_point(
    data = daily[daily$country %in% main_text_countries, ],
    aes(obs, rel_mae, col = phase)
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  xlab("(log) Deaths") +
  ylab("(log) Relative Error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("daily_obs_error_main_countries.png", pdaily)

######################################################################
######################################################################
#######################  Weekly Figures ##############################
######################################################################
######################################################################

## All countries, Relative mean error on log scale and weekly incidence
pall <- ggplot(
  weekly, aes(weekly_incid, rel_mae, col = phase)
) + geom_point() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  xlab("(log) Weekly Incidence") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_incid_all_countries.png", pall)

## Main text countries,
## Relative mean error on log scale and weekly incidence
pmain <- ggplot() +
  geom_point(
    data = weekly[weekly$country %in% main_text_countries, ],
    aes(weekly_incid, rel_mae, col = phase)
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  xlab("Weekly Incidence") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_incid_main_countries.png", pmain)

######################################################################


pcv_all <- ggplot(
  weekly, aes(cv, rel_mae, col = phase)
) + geom_point() +
    scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  xlab("(log) Weekly coefficient of variation") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_cv_all_countries.png", pcv_all)


pcv_main <- ggplot() +
  geom_point(
    data = weekly[weekly$country %in% main_text_countries, ],
    aes(cv, rel_mae, col = phase),
    size = 2
  ) +
    scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal() +
  xlab("(log) Weekly coefficient of variation") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_cv_main_countries.png", pcv_main)
##############

## x <- readr::read_csv("model_predictions_error2.csv")
## x <- x[x$si == "si_2", ]


## y <- split(x, x$model)
## y[[1]] <- dplyr::group_by(y[[1]], forecast_date, country) %>%
##   dplyr::summarise(baseline = mean(baseline_error), rel_mae = mean(rel_mae)) %>%
##   dplyr::ungroup()

## y[[1]]$ratio <- y[[1]]$rel_mae / y[[1]]$baseline

## out <- dplyr::select(y[[1]], forecast_date, country, ratio)
## out$forecast_date <- factor(out$forecast_date)

## ggplot(
##   out, aes(forecast_date, country)
## ) + geom_tile(aes(fill = ratio)) +
##   theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
##   coord_flip()
