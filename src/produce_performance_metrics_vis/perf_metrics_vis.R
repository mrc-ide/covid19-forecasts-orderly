## orderly::orderly_develop_start(
## use_draft = TRUE, parameters = list(use_si = "si_2"))
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
    weekly <- data.frame(weekly_incid = colSums(x[, -1]))
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

## This has one row for each quantile.
unweighted_rt_qntls <- readRDS("unweighted_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)


unwtd_pred_error <- dplyr::left_join(
  unwtd_pred_error, weekly_incidence
)

unwtd_pred_error <- dplyr::left_join(
  unwtd_pred_error, unweighted_rt_qntls
)

######################################################################
######################################################################
################ Figures for each strategy ###########################
######################################################################
######################################################################

## Trying dual axis
png("main_text_countries_rel_mae.png")
par(mar = c(5, 2, 1, 5), oma = c(3, 3, 2, 3))
layout(matrix(1:5, nrow = 5, ncol = 1))
for (country in main_text_countries) {
  x <- observed_tall[observed_tall$country %in% country, ]
  y <- wtd_prev_week_error[wtd_prev_week_error$country %in% country, ]
  y <- y[ , c("date", "country", "rel_mae", "forecast_date")]
  y <- tidyr::gather(y, var, val, rel_mae)
  y <- dplyr::left_join(y, x, by = c("date" = "dates", "country"))
  y <- na.omit(y)
  ## There is this one point in USA that is messing with the scale
  y <- y[y$val < 30, ]
  legend <- ifelse(country == "Brazil", TRUE, FALSE)
  xlab <- ifelse(
    country == "United_States_of_America", "Days since 100 deaths", ""
  )
  xticks <- ifelse(country == "United_States_of_America", TRUE, FALSE)

  scaled_incid_and_metric(incid = x, metrics = y, legend, xlab, xticks)
}
mtext(
  "Deaths (scaled)", side = 2, outer = TRUE, line = 0, col = "blue"
)
mtext(
  "Relative error", side = 4, outer = TRUE, line = 0, col = "red"
)
dev.off()

######################################################################
######################################################################
## Single axis for both scaled deaths and rmae
## With ggplot.
######################################################################
######################################################################
x <- observed_tall[observed_tall$country %in% main_text_countries, ]
y <- wtd_prev_week_error[wtd_prev_week_error$country %in% main_text_countries, ]
y <- y[ , c("date", "country", "rel_mae", "forecast_date")]
y <- tidyr::gather(y, var, val, rel_mae)
y <- dplyr::left_join(y, x, by = c("date" = "dates", "country"))
y <- na.omit(y)
## There is this one point in USA that is messing with the scale
y <- y[y$val < 30, ]

p <- ggplot() +
  geom_line(
    data = x, aes(days_since_100_deaths, deaths_scaled, col = "blue"),
  ) +
  geom_point(
    data = y, aes(days_since_100_deaths, val, col = "red"),
  ) +
  facet_wrap(
    ~country,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(country = snakecase::to_title_case)
  ) +
  theme_classic() +
  scale_color_identity(
    breaks = c("blue", "red"),
    labels = c("Deaths (scaled)", "Relative error"),
    guide = "legend",
  ) +
  theme(legend.position = "top", legend.title = element_blank()) +
  xlab("Days since 100 deaths") +
  ylab("Deaths (scaled)/Relative Error")

ggsave("main_text_countries_rel_mae_same_axis.png", p)
######################################################################
######################################################################
x <- dplyr::left_join(
  wtd_prev_week_error,
  observed_tall,
  by = c("date" = "dates", "country")
)
p1 <- metrics_over_time(
  x[x$rel_mae < 30, ], use_si, main_text_countries, "rel_mae", labels
) + xlab("")

## p1 <- p1 +
##   geom_hline(yintercept = 1, linetype = "dashed") +
##   scale_y_log10(
##     breaks = scales::trans_breaks("log10", function(x) 10^x),
##     labels = scales::trans_format("log10", scales::math_format(10^.x))
##   ) +
##   annotation_logticks(sides = "l")

p2 <- metrics_over_time(
  x, use_si, main_text_countries, "rel_sharpness", labels
)

p <- cowplot::plot_grid(
  p1, p2, labels = c('A', 'B'), label_size = 12, align = "l", ncol = 1
)

cowplot::save_plot("wtd_prev_week_metrics.png", p, base_height = 5)
######################################################################
######################################################################
######################################################################
######################################################################
#### By phase
######################################################################
######################################################################
######################################################################
######################################################################

p1_byphase <- p1 + geom_point(aes(col = phase)) +
  scale_color_manual(
    labels = c("Decline", "Stable/Growing slowly", "Growing", "Unclear"),
    breaks = c("decline", "stable/growing slowly", "growing", "unclear"),
    values = c("#33bd3e", "#ff7d4d", "#dec55c", "#0177dd")
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

p2_byphase <- p2 +
  geom_point(aes(col = phase)) +
  scale_color_manual(
    labels = c("Decline", "Stable/Growing slowly", "Growing", "Unclear"),
    breaks = c("decline", "stable/growing slowly", "growing", "unclear"),
    values = c("#33bd3e", "#ff7d4d", "#dec55c", "#0177dd")
  ) +
  theme(
    legend.position = "none"
  )

p_byphase <- cowplot::plot_grid(
  p1_byphase,
  p2_byphase,
  labels = c('A', 'B'),
  label_size = 12,
  align = "l",
  ncol = 1
)

cowplot::save_plot(
  "wtd_prev_week_metrics_by_phase.png", p_byphase, base_height = 5
)
######################################################################
######################################################################
######################################################################
######################################################################
#### By incidence level
######################################################################
######################################################################
######################################################################
######################################################################
