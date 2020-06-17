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

wtd_all_prev_weeks_error <- readr::read_csv(
  "wtd_all_prev_weeks_error.csv"
) %>% dplyr::filter(si == use_si)

wtd_all_prev_weeks_error$strategy <- "Weighted (all previous weeks)"

wtd_all_prev_weeks_error <- tidyr::separate(
  wtd_all_prev_weeks_error,
  col = "model",
  into = c(NA, NA, NA, NA, "forecast_date"),
  sep = "_"
)
wtd_all_prev_weeks_error$rel_mae <- log(
  wtd_all_prev_weeks_error$rel_mae, 10
)
p1 <- metrics_over_time(
  wtd_all_prev_weeks_error,
  use_si,
  main_text_countries,
  "rel_mae",
  labels
)

p1 <- p1 +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = "l")

p2 <- metrics_over_time(
  wtd_all_prev_weeks_error,
  use_si,
  main_text_countries,
  "rel_sharpness",
  labels
)

p <- cowplot::plot_grid(
  p1, p2, labels = c('A', 'B'), label_size = 12, align = "l", ncol = 1
)

cowplot::save_plot("wtd_all_prev_weeks_metrics.tiff", p, base_height = 5)

wtd_prev_week_error <- readr::read_csv(
  "wtd_prev_week_error.csv"
  )

wtd_prev_week_error$strategy <- "Weighted (previous week)"
wtd_prev_week_error <- tidyr::separate(
  wtd_prev_week_error,
  col = "model",
  into = c(NA, NA, NA, "forecast_date"),
  sep = "_"
)


p1 <- metrics_over_time(
  wtd_prev_week_error,
  use_si,
  main_text_countries,
  "rel_mae",
  labels
)

p1 <- p1 +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = "l")

p2 <- metrics_over_time(
  wtd_prev_week_error,
  use_si,
  main_text_countries,
  "rel_sharpness",
  labels
)

p <- cowplot::plot_grid(
  p1, p2, labels = c('A', 'B'), label_size = 12, align = "l", ncol = 1
)

cowplot::save_plot("wtd_prev_week_metrics.tiff", p, base_height = 5)


unwtd_pred_error <- readr::read_csv("unwtd_pred_error.csv")
unwtd_pred_error$rel_mae <- log(
  unwtd_pred_error$rel_mae, 10
)
unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- tidyr::separate(
  unwtd_pred_error,
  col = "model",
  into = c(NA, "forecast_date"),
  sep = "_"
)

p1 <- metrics_over_time(
  unwtd_pred_error,
  use_si,
  main_text_countries,
  "rel_mae",
  labels
)

p1 <- p1 +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = "l")

p2 <- metrics_over_time(
  unwtd_pred_error,
  use_si,
  main_text_countries,
  "rel_sharpness",
  labels
)

p <- cowplot::plot_grid(
  p1, p2, labels = c('A', 'B'), label_size = 12, align = "l", ncol = 1
)

cowplot::save_plot("unwtd_pred_metrics.tiff", p, base_height = 5)

df <- rbind(
  wtd_all_prev_weeks_error, wtd_prev_week_error, unwtd_pred_error
)
df <- df[df$si == use_si, ]

df <- dplyr::left_join(df, observed)
df$incid_level <- dplyr::case_when(
  df$deaths <= 100 ~ "Less than 100 deaths",
  df$deaths > 100 ~ "More than 100 deaths",
)

dftall <- tidyr::gather(df, var, val, `rel_mae`:`poisson_p`)


x <- dplyr::filter(
  dftall,
  var %in% c("rel_mae", "rel_sharpness") &
  country %in% main_text_countries
)

x$val[x$var == "rel_mae"] <- log(x$val[x$var == "rel_mae"], base = 10)


ggplot(x, aes(forecast_date, val, fill = strategy)) +
  geom_boxplot() +
  facet_grid(var~incid_level)

