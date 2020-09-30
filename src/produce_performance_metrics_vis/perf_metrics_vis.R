## orderly::orderly_develop_start(
## use_draft = "newer", parameters = list(use_si = "si_2"))
######### Performance metrics

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



weekly_incidence <- readRDS("weekly_incidence.rds")
weekly_incidence$forecast_date <- as.Date(weekly_incidence$week_starting)

continent <- readr::read_csv("country_continent.csv") %>%
  janitor::clean_names()



better_than_null <- readRDS("better_than_null.rds")

######################################################################
######################################################################
################## Unweighted Ensemble ###############################
######################################################################
######################################################################

unwtd_pred_error <- readr::read_csv("unwtd_pred_error.csv") %>%
  dplyr::filter(si == use_si)
unwtd_pred_error$country[unwtd_pred_error$country == "Czech_Republic"] <- "Czechia"
##unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- rename(unwtd_pred_error, "forecast_date" = "model")

unwtd_pred_error <- unwtd_pred_error[unwtd_pred_error$country != "Kyrgyzstan", ]

n_forecast <- group_by(unwtd_pred_error, country) %>%
  summarise(n = length(unique(forecast_date))) %>%
  ungroup()

more_than_15 <- n_forecast$country[n_forecast$n >= 15]
less_than_15 <- n_forecast$country[n_forecast$n < 15 & n_forecast$n > 3]

more_forecasts <- unwtd_pred_error[unwtd_pred_error$country %in% more_than_15, ]
less_forecasts <- unwtd_pred_error[unwtd_pred_error$country %in% less_than_15, ]

weekly_summary <- function(df) {
######################################################################
################## Weekly Summary for each country ###################
######################################################################
  weekly <- group_by(df, forecast_date, country) %>%
    summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each country ##########################
######################################################################
  by_country <- group_by(df, country) %>%
    summarise_if(is.numeric, list(c_mu = mean, c_sd = sd)) %>%
    ungroup()

######################################################################
################## Summary for each week ##########################
######################################################################
  by_week <- group_by(df, forecast_date) %>%
    summarise_if(is.numeric, list(d_mu = mean, d_sd = sd)) %>%
    ungroup()

  n_forecast <- count(weekly, country)

  weekly <- left_join(weekly, n_forecast)
  weekly <- left_join(weekly, by_country)
  weekly <- left_join(weekly, by_week)

  weekly$top_label <- glue(
    "{round_and_format(weekly$prop_in_50_d_mu)}",
    " %+-% {round_and_format(weekly$prop_in_50_d_sd)}"
  )

  weekly$right_label <- glue(
    "{round_and_format(weekly$prop_in_50_c_mu)}",
    " %+-% {round_and_format(weekly$prop_in_50_c_sd)}"
  )

  weekly$cell_label <- glue(
    "{round_and_format(weekly$prop_in_50_mu)}"
    ##" %+-% {round_and_format(weekly$rel_mae_sd)}"
  )

  weekly$country <- factor(
    weekly$country,
    levels = better_than_null$country, ordered = TRUE
  )
  weekly$country <- droplevels(weekly$country)
  weekly
}


more_forecasts <- weekly_summary(more_forecasts)
less_forecasts <- weekly_summary(less_forecasts)
overall <- weekly_summary(unwtd_pred_error)

readr::write_csv(unwtd_pred_error, "unwtd_pred_weekly_summary.csv")
######################################################################
################## Proportion in 50% CrI by country ##################
######################################################################

x <- rename(more_forecasts, "prop_in_CrI" = "prop_in_50_mu")
y <- rename(less_forecasts, "prop_in_CrI" = "prop_in_50_mu")

p1 <- prop_in_cri_heatmap(x)
p2 <- prop_in_cri_heatmap(y)

ggsave(
  filename = "main_proportion_in_50_CrI.tiff",
  plot = p1,
  width = 7,
  height = 7,
  unit = "in",
  dpi = 300,
  compression = "lzw"
)

ggsave(
  filename = "si_proportion_in_50_CrI.tiff",
  plot = p2,
  width = 7,
  height = 7,
  unit = "in",
  dpi = 300,
  compression = "lzw"
)


######################################################################
################## Proportion in 95% CrI by country ##################
######################################################################
more_forecasts$top_label <- glue(
  "{round_and_format(more_forecasts$prop_in_975_d_mu)}",
  " %+-% {round_and_format(more_forecasts$prop_in_975_d_sd)}"
)

more_forecasts$right_label <- glue(
  "{round_and_format(more_forecasts$prop_in_975_c_mu)}",
  " %+-% {round_and_format(more_forecasts$prop_in_975_c_sd)}"
)

more_forecasts$cell_label <- glue(
  "{round_and_format(more_forecasts$prop_in_975_mu)}"
)

x <- rename(more_forecasts, "prop_in_CrI" = "prop_in_975_mu")
y <- rename(less_forecasts, "prop_in_CrI" = "prop_in_975_mu")

p1 <- prop_in_cri_heatmap(x, CrI = "95%")


less_forecasts$top_label <- glue(
  "{round_and_format(less_forecasts$prop_in_975_d_mu)}",
  " %+-% {round_and_format(less_forecasts$prop_in_975_d_sd)}"
)

less_forecasts$right_label <- glue(
  "{round_and_format(less_forecasts$prop_in_975_c_mu)}",
  " %+-% {round_and_format(less_forecasts$prop_in_975_c_sd)}"
)

less_forecasts$cell_label <- glue(
  "{round_and_format(less_forecasts$prop_in_975_mu)}"
)

p2 <- prop_in_cri_heatmap(y, CrI = "95%")

ggsave(
  filename = "main_proportion_in_95_CrI.tiff",
  plot = p1,
  width = 7,
  height = 7,
  unit = "in",
  dpi = 300,
  compression = "lzw"
)

ggsave(
  filename = "si_proportion_in_95_CrI.tiff",
  plot = p2,
  width = 7,
  height = 7,
  unit = "in",
  dpi = 300,
  compression = "lzw"
)


#####################################################################
#####################################################################
#####################################################################
########## Observed vs Median Predicitons Density ###################
#####################################################################
#####################################################################
x50_all <- select(unwtd_pred_error, country, obs, median_pred)
x50_all <- distinct(x50_all)
x50_all <- filter(x50_all, obs >= 0)

##x50_all$log_obs <- log(x50_all$obs, 10)
##x50_all$log_median_pred <- log(x50_all$median_pred, 10)
##ymax <- max(x50_all$log_median_pred, x50_all$log_obs, na.rm = TRUE)
##ymax <- ceiling(ymax)

bin_start <- seq(0, 2000, by = 100)
bin_end  <- bin_start + 100
##bin_start <- c(-Inf, bin_start)
##bin_end <- c(0, bin_end)

x50_all$obs_category <- apply(
  x50_all,
  1,
  function(row) {
    idx <- map2_lgl(
      bin_start, bin_end, function(x, y) between(row[["obs"]], x, y)
    )
    idx <- Position(isTRUE, idx)
    glue::glue("[{bin_start[idx]}, {bin_end[idx]})")
  }
)


x50_all$pred_category <- apply(
  x50_all,
  1,
  function(row) {
    idx <- map2_lgl(
      bin_start, bin_end, function(x, y) between(row[["median_pred"]], x, y)
    )
    idx <- Position(isTRUE, idx)
    glue::glue("[{bin_start[idx]}, {bin_end[idx]})")
  }
)

x50_all$pred_category[x50_all$median_pred >= 2100] <- "[2100, Inf)"

categories <- glue::glue("[{bin_start}, {bin_end})")
categories <- c(categories, "[2100, Inf)")

y <- dplyr::count(x50_all, pred_category, obs_category)

normalised <- map_dfr(
  categories,
  function(category) {
    x <- y[y$obs_category == category, ]
    x$proportion <- x$n / sum(x$n)
    x
  }
)

normalised$pred_category <- factor(
  normalised$pred_category, levels = categories, ordered = TRUE
)

normalised$obs_category <- factor(
  normalised$obs_category, levels = categories, ordered = TRUE
)


pdensity <- ggplot() +
  geom_tile(
    data = normalised,
    aes(obs_category, pred_category, fill = proportion),
    width = 0.9, height = 0.9
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_distiller(
    palette = "YlOrRd", direction = 1,
    breaks = c(0, 0.5, 1),
    labels = c(0, 0.5, 1),
    limits = c(0, 1),
    name = "Proportion in bin"
  ) +
  theme_minimal() +
  xlab("Observed daily deaths") +
  ylab("Median predictied daily deaths") +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 6),
    axis.text.x = element_text(
      angle = 90, hjust = 0, vjust = 0, size = 6
    ),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 6),
    legend.key.width = unit(1, "lines"),
    legend.key.height = unit(1, "lines")
  )

y <- data.frame(pred_category = categories, obs_category = categories)

pdensity2 <- pdensity +
  geom_point(
    data = y, aes(obs_category, pred_category), shape = "cross"
  ) +
  scale_shape_identity() +
  coord_fixed()

ggsave(
  filename = "obs_predicted_2d_density.tiff",
  plot = pdensity2,
  width = 5.2,
  height = 5.2,
  unit = "in",
  compression = "lzw"
)

normalised <- select(normalised, -n) %>%
  spread(pred_category, proportion, fill = 0)

readr::write_csv(normalised, "obs_predicted_2d_density.csv")

######################################################################
######################################################################
######################################################################
############## SI Text Figure
############## Model Relative Error
##############
######################################################################
######################################################################
######################################################################
more_forecasts$top_label <- glue(
  "{round_and_format(more_forecasts$rel_mae_d_mu)}",
  " %+-% {round_and_format(more_forecasts$rel_mae_d_sd)}"
)

more_forecasts$right_label <- glue(
  "{round_and_format(more_forecasts$rel_mae_c_mu)}",
  " %+-% {round_and_format(more_forecasts$rel_mae_c_sd)}"
)

more_forecasts$cell_label <- glue(
  "{round_and_format(more_forecasts$rel_mae_mu)}"
)

p1 <- relative_error_heatmap(more_forecasts)


less_forecasts$top_label <- glue(
  "{round_and_format(less_forecasts$rel_mae_d_mu)}",
  " %+-% {round_and_format(less_forecasts$rel_mae_d_sd)}"
)

less_forecasts$right_label <- glue(
  "{round_and_format(less_forecasts$rel_mae_c_mu)}",
  " %+-% {round_and_format(less_forecasts$rel_mae_c_sd)}"
)

less_forecasts$cell_label <- glue(
  "{round_and_format(less_forecasts$rel_mae_mu)}"
)

p2 <- relative_error_heatmap(less_forecasts)

ggsave(
  filename = "main_relative_error_heatmap.tiff",
  plot = p1,
  width = 7,
  height = 5.2,
  unit = "in",
  compression = "lzw"
)

ggsave(
  filename = "si_relative_error_heatmap.tiff",
  plot = p2,
  width = 7,
  height = 5.2,
  unit = "in",
  compression = "lzw"
)

######################################################################
######################################################################
#######################  Weekly Figures ##############################
######################################################################
######################################################################
weekly <- left_join(overall, weekly_incidence)
## All countries, Relative mean error on log scale and weekly incidence
pall <- ggplot(
  weekly, aes(weekly_incid, rel_mae_mu)
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

f <- scales::trans_breaks("log10", function(x) 10^x)
g <- scales::trans_format("log10", scales::math_format(10^.x))
breaks <- union(f(weekly$weekly_cv), f(weekly$rel_mae_mu))
labels <- g(breaks)

pcv_all <- ggplot(
  weekly, aes(weekly_cv, rel_mae_mu)
) + geom_point() +
  scale_y_log10(
    breaks = breaks, labels = labels, limits = c(NA, 100)
  ) +
  scale_x_log10(
    breaks = breaks, labels = labels, limits = c(NA, 100)
  ) +
  theme_minimal() +
  xlab("(log) Coefficient of variation of incidence)") +
  ylab("(log) Relative mean error") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("rmae_vs_weekly_cv_all_countries.png", pcv_all)


######################################################################
######################################################################
##################### Relative Error #################################
######################################################################
######################################################################

p1 <- ggplot(overall) +
  geom_point(
    aes(country, rel_mae_c_mu)
  ) +
  geom_linerange(
    aes(
      x = country,
      ymin = rel_mae_c_mu - rel_mae_c_sd,
      ymax = rel_mae_c_mu + rel_mae_c_sd)
  ) +
  scale_x_discrete(labels = nice_country_name) +
  ##facet_wrap(~continent, ncol = 1, scales = "free") +
  theme_minimal() +
  xlab("") + ylab("Relative error") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)
  )

ggsave("relative_error_by_country.png", p1)


p2 <- ggplot(overall) +
  geom_point(
    aes(forecast_date, rel_mae_d_mu)
  ) +
  geom_linerange(
    aes(
      x = forecast_date,
      ymin = rel_mae_d_mu - rel_mae_d_sd,
      ymax = rel_mae_d_mu + rel_mae_d_sd)
  ) +
  scale_x_date(
    date_breaks = "1 week"
  ) +
  ##facet_wrap(~continent, ncol = 1, scales = "free") +
  theme_minimal() +
  xlab("") + ylab("Relative error") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)
  )

ggsave("relative_error_by_.png", p2)

######################################################################
######## Performance of longer forecasts #############################
######################################################################
######################################################################
long_perf <- readRDS("long_projections_error_weekly.rds")
more_forecasts <- long_perf[long_perf$country %in% more_than_15, ]
less_forecasts <- long_perf[long_perf$country %in% less_than_15, ]

weekly_summary_long <- function(df) {

  by_country <- group_by(df, country) %>%
    summarise_if(is.numeric, list(c_mu = mean, c_sd = sd)) %>%
    ungroup()

  by_week <- group_by(df, week_of_projection) %>%
    summarise_if(is.numeric, list(w_mu = mean, w_sd = sd)) %>%
    ungroup()

  df <- left_join(df, by_country)
  df <- left_join(df, by_week)

  df$country <- factor(
    df$country,
    levels = better_than_null$country, ordered = TRUE
  )
  df$country <- droplevels(df$country)

  df
}

more_forecasts <- weekly_summary_long(more_forecasts)
less_forecasts <- weekly_summary_long(less_forecasts)

more_forecasts$top_label <- glue(
  "{round_and_format(more_forecasts$rel_mae_w_mu)}",
  " %+-% {round_and_format(more_forecasts$rel_mae_w_sd)}"
)

more_forecasts$right_label <- glue(
  "{round_and_format(more_forecasts$rel_mae_c_mu)}",
  " %+-% {round_and_format(more_forecasts$rel_mae_c_sd)}"
)

more_forecasts$cell_label <- glue(
  "{round_and_format(more_forecasts$rel_mae)}"
)



p1 <- long_relative_error_heatmap(more_forecasts)

less_forecasts$top_label <- glue(
  "{round_and_format(less_forecasts$rel_mae_w_mu)}",
  " %+-% {round_and_format(less_forecasts$rel_mae_w_sd)}"
)

less_forecasts$right_label <- glue(
  "{round_and_format(less_forecasts$rel_mae_c_mu)}",
  " %+-% {round_and_format(less_forecasts$rel_mae_c_sd)}"
)

less_forecasts$cell_label <- glue(
  "{round_and_format(less_forecasts$rel_mae)}"
)

p2 <- long_relative_error_heatmap(less_forecasts)

ggsave(
  filename = "main_rel_error_long.tiff",
  plot = p1,
  width = 7,
  height = 7,
  unit = "in",
  dpi = 300,
  compression = "lzw"
)

ggsave(
  filename = "si_rel_error_long.tiff",
  plot = p2,
  width = 7,
  height = 7,
  unit = "in",
  dpi = 300,
  compression = "lzw"
)
