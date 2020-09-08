## orderly::orderly_develop_start(
## use_draft = FALSE, parameters = list(use_si = "si_2"))
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
          seq(to = -1, length.out = idx - 1, by = 1),
          seq(
            from = 1, length.out = length(cum_deaths) - idx + 1, by = 1
          )
        )
      }
      df$days_since_100_deaths <- days_since_100_deaths
      df
   }
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


######################################################################
################## Weekly Summary for each country ###################
######################################################################
weekly <- group_by(unwtd_pred_error, forecast_date, country) %>%
  summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
  ungroup()

######################################################################
################## Summary for each country ##########################
######################################################################
by_country <- group_by(unwtd_pred_error, country) %>%
  summarise_if(is.numeric, list(c_mu = mean, c_sd = sd)) %>%
  ungroup()

######################################################################
################## Summary for each week ##########################
######################################################################
by_week <- group_by(unwtd_pred_error, forecast_date) %>%
  summarise_if(is.numeric, list(d_mu = mean, d_sd = sd)) %>%
  ungroup()

n_forecast <- count(weekly, country)
######################################################################
######################################################################
################## Proportion in 95% CrI by country ##################
######################################################################
######################################################################
nice_country_name <- function(x) snakecase::to_title_case(as.character(x))

round_and_format <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}


prop_in_cri_heatmap <- function(df, CrI = "50%") {

  df$forecast_date <- factor(df$forecast_date)
  xmax <- max(as.numeric(df$forecast_date)) + 1
  ymax <- max(as.numeric(factor(df$country))) + 1

  p <- ggplot(df) +
  geom_tile(
    aes(forecast_date, country, fill = prop_in_50_mu),
    width = 0.8, height = 0.8
  ) +
    theme_classic() +
    scale_fill_distiller(
      palette = "Greens",
      direction = 1,
      breaks = c(0, 0.5, 1),
      labels = c(0, 0.5, 1),
      limits = c(0, 1),
      name = glue("Proportion in {CrI} CrI")
    ) +
  geom_text(
    aes(x = xmax, y = country, label = right_label),
    parse = TRUE, size = 2
  ) +
  geom_text(
    aes(x = forecast_date, y = ymax, label = top_label),
    parse = TRUE, angle = 90, hjust = 0, vjust = 0, size = 2
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
    plot.margin = margin(t = 30, r = 20, b = 0, l = 0),
    legend.position = "bottom",
    axis.line.x = element_blank()
  ) +
  scale_y_discrete(
    limits = rev(levels(df$country)),
    labels = nice_country_name
  ) +
  xlab("") +
  ylab("") +
  coord_cartesian(clip = "off")

  p
}

weekly <- left_join(weekly, n_forecast)
weekly <- left_join(weekly, by_country)
weekly <- left_join(weekly, by_week)

weekly$top_label <- glue(
  "{round_and_format(round(weekly$prop_in_50_d_mu))}",
  " %+-% {round_and_format(weekly$prop_in_50_d_sd)}"
)

weekly$right_label <- glue(
  "{round_and_format(round(weekly$prop_in_50_c_mu))}",
  " %+-% {round_and_format(weekly$prop_in_50_c_sd)}"
)

weekly$country <- factor(
  weekly$country,
  levels = better_than_null$country, ordered = TRUE
)

more_forecasts <- weekly[weekly$n >= 15, ]
more_forecasts$country <- droplevels(more_forecasts$country)

less_forecasts <- weekly[weekly$n < 15 & weekly$n > 3, ]
less_forecasts$country <- droplevels(less_forecasts$country)

p1 <- prop_in_cri_heatmap(more_forecasts)
p2 <- prop_in_cri_heatmap(less_forecasts)

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


#####################################################################
#####################################################################
#####################################################################
########## Observed vs Median Predicitons Density ###################
#####################################################################
#####################################################################
x50_all <- select(unwtd_pred_error_daily, country, obs, median_pred)
x50_all <- distinct(x50_all)
x50_all <- filter(x50_all, obs >= 0)

##x50_all$log_obs <- log(x50_all$obs, 10)
##x50_all$log_median_pred <- log(x50_all$median_pred, 10)
##ymax <- max(x50_all$log_median_pred, x50_all$log_obs, na.rm = TRUE)
##ymax <- ceiling(ymax)

bin_start <- seq(0, 4000, by = 100)
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

x50_all$pred_category[x50_all$median_pred >= 4100] <- "[4100, Inf)"

categories <- glue::glue("[{bin_start}, {bin_end})")
categories <- c(categories, "[4100, Inf)")

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
    name = "Proportion in bin"
  ) +
  theme_minimal() +
  xlab("Observed daily deaths") +
  ylab("Median predictied daily deaths") +
  theme(
    legend.position = "top",
    ##legend.title = element_blank(),
    axis.text.x = element_text(angle = -90, hjust = 0),
    legend.key.width = unit(2, "lines")
  )

y <- data.frame(pred_category = categories, obs_category = categories)

pdensity2 <- pdensity +
  geom_point(
    data = y, aes(obs_category, pred_category), shape = "cross"
  ) +
  scale_shape_identity() +
  coord_fixed()

ggsave("obs_predicted_2d_density.png", pdensity2)

normalised <- select(normalised, -n) %>%
  spread(pred_category, proportion, fill = 0)

readr::write_csv(normalised, "obs_predicted_2d_density.csv")
######################################################################
######################################################################
######################################################################
######################################################################
unwtd_pred_error <- dplyr::group_by(
  unwtd_pred_error, forecast_date, country, si
) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
dplyr::ungroup()

## This has one row for each quantile.
unweighted_rt_qntls <- readRDS("unweighted_rt_qntls.rds") %>%
  dplyr::filter(si == use_si)
unweighted_rt_qntls$country[unweighted_rt_qntls$country == "Czech_Republic"] <- "Czechia"

unwtd_pred_error$forecast_date <- as.Date(unwtd_pred_error$forecast_date)
unwtd_pred_error <- dplyr::left_join(unwtd_pred_error, weekly_incidence)

unweighted_rt_qntls$forecast_date <- as.Date(unweighted_rt_qntls$forecast_date)
unwtd_pred_error <- dplyr::left_join(
  unwtd_pred_error, unweighted_rt_qntls
)

unwtd_pred_error_daily$forecast_date <- as.Date(unwtd_pred_error_daily$forecast_date)
unwtd_pred_error_daily <- dplyr::left_join(
  unwtd_pred_error_daily, unweighted_rt_qntls
)


w_use_strategy <- list(
  ##wtd_prev_week = wtd_prev_week_error,
  ##wtd_all_prev_weeks = wtd_all_prev_weeks_error,
  unwtd = unwtd_pred_error
)

d_use_strategy <- list(
  ##wtd_prev_week = wtd_prev_week_error_daily,
  ##wtd_all_prev_weeks = wtd_all_prev_weeks_error_daily,
  unwtd = unwtd_pred_error_daily
)

weekly <- w_use_strategy[[strategy]]
daily <- d_use_strategy[[strategy]]






######################################################################
######################################################################
#######################  Daily Figures ###############################
######################################################################
######################################################################

pdaily <- ggplot(
  daily, aes(obs, rel_mae, col = phase)
) +
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
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +

  theme_minimal() +
  xlab("Weekly Incidence") +
  ylab("(log) Relative mean error") +
  theme(legend.position = "top", legend.title = element_blank())

ggsave("rmae_vs_weekly_incid_main_countries.png", pmain)

######################################################################
weekly$phase_label <- snakecase::to_title_case(
  weekly$phase, parsing_option = 0
)

pcv_all <- ggplot(
  weekly, aes(weekly_cv, rel_mae, col = phase_label)
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
  xlab("(log) Coefficient of variation of incidence)") +
  ylab("(log) Relative mean error") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("rmae_vs_weekly_cv_all_countries.png", pcv_all)


pcv_main <- ggplot() +
  geom_point(
    data = weekly[weekly$country %in% main_text_countries, ],
    aes(weekly_cv, rel_mae, col = phase),
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



######################################################################
######################################################################
##################### Relative Error #################################
######################################################################
######################################################################

x <- left_join(daily, null_error, by = c("date" = "dates", "country"))
x <- select(x, forecast_date, country, rel_null_error, rel_mae)
x <- gather(x, var, val, -forecast_date, -country)
x <- dplyr::distinct(x)


by_country <- group_by(x, country, var) %>%
  summarise(
    mu = mean(val, na.rm = TRUE),
    sigma = sd(val, na.rm = TRUE),
    median = quantile(val, probs = 0.5, na.rm = TRUE),
    low = quantile(val, probs = 0.25, na.rm = TRUE),
    high = quantile(val, probs = 0.75, na.rm = TRUE)
  ) %>% ungroup()

by_country <- left_join(
  by_country, continent, by = c("country" = "countries_and_territories")
)
by_country$country <- forcats::fct_reorder(by_country$country, by_country$continent, min)


by_week <- group_by(x, forecast_date, var) %>%
  summarise(
    mu = mean(val, na.rm = TRUE),
    sigma = sd(val, na.rm = TRUE),
    median = quantile(val, probs = 0.5, na.rm = TRUE),
    low = quantile(val, probs = 0.25, na.rm = TRUE),
    high = quantile(val, probs = 0.75, na.rm = TRUE)
  ) %>% ungroup()

p1 <- ggplot(by_week) +
  geom_point(
    aes(forecast_date, median, col = var),
    position = position_dodge(width = 0.5)
  ) +
  geom_linerange(
    aes(forecast_date, ymin = low, ymax = high, col = var),
    position = position_dodge(width = 0.5)
  ) +
  ##facet_wrap(~continent, ncol = 1, scales = "free") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 week") +
  xlab("") + ylab("Relative error") +
  theme(legend.position = "top", legend.title = element_blank())


out <- select(by_country, country, var, high)
out <- spread(out, var, high)
countries <- droplevels(
  out$country[out$rel_mae < 10 & out$rel_null_error < 10]
)

x1 <- by_country[by_country$country %in% countries, ]
x1$country <- factor(x1$country)

p1 <- ggplot(by_country) +
  geom_point(
    aes(country, median, col = var),
    position = position_dodge(width = 0.3)
  ) +
  geom_linerange(
    aes(country, ymin = low, ymax = high, col = var),
    position = position_dodge(width = 0.3)
  ) +
  facet_wrap(~continent, ncol = 1, scales = "free") +
  theme_minimal() +
  xlab("") + ylab("Relative error") +
  theme(legend.position = "top", legend.title = element_blank())


x2 <- by_country[by_country$high >= 10, ]
x2 <- by_country[by_country$country %in% x2$country, ]
ggplot(x2) +
  geom_point(
    aes(country, median, col = var),
    position = position_dodge(width = 0.1)
  ) +
  geom_linerange(
    aes(country, ymin = low, ymax = high, col = var),
    position = position_dodge(width = 0.1)
  ) +
  facet_wrap(~continent, ncol = 1, scales = "free")


by_country <- tidyr::gather(by_country, var, val, -country)
vars <- c("null_050", "null_025", "null_975")
x <- by_country[by_country$var %in% ]
ggplot(x) +
  geom_point(aes(country, ), col = "red") +
  geom_linerange(
    aes(country, ymin = null_025, ymax = weeklly_rel_null_975),
    col = "red"
  ) +
  geom_point(aes(country, weeklly_rel_pred_050), col = "blue") +
  geom_linerange(
    aes(country, ymin = weeklly_rel_pred_025, ymax = weeklly_rel_pred_975),
    col = "blue"
  ) + ylim(0, 50)


weekly_rel_err$forecast_date <- factor(weekly_rel_err$forecast_date)



## Across all countries
weekly_rel_err$forecast_date <- as.Date(weekly_rel_err$forecast_date)

x <- group_by(weekly_rel_err, country) %>%
  summarise_if(is.numeric, list(mean, median, quantile), na.rm = TRUE) %>%
  ungroup()

x <- gather(weekly_rel_err, var, val, -country, -forecast_date)

x1 <- x[x$country %in% x$country[1:50], ]
x2 <- x[x$country %in% x$country[51:99], ]

p1 <- ggplot(x, aes(as.factor(forecast_date), val, fill = var)) +
  geom_boxplot(position = "dodge") +
  ##scale_y_log10() +
  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top", legend.title = element_blank()
  ) +
  ylim(0, 5)


ggplot() +
  geom_half_violin(
    data = x[x$var == "weekly_rel_pred", ],
    aes(as.factor(forecast_date), val),
    side = "l", alpha = 0.3, fill = "blue"
  ) +
  geom_half_violin(
    data = x[x$var == "weekly_rel_null", ],
    aes(as.factor(forecast_date), val),
    side = "r", alpha = 0.3, fill = "red"
  ) +

  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top", legend.title = element_blank()
  )

p2 <- ggplot(x2, aes(country, val, col = var)) +
  geom_point() +
  ##scale_y_log10() +
  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "none", legend.title = element_blank()
  )


p <- p1 + p2 + plot_layout(ncol = 1)

## Across all weeks
x <- group_by(weekly_rel_err, forecast_date) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup()

x <- gather(x, var, val, -forecast_date)


p1 <- ggplot(x, aes(forecast_date, val, col = var)) +
  geom_point() +
  ##scale_y_log10() +
  theme_minimal() +
  xlab("") +
  ylab("Mean relative error") +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top", legend.title = element_blank()
  ) +
  scale_x_date(date_breaks = "1 week")

######################################################################
############### Proportion of observations in in 50% CrI
############### Proportion of observations in in 95% CrI
######################################################################
######################################################################
######################################################################
prop_by_country <- group_by(daily, country) %>%
  summarise(
    `Proportion in 50% CrI` = mean(prop_in_50),
    `Proportion in 95% CrI` = mean(prop_in_975)
  ) %>% ungroup()

prop_by_country <- left_join(
  prop_by_country, continent, by = c("country" = "countries_and_territories")
)
prop_by_country$country <- snakecase::to_title_case(prop_by_country$country)

prop_by_country$country <- fct_reorder(
  prop_by_country$country, prop_by_country$`Proportion in 50% CrI`, max
)

prop_by_country <- gather(
  prop_by_country, var, val, `Proportion in 50% CrI`:`Proportion in 95% CrI`
)

p <- ggplot(prop_by_country, aes(country, val, shape = var)) +
  geom_point() +
  geom_hline(yintercept = c(0.5, 0.975), linetype = "dashed") +
  facet_wrap(~continent, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  xlab("") +
  ylim(0, 1) +
  ylab("Proportion of observations in 50%/95% CrI")


ggsave("proportion_in_CrI_by_country.png", p)




prop_by_week <- group_by(daily, forecast_date) %>%
  summarise(
    `Proportion in 50% CrI` = mean(prop_in_50),
    `Proportion in 95% CrI` = mean(prop_in_975)
  ) %>% ungroup()


prop_by_week <- gather(
  prop_by_week, var, val, `Proportion in 50% CrI`:`Proportion in 95% CrI`
)

p <- ggplot(prop_by_week, aes(forecast_date, val, shape = var)) +
  geom_point() +
  geom_hline(yintercept = c(0.5, 0.975), linetype = "dashed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  scale_x_date(breaks = prop_by_week$forecast_date) +
  xlab("") +
  ylim(0, 1) +
  ylab("Proportion of observations in 50%/95% CrI")


ggsave("proportion_in_CrI_by_week.png", p)




######################################################################
######################################################################
######################################################################
############## SI Text Figure
############## Model Relative Error
##############
######################################################################
######################################################################
######################################################################
weekly_compare <- group_by(
  unwtd_pred_error, forecast_date, country, si
) %>%
  summarise(
    weekly_rel_err_mu = mean(rel_mae),
    weekly_rel_err_sd = sd(rel_mae)
  ) %>%
  ungroup()

by_country <- group_by(unwtd_pred_error, country, si) %>%
  summarise(
    mu_by_country = mean(rel_mae),
    sd_by_country = sd(rel_mae)
  ) %>%
  ungroup()

by_date <- group_by(unwtd_pred_error, forecast_date, si) %>%
  summarise(
    mu_by_date = mean(rel_mae),
    sd_by_date = sd(rel_mae)
  ) %>%
  ungroup()

n_forecasts <- count(weekly_compare, country)

weekly_compare <- left_join(weekly_compare, n_forecasts)
weekly_compare <- left_join(weekly_compare, by_country)
weekly_compare <- left_join(weekly_compare, by_date)
## weekly_compare <- mutate_if(weekly_compare, is.numeric, ~ signif(., 2))

## Format all numeric values so that a
## fixed number of digits are displayed
weekly_compare$weekly_rel_err_label <- format(
  round(weekly_compare$weekly_rel_err, 2),
  nsmall = 2
)

weekly_compare$mu_by_country <- format(
  round(weekly_compare$mu_by_country, 2),
  nsmall = 2
)

weekly_compare$mu_by_date <- format(
  round(weekly_compare$mu_by_date, 2),
  nsmall = 2
)


weekly_compare$country <- factor(
  weekly_compare$country,
  levels = better_than_null$country,
  ordered = TRUE
)
weekly_compare$forecast_date <- factor(weekly_compare$forecast_date)

more_forecasts <- weekly_compare[weekly_compare$n >= 15, ]
more_forecasts$country <- droplevels(more_forecasts$country)


less_forecasts <- weekly_compare[weekly_compare$n < 15 & weekly_compare$n > 3, ]
less_forecasts$country <- droplevels(less_forecasts$country)

ymax <- max(as.integer(more_forecasts$country)) + 1
xmax <- max(as.integer(more_forecasts$forecast_date)) + 1





p1 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = more_forecasts[more_forecasts$weekly_rel_err < 2, ],
    aes(forecast_date, country, fill = weekly_rel_err),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "Spectral", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Relative Error",
      title.position = "left",
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = more_forecasts[more_forecasts$weekly_rel_err >= 2, ],
    aes(forecast_date, country),
    fill = "#b2b2ff",
    width = 0.9,
    height = 0.8
  ) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "lines"),
    plot.margin = margin(t = 8, r = 8, b = 0, l = 0, unit = "pt")
  ) +
  scale_y_discrete(
    limits = rev(levels(more_forecasts$country)),
    labels = g
  ) +
  geom_text(
    data = more_forecasts,
    aes(x = forecast_date, y = ymax, label = mu_by_date),
    size = 2, fontface = "bold"
  ) +
  geom_text(
    data = more_forecasts,
    aes(x = xmax, y = country, label = mu_by_country),
    size = 2, fontface = "bold"
  ) +
  geom_text(
    data = more_forecasts,
    aes(x = forecast_date, y = country, label = weekly_rel_err_label),
    size = 2, fontface = "bold"
  ) +
  coord_cartesian(clip = "off")

ggsave(filename = "relative_error_heatmap.png", plot = p1)


######################################################################
######################################################################
######################################################################
############## SI Text Figure 2
############## Model Relative Error
############## Number of forecasts < 15 and > 3
######################################################################
######################################################################
######################################################################

ymax <- max(as.integer(less_forecasts$country)) + 1
xmax <- max(as.integer(less_forecasts$forecast_date)) + 1


p1 <- ggplot() +
  theme_classic() +
  geom_tile(
    data = less_forecasts[less_forecasts$weekly_rel_err < 2, ],
    aes(forecast_date, country, fill = weekly_rel_err),
    width = 0.9,
    height = 0.8
  ) +
  scale_fill_distiller(
    palette = "Spectral", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Relative Error",
      title.position = "left",
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = less_forecasts[less_forecasts$weekly_rel_err >= 2, ],
    aes(forecast_date, country),
    fill = "#b2b2ff",
    width = 0.9,
    height = 0.8
  ) +
  xlab("") +
  ylab("") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(2, "lines"),
    plot.margin = margin(t = 8, r = 8, b = 0, l = 0, unit = "pt")
  ) +
  scale_y_discrete(
    limits = rev(levels(less_forecasts$country)),
    labels = g
  ) +
  geom_text(
    data = less_forecasts,
    aes(x = forecast_date, y = ymax, label = mu_by_date),
    size = 2, fontface = "bold"
  ) +
  geom_text(
    data = less_forecasts,
    aes(x = xmax, y = country, label = mu_by_country),
    size = 2, fontface = "bold"
  ) +
  geom_text(
    data = less_forecasts,
    aes(x = forecast_date, y = country, label = weekly_rel_err_label),
    size = 2, fontface = "bold"
  ) +
  coord_cartesian(clip = "off")

ggsave(filename = "si_relative_error_heatmap.png", plot = p1)
