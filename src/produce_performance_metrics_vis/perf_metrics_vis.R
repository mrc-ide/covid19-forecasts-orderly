## orderly::orderly_develop_start(
## use_draft = "newer", parameters = list(use_si = "si_2"))
######### Performance metrics

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

better_than_null <- readRDS("better_than_null.rds")
country_groups <- readRDS("country_groups.rds")
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

weekly_summaries <- map(
  country_groups,
  function(countries) {
    df <- unwtd_pred_error[unwtd_pred_error$country %in% countries, ]
    weekly_summary(df)
  }
)

overall <- weekly_summary(unwtd_pred_error)

readr::write_csv(overall, "unwtd_pred_weekly_summary.csv")
######################################################################
################## Proportion in 50% CrI by country ##################
######################################################################
weeks <- seq(
  from = as.Date("2020-03-08"),
  to = as.Date("2020-11-30"),
  by = "7 days"
)

plots <- imap(
  weekly_summaries,
  function(df, page) {
    x <- rename(df, "prop_in_CrI" = "prop_in_50_mu")
    p <- prop_in_cri_heatmap(x, weeks)
    outfile <- glue("proportion_in_50_CrI_{page}.tiff")
    rincewind::save_multiple(plot = p, filename = outfile)
    p
  }
)

## For SI, we put 2 plots together with a common legend.
legend <- get_legend(plots[[2]])

prow <- plot_grid(
  plots[[1]] +
  theme(legend.position = "none", axis.text.x = element_blank()),
  plots[[2]] +
  theme(legend.position = "none", axis.text.x = element_blank()),
  plots[[3]] +
  theme(legend.position = "none"),
  plots[[4]] +
  theme(legend.position = "none"),
  ncol = 2
)

## Finally put the legend back in

p50 <- plot_grid(legend, prow, ncol = 1, rel_heights = c(0.1, 1))

outfile <- "proportion_in_50_CrI_si.tiff"
rincewind::save_multiple(plot = p50, filename = outfile, two_col = TRUE)
#### Overall metrics
## > mean(unwtd_pred_error$prop_in_50)
## [1] 0.580651
## > sd(unwtd_pred_error$prop_in_50)
## [1] 0.3271525
## > mean(unwtd_pred_error$prop_in_975)
## [1] 0.885443
## > sd(unwtd_pred_error$prop_in_975)
## [1] 0.2259728
####
######################################################################
################## Proportion in 95% CrI by country ##################
######################################################################
plots <- imap(
  weekly_summaries,
  function(df, page) {
    x <- rename(df, "prop_in_CrI" = "prop_in_975_mu")
    p <- prop_in_cri_heatmap(x, weeks, CrI = "95%")
    outfile <- glue("proportion_in_95_CrI_{page}.tiff")
    rincewind::save_multiple(plot = p, filename = outfile, two_col = FALSE)
    p
  }
)


legend <- get_legend(plots[[2]])

prow <- plot_grid(
  plots[[1]] +
  theme(legend.position = "none", axis.text.x = element_blank()),
  plots[[2]] +
  theme(legend.position = "none", axis.text.x = element_blank()),
  plots[[3]] + theme(legend.position = "none"),
  plots[[4]] + theme(legend.position = "none"),
  ncol = 2
)

## Finally put the legend back in

p95 <- plot_grid(legend, prow, ncol = 1, rel_heights = c(0.1, 1))
outfile <- glue("proportion_in_95_CrI_si.tiff")
rincewind::save_multiple(plot = p95, filename = outfile)

#####################################################################
#####################################################################
#####################################################################
########## Observed vs Median Predicitons Density ###################
#####################################################################
#####################################################################
x50_all <- select(
  unwtd_pred_error, country, obs, median_pred, forecast_date
)
x50_all <- distinct(x50_all)
x50_all <- filter(x50_all, obs >= 0)

##x50_all$log_obs <- log(x50_all$obs, 10)
##x50_all$log_median_pred <- log(x50_all$median_pred, 10)
##ymax <- max(x50_all$log_median_pred, x50_all$log_obs, na.rm = TRUE)
##ymax <- ceiling(ymax)

bin_start <- seq(0, 5000, by = 100)
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

x50_all$pred_category[x50_all$median_pred >= 5100] <- "[5100, Inf)"

categories <- glue::glue("[{bin_start}, {bin_end})")
categories <- c(categories, "[5100, Inf)")

##y <- dplyr::count(x50_all, pred_category, obs_category)
x50_all$forecast_month <- lubridate::month(
  x50_all$forecast_date, label = TRUE, abbr = FALSE
)

y <- group_by(
  x50_all, forecast_month, obs_category, pred_category
) %>% summarise(n = n()) %>%
  ungroup()


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

normalised$wave <- ifelse(
  normalised$forecast_month %in% levels(normalised$forecast_month)[1:6],
  "January - June",
  "June - November"
)

normalised <- group_by(
  normalised, wave, obs_category, pred_category
) %>% summarise(proportion = sum(proportion)) %>%
  ungroup()

## Zoom in on the [2100, Inf) region
normalised$facet <- case_when(
  (normalised$obs_category %in% categories[1:25]) &
  (normalised$pred_category %in% categories[1:25])  ~ "less_than_2500",
  TRUE ~ "greater_than_2500"
)

less_than_2500 <- normalised[normalised$facet == "less_than_2500", ]
less_than_2500 <- droplevels(less_than_2500)

more_than_2500 <- normalised[normalised$facet != "less_than_2500", ]
more_than_2500 <- droplevels(more_than_2500)

pdensity <- ggplot() +
  geom_tile(
    data = more_than_2500,
    aes(obs_category, pred_category, fill = proportion),
    width = 0.9, height = 0.9
  ) +
  scale_x_discrete(limits = categories[26:52], drop = FALSE) +
  scale_y_discrete(limits = categories[26:52], drop = FALSE) +
  scale_fill_distiller(
    palette = "YlOrRd", direction = 1,
    breaks = c(0, 0.5, 1),
    labels = c(0, 0.5, 1),
    limits = c(0, 1),
    name = "Proportion in bin"
  ) +
  xlab("Observed daily deaths") +
  ylab("Median predictied daily deaths") +
  theme_minimal() +
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
  ##facet_wrap(~wave)

y <- data.frame(pred_category = categories, obs_category = categories)

pdensity2 <- pdensity +
  geom_point(
    data = y, aes(obs_category, pred_category), shape = "cross"
  )
  ##scale_shape_identity()
  ##coord_fixed()

ggsave(
  filename = "obs_predicted_2d_density.tiff",
  plot = pdensity2,
  width = 5.2,
  height = 5.2,
  unit = "in",
  compression = "lzw"
)

normalised <- spread(normalised, pred_category, proportion, fill = 0)

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
plots <- map(
  weekly_summaries,
  function(df) {
    out <- augment_data(df, weeks, width = 2)
    relative_error_heatmap(
      out[["df"]], out[["x_labels"]], out[["y_labels"]]
    )
  }
)

plots <- rincewind::customise_for_rows(plots, in_rows = c(2, 3, 4))
iwalk(
  plots, function(p, page) {
    outfile <- glue("relative_error_heatmap_{page}.tiff")
    rincewind::save_multiple(plot = p, filename = outfile)
  }
)

## Comparison with linear error is all in SI.

plots <- rincewind::customise_for_rows(plots, in_rows = c(1, 2, 3, 4))
iwalk(
  plots, function(p, page) {
    outfile <- glue("relative_error_heatmap_{page}_2.tiff")
    rincewind::save_multiple(plot = p, filename = outfile)
  }
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
  xlab("(log) Coefficient of variation of incidence") +
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

ggsave("relative_error_overall.png", p2)


## mean(overall$rel_mae_mu[is.finite(overall$rel_mae_mu)])
## [1] 0.4768033
## > sd(overall$rel_mae_mu[is.finite(overall$rel_mae_mu)])
## [1] 0.4363448


if (! is.null(dev.list())) dev.off()
