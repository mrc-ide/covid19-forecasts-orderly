## orderly::orderly_develop_start(
## use_draft = "newer", parameters = list(use_si = "si_2"))
dir.create("figures")
dir.create("figures/null")
dir.create("figures/linear")

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
  dplyr::filter(si == use_si, model_name == "ensemble")

unwtd_pred_error$country[unwtd_pred_error$country == "Czech_Republic"] <- "Czechia"
unwtd_pred_error$strategy <- "Unweighted"
unwtd_pred_error <- rename(unwtd_pred_error, "forecast_date" = "model")

exclude <- readRDS("exclude.rds")
unwtd_pred_error <- unwtd_pred_error[!unwtd_pred_error$country %in% exclude, ]
null_error$country[null_error$country == "Czech Republic"] <- "Czechia"
linear_error$country[linear_error$country == "Czech Republic"] <- "Czechia"
unwtd_pred_error$country[unwtd_pred_error$country == "Czech Republic"] <- "Czechia"

######################################################################
######################################################################
######################################################################
######### Make sure there are no spurious NAs
######################################################################
## null_error[!complete.cases(null_error), ] No NAs
## linear_error[!complete.cases(linear_error), ] No NAs
## unwtd_pred_error[!complete.cases(unwtd_pred_error), ] No NAs
## range(as.Date(null_error$dates))
## range(as.Date(linear_error$dates))
## range(as.Date(unwtd_pred_error$date))
######################################################################
######################################################################
######################################################################
########### Comparison with baseline error ###########################
######################################################################
######################################################################
######################################################################
out <- data_prep(unwtd_pred_error, null_error)
##  out[["weekly_compare"]][!complete.cases(out[["weekly_compare"]]), ]

null_compare <- na.omit(out[["weekly_compare"]])
better_than_null <- out[["better_than_null"]]

phase <- readRDS("unweighted_rt_qntls.rds")
### We don't care about the quantiles of Rt for this analysis
phase <- select(phase, forecast_date:phase)
phase <- distinct(phase)
null_compare <- left_join(null_compare, phase)
null_compare$country <- as.factor(null_compare$country)

y <- tabyl(null_compare, phase, err_level) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

saveRDS(y, "better_than_null_by_phase.rds")

saveRDS(better_than_null, "better_than_null.rds")

## This is so that we have as many countries as weeks to make a neat
## square plot
cutoff <- 25
country_groups <- list()
## Select every 4th element rather than the top 25
idx <- seq(from = 1, length.out = cutoff, by = 3)
page <- 1
covered <- sum(map_int(country_groups, length))

while (covered < nrow(better_than_null)) {
  idx <- idx[idx <= nrow(better_than_null)]
  country_groups[[page]] <- better_than_null$country[idx]
  idx <- seq(from = min(idx) + 1, length.out = cutoff, by = 3)
  page <- page + 1
  covered <- sum(map_int(country_groups, length))
}

saveRDS(country_groups, "country_groups.rds")

plots <- map(
  country_groups,
  function(countries, page) {
    df <- null_compare[null_compare$country %in% countries, ]
    df$country <- droplevels(df$country)
    out <- augment_data(df)
    compare_with_baseline(
      out[["df"]], out[["x_labels"]], out[["y_labels"]]
    )
  }
)

byphase_plots <- map(
  plots,
  function(p) {
    p + facet_wrap(~phase, ncol = 2)
  }
)

plots <- rincewind::customise_for_rows(plots, in_rows = c(2, 3, 4))

iwalk(
  plots, function(p, page) {
    outfile <- glue(
      "figures/null/comparison_with_baseline_error_{page}"
    )
    rincewind::save_multiple(filename = outfile, plot = p)
  }
)

iwalk(
  byphase_plots, function(p, page) {
    outfile <- glue(
      "figures/null/comparison_with_baseline_error_{page}_facetted"
    )
    rincewind::save_multiple(filename = outfile, plot = p)
  }
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

ggsave(
  "figures/null/log_ratio_vs_delta.png", p, width = 5,
  height = 5, unit = "in"
)
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
null_compare <- left_join(null_compare, phase)
null_compare$country <- as.factor(null_compare$country)

y <- tabyl(null_compare, phase, err_level) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

saveRDS(y, "better_than_linear_by_phase.rds")

saveRDS(better_than_null, "better_than_linear.rds")


plots <- map(
  country_groups,
  function(countries, page) {
    df <- null_compare[null_compare$country %in% countries, ]
    df$country <- droplevels(df$country)
    out <- augment_data(df)
    compare_with_baseline(
      out[["df"]], out[["x_labels"]], out[["y_labels"]]
    )
  }
)

plots_byphase <- map(
  plots,
  function(p) {
    p + facet_wrap(~phase, ncol = 2)
  }
)


plots <- rincewind::customise_for_rows(plots, in_rows = c(1, 2, 3, 4))

iwalk(plots, function(p, page) {
  outfile <- glue(
    "figures/linear/comparison_with_linear_error_{page}"
  )
  rincewind::save_multiple(filename = outfile, plot = p)
})

iwalk(plots_byphase, function(p, page) {
  outfile <- glue(
    "figures/linear/comparison_with_linear_error_{page}_facetted"
  )
  rincewind::save_multiple(filename = outfile, plot = p)
})




## tabyl(by_phase, phase)
