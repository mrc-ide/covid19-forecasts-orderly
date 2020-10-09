## orderly::orderly_develop_start(
## use_draft = "newer", parameters = list(use_si = "si_2"))
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

saveRDS(better_than_null, "better_than_null.rds")

## This is so that we have as many countries as weeks to make a neat
## square plot
cutoff <- 25
country_groups <- list()
idx <- seq(from = 1, length.out = cutoff, by = 1)
page <- 1

while (max(idx) < nrow(better_than_null)) {
  idx <- idx[idx <= nrow(better_than_null)]
  country_groups[[page]] <- better_than_null$country[idx]
  idx <- seq(from = max(idx) + 1, length.out = cutoff, by = 1)
  page <- page + 1
}

saveRDS(country_groups, "country_groups.rds")

iwalk(
  country_groups,
  function(countries, page) {
    df <- null_compare[null_compare$country %in% countries, ]
    df$country <- droplevels(df$country)
    out <- augment_data(df)
    p1 <- compare_with_baseline(
      out[["df"]], out[["x_labels"]], out[["y_labels"]]
    )
    outfile <- glue("comparison_with_baseline_error_{page}.tiff")
    rincewind::save_multiple(
      filename = outfile, plot = p1, one_col = FALSE
    )
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

saveRDS(better_than_null, "better_than_linear.rds")


iwalk(
  country_groups,
  function(countries, page) {
    df <- null_compare[null_compare$country %in% countries, ]
    df$country <- droplevels(df$country)
    out <- augment_data(df)
    p1 <- compare_with_baseline(
      out[["df"]], out[["x_labels"]], out[["y_labels"]]
    )
    outfile <- glue("comparison_with_linear_error_{page}.tiff")
    rincewind::save_multiple(
      filename = outfile, plot = p1, one_col = FALSE
    )
  }
)
