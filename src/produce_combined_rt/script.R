infiles <- list.files(pattern = "*.rds")
rt_samples <- purrr::map_dfr(infiles, readRDS)
countries <- unique(
  rt_samples[rt_samples$model == week_ending, "country"]
)
names(countries) <- countries
map(
  countries,
  function(country) {
    weeks <- unique(
      rt_samples[rt_samples$country == country, "model"]
    )
    weeks <- as.Date(weeks)

  }
)


  x <- diff(weeks)
  message(country, "\n", paste0(x, collapse = " "))
}

country <- "United_States_of_America"

weeks <- seq(
  to = as.Date(week),
  from = as.Date("2020-03-08"),
  by = "7 days"
)

combined <- dplyr::filter(
  rt_samples, model == week & country == "United_States_of_America"
)

for (idx in 1:(length(weeks) - 1)) {
  iqr_combined <- quantile(
    combined$si_2, probs = c(0.25, 0.75)
  )
  iqr_combined[["25%"]] <- floor(iqr_combined[["25%"]])
  iqr_combined[["75%"]] <- ceiling(iqr_combined[["75%"]])

  prev_week <- weeks[length(weeks) - idx]
  message("Previous week is ", prev_week)
  prev_week_rt <- dplyr::filter(
    rt_samples,
    model == prev_week & country == "United_States_of_America"
  )
  iqr_prev <- quantile(
    prev_week_rt$si_2, probs = c(0.25, 0.75)
  )
  iqr_prev[["25%"]] <- floor(iqr_prev[["25%"]])
  iqr_prev[["75%"]] <- ceiling(iqr_prev[["75%"]])

  overlap <- rincewind::overlaps(iqr_prev, iqr_combined)
  if (overlap) {
    combined <- combine_rt(
      rt_samples, length(weeks):(length(weeks) - idx)
    )
  } else break
}
