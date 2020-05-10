rel_mse2 <- function(obs, pred) {
  nsims <- ncol(pred)
  obs <- matrix(
    rep(obs, each = nsims),
    ncol = nsims,
    byrow = TRUE
  )
  log_l <- rowSums(
    dpois(
      x = obs,
      lambda = pred + .5,
      log = TRUE
    )
  )
  log_l <- log_l / nsims# (nsims * (obs^2 + 1))
  log_l
}

model_input <- readRDS("model_input.rds")
deaths_tall <- tidyr::gather(model_input, country, deaths, -dates)
deaths_tall$dates <- as.Date(deaths_tall$dates)

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

infiles <- infiles[infiles != "model_input.rds"]
names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)

outputs <- purrr::map(infiles, readRDS)

f <- function(model_input, y_si, cntry) {

  y_si <- t(y_si) ## Need T X N matrix for assessr
  dates2 <- as.Date(rownames(y_si))
  x <- dplyr::filter(
     model_input, dates %in% dates2) %>% pull(cntry)

  rel_mae <- assessr::rel_mae(obs = x, pred = y_si)
  rel_mse <- assessr::rel_mse(obs = x, pred = y_si)
  avg_likelhd <- rel_mse2(obs = x, pred = y_si)
  bias <- assessr::bias(obs = x, pred = y_si)
  sharpness <- assessr::rel_mean_dvtn(pred = y_si)

  smallest <- apply(y_si, 1, quantile, 0.025)
  largest <- apply(y_si, 1, quantile, 0.975)
  propinci <- assessr::prop_in_ci(obs = x, min = smallest, max = largest)

  metrics <- data.frame(
    rel_abs = rel_mae,
    rel_sq = rel_mse,
    avg_likelhd = avg_likelhd,
    bias = bias,
    propinci = propinci,
    sharpness = sharpness
  )
  metrics <- tibble::rownames_to_column(metrics, var = "date")
  metrics

}

unwtd <- outputs[grep(
  x = names(outputs), pattern = "unwtd", value = TRUE
)]

##unwtd <- purrr::map(unwtd, ~ .[[1]])

unwtd_metrics <- purrr::map_dfr(
  unwtd,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred, ~ f(model_input, ., cntry), .id = "si"
        )
        out
      },
      .id = "country"
    )
  }, .id = "forecast_date"
)


wtd_all_prev_weeks <- outputs[grep(
  x = names(outputs), pattern = "wtd_all_prev_weeks", value = TRUE
)]

##unwtd <- purrr::map(unwtd, ~ .[[1]])

wtd_all_prev_weeks_metrics <- purrr::map_dfr(
  wtd_all_prev_weeks,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred, ~ f(model_input, ., cntry), .id = "si"
        )
        out
      },
      .id = "country"
    )
  }, .id = "forecast_date"
)


wtd_prev_week <- outputs[grep(
  x = names(outputs), pattern = "wtd_prev_week", value = TRUE
)]

##unwtd <- purrr::map(unwtd, ~ .[[1]])

wtd_prev_week_metrics <- purrr::map_dfr(
  wtd_prev_week,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred, ~ f(model_input, ., cntry), .id = "si"
        )
        out
      },
      .id = "country"
    )
  }, .id = "forecast_date"
)

library(ggplot2)

allmetrics <- dplyr::bind_rows(
  list(
    wtd_prev_week = wtd_prev_week_metrics,
    wtd_all_prev_weeks = wtd_all_prev_weeks_metrics,
    unweighted = unwtd_metrics
  ),
  .id = "model"
)


allmetrics <- allmetrics[allmetrics$si == "si_2", ]
allmetrics$forecast_date <- gsub(
  '[[:alpha:]]+', '', allmetrics$forecast_date
)
allmetrics$forecast_date <- gsub(
  '_', '', allmetrics$forecast_date
)
##allmetrics$forecast_date <- factor(allmetrics$forecast_date)
allmetrics$avg_likelhd[allmetrics$avg_likelhd < -2000] <- NA

ggplot(allmetrics) +
  geom_violin(aes(forecast_date, propinci, fill = model)) +
  theme_classic()

  ##ggforce::facet_wrap_paginate(~country, nrow = 2, ncol = 2, page = 1, scales = "free_y")
