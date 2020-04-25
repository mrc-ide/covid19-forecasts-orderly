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

infiles <- list(
  "wtd_2020-04-12.rds",
  "wtd_2020-04-05.rds",
  "wtd_2020-03-29.rds",
  "wtd_2020-03-22.rds",
  "wtd_2020-03-15.rds"

)

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)


weighted <- purrr::map(infiles, readRDS)

infiles <- list(
  "unwtd_2020-04-12.rds",
  "unwtd_2020-04-05.rds",
  "unwtd_2020-03-29.rds",
  "unwtd_2020-03-22.rds",
  "unwtd_2020-03-15.rds"
)

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)

unweighted <- purrr::map(infiles, readRDS)

model_predictions_error <- purrr::map_dfr(
  weighted,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred,
          function(y_si) {
            y_si <- t(y_si) ## Need T X N matrix for assessr
            dates2 <- as.Date(rownames(y_si))
            x <- dplyr::filter(
              model_input, dates %in% dates2) %>%
              pull(cntry)
            rel_mae <- assessr::rel_mae(obs = x, pred = y_si)
            rel_mse <- assessr::rel_mse(obs = x, pred = y_si)
            avg_likelhd <- rel_mse2(obs = x, pred = y_si)
            bias <- assessr::bias(obs = x, pred = y_si)
            metrics <- data.frame(
              rel_abs = rel_mae,
              rel_sq = rel_mse,
              avg_likelhd = avg_likelhd,
              bias = bias
            )
            metrics <- tibble::rownames_to_column(metrics, var = "date")
            metrics
          }, .id = "si"
        )
        out
      },
      .id = "country"
    )
  },   .id = "forecast_date"
)


unwtd_model_predictions_error <- purrr::map_dfr(
  unweighted,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred,
          function(y_si) {
            y_si <- t(y_si) ## Need T X N matrix for assessr
            dates2 <- as.Date(rownames(y_si))
            x <- dplyr::filter(
              model_input, dates %in% dates2) %>%
              pull(cntry)
            rel_mae <- assessr::rel_mae(obs = x, pred = y_si)
            rel_mse <- assessr::rel_mse(obs = x, pred = y_si)
            avg_likelhd <- rel_mse2(obs = x, pred = y_si)
            bias <- assessr::bias(obs = x, pred = y_si)
            metrics <- data.frame(
              rel_abs = rel_mae,
              rel_sq = rel_mse,
              avg_likelhd = avg_likelhd,
              bias = bias
            )
            metrics <- tibble::rownames_to_column(metrics, var = "date")
            metrics
          }, .id = "si"
        )
        out
      },
      .id = "country"
    )
  },   .id = "forecast_date"
)

library(ggplot2)
library(ggpubr)
model_predictions_error$date <- as.Date(model_predictions_error$date)
unwtd_model_predictions_error$date <- as.Date(unwtd_model_predictions_error$date)
model_predictions_error <- model_predictions_error[model_predictions_error$si == "si_2", ]
unwtd_model_predictions_error <- unwtd_model_predictions_error[unwtd_model_predictions_error$si == "si_2", ]

model_predictions_error$forecast_date <- gsub(
  x = model_predictions_error$forecast_date,
  pattern = "wtd_",
  replacement = ""
)
model_predictions_error$proj <- "Weighted"
unwtd_model_predictions_error$forecast_date <- gsub(
  x = unwtd_model_predictions_error$forecast_date,
  pattern = "unwtd_",
  replacement = ""
)
unwtd_model_predictions_error$proj <- "Unweighted"
df <- rbind(model_predictions_error, unwtd_model_predictions_error)
readr::write_csv(x = df, path = "weighted_vs_unweighted_metrics.csv")
df <- split(df, df$forecast_date)
purrr::iwalk(
  df,
  function(x, forecast_date) {
     p <- ggplot(x, aes(date, avg_likelhd, col = proj)) +
       geom_point() +
       theme_pubr() +
       theme(legend.position = "top", legend.title = element_blank())

     ggsave(
       glue::glue("weighted_unweighted_{forecast_date}.pdf"), p
     )
  }
)



