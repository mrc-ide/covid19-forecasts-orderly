## Expect a T X N matrix where N is the number
## of simulations
poisson_probability <- function(obs, pred) {
  nsims <- ncol(pred)
  obs <- matrix(
    rep(obs, each = nsims),
    ncol = nsims,
    byrow = TRUE
  )
  p <- rowSums(
    dpois(
      x = obs,
      lambda = pred + 0.5,
      log = FALSE
    )
  )
  p / nsims# (nsims * (obs^2 + 1))
}

empirical_probability <- function(obs, pred) {

  pred_rows <- lapply(
    seq_len(nrow(pred)), function(idx) pred[idx, ]
  )
  probs <- mapply(
    FUN = function(x, xhat) {
      idx <- which(x == xhat)
      message("Number of times obs in pred ", length(idx))
      length(idx) / length(xhat)
    },
    x = obs,
    xhat = pred_rows
 )

  probs

}

output_files <- list.files(covid_19_path)

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)

model_outputs <- purrr::map(output_files, ~ readRDS(paste0(covid_19_path, .)))

model_input <- readRDS("model_input.rds")

model_predictions_error <- purrr::imap_dfr(
  model_outputs,
  function(x, model) {
    message(model)
    pred <- x[["Predictions"]]
    purrr::imap_dfr(
      pred,
      function(y, cntry) {
        names(y) <- c("si_1", "si_2")
        out <- purrr::map_dfr(
          y,
          function(y_si) {
            y_si <- as.matrix(y_si)
            y_si <- t(y_si) ## Need T X N matrix for assessr
            dates2 <- as.Date(rownames(y_si))
            x <- dplyr::filter(
              model_input, dates %in% dates2) %>% pull(cntry)

            if (length(x) > 0) {
              rel_mae <- assessr::rel_mae(obs = x, pred = y_si)
              rel_mse <- assessr::rel_mse(obs = x, pred = y_si)
              poisson_p <- poisson_probability(obs = x, pred = y_si)
              bias <- assessr::bias(obs = x, pred = y_si)
              empirical_p <- empirical_probability(obs = x, pred = y_si)
              metrics <- data.frame(
                rel_abs = rel_mae,
                rel_sq = rel_mse,
                poisson_p = poisson_p,
                empirical_p = empirical_p,
                bias = bias
              )
              metrics <- tibble::rownames_to_column(metrics, var = "date")
            } else {
              metrics <- NULL
            }
            metrics
          },
          .id = "si"
        )
      },
      .id = "country"
    )
  },
  .id = "model"
)

model_predictions_error <- tidyr::separate(
  model_predictions_error,
  col = "model",
  into = c("model", NA, NA, NA, NA, "forecast_date"),
  sep = "_",
  convert = TRUE
)

readr::write_csv(
  x = model_predictions_error, path = "model_predictions_error.csv"
)
