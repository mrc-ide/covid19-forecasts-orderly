rel_mse2 <- function(obs, pred) {
  nsims <- ncol(pred)
  log_l <- rowSums(
    dpois(
      x = matrix(obs, ncol = nsims,  byrow = FALSE),
      lambda = pred + .5,
      log = TRUE
    )
  )
  log_l <- log_l / nsims# (nsims * (obs^2 + 1))
  log_l
}

output_files <- list(
  "DeCa_Std_results_week_end_2020-03-08.rds",
  "DeCa_Std_results_week_end_2020-03-15.rds",
  "DeCa_Std_results_week_end_2020-03-22.rds",
  "DeCa_Std_results_week_end_2020-03-29.rds",
  "DeCa_Std_results_week_end_2020-04-05.rds",
  "RtI0_Std_results_week_end_2020-03-08.rds",
  "RtI0_Std_results_week_end_2020-03-15.rds",
  "RtI0_Std_results_week_end_2020-03-22.rds",
  "RtI0_Std_results_week_end_2020-03-29.rds",
  "RtI0_Std_results_week_end_2020-04-05.rds",
  "sbkp_Std_results_week_end_2020-03-08.rds",
  "sbkp_Std_results_week_end_2020-03-15.rds",
  "sbkp_Std_results_week_end_2020-03-22.rds",
  "sbkp_Std_results_week_end_2020-03-29.rds",
  "sbkp_Std_results_week_end_2020-04-05.rds"
)

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)



model_outputs <- purrr::map(output_files, readRDS)

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
              model_input[["D_active_transmission"]],
              dates %in% dates2
            ) %>% pull(cntry)

            if (length(x) > 0) {
              out <- rel_mse2(obs = x, pred = y_si)
              out <- as.data.frame(out)
              out <- tibble::rownames_to_column(out, var = "date")
            } else {
              out <- NULL
            }
            out
          },
          .id = "si"
        )
      },
      .id = "country"
    )
  },
  .id = "model"
  )

readr::write_csv(
  x = model_predictions_error, path = "model_predictions_error.csv"
)
