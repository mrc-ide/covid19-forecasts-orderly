run_info <- orderly::orderly_run_info()
output_files <- run_info$depends$as
output_files <- output_files[output_files != "model_input.rds"]


##output_files <- list.files(covid_19_path)
## Exclude the latest outputs as we don't have observed data for these
## output_files <- grep(
##   exclude, output_files, value = TRUE, invert = TRUE
## )

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)

model_outputs <- purrr::map(output_files, readRDS)

f <- function(vec, window) {
  slider::slide_dbl(vec, ~mean(.x), .before = window, .after = window)
}

model_input <- readRDS("model_input.rds")
model_input <- dplyr::mutate_if(
  model_input, is.numeric, f, window = window
)
## Czech_Republic is the same as Czechia
model_input$Czech_Republic <- model_input$Czechia



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
            dates2 <- as.Date(colnames(y_si))
            obs <- model_input[model_input$dates %in% dates2, cntry]
            ## This is the week previous to the week for which
            ## forcasts were made
            prev_week <- dates2 - 7
            prev_week_avg <- mean(
              model_input[model_input$dates %in% prev_week, cntry]
            )
            ## The baseline error - if we only projected that the
            ## deaths next week will the average of the deaths last
            ## week
            null_pred <- matrix(
              mean(prev_week_avg), ncol = 10000, nrow = 7
            )
            baseline <- assessr::rel_mae(obs = obs, pred = null_pred)
            if (length(x) > 0) {
              metrics <- all_metrics(obs, y_si)
              metrics$date <- dates2
              metrics$baseline_error <- baseline
              metrics$prev_week_avg <- prev_week_avg

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

