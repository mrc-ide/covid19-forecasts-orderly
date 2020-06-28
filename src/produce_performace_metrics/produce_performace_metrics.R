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

model_input <- readRDS("model_input.rds")
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
            obs <- dplyr::filter(
              model_input, dates %in% dates2) %>% pull(cntry)
            if (length(x) > 0) {
              metrics <- all_metrics(obs, y_si)
              metrics$date <- dates2
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

