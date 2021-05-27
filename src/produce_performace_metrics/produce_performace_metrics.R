run_info <- orderly::orderly_run_info()
output_files <- run_info$depends$as
output_files <- output_files[output_files != "model_input.rds"]



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



model_predictions_error <- imap(
  model_outputs,
  function(x, model) {
    message("model")
    message(model)
    pred <- x[["Predictions"]]
    imap_dfr(
      pred,
      function(y, cntry) {
        message("cntry")
        names(y) <- c("si_1", "si_2")
        out <- map_dfr(
          y,
          function(y_si) {
            y_si <- as.matrix(y_si)
            dates2 <- as.Date(colnames(y_si))
            obs <- model_input[model_input$dates %in% dates2, cntry]
            if (! is.numeric(obs)) obs <- as.numeric(obs[[cntry]])
            ## This is the week previous to the week for which
            ## forcasts were made
            if (length(x) > 0) {
              metrics <- all_metrics(obs, y_si)
              metrics$date <- dates2
              metrics$obs <- obs
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
  }
)

model_predictions_error <- do.call('rbind', model_predictions_error)
model_predictions_error$model <- as.Date(week_ending)
model_predictions_error <- select(
  model_predictions_error, model, everything()
)


readr::write_csv(
  x = model_predictions_error, path = "model_predictions_error.csv"
)

