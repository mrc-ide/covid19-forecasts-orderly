output_files <- list.files(covid_19_path)
## Exclude the latest outputs as we don't have observed data for these
output_files <- grep(
  exclude, output_files, value = TRUE, invert = TRUE
)

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)

model_outputs <- purrr::map(output_files, ~ readRDS(paste0(covid_19_path, .)))

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


### The above metrics were for indivdiual models
### Now we produce similar metrics for ensemble model outputs
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
infiles <- infiles[infiles != "model_input.rds"]

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)

## All unweighted ensemble outputs
unweighted <- purrr::map(
  infiles[grep("unwtd", infiles)], readRDS
)

## Weighted using weights from previous weeks forecasts only
wtd_prev_week <- purrr::map(
  grep("wtd_prev_week", infiles, value = TRUE), readRDS
)

## Weighted using weights from all previous forecasts
wtd_all_prev_weeks <- purrr::map(
  grep("wtd_all_prev_weeks", infiles, value = TRUE), readRDS
)

unwtd_pred_error <- purrr::imap_dfr(
  unweighted,
  function(x, model) {
    message("########################################")
    message("####################", model, "####################")
    message("########################################")
    pred <- x[[1]]
    purrr::imap_dfr(
      pred,
      function(y, cntry) {
        message(cntry)
        names(y) <- c("si_1", "si_2")
        out <- purrr::map_dfr(
          y,
          function(y_si) {
            y_si <- as.matrix(y_si)
            dates2 <- as.Date(colnames(y_si))
            obs <- dplyr::filter(
              model_input, dates %in% dates2) %>% pull(cntry)

            out <- all_metrics(obs, y_si)
            out$date <- dates2
            out

          }, .id = "si"
        )
      }, .id = "country"
    )
  }, .id = "model"
)


wtd_prev_week_error <- purrr::imap_dfr(
  wtd_prev_week,
  function(x, model) {
    message(model)
    pred <- x[[1]]
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

            out <- all_metrics(obs, y_si)
            out$date <- dates2
            out
          }, .id = "si"
        )
      }, .id = "country"
    )
  }, .id = "model"
)


wtd_all_prev_weeks_error <- purrr::imap_dfr(
  wtd_all_prev_weeks,
  function(x, model) {
    message(model)
    pred <- x[[1]]
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
            out <- all_metrics(obs, y_si)
            out$date <- dates2
            out
          }, .id = "si"
        )
      }, .id = "country"
    )
  }, .id = "model"
)

readr::write_csv(
  x = model_predictions_error, path = "model_predictions_error.csv"
)

readr::write_csv(
  x = wtd_all_prev_weeks_error, path = "wtd_all_prev_weeks_error.csv"
)

readr::write_csv(
  x = wtd_prev_week_error, path = "wtd_prev_week_error.csv"
)

readr::write_csv(
  x = unwtd_pred_error, path = "unwtd_pred_error.csv"
)


