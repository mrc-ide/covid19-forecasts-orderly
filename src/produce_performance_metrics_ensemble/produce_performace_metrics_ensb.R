f <- function(vec, window) {
  slider::slide_dbl(
    vec, ~round(mean(.x)), .before = window, .after = window
  )
}

model_input <- readRDS("model_input.rds")
model_input <- dplyr::mutate_if(
  model_input, is.numeric, f, window = window
)
## Czech_Republic is the same as Czechia
model_input$Czech_Republic <- model_input$Czechia

### The above metrics were for indivdiual models
### Now we produce similar metrics for ensemble model outputs
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
infiles <- infiles[infiles != "model_input.rds"]

names(infiles) <- week_ending
## Weighted using weights from previous weeks forecasts only
wtd_prev_week <- map(
  grep("wtd_ensb_prev_week", infiles, value = TRUE), readRDS
)

## Weighted using weights from all previous forecasts
wtd_all_prev_weeks <- map(
  grep("wtd_ensb_all_prev_weeks", infiles, value = TRUE), readRDS
)

unweighted <- map(
  infiles[grep("unwtd_ensemble_model", infiles)], readRDS
)

unwtd_pred_error <- imap_dfr(
  unweighted,
  function(x, model) {
    message("########################################")
    message("####################", model, "####################")
    message("########################################")
    pred <- x[[1]]
    imap_dfr(
      pred,
      function(y, cntry) {
        message(cntry)
        names(y) <- c("si_1", "si_2")
        out <- purrr::map_dfr(
          y,
          function(y_si) {
            y_si <- as.matrix(y_si)
            dates2 <- as.Date(colnames(y_si))
            obs <- model_input[model_input$dates %in% dates2, cntry]
            if(!is.numeric(obs) )obs <- obs[[cntry]]
            out <- all_metrics(obs, y_si)
            out$date <- dates2
            out$obs <- obs
            out
          }, .id = "si"
        )
      }, .id = "country"
    )
  }, .id = "model"
)

wtd_prev_week_error <- imap_dfr(
  wtd_prev_week,
  function(x, model) {
    message(model)
    pred <- x[[1]]
    imap_dfr(
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
            out$obs <- obs

            out
          }, .id = "si"
        )
      }, .id = "country"
    )
  }, .id = "model"
)


wtd_all_prev_weeks_error <- imap_dfr(
  wtd_all_prev_weeks,
  function(x, model) {
    message(model)
    pred <- x[[1]]
    imap_dfr(
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
            out$obs <- obs

            out
          }, .id = "si"
        )
      }, .id = "country"
    )
  }, .id = "model"
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
