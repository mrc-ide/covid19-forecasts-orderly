######################################################################
## Extract model quantiles
## Each model output is a list with several elements,
## the key one is Predictions.
## This is a list of data.frames with one data.frame for each country
## For each country, we have a list of 2 components, corresponding
## to the 2 serial intervals being considered.
## it is this last list (country, 2 components) that is passed to
## this function.
extract_predictions_qntls <- function(y) {

  names(y) <- paste0("si_", seq_along(y))
  out <- map_dfr(
        y,
    function(y_si) {
      out2 <- t(
        apply(y_si,
              2,
              quantile,
              prob = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
              na.rm = TRUE
              )
      )
      out2 <- as.data.frame(out2)
      out2 <- tibble::rownames_to_column(out2, var = "date")
      out2

    }, .id = "si"
  )
  out
}

######################################################################
## Extract model quantiles
## Each model output is a list with several elements,
## the key one is Predictions.
## This is a list of data.frames with one data.frame for each country
## For each country, we have a list of 2 components, corresponding
## to the 2 serial intervals being considered.
## it is this last list (country, 2 components) that is passed to
## this function.
daily_to_weekly <- function(y) {

  names(y) <- paste0("si_", seq_along(y))
  out <- map_dfr(
    y,
    function(y_si) {
      weekly <- rowSums(y_si)
      weekly <- quantile(
        weekly,
        prob = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
        na.rm = TRUE
      )
      weekly_df <- as.data.frame(weekly)
      ## This is not the last date for which predictions are
      ## available, but the last date for which observations are
      ## available.
      weekly_df$week_ending <- as.Date(colnames(y_si)[1]) - 1
      weekly_df <- tibble::rownames_to_column(weekly_df, var = "quantile")
      tidyr::spread(weekly_df, key = quantile, value = weekly)
    }, .id = "si"
  )
  out
}
