df_to_list <- function(df) {
  
  out <- df$normalised_wt
  names(out) <- df$model
  out
}

produce_ensemble <- function(outputs, weights) {
  
  y <- outputs
  models_this_week <- names(y)
  
  if (is.null(weights)) {
    message("Unweighted ensemble outputs")
    wts <- data.frame(
      model = models_this_week,
      normalised_wt = 1
    )
    weights <- list(si_1 = wts, si_2 = wts)
    
  }
  
  y_1 <- purrr::map(y, ~ .[[1]])
  y_2 <- purrr::map(y, ~ .[[2]])
  weights <- purrr::map(weights, df_to_list)
  out <-  list(
    si_1 = pool_predictions_weighted(y_1, weights$si_1),
    si_2 = pool_predictions_weighted(y_2, weights$si_2)
  )
  out
}