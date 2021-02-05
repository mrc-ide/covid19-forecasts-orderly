df_to_list <- function(df) {
  
  out <- df$normalised_wt
  names(out) <- df$model
  out
}

#####

ensemble_predictions <- function(outputs, weights) {
  
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

#####

ensemble_rt <- function(outputs) {
  
  ## y is the state-specific outputs
  y <- outputs
  ## Each model in y has 2 components, one for each SI.
  ## Determine quantiles
  
  y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
  y_1_all <- unlist(y_1)
  y_1 <- quantile(
    y_1_all,
    probs = probs
  )
  y_1 <- tibble::rownames_to_column(
    data.frame(out2 = y_1),
    var = "quantile"
  )
  y_1$si <- "si_1"
  
  y_2 <- purrr::map(y, ~ .[[2]]) ## si_2
  y_2_all <- unlist(y_2)
  y_2 <- quantile(
    y_2_all,
    probs = probs
  )
  y_2 <- tibble::rownames_to_column(
    data.frame(out2 = y_2),
    var = "quantile"
  )
  y_2$si <- "si_2"
  
  out <- rbind(y_1, y_2)
  
  out
  
}
