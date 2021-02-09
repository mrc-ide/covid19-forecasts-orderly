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

pool_rt_weighted <- function(outputs, weights, nsim = 10000) {
  
  models <- names(weights)
  ## Sample model with weights
  n_1 <- sample(
    x = names(weights), nsim, replace = TRUE, prob = weights
  )
  n_1 <- table(n_1)
  if (! all(models %in% names(n_1))) {
    idx <- which(! models %in% names(n_1))
    n_1[[models[idx]]] <- 0
  }
  message("Number of times models picked ")
  message(paste(n_1, collapse = "\n"))
  
  out <- purrr::imap(
    outputs,
    function(output, model) {
      sample(output, size = n_1[[model]])
    }
  )
  out <- Reduce(c, out)
  out
}




