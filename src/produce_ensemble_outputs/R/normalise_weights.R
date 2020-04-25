weights_for_country <- function(country, weights, models_this_week, models_prev_week) {

  message(country)
  idx_curr <- names(purrr::keep(models_this_week, ~ country %in% .))
  models_curr <- strsplit(x = idx_curr, split = "-")[[1]]
  message("Current models ", idx_curr)

  if (idx_curr %in% names(models_prev_week)) {
    models_old <- models_curr
  } else {
    ## For our simple case this will suffice.
    ## We have M1, M2, M3 for some countries
    ## And for some countries we have M1, M2, M3, M4.
    ## We want to use the model weights from last weekf or M1, M2, M3
    ## and for M4 we want to assign 1/4.
    ## This will get complicated when we start having diff countries
    ## for diff models.
    models_old <- names(models_prev_week)[1]

  }
  message("Old models ", models_old)

  n_models <- length(models_curr)
  new_models <- length(setdiff(models_curr, models_old))
  names(models_curr) <- models_curr
  out <- purrr::map(
    models_curr,
    function(model) {
      if (!model %in% models_old) {
        1 / n_models
      } else {
        unassigned_wt <- (n_models - new_models) / n_models
        normalised_wt <- weights$weight[weights$model == model] /
          sum(weights$weight[weights$model %in% models_curr])
        normalised_wt * unassigned_wt
      }
    }
  )
  out
}

normalise_weights <- function(weights, models_this_week, models_prev_week) {
  this_week <- names(models_this_week)
  prev_week <- names(models_prev_week)

  countries <- unname(unlist(models_this_week))
  names(countries) <- countries
  normalised_wts <- purrr::map(
    countries, ~ weights_for_country(., weights, models_this_week, models_prev_week)
  )
  normalised_wts
}
