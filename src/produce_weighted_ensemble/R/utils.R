df_to_list <- function(df) {

  out <- df$normalised_wt
  names(out) <- df$model
  out
}

f <- function(outputs, country, weights) {

  y <- purrr::map(outputs, ~ .[[country]])
  y <- purrr::keep(y, ~ ! is.null(.))
  models_this_week <- names(y)

  if (is.null(weights)) {
    message("Unweighted ensemble outputs")
    wts <- data.frame(
      model = models_this_week,
      normalised_wt = 1
    )
    weights <- list(si_1 = wts, si_2 = wts)

  }
  ## If not all models used for a country this week were used
  ## in the previous week, then assign it some weight (1/M)
  ## where M is the number of models run this week
  if (! all(models_this_week %in% weights$si_1[["model"]])) {
    message("Some new models for ", country, "this week.")
    new_models <- which(
      ! models_this_week %in% weights$si_1[["model"]]
    )
    message("New models are ", models_this_week[new_models])
    unassigned_wt <- 1 - length(new_models) / length(models_this_week)
    weights <- purrr::map(weights, function(x) {
      x$normalised_wt <- x$normalised_wt * unassigned_wt
      out <- data.frame(
        model = models_this_week[new_models],
        weight = 1 / length(models_this_week),
        npreds = NA,
        normalised_wt = 1 / length(models_this_week),
        row.names = models_this_week[new_models]
      )
      x <- rbind(x, out)
      x
    }
   )
  }
  y_1 <- purrr::map(y, ~ .[[1]])
  y_2 <- purrr::map(y, ~ .[[2]])
  weights <- purrr::map(weights, df_to_list)
  saveRDS(weights, glue::glue("weights_{country}.rds"))
  out <-  list(
    si_1 = pool_predictions_weighted(y_1, weights$si_1),
    si_2 = pool_predictions_weighted(y_2, weights$si_2)
  )
  out
}

normalise_weights <- function(weight, weight_col) {

  ngroups <- length(weight)
  normalised <- purrr::map(
    weight, function(x) {
      x$normalised_wt <- x[[weight_col]] / sum(x[[weight_col]])
      x
    }
  )
  ## If there is more than one group assume that there are 2
  ## {RtI0, DeCA, sbsm} and {RtI0, DeCA, sbsm, sbkp}
  if (ngroups > 1) {
    bigger <- which.max(sapply(weight, nrow, USE.NAMES = FALSE))
    smaller <- which.min(sapply(weight, nrow, USE.NAMES = FALSE))
    models <- sapply(weight, function(x) as.character(x$model))
    new_models <- models[[bigger]][which(! models[[bigger]] %in%
                                         models[[smaller]])]

    ## Propagate the weight of this model back, in the same
    ## proportion.
    bigger <- normalised[[bigger]]
    unassigned_wt <- 1 -
      sum(bigger$normalised_wt[bigger$model %in% new_models])

    smaller <- normalised[[smaller]]
    smaller$normalised_wt <- smaller$normalised_wt * unassigned_wt

    out <- rbind(
      smaller,
      bigger[bigger$model %in% new_models, ]
    )
  } else {
    message("Only one model combination here")
    out <- normalised[[1]]
  }
  out
}
