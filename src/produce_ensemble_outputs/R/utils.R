df_to_list <- function(df) {

  out <- df$normalised_wt
  names(out) <- df$model
  out
}

f <- function(outputs, country, weights) {

  y <- purrr::map(outputs, ~ .[[country]])
  y <- purrr::keep(y, ~ ! is.null(.))
  models_this_week <- names(y)
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

normalise_weights <- function(weight) {

  ngroups <- length(weight)
  normalised <- purrr::map(
    weight, function(x) {
      x$normalised_wt <- round(x$weight / sum(x$weight), 2)
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


## outouts is a matrix of size N X T.
## weights should be a vector of the same length as the number of
## samples in outputs. That is there is a probability associated with
## each elemnt
pool_predictions_weighted <- function(outputs, weights, nsim = 10000) {

  models <- names(weights)
  ## Sample model with weights
  n_1 <- sample(
    x = names(weights), nsim, replace = TRUE, prob = weights
  )
  n_1 <- table(n_1)
  message("Number of times models picked ")
  message(paste(n_1, collapse = "\n"))

  out <- purrr::imap(
    outputs,
    function(output, model) {
      apply(output, 2, function(y) sample(y, size = n_1[[model]]))
    }
  )
  out <- Reduce('rbind', out)
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
extract_predictions_qntls <- function(y, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) {

    names(y) <- paste0("si_", seq_along(y))
    out <- purrr::map_dfr(
        y,
        function(y_si) {
            out2 <- t(
              apply(y_si, 2, quantile, prob = prob)
            )
            out2 <- as.data.frame(out2)
            out2 <- tibble::rownames_to_column(out2, var = "date")
            out2

        }, .id = "si"
    )
    out
}


daily_to_weekly <- function(y) {

    names(y) <- paste0("si_", seq_along(y))
    out <- purrr::map_dfr(
        y,
        function(y_si) {
            weekly <- rowSums(y_si)
            weekly <- quantile(
                weekly,
                prob = c(0.025, 0.25, 0.5, 0.75, 0.975)
            )
            weekly_df <- as.data.frame(weekly)
            ## This is not the last date for which predictions are
            ## available, but the last date for which observations are
            ## available.
            weekly_df$week_ending <- as.Date(colnames(y_si)[1]) - 1

            weekly_df <- tibble::rownames_to_column(
                weekly_df, var = "quantile"
            )

            weekly_df<- tidyr::spread(
                weekly_df, key = quantile, value = weekly
            )
            weekly_df

        }, .id = "si"
    )
    out

}
