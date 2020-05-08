
pool_predictions <- function(outputs, weights = 1) {

  wtd_outputs <- purrr::map2(outputs, weights, function(x, w) x * w)
  out <- Reduce('rbind', wtd_outputs)
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
  names(outputs) <- sapply(
    strsplit(names(outputs), split = "_"), function(x) x[[1]][1]
  )

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
