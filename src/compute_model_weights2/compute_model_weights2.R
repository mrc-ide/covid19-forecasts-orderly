## df is a data.frame that has for a forecast period, for each
## country, for each model, for each SI the rank of the model. Return
## unnormalised weights for each forecast period and for each model.
## That is, weights summed aross all countries.
## Normalise at the point at which they
## are used to create ensemble.
## rank is the column name that holds the rank - useful so that
## we can switch rank to a different metric if needed.
model_weights <- function(df, rank) {
  ## How many times was this model awarded each rank
  out <- dplyr::count(df, model, rank_by_likelhd)
  out$weight <- out$n * (1 / out$rank_by_likelhd)

  dplyr::group_by(out, model) %>%
    dplyr::summarise(weight = sum(weight)) %>%
    dplyr::ungroup()
}

model_metrics <- readr::read_csv("model_predictions_error.csv")
weeks_predicted <- unique(model_metrics$forecast_date)
## For the first set of predictions, there is nothing to look back to
weeks_predicted <- weeks_predicted[!weeks_predicted == "2020-03-08"]
names(weeks_predicted) <- weeks_predicted

all_preds_so_far <- purrr::map(
  weeks_predicted,
  function(week) dplyr::filter(model_metrics, forecast_date <= week)
)

## For each week in which we produced forecasts, compute
## model ranks based on all previous forecasts.
model_ranks <- purrr::map(
  all_preds_so_far,
  function(df) {
    x <- split(df, list(df$country, df$si))
    x <- purrr::keep(x, ~ nrow(.) > 0)
    ## For each country we have M models that have projections.
    ## Rank these models according to a specified metric.
    model_ranks <- purrr::map_dfr(
      x,
      function(y) {
        y <- dplyr::group_by(y, model, country, si) %>%
          dplyr::summarise_if(is.numeric, sum) %>%
          dplyr::ungroup()

        y$rank_by_relsq <- rank(y$rel_sq)
        y$rank_by_relabs <- rank(y$rel_abs)
        ## The average likelihood is -ve, so flip the sign to get the
        ## correct rank.
        y$rank_by_likelhd <- rank(-y$avg_likelhd)
        y
      }
    )
  }
)

unnormalised_weights <- purrr::map(
  model_ranks,
  ~ purrr::map_dfr(split(., .$si), model_weights, .id = "si")
)
## And we want to know how many observations did we have for each
## model so that we can adjust for unequal number of observations
## when we normalise these weights.
n_predictions <- purrr::map(model_ranks, ~ dplyr::count(., model, si))


saveRDS(
  object = unnormalised_weights,
  file = "unnormalised_weights.rds"
)

saveRDS(
  object = n_predictions,
  file = "number_predictions_per_model.rds"
)
saveRDS(
  object = model_ranks,
  file = "model_ranks.rds"
)
