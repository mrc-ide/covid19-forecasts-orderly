model_metrics <- readr::read_csv("model_predictions_error.csv")

total_error <- dplyr::group_by(
  model_metrics, model, forecast_date, country, si
) %>%
      dplyr::summarise(
        total_rel_mae = sum(rel_mae),
        total_rel_mse = sum(rel_mse)
      ) %>% dplyr::ungroup()

## Within each forecast period, how many countries did we produce
## forecast for? Sanity check: Within each forecast period, this
## number should be the same for all models. Except for SBSM
## model which only projects for 12 countries.
ncountries <- dplyr::group_by(
  total_error, forecast_date, si, model) %>%
  dplyr::summarise(ncountries = n()) %>%
  dplyr::ungroup()

## Sanity check
# tidyr::spread(ncountries, model, ncountries)
## For each country get the best model in each
## forecast period.

x <- split(
  total_error,
  list(
    total_error$forecast_date,
    total_error$country,
    total_error$si
  ),
  sep = "_"
)

x <- purrr::keep(x, ~ nrow(.) > 0)

model_ranks <- purrr::map_dfr(
  x,
  function(df) {
    df$rank_by_mae <- order(df$total_rel_mae)
    df$rank_by_mse <- order(df$total_rel_mse)
    df
  }
)

## For each forecast period, for how many countries
## was a given model the "best" model.
mae_rank_counts <- dplyr::group_by(model_ranks, forecast_date, si) %>%
  dplyr::group_modify(~ dplyr::count(., model, rank_by_mae), keep = TRUE) %>%
  dplyr::ungroup()

## ncountries from above tells us how many predicitons each model made.
mae_rank_counts <- dplyr::left_join(mae_rank_counts, ncountries)

mae_rank_1 <- dplyr::filter(mae_rank_counts, rank_by_mae == 1)


mse_rank_counts <- dplyr::group_by(model_ranks, forecast_date, si) %>%
  dplyr::group_modify(~ dplyr::count(., model, rank_by_mse), keep = TRUE) %>%
  dplyr::ungroup()


mse_rank_counts <- dplyr::left_join(mse_rank_counts, ncountries)

mse_rank_1 <- dplyr::filter(mse_rank_counts, rank_by_mse == 1)

## Model weights
## These will be correct for the period that does not include
## Sam's model because of the issue with different number of
## countries.
mae_rank_1$model_weights <- mae_rank_1$n / mae_rank_1$ncountries
mse_rank_1$model_weights <- mse_rank_1$n / mse_rank_1$ncountries

### Joining them with ncountries so that the model that never got
### picked gets a weight of 0. Identified by NA in the row

mae_model_weights <- dplyr::left_join(
  dplyr::select(ncountries, forecast_date, si, model),
  mae_rank_1
)

mse_model_weights <- dplyr::left_join(
  dplyr::select(ncountries, forecast_date, si, model),
  mse_rank_1
)


readr::write_csv(x = mae_model_weights, path = "mae_model_weights.csv")
readr::write_csv(x = mse_model_weights, path = "mse_model_weights.csv")
