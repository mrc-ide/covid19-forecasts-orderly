## df is a data.frame that has for a forecast period, for each
## country, for each model, for each SI the rank of the model. Return
## unnormalised weights for each forecast period and for each model.
## That is, weights summed aross acc countries.
## Normalise at the point at which they
## are used to create ensemble.
## rank is the column name that holds the rank - useful so that
## we can switch rank to a different metric if needed.
model_weights <- function(df) {
  ## How many times was this model awarded each rank
  out <- dplyr::count(df, model, rank_by_likelhd)
  out$weight <- out$n * (1 / out$rank_by_likelhd)

  dplyr::group_by(out, model) %>%
    dplyr::summarise(weight = sum(weight)) %>%
    dplyr::ungroup()
}

model_metrics <- readr::read_csv("model_predictions_error.csv")


## For each country we have M models that have projections.
## Rank these models according to a specified metric.
x <- split(
  model_metrics,
  list(
    model_metrics$forecast_date,
    model_metrics$country,
    model_metrics$si
  )
)

x <- purrr::keep(x, ~ nrow(.) > 0)

model_ranks <- purrr::map_dfr(
  x,
  function(df) {
    df <- dplyr::group_by(df, forecast_date, model, country, si) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      dplyr::ungroup()

    df$rank_by_relsq <- rank(df$rel_sq, )
    df$rank_by_relabs <- rank(df$rel_abs)
    ## The average likelihood is -ve, so flip the sign to get the
    ## correct rank.
    df$rank_by_likelhd <- rank(-df$avg_likelhd)
    df
  }
)

## For each country, extract a list of models that were run for it.
## Then group countries that have the same set of models
## The SI doesn't matter here, because presumably a model that was run
## using one serial interval was also run using the 2nd.
model_ranks_by_period <- split(model_ranks, model_ranks$forecast_date)

countries_for_model <- purrr::map(
  model_ranks_by_period,
  function(df) {
    nmodels_per_country <- dplyr::count(df, model, country) %>%
      tidyr::spread(country, n, fill = 0)
    countries <- colnames(nmodels_per_country)[-1]
    names(countries) <- countries
    ## A non-zero entry in row i, column j
    ## nmodels_per_country indicates that model i was run for country j.
    models_for_country <- purrr::map(
      countries,
      function(cntry) {
        idx <- which(nmodels_per_country[[cntry]] > 0)
        nmodels_per_country$model[idx]
      }
    )
    models <- unique(models_for_country)
    models <- purrr::map(models, ~ paste(., sep = "-", collapse = "-"))
    names(models) <- models
    purrr::map(
      models,
      function(model) {
        out <- purrr::keep(
          models_for_country, function(x) model == paste(x, sep = "-", collapse = "-")
        )
        names(out)
      }
    )
  }
)

### For each forecast period,
### For each set of countries identified above, compute model
### weights for each model.

model_weights_per_group <- purrr::imap_dfr(
  countries_for_model,
  function(model_grp, period) {
    purrr::map_dfr(
      model_grp,
      function(cntry_grp) {
        df <- model_ranks[model_ranks$forecast_date == period, ]
        df <- df[df$country %in% cntry_grp, ]
        by_si <- split(df, df$si)
        purrr::map_dfr(by_si, model_weights, .id = "si")
      }, .id = "model_group"
    )
  }, .id = "forecast_date"
)

saveRDS(
  object = model_weights_per_group,
  file = "unnormalised_model_weights_per_group.rds"
)

saveRDS(
  object = model_ranks,
  file = "model_ranks.rds"
)
saveRDS(
  object = countries_for_model,
  file = "countries_grouped_by_models.rds"
)
