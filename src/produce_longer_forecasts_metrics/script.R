## orderly::orderly_develop_start(parameters = list(week_ending =
## "2020-03-29", window = 1, latest_week = "2020-10-11"), use_draft = "newer")
f <- function(vec, window) {
  slider::slide_dbl(
    vec, ~round(mean(.x)), .before = window, .after = window
  )
}

all_metrics <- function(obs, pred) {
  qntls <-
    t(apply(pred, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))

  qntls <- data.frame(qntls, check.names = FALSE)
  pred <- t(pred)
  data.frame(
    mae = assessr::mae(obs, pred),
    rel_mae = assessr::rel_mae(obs, pred),
    rel_mse = assessr::rel_mse(obs, pred),
    rel_sharpness = assessr::rel_mean_dvtn(pred),
    bias = assessr::bias(obs, pred),
    prop_in_50 = assessr::prop_in_ci(obs, qntls[["25%"]], qntls[["75%"]]),
    prop_in_975 = assessr::prop_in_ci(obs, qntls[["2.5%"]], qntls[["97.5%"]]),
    median_pred = qntls[["50%"]]
  )
}


model_input <- readRDS("latest_deaths_wide_no_filter.rds")
model_input <- dplyr::mutate_if(
  model_input, is.numeric, f, window = window
)
model_input$Czech_Republic <- model_input$Czechia
latest_obs <- max(model_input$dates)

infiles <- list(
  unwieghted = "unweighted_projections.rds",
  weighted_across_countries = "weighted_across_countries_projections.rds",
  weighted_per_country = "weighted_per_country_projections.rds"
)

projections <- map(infiles, readRDS)


pred_error <- map_dfr(
  projections,
  function(projections_strategy) {
      imap_dfr(
        projections_strategy,
        function(country_pred, country) {
          message("########################################")
          message("####################", country, "#######")
          message("########################################")
          dates_pred <- seq(
            as.Date(week_ending) + 1,
            length.out = ncol(country_pred),
            by = "1 day"
          )
          dates_pred <- dates_pred[dates_pred <= latest_obs]
          country_pred <- country_pred[, 1:length(dates_pred)]
          colnames(country_pred) <- as.character(dates_pred)
          obs <- model_input[model_input$dates %in% dates_pred, country]
          if (! inherits(obs, "numeric")) obs <- as.numeric(obs[[country]])
          out <- all_metrics(obs, country_pred)
          out$date <- dates_pred
          out$obs <- obs
          nweeks <- length(dates_pred) / 7
          out$week_of_projection <- rep(1:nweeks, each = 7)
          out
        }, .id = "country"
      )
  }, .id = "strategy"
)



saveRDS(pred_error, "long_projections_error.rds")
