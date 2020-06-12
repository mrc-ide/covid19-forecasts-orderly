probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
output_files <- list.files(covid_19_path)
output_files <- output_files[grepl(x = output_files, pattern = week_ending)]
output_files <- output_files[!grepl(x = output_files, pattern = "sbsm")]
names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)
names(week_ending) <- week_ending
message("For week ending ", week_ending)

message("Output Files \n", paste(output_files, collapse = "\n"))

model_outputs <- purrr::map(
  output_files, ~ readRDS(paste0(covid_19_path, .))
)

## Equal weighted models
## First Level is model, 2nd is country, 3rd is SI.
idx <- grep(x = names(model_outputs), pattern = week_ending)
outputs <- purrr::map(model_outputs[idx], ~ .[["Predictions"]])
rt <- purrr::map(model_outputs[idx], ~ .[["R_last"]])
names(outputs) <-  sapply(
  names(outputs), function(x) strsplit(x, "_")[[1]][1]
)
names(rt) <-  sapply(
  names(rt), function(x) strsplit(x, "_")[[1]][1]
)

countries <- names(outputs[[1]])
names(countries) <- countries

## Model weights derived from last week's forecasts only.
weights_prev_week <- readRDS("weights_prev_week.rds")
weights_all_prev_weeks <- readRDS("weights_all_prev_weeks.rds")

prev_week <- as.Date(week_ending) - 7
## ## This will be a list with two components corresponding to the two
## ## serial intervals used.
weights_prev_week <- weights_prev_week[[as.character(prev_week)]]
weights_all_prev_weeks <- weights_all_prev_weeks[[as.character(prev_week)]]



weights_prev_week_normalised <- purrr::map(
  weights_prev_week, ~ normalise_weights(., "wt_empirical")
)

weights_all_prev_weeks_normalised <- purrr::map(
  weights_all_prev_weeks, ~ normalise_weights(., "wt_empirical")
)

## ## Sanity check:  purrr::map(normalised_wts, ~ sum(unlist(.)))
wtd_ensb_prev_week <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
        f(outputs, country, weights_prev_week_normalised)
      }
    )
  }
)


wtd_ensb_all_prev_weeks <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
        f(outputs, country, weights_all_prev_weeks_normalised)
      }
    )
  }
)

saveRDS(
  object = wtd_ensb_prev_week,
  "wtd_ensb_prev_week.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks,
  "wtd_ensb_all_prev_weeks.rds"
)

wtd_ensb_prev_week_daily_qntls <- purrr::map_dfr(
  wtd_ensb_prev_week,
  function(pred) {
    purrr::map_dfr(
      pred, ~ extract_predictions_qntls(., probs),
      .id = "country"
    )
  },
  .id = "proj"
)

wtd_ensb_prev_week_weekly_qntls <- purrr::map_dfr(
  wtd_ensb_prev_week,
  function(pred) {
    purrr::map_dfr(
      pred,
      function(x) {
        message(colnames(x))
        daily_to_weekly(x, prob = probs)
      },
      .id = "country"
    )
  },
  .id = "proj"
)

wtd_ensb_all_prev_weeks_daily_qntls <- purrr::map_dfr(
  wtd_ensb_all_prev_weeks,
  function(pred) {
    purrr::map_dfr(
      pred, ~ extract_predictions_qntls(., probs),
      .id = "country"
    )
  },
  .id = "proj"
)

wtd_ensb_all_prev_weeks_weekly_qntls <- purrr::map_dfr(
  wtd_ensb_all_prev_weeks,
  function(pred) {
    purrr::map_dfr(
      pred,
      function(x) {
        message(colnames(x))
        daily_to_weekly(x, probs)
      },
      .id = "country"
    )
  },
  .id = "proj"
)

saveRDS(
  object = wtd_ensb_prev_week_daily_qntls,
  file = "wtd_ensb_prev_week_daily_qntls.rds"
)

saveRDS(
  object = wtd_ensb_prev_week_weekly_qntls,
  file = "wtd_ensb_prev_week_weekly_qntls.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks_daily_qntls,
  file = "wtd_ensb_all_prev_weeks_daily_qntls.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks_weekly_qntls,
  file = "wtd_ensb_all_prev_weeks_weekly_qntls.rds"
)



ensemble_model_rt_samples <- purrr::map_dfr(
  week_ending,
  function(week) {
    message("Week is ", week)
    idx <- grep(x = names(model_outputs), pattern = week)
    message("Working on models ", names(model_outputs)[idx])
    outputs <- purrr::map(model_outputs[idx], ~ .[["R_last"]])
    ## First Level is model, 2nd is country, 3rd is SI.
    ## TODO pick countries from inout
    countries <- names(outputs[[2]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(country) {
        ## y is country specific output
        y <- purrr::map(outputs, ~ .[[country]])
        ## y has 2 components, one for each SI.
        ## Determine quantiles
        y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
        y_2 <- purrr::map(y, ~ .[[2]]) ## si_2
        data.frame(
          si_1 = unlist(y_1),
          si_2 = unlist(y_2)
        )
      },
      .id = "country"
    )
  },
  .id = "model" ## this is really week ending, but to be able to resue
  ## prev code, i am calling it model
)

## r is  a named list
## weights is a named list
pool_rt_weighted <- function(r, weights, nsim) {
  models <- names(weights)
  n_1 <- sample(
    x = names(weights), nsim, replace = TRUE, prob = weights
  )
  n_1 <- table(n_1)
  if (!all(models %in% names(n_1))) {
    idx <- which(!models %in% names(n_1))
    n_1[[models[idx]]] <- 0
  }
  message("Number of times models picked ")
  message(paste(n_1, collapse = "\n"))
  purrr::imap(
    r,
    function(output, model) {
      idx <- sample(seq_along(output), size = n_1[[model]])
      r[idx]
    }
  )
}

wtd_rt_prev_week <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)

        y <- purrr::map(rt, ~ .[[country]])

        y_1 <- purrr::map(y, ~ .[[1]])
        weights_1 <- weights_prev_week_normalised[[1]]$normalised_wt
        names(weights_1) <- weights_prev_week_normalised[[1]]$model

        y_2 <- purrr::map(y, ~ .[[2]])
        weights_2 <- weights_prev_week_normalised[[2]]$normalised_wt
        names(weights_2) <- weights_prev_week_normalised[[2]]$model

        list(
          si_1 = pool_rt_weighted(y_1, weights_1, 10000),
          si_2 = pool_rt_weighted(y_2, weights_2, 10000)
        )
      }
    )
  }
)

wtd_rt_all_prev_week <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)

        y <- purrr::map(rt, ~ .[[country]])

        y_1 <- purrr::map(y, ~ .[[1]])
        weights_1 <- weights_all_prev_weeks_normalised[[1]]$normalised_wt
        names(weights_1) <- weights_all_prev_weeks_normalised[[1]]$model

        y_2 <- purrr::map(y, ~ .[[2]])
        weights_2 <- weights_all_prev_weeks_normalised[[2]]$normalised_wt
        names(weights_2) <- weights_all_prev_weeks_normalised[[2]]$model

        list(
          si_1 = pool_rt_weighted(y_1, weights_1, 10000),
          si_2 = pool_rt_weighted(y_2, weights_2, 10000)
        )
      }
    )
  }
  )

saveRDS(wtd_rt_prev_week, "wtd_rt_prev_week.rds")
saveRDS(wtd_rt_all_prev_week, "wtd_rt_all_prev_week.rds")

