## orderly::orderly_develop_start(parameters = list(week_ending = "2020-04-12"))
## probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
## weeks_ending <- readr::read_rds("latest_week_ending.rds")

output_files <- list.files(covid_19_path)
output_files <- output_files[grepl(x = output_files, pattern = week_ending)]

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)
names(week_ending) <- week_ending
message("For week ending ", week_ending)

message("Output Files ", output_files)

model_outputs <- purrr::map(
  output_files, ~ readRDS(paste0(covid_19_path, .))
)

## Equal weighted models
## First Level is model, 2nd is country, 3rd is SI.
idx <- grep(x = names(model_outputs), pattern = week_ending)
outputs <- purrr::map(model_outputs[idx], ~ .[["Predictions"]])
names(outputs) <-  sapply(
  names(outputs), function(x) strsplit(x, "_")[[1]][1]
)

countries <- names(outputs[[1]])
names(countries) <- countries

ensemble_model_predictions <- purrr::map(
  week_ending,
  function(week) {
    purrr::map(
      countries,
      function(country) {
        message(country)
        message(paste(names(outputs), collapse = "\n"))
        wts <- data.frame(
          model = names(outputs),
          normalised_wt = 1
        )
        wts <- list(si_1 = wts, si_2 = wts)
        f(outputs, country, wts)
      }
    )
  }
)


## Model weights derived from last week's forecasts only.
weights_prev_week <- readRDS("weights_prev_week.rds")
weights_all_prev_weeks <- readRDS("weights_all_prev_weeks.rds")

prev_week <- as.Date(week_ending) - 7
## This will be a list with two components corresponding to the two
## serial intervals used.
weights_prev_week <- weights_prev_week[[as.character(prev_week)]]
weights_all_prev_weeks <- weights_all_prev_weeks[[as.character(prev_week)]]



## weights_prev_week has a date associated with it
## so that weights_prev_week[[1]] is what we really want
weights_prev_week_normalised <- purrr::map(
  weights_prev_week, normalise_weights
)

weights_all_prev_weeks_normalised <- purrr::map(
  weights_all_prev_weeks, normalise_weights
)

## Sanity check:  purrr::map(normalised_wts, ~ sum(unlist(.)))
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
        wts <- rep(1, length(outputs))
        names(wts) <- names(outputs)
        wts <- list(si_1 = wts, si_2 = wts)
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


ensemble_daily_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred, ~ extract_predictions_qntls(., probs),
      .id = "country"
    )
  },
  .id = "proj"
)

ensemble_weekly_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred,
      function(x) {
        message(colnames(x))
        daily_to_weekly(x)
      },
      .id = "country"
    )
  },
  .id = "proj"
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
        daily_to_weekly(x)
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
        daily_to_weekly(x)
      },
      .id = "country"
    )
  },
  .id = "proj"
)

saveRDS(
  object = ensemble_model_predictions,
  file = "ensemble_model_predictions.rds"
)

saveRDS(
  object = wtd_ensb_prev_week,
  file = "wtd_ensb_prev_week.rds"
)

saveRDS(
  object = wtd_ensb_all_prev_weeks,
  file = "wtd_ensb_all_prev_weeks.rds"
)


saveRDS(
  object = ensemble_daily_qntls,
  file = "ensemble_daily_qntls.rds"
)

saveRDS(
  object = ensemble_weekly_qntls,
  file = "ensemble_weekly_qntls.rds"
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

######################################################################
########### Rt quantiles #############################################
########### Unweighted ###############################################
######################################################################
######################################################################
outputs <- purrr::map(model_outputs[idx], ~ .[["R_last"]])
ensemble_model_rt <- purrr::map_dfr(
  week_ending,
  function(week) {
    message("Week is ", week)
    ## First Level is model, 2nd is country, 3rd is SI.
    ## TODO pick countries from inout
    countries <- names(outputs[[2]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(country) {
        if (week == "2020-04-12" & country == "United_Kingdom") {
          outputs <- outputs["sbsm_Std_results_week_end_2020-04-12"]
        }

        ## y is country specific output
        y <- purrr::map(outputs, ~ .[[country]])
        ## y has 2 components, one for each SI.
        ## Determine quantiles

        y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
        ## smallest observation greater than or equal to lower hinge - 1.5 * IQR
        y_1_all <- unlist(y_1)
        y_1 <- quantile(
          y_1_all,
          probs = probs
        )
        y_1 <- tibble::rownames_to_column(
          data.frame(out2 = y_1),
          var = "quantile"
        )
        y_1$si <- "si_1"

        y_2 <- purrr::map(y, ~ .[[2]]) ## si_1
        y_2_all <- unlist(y_2)
        y_2 <- quantile(
          y_2_all,
          probs = probs
        )
        y_2 <- tibble::rownames_to_column(
          data.frame(out2 = y_2),
          var = "quantile"
        )
        y_2$si <- "si_2"
        rbind(y_1, y_2)
      },
      .id = "country"
    )
  },
  .id = "model" ## this is really week ending, but to be able to resue prev code, i am calling it model
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
        y_2 <- purrr::map(y, ~ .[[2]]) ## si_1
        data.frame(
          si_1 = unlist(y_1),
          si_2 = unlist(y_2)
        )
      },
      .id = "country"
    )
  },
  .id = "model" ## this is really week ending, but to be able to resue prev code, i am calling it model
)

saveRDS(
  object = ensemble_model_rt_samples,
  file = "ensemble_model_rt_samples.rds"
)

saveRDS(
  object = ensemble_model_rt,
  file = "ensemble_model_rt.rds"
)



######################################################################
x <- dplyr::bind_rows(
  list(
    unweighted = ensemble_daily_qntls,
    wtd_ensb_prev_week = wtd_ensb_prev_week_daily_qntls,
    wtd_ensb_all_prev_weeks = wtd_ensb_all_prev_weeks_daily_qntls
  ), .id = "model"
)

x$date <- as.Date(x$date)

p <- ggplot(x) +
  geom_ribbon(
    aes(x = date, ymin = `2.5%`, ymax = `97.5%`, fill = model),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = date, y = `50%`, col = model)
  ) +
  ggforce::facet_wrap_paginate(
    ~country, ncol = 1, nrow = 3, scales = "free_y", page = 1
  )

npages <- ggforce::n_pages(p)

for (page in seq_len(npages)) {
  p <-  p +   ggforce::facet_wrap_paginate(
    ~country, ncol = 1, nrow = 3, scales = "free_y", page = page
    )
  ggsave("{page}.png", p)

}
