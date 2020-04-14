probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
weeks_ending <- list(
  "2020-03-08" = "2020-03-08",
  "2020-03-15" = "2020-03-15",
  "2020-03-22" = "2020-03-22",
  "2020-03-29" = "2020-03-29",
  "2020-04-05" = "2020-04-05",
  "2020-04-12" = "2020-04-12"
)

output_files <- list(
  "DeCa_Std_results_week_end_2020-03-08.rds",
  "DeCa_Std_results_week_end_2020-03-15.rds",
  "DeCa_Std_results_week_end_2020-03-22.rds",
  "DeCa_Std_results_week_end_2020-03-29.rds",
  "DeCa_Std_results_week_end_2020-04-05.rds",
  "DeCa_Std_results_week_end_2020-04-12.rds",
  "RtI0_Std_results_week_end_2020-03-08.rds",
  "RtI0_Std_results_week_end_2020-03-15.rds",
  "RtI0_Std_results_week_end_2020-03-22.rds",
  "RtI0_Std_results_week_end_2020-03-29.rds",
  "RtI0_Std_results_week_end_2020-04-05.rds",
  "RtI0_Std_results_week_end_2020-04-12.rds",
  "sbkp_Std_results_week_end_2020-03-08.rds",
  "sbkp_Std_results_week_end_2020-03-15.rds",
  "sbkp_Std_results_week_end_2020-03-22.rds",
  "sbkp_Std_results_week_end_2020-03-29.rds",
  "sbkp_Std_results_week_end_2020-04-05.rds",
  "sbkp_Std_results_week_end_2020-04-12.rds",
  "sbsm_Std_results_week_end_2020-04-12.rds"
)

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)

model_outputs <- purrr::map(output_files, readRDS)

## For each, for each country, pool projections from diff models
## In the output, the first level is week, 2nd is country and 3rd
## is SI.
ensemble_model_predictions <- purrr::map(
  weeks_ending,
  function(week) {
    idx <- grep(x = names(model_outputs), pattern = week)
    outputs <- purrr::map(model_outputs[idx], ~ .[["Predictions"]])
    ## First Level is model, 2nd is country, 3rd is SI.
    countries <- names(outputs[[1]])
    names(countries) <- countries
    purrr::map(
      countries,
      function(country) {
        ## y is country specific output
        y <- purrr::map(outputs, ~ .[[country]])
        ## y has 2 components, one for each SI.
        y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
        y_2 <- purrr::map(y, ~ .[[2]]) ## si_1

        out <- list(
          pool_predictions(y_1),
          pool_predictions(y_2)
        )
      }
    )
  }
  )




ensemble_model_rt <- purrr::map_dfr(
  weeks_ending,
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

ensemble_daily_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred, extract_predictions_qntls,
      .id = "country"
    )
  },
  .id = "proj"
)

ensemble_weekly_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred, daily_to_weekly,
      .id = "country"
    )
  },
  .id = "proj"
)

readr::write_rds(
  x = ensemble_model_rt,
  path = "ensemble_model_rt.rds"
)

readr::write_rds(
  x = ensemble_daily_qntls,
  path = "ensemble_daily_qntls.rds"
)

readr::write_rds(
  x = ensemble_weekly_qntls,
  path = "ensemble_weekly_qntls.rds"
)

ensemble_model_rt_samples <- purrr::map_dfr(
  weeks_ending,
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

readr::write_rds(
  x = ensemble_model_rt_samples,
  path = "ensemble_model_rt_samples.rds"
)




### Ensemnle With SBSM Model.
output_files <- list(
  "DeCa_Std_results_week_end_2020-04-12.rds"
 ,"sbsm_Std_results_week_end_2020-04-12.rds"
 ,"RtI0_Std_results_week_end_2020-04-12.rds"
 ,"sbkp_Std_results_week_end_2020-04-12.rds"
)

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)

model_outputs <- purrr::map(output_files, readRDS)


outputs <- purrr::map(model_outputs, ~ .[["Predictions"]])
## First Level is model, 2nd is country, 3rd is SI.
countries <- names(outputs[[1]])
names(countries) <- countries
ensemble_model_with_sbsm <- purrr::map(
      countries,
      function(country) {
        ## y is country specific output
        y <- purrr::map(outputs, ~ .[[country]])
        ## y has 2 components, one for each SI.
        y_2 <- purrr::map(y, ~ .[[2]]) ## si_1
        pool_predictions(y_2)
      }
    )

readr::write_rds(
  x = ensemble_model_with_sbsm, path = "ensemble_model_with_sbsm.rds"
)

ensemble_with_sbsm_qntls <- purrr::map_dfr(
  ensemble_model_with_sbsm,
  function(cntry) {

    out <- apply(cntry, 2, quantile, probs)
    out <- t(out)
    out <- data.frame(out)
    colnames(out) <- c("2.5%", "25%", "50%", "75%", "97.5%")
    out <- tibble::rownames_to_column(out, var = "date")
    out
  }, .id = "country"
)


readr::write_rds(
  x = ensemble_with_sbsm_qntls, path = "ensemble_with_sbsm_qntls.rds"
)
