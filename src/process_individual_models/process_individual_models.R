output_files <- list(
  "DeCa_Std_results_week_end_2020-03-08.rds",
  "DeCa_Std_results_week_end_2020-03-15.rds",
  "DeCa_Std_results_week_end_2020-03-22.rds",
  "DeCa_Std_results_week_end_2020-03-29.rds",
  "DeCa_Std_results_week_end_2020-04-05.rds",
  "RtI0_Std_results_week_end_2020-03-08.rds",
  "RtI0_Std_results_week_end_2020-03-15.rds",
  "RtI0_Std_results_week_end_2020-03-22.rds",
  "RtI0_Std_results_week_end_2020-03-29.rds",
  "RtI0_Std_results_week_end_2020-04-05.rds",
  "sbkp_Std_results_week_end_2020-03-08.rds",
  "sbkp_Std_results_week_end_2020-03-15.rds",
  "sbkp_Std_results_week_end_2020-03-22.rds",
  "sbkp_Std_results_week_end_2020-03-29.rds",
  "sbkp_Std_results_week_end_2020-04-05.rds"
)

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)



model_outputs <- purrr::map(
  output_files,
  ~ readRDS(paste0("model_outputs/", .))
  )

## Filter model outputs from DeCa models to reflect the new
## threshold. Needed to be done only once.
## outdated <- output_files <- list(
##   "DeCa_Std_results_week_end_2020-03-08",
##   "DeCa_Std_results_week_end_2020-03-15",
##   "DeCa_Std_results_week_end_2020-03-22",
##   "DeCa_Std_results_week_end_2020-03-29",
##   "DeCa_Std_results_week_end_2020-04-05"
##   )

## purrr::walk(
##   outdated,
##   function(x) {
##     right <- gsub(pattern = "DeCa", replacement = "RtI0", x = x)
##     countries <- names(model_outputs[[right]][["Predictions"]])
##     message(x)
##     message(countries)
##     model_outputs[[x]][["Predictions"]] <-
##       model_outputs[[x]][["Predictions"]][countries]
##     model_outputs[[x]][["Country"]] <- countries
##     readr::write_rds(x = model_outputs[[x]], path = glue::glue("model_outputs/{x}.rds"))
##   }
## )

model_input <- readRDS("model_input.rds")

daily_predictions_qntls <- purrr::map_dfr(
  model_outputs,
  function(x) {
    pred <- x[["Predictions"]]
    purrr::map_dfr(
      pred, extract_predictions_qntls,
      .id = "country"
    )
  }, .id = "model"
)

readr::write_rds(
    x = daily_predictions_qntls,
    path = "daily_predictions_qntls.rds"
)


##purrr::walk2(model_predictions_qntls, outfiles, ~ readr::write_rds(.x, .y))

weekly_predictions_qntls <- purrr::map_dfr(
  model_outputs,
  function(x) {
    pred <- x[["Predictions"]]
    purrr::imap_dfr(pred, function(y, country) {
      dates <- as.Date(colnames(y[[1]]))
      obs_deaths <- model_input[["D_active_transmission"]][c("dates", country)]
      ## We now want deaths observed in the week preceding the one
      ## for which we are forecasting.
      dates_prev_week <- dates - 7
      message("Dates of previous week")
      message(paste(dates_prev_week, collapse = ""))
      obs_deaths <- obs_deaths[obs_deaths$dates %in% dates_prev_week, ]
      if (nrow(obs_deaths) == 0) {
        message(
          "No observations for dates ", dates, " in ", country
        )
        obs_deaths <- NA
      } else {
        obs_deaths <- sum(obs_deaths[[country]])
      }

      weekly_df <- daily_to_weekly(y)

      weekly_df$observed <- obs_deaths
      weekly_df
    }, .id = "country")
  },
  .id = "model"
)

readr::write_rds(
    x = weekly_predictions_qntls,
    path = "weekly_predictions_qntls.rds"
)


model_rt_qntls <- purrr::map_dfr(
  model_outputs,
  function(x) {
    pred <- x[["R_last"]]
    purrr::map_dfr(pred, function(y) {
      names(y) <- c("si_1", "si_2")
      out <- purrr::map_dfr(
        y,
        function(y_si) {
          out2 <- quantile(
            y_si,
            prob = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
          )
          out2 <- as.data.frame(out2)
          out2 <- tibble::rownames_to_column(
            out2,
            var = "quantile"
          )
          out2
        },
        .id = "si"
      )
    }, .id = "country")
  }, .id = "model"
)

readr::write_rds(
    x = model_rt_qntls,
    path = "model_rt_qntls.rds"
)


model_rt_samples <- purrr::map_dfr(
  model_outputs,
  function(x) {
    rt <- x[["R_last"]]
    purrr::map_dfr(
      rt,
      function(cntry) {
        data.frame(
          si_1 = cntry[[1]],
          si_2 = cntry[[2]]
        )
     }, .id = "country"
  )
  }, .id = "model"
)

readr::write_rds(
    x = model_rt_samples,
    path = "model_rt_samples.rds"
)
