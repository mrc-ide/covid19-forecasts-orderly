## countries_to_keep <- c("Algeria", "Austria", "Belgium", "Brazil", "Canada", "China",
## "Colombia", "Czechia", "Denmark", "Dominican_Republic", "Ecuador",
## "Egypt", "France", "Germany", "India", "Indonesia", "Iran", "Ireland",
## "Israel", "Italy", "Mexico", "Morocco", "Netherlands", "Peru",
## "Philippines", "Poland", "Portugal", "Romania", "Russia", "South_Korea",
## "Spain", "Sweden", "Switzerland", "Turkey", "United_Kingdom",
## "United_States_of_America")
run_info <- orderly::orderly_run_info()
output_files <- run_info$depends$as
output_files <- output_files[output_files != "model_input.rds"]

names(output_files) <- gsub(
  pattern = ".rds", replacement = "", x = output_files
)

##week_ending <- max(as.Date(params))

##output_files <- output_files[grepl(pattern = week_ending, x = names(output_files))]
message("Processing ", paste(output_files, collapse = "\n"))

model_outputs <- purrr::map(output_files, readRDS)

model_input <- readRDS(
  glue::glue(
    "{dirname(covid_19_path)}/model_inputs/data_{week_ending}.rds"
  )
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

## purrr::iwalk(
##   model_outputs,
##   function(x, outfile) {

##     ##countries <- names(model_outputs[[right]][["Predictions"]])
##     ##message(countries)
##     idx <- which(
##       names(x[["Predictions"]]) %in% countries_to_keep
##     )
##     message("Keeping ", paste0(idx, collapse = " "))
##     x[["Predictions"]] <- x[["Predictions"]][idx]
##     x[["Country"]] <- countries_to_keep[idx]
##     readr::write_rds(x = x, path = glue::glue("model_outputs/{outfile}.rds"))
##   }
## )



daily_predictions_qntls <- purrr::imap_dfr(
  model_outputs,
  function(x, model) {
    message(model)
    pred <- x[["Predictions"]]
    purrr::imap_dfr(
      pred, function(y, country) {
               message(country)
               extract_predictions_qntls(y)
             },
      .id = "country"
    )
  }, .id = "model"
)

saveRDS(
  object = daily_predictions_qntls,
  file = "daily_predictions_qntls.rds"
)


##purrr::walk2(model_predictions_qntls, outfiles, ~ readr::write_rds(.x, .y))
dates <-seq(from = as.Date(week_ending) + 1, length.out = 7, by = "1 day")
weekly_predictions_qntls <- purrr::map_dfr(
  model_outputs,
  function(x) {
    pred <- x[["Predictions"]]
    purrr::imap_dfr(pred, function(y, country) {
      message(country)
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

saveRDS(
  object = weekly_predictions_qntls,
  file = "weekly_predictions_qntls.rds"
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

saveRDS(object = model_rt_qntls, file = "model_rt_qntls.rds")


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

saveRDS(model_rt_samples, "model_rt_samples.rds")
