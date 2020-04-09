## Model 1 Rt Estimates
model_rt_files <- list(
  `RtI0_Std_results_week_end_2020-03-08` = "RtI0_Std_rt_qntls_2020-03-08.rds",
  `sbkp_Std_results_week_end_2020-03-08` = "sbkp_Std_rt_qntls_2020-03-08.rds"
)


model_rt_qntls <- purrr::map(model_rt_files, readRDS)
model_rt_qntls <- purrr::map_dfr(
  model_rt_qntls,
  ~ dplyr::bind_rows(., .id = "si"),
  .id = "model"
)

model_rt_qntls <- tidyr::spread(model_rt_qntls,
  key = quantile,
  value = out2
)

weekly_predictions_qntls <- readRDS("weekly_predictions_qntls.rds")

formatted_weekly_predictions_qntls <- split(
  weekly_predictions_qntls,
  weekly_predictions_qntls$si
) %>%
  purrr::imap(
    function(x, si_name) {
      out <- format_weekly_pred(x)
      out$model <- x$model
      out$`Observed Deaths` <- x$observed
      ## Get Rt Estimates for this SI and this country
      ## model_rt_qntls2 <- model_rt_qntls[!grepl(pattern = "DeCa", x = names(model_rt_qntls))]
      rt <- dplyr::filter(
        model_rt_qntls,
        !grepl(x = model, pattern = "DeCa") & si == si_name
      )
      rt <- format_last_rt(rt)
      out <- dplyr::left_join(x = out, y = rt)
      out$Country <- snakecase::to_any_case(
        as.character(out$Country),
        case = "title"
      )

      out <- dplyr::arrange(out, Country)
      out
    }
  )

model_1_formatted_output <- purrr::map(
  formatted_weekly_predictions_qntls,
  function(x) {
    dplyr::filter(x, grepl("RtI0", model))
  }
)

model_2_formatted_output <- purrr::map(
  formatted_weekly_predictions_qntls,
  function(x) {
    dplyr::filter(x, grepl("sbkp", model))
  }
)
model_3_formatted_output <- purrr::map(
  formatted_weekly_predictions_qntls,
  function(x) {
    dplyr::filter(x, grepl("DeCa", model))
  }
)

readr::write_rds(
  x = formatted_weekly_predictions_qntls,
  path = "formatted_weekly_predictions_qntls.rds"
)


readr::write_rds(
  x = model_1_formatted_output,
  path = "model_1_formatted_output.rds"
)


readr::write_rds(
  x = model_2_formatted_output,
  path = "model_2_formatted_output.rds"
)

readr::write_rds(
  x = model_3_formatted_output,
  path = "model_3_formatted_output.rds"
)
