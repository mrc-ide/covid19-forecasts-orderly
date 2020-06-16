##Model 1 Rt Estimates
model_rt_qntls <- readRDS("model_rt_qntls.rds")

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

## Formatting Ensemble Model Outputs
ensemble_weekly_qntls <- readr::read_rds("ensemble_weekly_qntls.rds")
ensemble_model_rt <- readr::read_rds("ensemble_model_rt.rds")
ensemble_model_rt <- tidyr::spread(
  data = ensemble_model_rt,
  key = quantile,
  value = out2
)

fmtd_ensemble_weekly_qntls <- split(
  ensemble_weekly_qntls,
  ensemble_weekly_qntls$si
) %>% purrr::map(function(x) {
  this_si <- x$si[1]
  ## Get observed number of deaths calculated earlier.
  y <- dplyr::select(
    weekly_predictions_qntls,
    week_ending,
    country,
    Observed = observed
  )
  y <- dplyr::distinct(y)
  x <- format_weekly_pred(x)
  x$`Week Ending` <- as.Date(x$`Week Ending`)
  y$week_ending <- as.Date(y$week_ending)
  x <- dplyr::left_join(
    x,
    y,
    by = c(
      "Country" = "country",
      "Week Ending" = "week_ending"
    )
  )
  x <- dplyr::arrange(x, Country)

  ## Get R_t estimates for this country and this
  ## Week.
  rt <- dplyr::filter(ensemble_model_rt, si == this_si)
  rt <- format_last_rt(rt)
  rt$model <- as.Date(rt$model)
  x <- dplyr::left_join(
    x,
    rt,
    by = c(
      "Week Ending" = "model",
      "Country" = "Country"
    )
  )
  x$Country <- snakecase::to_any_case(
    as.character(x$Country),
    case = "title"
  )

  x
})

readr::write_rds(
  x = fmtd_ensemble_weekly_qntls,
  path = "fmtd_ensemble_weekly_qntls.rds"
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
