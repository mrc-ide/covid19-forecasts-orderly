output_files <- list(
    `RtI0_Std_results_week_end_2020-03-08` =
        "RtI0_Std_results_week_end_2020-03-08.rds",
    `model_outputs/sbkp_Std_results_week_end_2020-03-08`=
        "sbkp_Std_results_week_end_2020-03-08.rds"
)

model_input <- readRDS("model_input.rds")

model_outputs <- purrr::map(
    output_files,
    ~ readRDS(paste0("model_outputs/", .))
)

model_predictions_qntls <- purrr::map(
  model_outputs,
  function(x) {
    pred <- x[["Predictions"]]
    purrr::map_dfr(
      pred, extract_predictions_qntls,
      .id = "country"
    )
  }
)

outfiles <- stringr::str_replace(
                         string = output_files,
                         pattern = "results_week_end",
                         replacement = "daily_predictions_qntls"
    )

purrr::walk2(model_predictions_qntls, outfiles, ~ readr::write_rds(.x, .y))

weekly_predictions_qntls <- purrr::map_dfr(
  model_outputs,
  function(x) {
    pred <- x[["Predictions"]]
    purrr::imap_dfr(pred, function(y, country) {
      dates <- as.Date(colnames(y[[1]]))
      obs_deaths <- model_input[["D_active_transmission"]][c("dates", country)]
      obs_deaths <- obs_deaths[obs_deaths$dates %in% dates, ]
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
  }, .id = "model"
)

outfiles <- "weekly_predictions_qntls.rds"

purrr::walk2(weekly_predictions_qntls,
             outfiles, ~ readr::write_rds(.x, .y))

model_rt_qntls <- purrr::map(
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
            prob = c(0.025, 0.1, 0.4, 0.5, 0.6, 0.9, 0.975)
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
  }
)

outfiles <- stringr::str_replace(
                         string = output_files,
                         pattern = "results_week_end",
                         replacement = "rt_qntls"
)

purrr::walk2(model_rt_qntls,
             outfiles, ~ readr::write_rds(.x, .y))
