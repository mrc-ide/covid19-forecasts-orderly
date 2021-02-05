## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-01-10", location = "Arizona"))

probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

run_info <- orderly::orderly_run_info()
output_files <- run_info$depends$as
output_files <- output_files[output_files != "model_input.rds"]

## Only need this during report development stage
output_files <- list("sbkp_Std_results.rds",
                     "DeCa_Std_results.rds")

model_outputs <- purrr::map(output_files, readRDS)

## Names and data formats don't match in dependency files so changing manually here
## To do: Change in the prior tasks so that this can be deleted
names(model_outputs) <- c("sbkp", "DeCa")
names(model_outputs[["DeCa"]]) <- c("R_last", "Predictions")

outputs <- purrr::map(model_outputs, ~ .[["Predictions"]])
outputs[["sbkp"]] <- purrr::map(outputs[["sbkp"]], as.data.frame)


## Equal weighted models

ensemble_model_predictions <- ensemble_predictions(outputs, weights = NULL)

ensemble_daily_qntls <- rincewind::extract_predictions_qntls(ensemble_model_predictions, probs)

ensemble_weekly_qntls <- rincewind::daily_to_weekly(ensemble_model_predictions)


saveRDS(
  object = ensemble_model_predictions,
  file = "ensemble_model_predictions.rds"
)

saveRDS(
  object = ensemble_daily_qntls,
  file = "ensemble_daily_qntls.rds"
)

saveRDS(
  object = ensemble_weekly_qntls,
  file = "ensemble_weekly_qntls.rds"
)



######################################################################
########### Rt quantiles #############################################
########### Unweighted ###############################################
######################################################################
######################################################################
outputs <- purrr::map(model_outputs, ~ .[["R_last"]])

ensemble_model_rt <- ensemble_rt(outputs)


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
## countries <- names(ensemble_model_predictions[[1]])
## purrr::walk(
##   countries,
##   function(country) {

##     unwtd <- t(ensemble_model_predictions[[1]][[country]][[2]])
##     all_prev <- t(wtd_ensb_all_prev_weeks[[1]][[country]][[2]])
##     prev_week <- t(wtd_ensb_prev_week[[1]][[country]][[2]])

##     unwtd <- data.frame(unwtd) %>%
##       tibble::rownames_to_column(var = "date") %>%
##       tidyr::gather("var", "val", -date)

##     all_prev <-  data.frame(all_prev) %>%
##       tibble::rownames_to_column(var = "date") %>%
##       tidyr::gather("var", "val", -date)

##     prev_week <- data.frame(prev_week) %>%
##       tibble::rownames_to_column(var = "date") %>%
##       tidyr::gather("var", "val", -date)

##     x <- bind_rows(
##       list(
##         unwtd = unwtd, all_prev = all_prev, prev_week = prev_week
##       ),
##       .id = "model"
##     )
##     x$date <- as.Date(x$date)

##     p <- ggplot(x) +
##       geom_density(
##         aes(x = val, color = model)
##       ) +
##       facet_wrap(~date, nrow = 7) +
##       theme_classic() +
##       theme(
##         legend.position = "top",
##         legend.title = element_blank()
##       ) + xlab("")

##     ggsave(
##       filename = glue::glue("{country}_density.png"),
##       plot = p
##     )
##   }
## )