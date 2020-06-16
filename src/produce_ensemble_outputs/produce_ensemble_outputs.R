## orderly::orderly_develop_start(parameters = list(week_ending = "2020-05-10"))
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
        f(outputs, country, weights = NULL)
      }
    )
  }
)

ensemble_daily_qntls <- purrr::map_dfr(
  ensemble_model_predictions,
  function(pred) {
    purrr::map_dfr(
      pred, function(x) rincewind::extract_predictions_qntls(x, probs),
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
        rincewind::daily_to_weekly(x)
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

## saveRDS(
##   object = wtd_ensb_prev_week,
##   file = "wtd_ensb_prev_week.rds"
## )

## saveRDS(
##   object = wtd_ensb_all_prev_weeks,
##   file = "wtd_ensb_all_prev_weeks.rds"
## )


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
