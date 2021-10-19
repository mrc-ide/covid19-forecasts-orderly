## orderly::orderly_develop_start(parameters = list(week_ending = "2021-05-23"))

##infiles <- orderly::orderly_run_info()$depends

infiles <- list.files(pattern = "*.rds")
infiles <- infiles[! infiles %in% "ensemble_model_predictions.rds"]
infiles <- grep("predictions", infiles, value = TRUE)
states <- gsub(
  "ensemble_model_predictions_", "", infiles
)

states <- gsub(".rds", "", states)
names(infiles) <- states


dates_projected <- seq(
  as.Date("2021-05-24"), length.out = 7, by = "1 day"
)

projections <- map(infiles, readRDS)

projections <- map(
  projections, function(x) {
    out <- x[["si_2"]]
    ## 10000 rows, 7 columns
    out <- data.frame(out)
    colnames(out) <- as.character(dates_projected)
    out$sim <- seq_len(nrow(out))
    out
  }
)

map(projections, function(x) apply(x, 2, quantile))

projections <- bind_rows(projections)

states_summed <- group_by(projections, sim) %>%
  summarise(across(.cols = `2021-05-24`:`2021-05-30`, .funs = sum))

states_summed <- ungroup(states_summed)

apply(states_summed[, -1], 2, quantile)
national <- readRDS("ensemble_model_predictions.rds")
national <- national[[1]][["United_States_of_America"]][["si_2"]]
apply(national, 2, quantile)

x <- projections[, c(1, 8)]
## Notes: netx week, look at the cumulative contribution of the
## excluded states to the death toll to identify if including them
## would make a difference.
