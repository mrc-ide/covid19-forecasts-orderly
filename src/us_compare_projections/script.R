## orderly::orderly_develop_start(parameters = list(week_ending = "2021-05-23"), use_draft = "newer")

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
  summarise_all(sum)

states_summed <- ungroup(states_summed)

apply(states_summed[, -1], 2, quantile)
national <- readRDS("ensemble_model_predictions.rds")
national <- national[[1]][["United_States_of_America"]][["si_2"]]
apply(national, 2, quantile)

x <- projections[, c(1, 8)]
## Notes: next week, look at the cumulative contribution of the
## excluded states to the death toll to identify if including them
## would make a difference.
obs <- readRDS("latest_deaths_wide_no_filter.rds")
states_excluded <- which(! colnames(obs)[-1] %in% states)
states_excluded <- colnames(obs)[-1][states_excluded]
## Total deaths across excluded states in the 10 days before
## the week for which forecasting was done.
prior_10days <- seq(
  from = as.Date("2021-05-23") - 9,
  to = as.Date("2021-05-23"), by = "1 day"
)

x <- obs[obs$dates %in% prior_10days, states_excluded]
total_deaths <- 236

week_of_forecast <- seq(
  from = as.Date("2021-05-24"), by = "1 day",
  length.out = 7
)

y <- obs[obs$dates %in% week_of_forecast, c("dates", states_excluded)]
ytall <- gather(y, state, deaths, -dates)
ydaily <- group_by(ytall, dates) %>%
  summarise(deaths = sum(deaths)) %>%
  spread(dates, deaths)


## Notes for next week. Projections at national scale and aggregate of projections at sub-national
## scale seem to be consistent with each other. This is slightly surprising.
## Next week: Set up this task for each week since 23rd May, and then check if the trend is
## consistent across time.
