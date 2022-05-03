run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
source("R/utils.R")
short_term <- grep("short_term", infiles, value = TRUE) %>%
  map_dfr(readRDS)

short_term <- rename(short_term, c('forecast_date' = 'model'))
short_term$forecast_date <- as.Date(short_term$forecast_date)

short_term$phase_for_week <- phase_for_week(
  short_term$forecast_date - 6, short_term$forecast_date
)

medium_term <- grep("medium_term", infiles, value = TRUE) %>%
  map_dfr(readRDS)

medium_term$day <- as.integer(medium_term$day)
medium_term <- medium_term[medium_term$day <= 28, ]
medium_term$week_of_forecast <- case_when(
  medium_term$day <= 7 ~ "1-week ahead",
  7 < medium_term$day & medium_term$day <= 14 ~ "2-weeks ahead",
  14 < medium_term$day & medium_term$day <= 21 ~ "3-weeks ahead",
  21 < medium_term$day & medium_term$day <= 28  ~ "4-weeks ahead"
)

### Assign phase to week rather than day, rule - the phase assigned
### to majority of days in the week is the weekly phase.
weekly_phase <- split(
  medium_term,
  list(medium_term$country, medium_term$model,
       medium_term$week_of_forecast)
) %>%
  keep(~ nrow(.) > 0) %>%
  map_dfr(function(x) {

  freq <- tabyl(x$phase)
  if (nrow(freq) > 1) {
    message("More than 1 phase detected in ", x$country[1],
            " week ", x$model[1])
  }
  most_freq <- which.max(freq$n)
  ## Return just the first row without the day, and the most
  ## frequent phase
  x$phase <- freq[[1]][most_freq]
  out <- x[1, ]
  if (is.na(out$country)) browser()
  f <- function(y, day) {
    ## starts on a Monday
    y <- as.Date(y)
    start <- y + ((day - 1) * 7) + 1
    end <- start + 6
    phase_for_week(start, end)
  }
  out$phase_for_week <- case_when(
    out$week_of_forecast == "1-week ahead" ~ f(out$model, 1),
    out$week_of_forecast == "2-weeks ahead" ~ f(out$model, 2),
    out$week_of_forecast == "3-weeks ahead" ~ f(out$model, 3),
    out$week_of_forecast == "4-weeks ahead" ~ f(out$model, 4)
  )
  out
})

saveRDS(short_term, "collated_short_term_phase.rds")
saveRDS(medium_term, "collated_medium_term_phase_daily.rds")
saveRDS(weekly_phase, "collated_medium_term_phase_weekly.rds")
