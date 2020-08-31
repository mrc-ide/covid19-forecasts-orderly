##  orderly::orderly_develop_start("src/produce_baseline_error/", parameters = list(week_ending = "2020-08-16", week_starting = "2020-03-08"))
week_starting <- as.Date(week_starting)
week_ending <- as.Date(week_ending)

weeks <- seq(from = week_starting, to = week_ending, by = "7 days")

model_input <- readRDS("model_input.rds")
model_input <- model_input[model_input$dates > week_starting &
                           model_input$dates <= week_ending, ]
tall <- tidyr::gather(model_input, country, deaths, -dates)

weekly_cv <- function(vec) sd(vec) / mean(vec)

weekly <- split(tall, tall$country) %>%
  map_dfr(
    function(x) {
      dates <- slider::slide_period_vec(
        x$dates, x$dates, "week", ~ .[1], .origin = week_starting
      )
      incid <- slider::slide_period_vec(
        x$deaths, x$dates, "week", sum, .origin = week_starting
        )
      cv <- slider::slide_period_vec(
        x$deaths, x$dates, "week", weekly_cv, .origin = week_starting
      )
      data.frame(
        week_starting = dates,
        weekly_incid = incid,
        weekly_cv = cv,
        country = x$country[1]
      )
  }, .id = "country"
) ##%>% tidyr::spread(country, weekly_incid)

### If our predicitons for a week were the average deaths in the last
### week, what would the error we make?


countries <- setNames(
  unique(weekly$country), unique(weekly$country)
)

weeks_day1 <- seq(
  week_starting, week_ending, "7 days"
)

weeks <- map(
  weeks_day1, function(x) seq(from = x + 1, length.out = 7, by = "1 day")
)

names(weeks) <- weeks_day1

null_model_error <- map_dfr(
  countries,
  function(country) {
    message(country)
    imap_dfr(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        obs <- model_input[model_input$dates %in% week, country]
        if (length(obs) == 0) return(NULL)
        prev_week <- model_input[model_input$dates %in% (week - 7), country]
        null_pred <- matrix(mean(prev_week), ncol = 10000, nrow = 7)
        baseline <- assessr::mae(obs = obs, pred = null_pred)
        baseline_rel <- assessr::rel_mae(obs = obs, pred = null_pred)
        data.frame(
          week_starting = week_starting,
          dates = week,
          null_error = baseline,
          rel_null_error = baseline_rel,
          prev_week_avg = mean(prev_week)
        )
      }
    )
  }, .id = "country"
)

saveRDS(weekly, "weekly_incidence.rds")
saveRDS(null_model_error, "null_model_error.rds")
