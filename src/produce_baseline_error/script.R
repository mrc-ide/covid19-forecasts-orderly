week_starting <- as.Date(week_starting)
week_ending <- as.Date(week_ending)

weeks <- seq(from = week_starting + 1, to = week_ending, by = "7 days")

model_input <- readRDS("model_input.rds")
model_input <- model_input[model_input$dates > week_starting & model_input$dates <= week_ending, ]
tall <- tidyr::gather(model_input, country, deaths, -dates)

weekly <- split(tall, tall$country) %>%
  purrr::map_dfr(
    function(x) {
      dates <- slider::slide_period_vec(
        x$dates, x$dates, "week", ~ .[1], .origin = week_starting
      )
      incid <- slider::slide_period_vec(
        x$deaths, x$dates, "week", sum, .origin = week_starting
      )
      data.frame(
        week_starting = dates,
        weekly_incid = incid,
        country = x$country[1]
      )
  }, .id = "country"
) %>% tidyr::spread(country, weekly_incid)

### If our predicitons for a week were the average deaths in the last
### week, what would the error we make?


countries <- setNames(colnames(weekly)[-1], colnames(weekly)[-1])
weeks <- slider::slide_period(
  model_input$dates, model_input$dates, "week", identity, .origin = week_starting
  )
weeks <- weeks[-1]
weeks <- purrr::keep(weeks, ~ length(.) == 7)

null_model_error <- purrr::map_dfr(
  countries,
  function(country) {
    purrr::map_dfr(
      weeks,
      function(week) {
        obs <- model_input[model_input$dates %in% week, country]
        prev_week <- model_input[model_input$dates %in% week - 7, country]
        null_pred <- matrix(mean(prev_week), ncol = 10000, nrow = 7)
        baseline <- assessr::rel_mae(obs = obs, pred = null_pred)
        data.frame(
          dates = week,
          null_error = baseline,
          prev_week_avg = mean(prev_week)
        )
      }
    )
  }, .id = "country"
)

saveRDS(weekly, "weekly_incidence.rds")
saveRDS(null_model_error, "null_model_error.rds")
