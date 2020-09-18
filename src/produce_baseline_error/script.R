##  orderly::orderly_develop_start("src/produce_baseline_error/", parameters = list(week_ending = "2020-09-07", week_starting = "2020-03-08"))
weekly_cv <- function(vec) sd(vec) / mean(vec)

week_starting <- as.Date(week_starting)
week_ending <- as.Date(week_ending)

weeks <- seq(from = week_starting, to = week_ending, by = "7 days")

model_input <- readRDS("model_input.rds")
model_input <- model_input[model_input$dates > week_starting &
                           model_input$dates <= week_ending, ]

countries <- setNames(
  colnames(model_input)[-1], colnames(model_input)[-1]
)

weeks_day1 <- seq(
  week_starting, week_ending, "7 days"
)

weeks <- map(
  weeks_day1, function(x) seq(from = x + 1, length.out = 7, by = "1 day")
)

names(weeks) <- weeks_day1

tall <- tidyr::gather(model_input, country, deaths, -dates)

weekly <- map_dfr(
  countries,
  function(country) {
    message(country)
    imap_dfr(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        incid <- model_input[model_input$dates %in% week, country]
        cv <- weekly_cv(incid)
        data.frame(
          week_starting = week_starting,
          dates = week,
          weekly_incid = sum(incid),
          weekly_cv = cv,
          country = country
        )
      }
    )
  }, .id = "country"
)

## threshold for excluding countries
by_country <- filter(weekly, weekly_cv > 0) %>%
  group_by(country) %>%
  summarise(cv = mean(weekly_cv, na.rm = TRUE)) %>%
  ungroup()

exclude <- filter(by_country, cv > 1.38) %>% pull(country)
saveRDS(exclude, "exclude.rds")
### If our predicitons for a week were the average deaths in the last
### week, what would the error we make?
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
