week_starting <- as.Date(week_starting)
week_ending <- as.Date(latest_week)
weeks <- seq(from = week_starting, to = week_ending, by = "7 days")
weeks_day1 <- seq(week_starting, week_ending, "7 days")
weeks <- map(
  weeks_day1,
  function(x) seq(from = x + 1, length.out = 7, by = "1 day")
)

names(weeks) <- weeks_day1

## We only include countries with at least 100 deaths
total_deaths <- colSums(model_input[, -1])
include <- total_deaths[total_deaths >= 100]
model_input <- model_input[, c("dates", names(include))]
countries <- setNames(
  colnames(model_input)[-1], colnames(model_input)[-1]
)


tall <- gather(model_input, country, deaths, -dates)

weekly <- map_dfr(
  countries,
  function(country) {
    message(country)
    imap_dfr(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        incid <- model_input[model_input$dates %in% week, country]
        if (inherits(incid, "data.frame")) incid <- as.numeric(incid[[country]])
        mu <- mean(incid)
        sigma <- sd(incid)
        data.frame(
          week_starting = week_starting,
          dates = week,
          weekly_incid = sum(incid),
          mu = mu, sigma = sigma,
          weekly_cv = mu / sigma,
          country = country
        )
      }
    )
  }, .id = "country"
)

