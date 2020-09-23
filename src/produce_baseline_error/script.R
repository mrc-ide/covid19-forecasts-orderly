##  orderly::orderly_develop_start("src/produce_baseline_error/", parameters = list(week_ending = "2020-09-07", week_starting = "2020-03-08"))
dir.create("figures")
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

nsim <- 10000

linear_model_predictions <- map(
  countries,
  function(country) {
    message(country)
    imap(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        prev_week <- model_input[model_input$dates %in% (week - 7), country]
        if (length(prev_week) == 0) return(NULL)
        ## fit a line
        df <- data.frame(x = seq_along(prev_week), y = prev_week)
        lmfit <- lm(y ~ x, data = df)
        coeffs <- lmfit$coefficients
        err_var <- sd(lmfit$residuals)
        pred <- coeffs[["(Intercept)"]] + (coeffs[["x"]] * (8:14))
        lm_pred <- matrix(pred, ncol = nsim, nrow = 7, byrow = FALSE)
        noise <- rnorm(nsim, mean = 0, sd = err_var)
        noise <- rep(noise, each = 7)
        noise <- matrix(noise, ncol = nsim, nrow = 7, byrow = FALSE)

        lm_pred + noise
      }
    )
  }
)


linear_model_pred_qntls <- map_dfr(
  countries,
  function(country) {
    message(country)
    imap_dfr(
      weeks,
      function(week, week_starting) {
        pred <- linear_model_predictions[[country]][[week_starting]]
        if (is.null(pred)) return(NULL)
        df <- apply(pred, 1, quantile, prob = seq(0, 1, by = 0.05))
        df <- data.frame(df, check.names = FALSE)
        df <- tibble::rownames_to_column(df)
        dates_pred <- seq(
          from = as.Date(week_starting) + 1,
          length.out = 7,
          by = "1 day"
        )
        colnames(df) <-  c("qntl", as.character(dates_pred))
        df <- gather(df, date, val, -qntl) %>%
          spread(qntl, val)

        df
      }, .id = "forecast_week"
    )
  }, .id = "country"
)

linear_model_pred_qntls$date <- as.Date(linear_model_pred_qntls$date)

walk(
  countries,
  function(country) {
    message(country)
    df <- linear_model_pred_qntls[linear_model_pred_qntls$country == country, ]
    model_input$deaths <- model_input[[country]]
    p <- ggplot() +
      geom_point(data = model_input, aes(dates, deaths)) +
      geom_line(
        data = df,
        aes(x = date, y = `50%`, group = forecast_week), col = "blue"
      ) +
      geom_ribbon(
        data = df,
        aes(x = date, ymin = `25%`, ymax = `75%`, group = forecast_week),
        alpha = 0.3,
        fill = "blue"
      ) +
      scale_x_date(
        date_breaks = "2 weeks",
        limits = c(as.Date("2020-03-01"), NA)
      ) +
      theme_minimal() +
      theme(
        axis.text.x =
          element_text(angle = 90, hjust = 0.5, vjust = 0)
      ) + xlab("") + ylab("Daily Deaths")
    ggsave(glue("figures/{country}_linear.png"), p)
  }
)




linear_model_error <- map_dfr(
  countries,
  function(country) {
    imap_dfr(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        obs <- model_input[model_input$dates %in% week, country]
        if (length(obs) == 0) return(NULL)
        lm_pred <- linear_model_predictions[[country]][[week_starting]]
        if (is.null(lm_pred)) return(NULL)
        baseline <- assessr::mae(obs = obs, pred = lm_pred)
        baseline_rel <- assessr::rel_mae(obs = obs, pred = lm_pred)
        data.frame(
          week_starting = week_starting,
          dates = week,
          null_error = baseline,
          rel_null_error = baseline_rel
        )
      }, .id = "forecast_week"
    )
  }, .id = "country"
)

saveRDS(weekly, "weekly_incidence.rds")
saveRDS(null_model_error, "null_model_error.rds")
saveRDS(linear_model_error, "linear_model_error.rds")
