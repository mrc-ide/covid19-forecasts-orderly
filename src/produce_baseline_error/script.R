## orderly::orderly_develop_start("src/produce_baseline_error/", parameters = list(latest_week = "2020-12-13", week_starting = "2020-02-22"), use_draft = "newer")
dir.create("figures")

weekly_cv <- function(vec) sd(vec) / mean(vec)

window_past <- 10
window_future <- 7
model_input <- readRDS("model_input.rds")
model_input <- model_input[model_input$dates > week_starting - window_past &
                           model_input$dates <= week_ending, ]

weekly <- readRDS("weekly_incidence.rds")
countries <- readRDS("countries_included.rds")
weeks <- readRDS("weeks_included.rds")
## threshold for excluding countries
by_country <- filter(weekly, weekly_cv > 0) %>%
  group_by(country) %>%
  summarise(cv = mean(weekly_cv, na.rm = TRUE)) %>%
  ungroup()

## Excluding countries where the mean weekly CV was greater than
## 1
## of 133 countries with 100 or more deaths,
## we exclude 53 countries. 80 countries are therefore included in the
## analysis
## 1.1 is the 60th quantile of the distribution of weekly CVs
exclude <- filter(by_country, cv > 1.1) %>% pull(country)
saveRDS(exclude, "exclude.rds")
countries <- countries[!countries %in% exclude]
### If our predicitons for a week were the average deaths in the last
### week, what error would we make?
nsim <- 4000

null_model_error <- map_dfr(
  countries,
  function(country) {
    message(country)
    imap_dfr(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        obs <- model_input[model_input$dates %in% week, country]
        if (inherits(obs, "data.frame")) obs <- as.numeric(obs[[country]])
        if (length(obs) == 0) return(NULL)
        dates_prev <- seq(
          from = as.Date(week_starting) - window_past + 1,
          to = as.Date(week_starting),
          by = "1 day"
        )
        prev_week <- model_input[model_input$dates %in% dates_prev,
                                 country]
        if (inherits(prev_week, "data.frame")) {
          prev_week <- as.numeric(prev_week[[country]])
        }
        if (length(prev_week) == 0) return(NULL)
        if (length(prev_week) < window_past) return(NULL)

        null_pred <- matrix(mean(prev_week), ncol = nsim, nrow = window_future)
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

linear_model_fits <- map(
  countries,
  function(country) {
    message(country)
    imap(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        dates_prev <- seq(
          from = as.Date(week_starting) - window_past + 1,
          to = as.Date(week_starting),
          by = "1 day"
        )
        prev_week <- model_input[model_input$dates %in% dates_prev,
                                 country]
        if (inherits(prev_week, "data.frame")) {
          prev_week <- as.numeric(prev_week[[country]])
        }
        if (length(prev_week) == 0) return(NULL)
        if (length(prev_week) < window_past) return(NULL)

        if (all(prev_week == 0)) {
          lm_pred <- matrix(0, ncol = window_future, nrow = nsim)
          return(lm_pred)
        }
        ## If all values are the same, then we can't fit a line
        if (sd(prev_week) == 0) return(NULL)
        ## fit a line
        df <- data.frame(x = seq_along(prev_week), y = prev_week)
        lmfit <- stan_lm(y ~ x, data = df, prior = NULL)
        lmfit
      }
    )
  }
)


linear_model_predictions <- map(
  countries,
  function(country) {
    message(country)
    imap(
      weeks,
      function(week, week_starting) {
        message(paste(week, collapse = " "))
        lmfit <- linear_model_fits[[country]][[week_starting]]
        if (is.null(lmfit)) return(NULL)
        if (is.matrix(lmfit)) return(lmfit)
        newdata <- data.frame(
          x = seq(
            from = window_past + 1, length.out = window_future, by = 1
          )
        )
        lm_pred <- posterior_predict(
          lmfit, draws = nsim, newdata = newdata
        )
        ##lm_pred[lm_pred < 0] <-  0
        lm_pred
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
        df <- apply(pred, 2, quantile, prob = seq(0, 1, by = 0.05))
        df <- data.frame(df, check.names = FALSE)
        df <- tibble::rownames_to_column(df)
        dates_pred <- seq(
          from = as.Date(week_starting) + 1,
          length.out = window_future,
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
        if (inherits(obs, "data.frame")) obs <- as.numeric(obs[[country]])
        if (length(obs) == 0) return(NULL)
        lm_pred <- linear_model_predictions[[country]][[week_starting]]
        if (is.null(lm_pred)) return(NULL)
        lm_pred <- t(lm_pred)
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
