##orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"))
## infiles <- list.files(pattern = "*.rds")

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

infiles <- grep("latest", infiles, invert = TRUE, value = TRUE)

names(infiles) <- gsub(
  pattern = "ensemble_model_rt_samples_", replacement = "", x = infiles
) %>% gsub(pattern = ".rds", replacement = "", x = .)

rt_samples <- purrr::map(infiles, readRDS)
observed <- readRDS("latest_deaths_wide_no_filter.rds")

week_ending <- as.Date(week_ending)
week_prev <- week_ending - 7

model_input <- readRDS(
  glue::glue("{dirname(covid_19_path)}/model_inputs/data_{week_prev}.rds")
)
deaths_to_use <- model_input$D_active_transmission
si <- EpiEstim::discr_si(0:30, model_input$si_mean[2], model_input$si_std[2])

## countries included in the week_prev week of analysis
countries <- setNames(model_input$Country, model_input$Country)

country_weeks <- purrr::map(
  countries,
  function(country) {
    weeks <- map(
      rt_samples, ~ .[.$country == country, "model"][1]
    )
    weeks <- as.Date(unlist(weeks, recursive = FALSE))
    consecutive_weeks <- list()
    prev_week <- week_ending
    while (prev_week %in% weeks) {
      message(country, " in ", prev_week)
      consecutive_weeks <- append(consecutive_weeks, prev_week)
      prev_week <- prev_week - 7
    }
    consecutive_weeks
  }
)

country_weeks <- keep(country_weeks, ~ length(.) > 0)

## week_ending is the week for which we already have observation,
## so that we can minimise the error for this week.
betas <- seq(0, 5, by = 1)
names(betas) <- betas

combined_rts <- map(
  betas,
  function(beta) {
    weights <- rev(exp(-beta * seq_along(weeks)))
    weights <- weights / sum(weights)

    idx <- sample(
      x = weeks, size = 10000, replace = TRUE, prob = weights
    )
    nsamples <- data.frame(table(idx), stringsAsFactors = FALSE)
    message(nsamples)
    message(beta)
    map_dfr(
      weeks,
      function(week) {
        message(week)
        week <- as.character(week)
        df <- rt_samples[[week]]
        out <- split(df, df$country) %>%
          map_dfr(
            function(country) {
              size <- nsamples$Freq[nsamples$idx == week]
              if (length(size) == 0) return(NULL)
              idx <- sample(
                nrow(country),
                size = size,
                replace = TRUE
              )
              country[idx, ]
            }
        )
        out
      }
    )
  }
)


combined_rts <- map(
  betas,
  function(beta) {
    weights <- rev(exp(-beta * seq_along(weeks)))
    weights <- weights / sum(weights)

    idx <- sample(
      x = weeks, size = 10000, replace = TRUE, prob = weights
    )
    nsamples <- data.frame(table(idx), stringsAsFactors = FALSE)
    message(nsamples)
    message(beta)
    map_dfr(
      weeks,
      function(week) {
        message(week)
        week <- as.character(week)
        df <- rt_samples[[week]]
        out <- split(df, df$country) %>%
          map_dfr(
            function(country) {
              size <- nsamples$Freq[nsamples$idx == week]
              if (length(size) == 0) return(NULL)
              idx <- sample(
                nrow(country),
                size = size,
                replace = TRUE
              )
              country[idx, ]
            }
        )
        out
      }
    )
  }
)



projections <- map(
  combined_rts,
  function(rt) {
    by_country <- split(rt, rt$country)
    imap(
      by_country,
      function(rt_country, country) {
        message(country)
        obs <- deaths_to_use[deaths_to_use$dates <= week_ending - 7, c("dates", country)]
        obs$deaths <- obs[[country]]
        incid <- rincewind:::ts_to_incid(obs, "dates", "deaths")
        out <- rerun(
          10,
          projections::project(incid, rt_country$si_2, si, model = "poisson")
        )
        do.call(what = 'cbind', args = out)
      }
    )
  }
)






countries <- setNames(
  colnames(deaths_to_use)[-1], colnames(deaths_to_use)[-1]
)

error <- map(
  betas,
  function(beta) {

    pred <- projections[[as.character(beta)]]
    imap(
      pred,
      function(pred_country, country) {
        obs <- observed[observed$dates %in% as.Date(rownames(pred_country)), country]
        mean(assessr::rel_mae(obs, pred_country))
      }
    )
  }
)


per_country <- map_dfr(
  countries,
  function(country) {
    message(country)
    ## For each country, the value of beta that gives smallest error
    x <- map_dbl(error, ~ .[[country]])
    y <- x[which.min(x)]
    data.frame(
      beta = as.numeric(names(y)),
      error = y[[1]]
    )
  }, .id = "country"
)

across_countries <- map(error, ~ sum(unlist(.)))
across_countries <- across_countries[which.min(across_countries)]

across_countries <- data.frame(
  beta = as.numeric(names(across_countries)),
  error = across_countries[[1]]
)

saveRDS(across_countries, "across_countries.rds")
saveRDS(per_country, "per_country.rds")
