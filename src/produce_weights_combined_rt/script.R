## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"), use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

exclude <- readRDS("exclude.rds")
infiles <- grep("exclude", infiles, invert = TRUE, value = TRUE)
infiles <- grep("latest", infiles, invert = TRUE, value = TRUE)
infiles <- grep("model_input", infiles, invert = TRUE, value = TRUE)

names(infiles) <- gsub(
  pattern = "ensemble_model_rt_samples_", replacement = "", x = infiles
) %>% gsub(pattern = ".rds", replacement = "", x = .)

rt_samples <- purrr::map(infiles, readRDS)
observed <- readRDS("latest_deaths_wide_no_filter.rds")
observed[["Czech_Republic"]] <- observed[["Czechia"]]
week_ending <- as.Date(week_ending)
week_prev <- week_ending - 7

model_input <- readRDS("model_input.rds")
##deaths_to_use <- model_input$D_active_transmission

deaths_to_use <- observed[observed$dates <= week_prev, ]
si <- EpiEstim::discr_si(0:30, model_input$si_mean[2], model_input$si_std[2])

## countries included in the week_prev week of analysis
countries <- setNames(model_input$Country, model_input$Country)
message("################ Including countries ######################")
message(paste(countries, collapse = "\n"))

country_weeks <- map(
  countries,
  function(country) {
    weeks <- map(
      rt_samples, ~ .[.$country == country, "model"][1]
    )
    weeks <- keep(weeks, ~ !is.na(.))
    weeks <- as.Date(unlist(weeks, recursive = FALSE))
    gaps <- as.numeric(diff(weeks))
    if (all(gaps == 7)) consecutive_weeks <- weeks
    else {
      ## Find the first date which is more than 7 days
      ## away from the next date
      idx <- which(gaps != 7)[1]
      idx <- idx + 1
      consecutive_weeks <- tail(weeks, idx)
    }
    consecutive_weeks
  }
)

country_weeks <- keep(country_weeks, ~ length(.) > 0)
saveRDS(country_weeks, "country_weeks.rds")
## week_ending is the week for which we already have observation,
## so that we can minimise the error for this week.
betas <- seq(0, 5, by = 0.1)
names(betas) <- betas

combined_rts <- imap(
  country_weeks,
  function(weeks, country) {
    message(country)
    map(
      betas,
      function(beta) {
        message(beta)
        weights <- exp(-beta * seq_along(weeks))
        weights <- weights / sum(weights)
        message(weights)
        ## weeks goes from earlier to later; so reverse the weights
        ## so that later weeks get higher weights
        weights <- rev(weights)
        idx <- sample(
          x = weeks, size = 10000, replace = TRUE, prob = weights
        )
        nsamples <- data.frame(table(idx), stringsAsFactors = FALSE)

        rt_country <- map_dfr(
          weeks,
          function(week) {
            message(week)
            week <- as.character(week)
            df <- rt_samples[[week]]
            df <- df[df$country == country, ]
            size <- nsamples$Freq[nsamples$idx == week]
            if (length(size) == 0) return(NULL)
            idx <- sample(
              nrow(df), size = size, replace = TRUE
            )
            df[idx, ]
          }
        )
        rt_country
      }
    )
  }
)


projections <- imap(
  combined_rts,
  function(rt, country) {
    message(country)
    obs <- deaths_to_use[, c("dates", country)]
    obs$deaths <- obs[[country]]
    incid <- rincewind:::ts_to_incid(obs, "dates", "deaths")
    map(
      rt,
      function(rt_beta) {
        out <- rerun(
          10,
          project(
            incid, rt_beta$si_2, si[-1], model = "poisson", R_fix_within = TRUE
          )
        )
        do.call(what = 'cbind', args = out)
      }
    )
  }
)

dates_projected <- seq(week_prev + 1, length.out = 7, by = "1 day")

error <- imap(
  projections,
  function(pred, country) {
    message(country)
    obs <- observed[observed$dates %in% dates_projected, country]
    ## Not sure why but sometimes obs is a tibble.
    if (! inherits(obs, "numeric")) obs <- obs[[country]]
    map(
      pred, function(pred_beta) {
        mean(assessr::rel_mae(obs, pred_beta))
      }
    )
  }
)


per_country <- map_dfr(
  error,
  function(err_country) {
    ## For each country, the value of beta that gives smallest error
    y <- err_country[which.min(err_country)]
    data.frame(
      beta = as.numeric(names(y)),
      error = y[[1]]
    )
  }, .id = "country"
)

across_countries <- map(
  betas, function(beta) sum(map_dbl(error, ~ .[[as.character(beta)]]))
)

across_countries <- across_countries[which.min(across_countries)]

across_countries <- data.frame(
  beta = as.numeric(names(across_countries)),
  error = across_countries[[1]]
)

saveRDS(across_countries, "across_countries.rds")
saveRDS(per_country, "per_country.rds")
