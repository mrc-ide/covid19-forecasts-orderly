run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as


names(infiles) <- gsub(
  pattern = "ensemble_model_rt_samples_", replacement = "", x = infiles
) %>% gsub(pattern = ".rds", replacement = "", x = .)

rt_samples <- purrr::map(infiles, readRDS)
week_ending <- as.Date(week_ending)

## week_ending is the week for which we already have observation,
## so that we can minimise the error for this week.
weeks <- seq(from = week_ending - 7, to = as.Date("2020-03-08"), by = "-7 days")
betas <- seq(0, 5, by = 1)
combined_rts <- map(
  betas,
  function(beta) {
    weights <- rev(exp(-beta * seq_along(weeks)))
    weights <- weights / sum(weights)

    idx <- sample(
      x = weeks, size = 10000, replace = TRUE, prob = weights
    )
    nsamples <- data.frame(table(idx), stringsAsFactors = FALSE)
    ## nsamples$idx <- as.character(nsamples$idx)
    ## if (
    ##   ! all(
    ##       as.character(weeks) %in% as.character(nsamples$idx)
    ##     )
    ## ) {
    ##   missing <- which(! as.character(weeks) %in% as.character(nsamples$idx))
    ##   row <- data.frame(
    ##     idx = weeks[missing],
    ##     Freq = 1
    ##   )
    ##   nsamples <- rbind(nsamples, row)
    ## }
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

model_input <- readRDS(
  "~/OneDrive - Imperial College London/covid19-short-term-forecasts/model_inputs/data_2020-03-22.rds"
)
deaths_to_use <- model_input$D_active_transmission
si <- EpiEstim::discr_si(0:30, model_input$si_mean[2], model_input$si_std[2])
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


observed <- readRDS(
  "~/OneDrive - Imperial College London/covid19-short-term-forecasts/model_inputs/data_2020-03-29.rds"
)
