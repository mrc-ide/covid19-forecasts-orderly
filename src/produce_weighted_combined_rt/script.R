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
betas <- seq(0, 5, by = 0.1)
combined_rts <- map(
  betas,
  function(beta) {
    weights <- exp(-beta * seq_along(weeks))
    weights <- weights / sum(weights)

    idx <- sample(
      x = weeks, size = 100000, replace = TRUE, prob = weights
    )
    ## TODO the weight on some weeks might be small enough that
    ## they are not sampled at all. Add component with 0 in nsamples
    ## so that the next bit can work.
    nsamples <- data.frame(table(idx))
    message(nsamples)
    message(beta)
    map(
      weeks,
      function(week) {
        message(week)
        week <- as.character(week)
        df <- rt_samples[[week]]
        out <- split(df, df$country) %>%
          map_dfr(
            function(country) {
              idx <- sample(
                nrow(country),
                size = nsamples$Freq[nsamples$idx == week],
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




