run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as


names(infiles) <- gsub(
  pattern = "ensemble_model_rt_samples", replacement = "", x = infiles
) %>% gsub(pattern = ".rds", replacement = "", x = .)

rt_samples <- purrr::map(infiles, readRDS)
week_ending <- as.Date(week_ending)

weeks <- seq(from = week_ending, to = as.Date("2020-03-08"), by = "-7 days")
betas <- seq(0, 5, by = 0.1)
combined_rts <- map(
  betas,
  function(beta) {
    weights <- exp(-beta * (1:4))
    weights <- weights / sum(weights)

    idx <- sample(
      x = weeks, size = 100000, replace = TRUE, prob = weights
    )
    ## TODO the weight on some weeks might be small enough that
    ## they are not sampled at all. Add component with 0 in nsamples
    ## so that the next bit can work.
    nsamples <- table(idx)
    map(weeks, function(week) sample(rt_samples))

  }
)
