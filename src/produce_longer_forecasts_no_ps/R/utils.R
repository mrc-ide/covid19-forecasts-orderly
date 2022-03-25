proportion_susceptible <- function(deaths_per_capita, cfr) {
  x <- 1 - (deaths_per_capita / cfr)
  x[x < 0] <- 0
  x
}

## ws T X 1 matrix where N is the number of simulations,
## flip SI before calling this function
## deaths N X T matrix where N is the number of simulations
## R numeric.
force_of_infection <- function(deaths, ws, R) {
  (deaths %*% ws) * R
}

## deaths is a vector of deaths
## r_eff = r_obs / p_susceptible
## p_susceptible is the proportion susceptible at the point at which
## we start projecting ahead.
project_without_saturation <- function(deaths,
                                       r_eff,
                                       p_susceptible,
                                       si,
                                       n_days,
                                       cfr,
                                       pop,
                                       n_sim = 1000,
                                       sims_per_rt = 10) {

  total_sims <- n_sim * sims_per_rt
  deaths_obs <- sum(deaths)
  I0 <- matrix(
    deaths, ncol = length(deaths), nrow = total_sims, byrow = TRUE
  )
  day_max <- length(deaths) + n_days
  si <- c(si, rep(0, day_max - length(si)))
  out <- list(
    r_effective = vector(mode = "list", length = n_days),
    p_s = vector(mode = "list", length = n_days)
  )

  ##p_susceptible <- sample(p_susceptible, n_sim)
  ## Sample once here, so that values are carried through a
  ## single simulation
  idx <- sample(length(cfr), n_sim)
  r_eff <- r_eff[idx]
  cfr <- cfr[idx]
  p_susceptible <- p_susceptible[idx]
  ## Repeat each value so that we get multiple simulations for
  ## each value of R and p_susceptible
  r_eff <- rep(r_eff, each = sims_per_rt)
  p_susceptible <- rep(p_susceptible, each = sims_per_rt)
  cfr <- rep(cfr, each = sims_per_rt)
  R <- r_eff * p_susceptible
  for (day in 1:n_days) {
    message(day)
    ws <- tail(rev(si), length(deaths) + day - 1)
    ws <- matrix(
      ws, ncol = 1,  nrow = length(deaths) + day - 1
    )
    lambda <- force_of_infection(I0, ws, R)
    if (any(is.na(lambda)) | any(lambda < 0)) {
      message("ws at this point is", paste(ws, collapse = "\n"))
      stop("Force of infection is NA on day ", day)
    }
    pred <- map_int(lambda, function(x) rpois(1, x))
    I0 <- cbind(I0, pred)
  }
  ncols_pred <- tail(seq_len(ncol(I0)), n_days)
  out[["pred"]] <- I0[ , ncols_pred]
  out
}
