proportion_susceptible <- function(deaths_per_capita, cfr) {
  1 - (deaths_per_capita / cfr)
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
project_with_saturation <- function(deaths, r_eff, p_susceptible, si, n_sim = 100, n_days, cfr, pop) {

  deaths_obs <- sum(deaths)
  I0 <- matrix(
    deaths, ncol = length(deaths), nrow = n_sim, byrow = TRUE
  )
  day_max <- length(deaths) + n_days
  si <- c(si, rep(0, day_max - length(si)))
  out <- list(
    r_effective = vector(mode = "list", length = n_days),
    p_s = vector(mode = "list", length = n_days)
  )
  for (day in 1:n_days) {
    message(day)
    R <- r_eff * p_susceptible
    R <- sample(R, n_sim)
    ws <- tail(rev(si), length(deaths) + day - 1)
    ws <- matrix(
      ws, ncol = 1,  nrow = length(deaths) + day - 1
    )
    lambda <- force_of_infection(I0, ws, R)
    pred <- map_int(lambda, function(x) rpois(1, x))
    I0 <- cbind(I0, pred)
    deaths_so_far <- rowSums(I0)
    deaths_per_cap <- deaths_so_far / pop
    p_susceptible <- proportion_susceptible(deaths_per_cap, cfr)

    out[["r_effective"]][[day]] <- R
    out[["p_s"]][[day]] <- p_susceptible
  }

  out[["incid_and_pred"]] <- I0
  out
}
