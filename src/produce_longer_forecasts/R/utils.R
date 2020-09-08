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
  r_eff <- sample(r_eff, n_sim)
  ##p_susceptible <- sample(p_susceptible, n_sim)
  for (day in 1:n_days) {
    message(day)
    R <- r_eff * p_susceptible
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
    deaths_so_far <- rowSums(I0)
    deaths_per_cap <- deaths_so_far / pop
    p_susceptible <- proportion_susceptible(deaths_per_cap, cfr)

    out[["r_effective"]][[day]] <- R
    out[["p_s"]][[day]] <- p_susceptible
  }
  ncols_pred <- tail(seq_len(ncol(I0)), n_days)
  out[["pred"]] <- I0[ , ncols_pred]
  out
}

combined_plot <- function(obs, pred, ps, reff) {

  p1 <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
    geom_ribbon(
      data = pred, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) +
    geom_line(
      data = pred, aes(x = date, y = `50%`)
    ) +
    ylab("Daily Deaths") +
    xlab("") +
    theme(axis.text.x = element_blank())

  p2 <- ggplot() +
    geom_ribbon(
      data = reff, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) +
    geom_line(
      data = reff, aes(x = date, y = `50%`)
    ) +
    ylim(0, NA) +
    geom_hline(yintercept = 1, col = "red", linetype = "dashed") +
    ylab("Effective reproduction number") +
    xlab("") +
    theme(axis.ticks.x = element_blank())


  p3 <- ggplot() +
    geom_ribbon(
      data = ps, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) +
    geom_line(
      data = ps, aes(x = date, y = `50%`)
    ) + ylim(0, 1) +
    xlab("") +
    ylab("Proportion susceptible")

  p <- p1 + p2 + p3 + plot_layout(nrow = 3) +
    plot_annotation(tag_levels = 'A') &
        scale_x_date(
      date_breaks = "2 weeks", limits = c(as.Date("2020-03-01"), NA)
    ) &
    theme_minimal()
  p

}
