raw_data <- readRDS("model_input.rds")

si_distrs <- map2(
  raw_data[["si_mean"]],
  raw_data[["si_std"]],
  function(mu, sigma) {
    reparams <- epitrix::gamma_mucv2shapescale(
      mu = mu, cv = sigma / mu
    )
    miss_at_most <-0.001
    cutoff <- ceiling(
      qgamma(
        1 - miss_at_most,
        shape = reparams$shape,
        scale = reparams$scale
      )
    )
    EpiEstim::discr_si(k = 0:cutoff, mu = mu, sigma = sigma)
  }
)

names(si_distrs) <- c("si_1", "si_2")

saveRDS(si_distrs, "si_distrs.rds")

## Checked that this gives the same results as previous code
population_weighted_ifr <- rincewind::age_wtd_ifr("United States of America")
names(population_weighted_ifr) <- "United States of America"

saveRDS(population_weighted_ifr, "population_weighted_ifr.rds")

pop_wtd_ifr_qntls <- map(
  population_weighted_ifr,
  ~ quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))
)

saveRDS(pop_wtd_ifr_qntls, "pop_wtd_ifr_qntls.rds")
