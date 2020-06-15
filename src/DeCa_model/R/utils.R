report_to_death_distr <- function(mu, std, trunc) {
  out <- EpiEstim::discr_si(seq(0, trunc), mu, std)
  out / sum(out)
}

quantiles_to_df <- function(mat, probs = c(0.025, 0.50, 0.975)) {
  qntls <- t(apply(mat, 1, quantile, probs = probs, na.rm = TRUE))
  out <- data.frame(qntls)
  colnames(out) <- scales::percent(probs, accuracy = 0.1)
  out
}
