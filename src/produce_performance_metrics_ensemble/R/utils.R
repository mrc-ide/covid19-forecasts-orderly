## Expect a T X N matrix where N is the number
## of simulations
poisson_probability <- function(obs, pred) {

  pred_rows <- lapply(
    seq_len(nrow(pred)), function(idx) pred[idx, ]
  )
  probs <- mapply(
    FUN = function(x, xhat) {
      lambda <- mean(xhat)
      message("mean of predictions ", lambda)
      dpois(x = x, lambda = lambda)
    },
    x = obs,
    xhat = pred_rows
 )
  message("Poisson probability ", paste(probs, collapse = " "))
  probs

}

empirical_probability <- function(obs, pred) {

  pred_rows <- lapply(
    seq_len(nrow(pred)), function(idx) pred[idx, ]
  )
  probs <- mapply(
    FUN = function(x, xhat) {
      idx <- which(x == xhat)
      message("Number of times obs in pred ", length(idx))
      length(idx) / length(xhat)
    },
    x = obs,
    xhat = pred_rows
 )
  message("Empirical probability ", paste(probs, collapse = " "))
  probs

}

all_metrics <- function(obs, pred) {
  qntls <- t(
    apply(pred, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  )
  qntls <- data.frame(qntls, check.names = FALSE)
  pred <- t(pred)
  data.frame(
    mae = assessr::mae(obs, pred),
    rel_mae = assessr::rel_mae(obs, pred),
    rel_mse = assessr::rel_mse(obs, pred),
    rel_sharpness = assessr::rel_mean_dvtn(pred),
    bias = assessr::bias(obs, pred),
    prop_in_50 = assessr::prop_in_ci(obs, qntls[["25%"]], qntls[["75%"]]),
    prop_in_975 = assessr::prop_in_ci(obs, qntls[["2.5%"]], qntls[["97.5%"]]),
    empirical_p = empirical_probability(obs, pred),
    poisson_p = poisson_probability(obs, pred)
  )
}
