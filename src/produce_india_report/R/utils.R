## mat is T X N where N is the number of samples
quantiles_to_df <- function(mat, probs = c(0.025, 0.50, 0.975)) {

    qntls <- t(apply(mat, 1, quantile, probs = probs, na.rm = TRUE))
    out <- data.frame(qntls)
    colnames(out) <- scales::percent(probs, accuracy = 0.1)
    out

}
