deaths_threshold <- function(ts,
                             Threshold_criterion_7days = 10,
                             Threshold_criterion_prev7days = 10) {
  th1 <- sum(
    ts$Deaths[ts$DateRep >= max(ts$DateRep) - 7],
    na.rm = TRUE
  ) >= Threshold_criterion_7days

  th2 <- sum(
    ts$Deaths[ts$DateRep >= max(ts$DateRep) - 14 &
      ts$DateRep < max(ts$DateRep) - 7]
  ) >= Threshold_criterion_prev7days

  th3 <- sum(ts$Deaths) >= 100

  th1 & th2 & th3
}

## Paarmeters for data preparation
## week_finishing in the form yyy-mm-dd
parameters <- function(week_finishing) {
  shape <- 3.16
  scale <- 1.52
  # hist(rgamma(1e4,shape = shape, scale = scale))
  new_params <- epitrix::gamma_shapescale2mucv(
    shape = shape, scale = scale
  )
  si_mean <- new_params$mu
  si_std <- new_params$mu * new_params$cv
  ## Neil suggested sensitivity analyses with a shorter and a longer time
  ## window
  si_mean <- c(si_mean, 6.48)
  si_std <- c(si_std, 3.83)

  params <- list(
    infile = glue::glue(
      "WHO-COVID-19-global-data-", "{week_finishing}.csv"
    ),
    si_mean = si_mean,
    si_std = si_std,
    threshold_criterion_7days = 10,
    Threshold_criterion_prev7days = 10,
    outfile = glue::glue("data_{week_finishing}.rds")
  )

  params
}
