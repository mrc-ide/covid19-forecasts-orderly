## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-01-01", location = "Arizona"))
set.seed(1)
## ------------------------------------------------------------------

week_ending <-  as.Date(week_ending)
# delay_report_death <- 10 # need checking!!
day.project <- 7
t.window.range <- 7
rep <- 2e4
n_post <- 1e4
mu_delta <- 10
s_delta <- 2
SItrunc <- 30
t.window <- 10
## ------------------------------------------------------------------
ifr <- readRDS("population_weighted_ifr.rds")[[1]]
model_input <- readRDS("model_input.rds")
ascertainr_deaths <- model_input$D_active_transmission
ascertainr_cases <- model_input$I_active_transmission

report_to_death_distr <- function(mu, std, trunc) {
  out <- EpiEstim::discr_si(seq(0, trunc), mu, std)
  out / sum(out)
}

report_to_death <- report_to_death_distr(
  mu = mu_delta, std = s_delta, trunc = 30
)

## -------------------------------------------------------------------
## Checked that this gives exactly the same results as Pierre's code
x <- matrix(abs(ascertainr_cases[[location]]), ncol = 1)
weighted_cases <- weighted_incid(
  incid = x, weights = report_to_death, trunc = SItrunc
)

deaths <- matrix(ascertainr_deaths[[location]], ncol = 1)

deaths_to_cases <- ratio_deaths_cases(
  wtd_incid = weighted_cases, deaths = deaths, nsamples = 10000
)

incid <- abs(ascertainr_cases[[location]])
avg_last_week <- sum(tail(incid, 7)) / 7
sd_last_week <- sd(tail(incid, 7))
param <- gamma_mucv2shapescale(
  mu = avg_last_week, cv = sd_last_week/ avg_last_week
)
i_augm <- matrix(
  rgamma(n = 7 * n_post, shape = param$shape, scale = param$scale),
  nrow = n_post,
  ncol = 7
)
i_old <- matrix(
  tail(incid, SItrunc), nrow = n_post, ncol = SItrunc, byrow = TRUE
)
cases_augmented <- cbind(i_old, i_augm)

######################################################################
######################################################################

weighted_cases_augm <- apply(
  cases_augmented, 1, function(x) {
    x <- matrix(x[2:length(x)], ncol = 1)
    weighted_incid(
      incid = x, weights = report_to_death, trunc = SItrunc
    )
  }
)

weighted_cases_augm <- t(weighted_cases_augm)
idx <- seq(to = ncol(weighted_cases_augm), length.out = 7)
weighted_cases_augm <- weighted_cases_augm[ , idx]

######################################################################
######################################################################
reporting <- matrix(
  sample(
    x = tail(deaths_to_cases, 1), size = 7 * n_post, replace = TRUE
  ),
  nrow = n_post,
  ncol = 7
)

predictions <- data.frame(matrix(NA, nrow = n_post, 7))
for (k in 1:7){
  predictions[, k] <- rbinom(
    n = n_post,
    size = round(weighted_cases_augm[, k]),
    prob = reporting[, k]
  )
}
colnames(predictions) <- seq(
  from = as.Date(week_ending) + 1, length.out = 7, by = "1 day"
)

pred_qntls <- tidyr::gather(predictions, dates, val) %>%
  dplyr::group_by(dates) %>%
  ggdist::median_qi(.width = c(0.75, 0.95))

pred_qntls$dates <- as.Date(pred_qntls$dates)

obs <- ascertainr_deaths[, c("dates", location)]
obs$deaths <- obs[[location]]
p <- rincewind::plot_projections(obs, pred_qntls)
p <- p +
  ggtitle(
    glue::glue("Projections for {location} for week starting {week_ending}")
  )
ggsave(glue::glue("projections_{location}.png"), p)

######################################################################
######################################################################
pred <- apply(predictions, 2, median, na.rm = TRUE)
obs <- c(abs(ascertainr_deaths[[location]]), pred)
si_distrs <- readRDS("si_distrs.rds")
r_estim <-map(
  si_distrs,
  function(si_distr) {
    res <- estimate_R(
      obs,
      method = 'non_parametric_si',
      config = make_config(
        list(
          mean_prior = 1,
          si_distr = si_distr,
          t_start = length(obs) - t.window + 1,
          t_end = length(obs)
        )
      )
    )
    res <- res$R
    param <- gamma_mucv2shapescale(
      mu = res$`Mean(R)`, cv = res$`Std(R)`/res$`Mean(R)`
    )
    stats::rgamma(n = n_post, shape = param$shape, scale = param$scale)
  }
)

## Repeat predictions to be consistent with outputs from other models
predictions <- list(si_1 = predictions, si_2 = predictions)
out <- list(R_last = r_estim, Predictions = predictions)

saveRDS(object = out, file = paste0('DeCa_latest.rds'))


