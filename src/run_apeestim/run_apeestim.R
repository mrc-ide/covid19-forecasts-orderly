## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-01-10", location = "Arizona"))

model_input <- readRDS("model_input.rds")
deaths_to_use <- model_input$D_active_transmission

## locations <- model_input$State
## Can modify this if we want to run for a smaller selection of states
## Possibly base this on another file

## Convert to incidence object
tall_deaths <- gather(
  deaths_to_use, key = province_state, value = deaths, -dates
) %>%
  split(.$province_state) %>%
  map(
    function(x) {
      message(x$province_state[1])
      ts_to_incid(ts = x, date_col = "dates", case_col = "deaths")
    }
  )

tall_deaths <- tall_deaths[[location]]

si_distrs <- readRDS("si_distrs.rds")

##apeestim
inftvty <- purrr::map(
  si_distrs,
  function(si_distr) {
    incid <- as.numeric(incidence::get_counts(tall_deaths))
    EpiEstim::overall_infectivity(
      incid, si_distr
    )
  }
)


## Run for selected location, for each SI Distribution,
r_prior <- c(1, 5)
a <- 0.025
trunctime <- first_nonzero_incidence(y)

r_apeestim <- purrr::map(
  si_distrs,
  function(si_distr) {
    incid <- as.numeric(incidence::get_counts(y))
    inftvty <- EpiEstim::overall_infectivity(
      incid, si_distr
    )
    apeEstim(
      incid,
      si_distr,
      inftvty,
      r_prior,
      a,
      trunctime,
      province_state
    )
  }
)


## Each element of r_apeestim is a list with the following
## components: "best_k_ape", "best_set_ape", "best_k_pmse",
## "best_set_pmse". To use the window determined according to the
## APE score, use best_set_ape, which is again a list with
## "ape", "pmse", "prob", "rhat", "rhatci", "post_mean_tplus1",
## "tplus1_ci", "alpha", "beta", "post_negbin_pr". We want the
## last values of alpha and beta.
n_sim <- 1000
y_rsamples_ape <- map(
  y_r_apeestim,
  function(r_si) {
    shape <- tail(
      r_si[["best_set_ape"]][["alpha"]], 1
    )
    scale <- tail(
      r_si[["best_set_ape"]][["beta"]], 1
    )
    rgamma(n_sim, shape = shape, scale = scale)
  }
)

date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_days <- 7
## Projections using projections package with
## Poisson offspring distribution
df <- subset(tall_deaths, to = as.Date(date_to_project_from))

ape_projections <- purrr::map2(
  rsamples_ape, si_distrs,
  function(rt_si, si) {
    out <- map(
      seq_len(sims_per_rt),
      function(i) {
        projections::project(
          x = df,
          R = rt_si,
          si = si,
          n_sim = n_sim,
          n_days = n_days,
          R_fix_within = TRUE,
          model = "poisson"
        ) %>%
          as.matrix() %>% t
      }
    )
    do.call(what = "rbind", args = out)
  }
)

pred_qntls <- purrr::map(
  ape_projections,
  function(pred) {
    pred <- pred[[2]]
    pred <- data.frame(pred, check.names = FALSE)
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)

purrr::iwalk(
  pred_qntls,
  function(pred, state) {
    obs <- deaths_to_use[, c("dates", state)]
    obs$deaths <- obs[[state]]
    p <- rincewind::plot_projections(obs, pred)
    p <- p +
      ggtitle(
        glue::glue("Projections for {state} for week starting {week_ending}")
      )
    ggsave(glue::glue("figures/projections_{state}.png"), p)
  }
)

## ## Save in format required
out <- saveRDS(
  object = list(
    I_active_transmission = model_input[["I_active_transmission"]],
    D_active_transmission = model_input[["D_active_transmission"]],
    State = model_input[["State"]],
    R_last = rsamples_ape,
    Predictions = ape_projections
  ),
  file = "apeestim_model_outputs.rds"
)

saveRDS(object = r_apeestim, file = "r_apeestim.rds")