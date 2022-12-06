## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-01-10", location = "Arizona"))

if(reconstructed == TRUE){
  model_input <- readRDS("model_input_reconstructed.rds")
} else {
  model_input <- readRDS("model_input.rds")
}


deaths_to_use <- model_input$D_active_transmission

## locations <- model_input$State
## Can modify this if we want to run for a smaller selection of states
## Possibly base this on another file

## Convert to incidence object
tall_deaths <- gather(
  deaths_to_use[,c("dates", location)], key = province_state, value = deaths, -dates
) %>%
  ts_to_incid(date_col = "dates", case_col = "deaths")

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
trunctime <- first_nonzero_incidence(tall_deaths)

r_apeestim <- purrr::map(
  si_distrs,
  function(si_distr) {
    incid <- as.numeric(incidence::get_counts(tall_deaths))
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
n_sim <- 1e4
rsamples_ape <- map(
  r_apeestim,
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
          si = si[-1],
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

## Sample 10,000 values for consistent output with other models
ape_projections <- purrr::map(ape_projections,
    function(projections) {
      apply(projections, 2, function(y) sample(y, size = 1e4))
    }
)

pred_qntls <- data.frame(ape_projections[[2]], check.names = FALSE) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "dates",
                      values_to = "val") %>%
  dplyr::group_by(dates) %>%
  ggdist::median_qi(.width = c(0.75, 0.95))

pred_qntls$dates <- as.Date(pred_qntls$dates)

obs <- deaths_to_use[, c("dates", location)]
obs$deaths <- obs[[location]]
p <- rincewind::plot_projections(obs, pred_qntls)
p <- p +
  ggtitle(
    glue::glue("Projections for {location} for week starting {week_ending}")
  )
ggsave(glue::glue("projections_{location}.png"), p)

## ## Save in format required
out <- saveRDS(
  object = list(
    I_active_transmission = model_input[["I_active_transmission"]][, c("dates", location)],
    D_active_transmission = model_input[["D_active_transmission"]][, c("dates", location)],
    State = location,
    R_last = rsamples_ape,
    Predictions = purrr::map(ape_projections, as.data.frame)
  ),
  file = "apeestim_model_outputs.rds"
)

saveRDS(object = r_apeestim, file = "r_apeestim.rds")
