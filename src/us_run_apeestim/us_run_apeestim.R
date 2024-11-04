# set up for orderly2--------------------------------------

orderly_parameters(week_ending = NULL,
                   location = NULL,
                   reconstructed = NULL)

packages <- c("purrr", "stringr", "projections", "tidyr", "dplyr", "incidence",
              "EpiEstim", "ggplot2", "rincewind", "ggdist")
lapply(packages, require, character.only = TRUE)

orderly_artefact(
  "Model outputs",
  c("apeestim_model_outputs.rds",
    "r_apeestim.rds")
)

orderly_dependency(
  "prepare_jhu_data",
  "latest",
  files = c("model_input.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "reconstruct_daily_incidence",
  "latest",
  files = c("model_input_reconstructed.rds" = "latest_model_input.rds")
)
orderly_dependency(
  "produce_epi_params",
  "latest",
  files = c("si_distrs.rds")
)

source("R/apeEstim.R")
source("R/apePredPost.R")
source("R/apeSpecific.R")
source("R/util.R")


###--------------------------------------------------------

if(reconstructed){
  model_input <- readRDS("model_input_reconstructed.rds")
} else {
  model_input <- readRDS("model_input.rds")
}

deaths_to_use <- model_input$D_active_transmission %>% filter(dates <= week_ending)

# Only use si_1
si_distr <- readRDS("si_distrs.rds")$si_1

if(location == "all") {
  location <- model_input$State
} else {
  location <- c(
    "California", "Colorado", "Delaware", "Hawaii", "Maryland", "Missouri",
    "New Jersey", "New York", "North Dakota", "Ohio", "Pennsylvania",
    "Tennessee", "Texas"
  )
}

## Sliding window of four-week ahead projections
projection_week <- seq(from = as.Date(week_ending) - (7 * 4),
                       to = min(deaths_to_use$dates), by = -7)

## Convert to incidence object
tall_deaths <- gather(
  deaths_to_use[,c("dates", location)], key = province_state, value = deaths, -dates
) %>%
  split(.$province_state) %>%
  map(
    function(x) {
      message(x$province_state[1])
      ts_to_incid(ts = x, date_col = "dates", case_col = "deaths")
    }
  )

##apeestim

# Function to find first index where at least 5 cases have been reported
first_five_incidence <- function(x) {
  counts <- as.numeric(get_counts(x))
  cum_counts <- cumsum(counts)
  which(cum_counts >= 5)[1]
}

first_five_date <- function(x) {
  counts <- as.numeric(get_counts(x))
  cum_counts <- cumsum(counts)
  dates <- get_dates(x)
  dates[which(cum_counts >= 5)[1]]
}

## Estimate R for each state
r_apeestim <- purrr::imap(
  tall_deaths,
  function(deaths, location) {
    r_prior <- c(1, 5)
    a <- 0.025
    trunctime <- first_five_incidence(deaths)
    truncdate <- first_five_date(deaths)
    message("Truncating for ", location, " at ", trunctime, " (", truncdate, ")")
    incid <- as.numeric(incidence::get_counts(deaths))
    inftvty <- EpiEstim::overall_infectivity(incid, si_distr)
    out <- apeEstim(
      incid,
      si_distr,
      inftvty,
      r_prior,
      a,
      trunctime,
      location
      )
    out
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
  function(location) {
    shape <- tail(location[["best_set_ape"]][["alpha"]], 1)
    scale <- tail(location[["best_set_ape"]][["beta"]], 1)
    rgamma(n_sim, shape = shape, scale = scale)
    }
  )

# Sliding window of four-week ahead forecasts
date_to_project_from <- projection_week
sims_per_rt <- 10
n_days <- 7 * 4

## Projections using projections package with Poisson offspring distribution
ape_projections <- purrr::map(date_to_project_from, function(proj_date) {
  purrr::map2(
    tall_deaths,
    rsamples_ape,
    function(df, rt) {
      message("Projecting from: ", proj_date)
      df <- subset(df, to = as.Date(proj_date))
      out <- map(
        seq_len(sims_per_rt),
        function(i) {
          projections::project(
            x = df,
            R = rt,
            si = si_distr[-1],
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
  }) %>% set_names(as.character(date_to_project_from))


saveRDS(ape_projections, file = "ape_projections.rds")

## Sample 10,000 values for consistent output with other models
ape_projections <- purrr::map(ape_projections, function(state_projections) {
  purrr::map(state_projections, function(projections) {
    apply(projections, 2, function(y) sample(y, size = 1e4))
    })
  })

## 75% and 95% Quantiles
pred_qntls <- purrr::map(
  ape_projections,
  function(proj_date) {
  # Iterate over each state within the current projection date
  purrr::map(proj_date, function(pred) {
    pred <- data.frame(pred, check.names = FALSE)
    pred <- tidyr::gather(pred, dates, val)
    
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    
    qntls$dates <- as.Date(qntls$dates)
    qntls
  })
})

## Plots
purrr::iwalk(
  pred_qntls,
  function(week_pred, week) {
    purrr::iwalk(
      week_pred,
      function(pred, location) {
        obs <- deaths_to_use[, c("dates", location)]
        obs$deaths <- obs[[location]]
        p <- rincewind::plot_projections(obs, pred)
        p <- p +
          ggtitle(
            glue::glue("Projections for {location} for week ending {week}")
          )
        ggsave(glue::glue("figures/projections_{location}_{week}.png"), p)
      }
    )
  }
)


## Save in format required
out <- saveRDS(
  object = list(
    I_active_transmission = model_input[["I_active_transmission"]],
    D_active_transmission = model_input[["D_active_transmission"]],
    State = location,
    R_last = rsamples_ape,
    Predictions = ape_projections
  ),
  file = "apeestim_model_outputs.rds"
)

saveRDS(object = r_apeestim, file = "r_apeestim.rds")

