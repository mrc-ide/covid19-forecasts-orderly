## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-01-01", location = "Arizona"))
set.seed(1)
dir.create("figures")
## -----------------------------------------------------------------------------

week_ending <-  as.Date(week_ending)
# delay_report_death <- 10 # need checking!!
day.project <- 7
t.window.range <- 7
rep <- 2e4
n_post <- 1e4
mu_delta <- 10
s_delta <- 2
SItrunc <- 30
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

d_exp <- data.frame(matrix(NA, nrow = n_post, 7))

predictions <-
    for (k in 1:7){
      d_exp[, k] <- rbinom(
        n = n_post,
        size = round(weighted_cases_augm[, k]),
        prob = reporting[, k]
      )
    }
    colnames(d_exp) <- seq(
      from = as.Date(week_ending) + 1, length.out = 7, by = "1 day"
    )
    list(si_1 = d_exp, si_2 = d_exp)
  }
)
######################################################################
######################################################################
################# Visualisation #####################################
######################################################################
######################################################################
pred_qntls <- purrr::map(
  predictions,
  function(pred) {
    pred <- pred[[2]]
    pred <- tidyr::gather(pred, dates, val)
    qntls <- dplyr::group_by(pred, dates) %>%
      ggdist::median_qi(.width = c(0.75, 0.95))
    qntls$dates <- as.Date(qntls$dates)
    qntls
  }
)

### Test against Pierre's outputs for previous weeks
## pierre <- readRDS(
##   glue::glue("{covid_19_path}DeCa_Std_results_week_end_{week_ending}.rds")
## )
## pierre_qntls <- purrr::map(
##   pierre[["Predictions"]],
##   function(pred) {
##     pred <- pred[[2]]
##     pred <- tidyr::gather(pred, dates, val)
##     qntls <- dplyr::group_by(pred, dates) %>%
##       ggdist::median_qi(.width = c(0.75, 0.95))
##     qntls$dates <- as.Date(qntls$dates)
##     qntls
##   }
## )

## purrr::walk(
##   countries,
##   function(country) {
##     x <- pred_qntls[[country]]
##     y <- pierre_qntls[[country]]
##     z <- dplyr::bind_rows(list(packaged = x, old = y), .id = "category")
##     z <- z[z$`.width` == 0.75, ]
##     p <- ggplot(z) +
##       geom_ribbon(
##         aes(x = dates, ymin = .lower, ymax = .upper, fill = category),
##         alpha = 0.3
##       ) +
##       geom_line(aes(dates, val, col = category)) +
##       theme_minimal()

##     ggsave(glue::glue("figures/{country}_compare.png"), p)
##   }
## )



purrr::iwalk(
  pred_qntls,
  function(pred, cntry) {
    obs <- ascertainr_deaths[, c("dates", cntry)]
    obs$deaths <- obs[[cntry]]
    p <- rincewind::plot_projections(obs, pred)
    p <- p +
      ggtitle(
        glue::glue("Projections for {cntry} for week starting {week_ending}")
      )

    ggsave(glue::glue("figures/projections_{cntry}.png"), p)
  }
)

######################################################################
######################################################################


t.window <- 10

r_estim <- purrr::map(
  countries,
  function(country){
    message(country)
    pred <- apply(
      predictions[[country]][[1]], 2, median, na.rm = TRUE
    )
    obs <- c(abs(ascertainr_deaths[[country]]), pred)
    purrr::map2(
      model_input$si_mean,
      model_input$si_std,
      function(s_mean, s_sd) {
        si_distr <- gamma_dist_EpiEstim(
          si_mu = s_mean, si_std = s_sd, SItrunc = 30
        )
        res <- EpiEstim::estimate_R(
          obs,
          method = 'non_parametric_si',
          config = make_config(
            list(
              mean_prior = 1,
              si_distr = si_distr$dist,
              t_start = length(obs) - t.window + 1,
              t_end = length(obs))
          )
        )
        res <- res$R
        param <- epitrix::gamma_mucv2shapescale(
          mu = res$`Mean(R)`, cv = res$`Std(R)`/res$`Mean(R)`
        )
        stats::rgamma(n = n_post, shape = param$shape, scale = param$scale)
      }
    )
  }
)

out <- list(
  I_active_transmission = model_input$I_active_transmission,
  D_active_transmission = model_input$D_active_transmission,
  Country = model_input$Country,
  R_last = r_estim,
  Predictions = predictions
)

saveRDS(object = out, file = paste0('DeCa_latest.rds'))


#####################################################################
############################# Summary ###############################
#####################################################################
## country,deaths_to_reported_ratio,estimated_reporting,factor_to_real_size,Observed_case_last_week,Predicted_True_case_last_week
## Yemen,0.32 (0.248 - 0.396),4.3% (3.4% - 5.7%),23.23 (17.66 - 29.64),232,5230 (4290 - 6400)
## United_States_of_America,0.038 (0.037 - 0.038),36.7% (32.7% - 40.8%),2.73 (2.45 - 3.05),154465,434000 (390000 - 486000)
## United_Kingdom,0.091 (0.086 - 0.096),15.1% (13.3% - 17.0%),6.64 (5.89 - 7.51),9507,66600 (59700 - 74700)
## United_Arab_Emirates,0.003 (0.002 - 0.005),100.0% (100.0% - 100.0%),1 (1 - 1),3632,3630 (3630 - 3630)
summary_14days <- purrr::map_dfr(
  countries,
  function(country) {
    summary_r <- deaths_to_cases_qntls[[country]]
    summary_rho <- ascertainment_qntls[[country]]
    last_week <- quantile(
      colSums(episize_projected[[country]], na.rm = TRUE),
      c(0.5, 0.025, 0.975),
      na.rm = TRUE
    )
    last_week <- data.frame(last_week) %>%
      tibble::rownames_to_column() %>%
      tidyr::spread(rowname, last_week)

    data.frame(
      country = country,
      deaths_to_reported_ratio = glue::glue(
        "{round(summary_r$median_ratio[nrow(ascertainr_deaths)], 3) }",
        " ({round(summary_r$low_ratio[nrow(ascertainr_deaths)], 3)} - {round(summary_r$up_ratio[nrow(ascertainr_deaths)],3)})",
      ),
      estimated_reporting = glue::glue(
        "{scales::percent(as.numeric(round(summary_rho$`50.0%`[nrow(ascertainr_deaths)], 3)), accuracy = 0.1)}",
        " ({scales::percent(as.numeric(round(summary_rho$`2.5%`[nrow(ascertainr_deaths)],3)), accuracy = 0.1)} - {scales::percent(as.numeric(round(summary_rho$`97.5%`[nrow(ascertainr_deaths)],3)), accuracy = 0.1)})",
        ),
      factor_to_real_size = glue::glue(
        "{round(1/summary_rho$`50.0%`[nrow(ascertainr_deaths)], 2)}",
        " ({round(1/summary_rho$`97.5%`[nrow(ascertainr_deaths)], 2)} - {round(1/summary_rho$`2.5%`[nrow(ascertainr_deaths)], 2)})",
        ),
      Observed_case_last_week = sum(tail(ascertainr_cases[[country]], 7)),
      Predicted_True_case_last_week = glue::glue(
          "{prettyNum(signif(last_week$`50%`, digits = 3), big.mark = ",")} ",
          "({prettyNum(signif(last_week$`2.5%`, digits = 3), big.mark = ",")}",
          " - {prettyNum(signif(last_week$`97.5%`, digits = 3), big.mark = ",")})"
      )
    )
  }
)

## TODO check why we have NAs
summary_14days <- na.omit(summary_14days)
readr::write_csv(
  x = summary_14days,
  path = 'summary_DeathToRepoted_14days.csv'
)
