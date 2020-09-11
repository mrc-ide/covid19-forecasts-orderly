## orderly::orderly_develop_start(parameters = list(week_ending = "2020-04-05", use_si = "si_2"))
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")
prob <- c(0.025, 0.25, 0.50, 0.75, 0.975)
population <- readr::read_csv("ecdc_pop2018.csv")
unwtd_rt_estimates <- readRDS("combined_rt_estimates.rds")
wtd_rt_estimates_across_countries <- readRDS(
  "combined_weighted_estimates_across_countries.rds"
)
wtd_rt_estimates_per_country <- readRDS(
  "combined_weighted_estimates_per_country.rds"
)

#######
names(unwtd_rt_estimates)[names(unwtd_rt_estimates) == "Czech_Republic"] <- "Czechia"
names(wtd_rt_estimates_across_countries)[names(wtd_rt_estimates_across_countries) == "Czech_Republic"] <- "Czechia"
names(wtd_rt_estimates_per_country)[names(wtd_rt_estimates_per_country) == "Czech_Republic"] <- "Czechia"

######
date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_sim <- 1000

##shape <- readRDS("cfr_shape_params.rds")
##cfr_samples <- rbeta(n_sim, shape1 = shape[1],shape2 = shape[2])
ifr_samples <- readRDS("population_weighted_ifr.rds")
names(ifr_samples) <- snakecase::to_snake_case((names(ifr_samples)))

indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

si_distrs <- readRDS("si_distrs.rds")
si <- si_distrs[[use_si]]

deaths_to_use <- raw_data[["D_active_transmission"]]
colnames(deaths_to_use)[colnames(deaths_to_use) == "Czech_Republic"] <- "Czechia"



## rt_estimates <- purrr::keep(
##   rt_estimates, ~ length(.$weeks_combined) > 1
## )

countries <- setNames(
  names(unwtd_rt_estimates), names(unwtd_rt_estimates)
)

all_restimates <- list(
  unweighted = unwtd_rt_estimates,
  weighted_across_countries = wtd_rt_estimates_across_countries,
  weighted_per_country = wtd_rt_estimates_per_country
)


all_reff <- map(
  all_restimates,
  function(restimate) {
    imap(
      restimate,
      function(rt, country) {
        message(country)
        deaths_so_far <- sum(
          deaths_to_use[deaths_to_use$dates <= week_ending, country]
        )
        pop <- population$pop_data2018[population$countries_and_territories == country]
        deaths_per_capita <- deaths_so_far / pop
        ifr <- ifr_samples[[tolower(country)]]
        p <- proportion_susceptible(deaths_per_capita, ifr)
        list(
          p_susceptible = p,
          r_eff = rt$rt_samples / p
        )
      }
    )
  }
)


all_projections <- map(
  all_reff,
  function(reff) {
    map(
      countries,
      function(country) {
        message(country)
        pop <- population$pop_data2018[population$countries_and_territories == country]
        n_days <- length(
          unwtd_rt_estimates[[country]]$weeks_combined
        ) * 7
        x <- deaths_to_use[[country]]
        ##rt <- unwtd_rt_estimates[[country]]$rt_samples
        rt <- reff[[country]][["r_eff"]]
        p <- reff[[country]][["p_susceptible"]]
        ifr <- ifr_samples[[tolower(country)]]
        project_with_saturation(
          deaths = x,
          r_eff = rt,
          p_susceptible = p,
          si = si,
          n_days = n_days,
          cfr = ifr,
          pop,
          n_sim = n_sim,
          sims_per_rt = sims_per_rt
        )
      }
    )
  }
)

projections <- map_depth(all_projections, 2, ~ .[["pred"]])

r_effective <- map_depth(all_projections, 2, ~ .[["r_effective"]])

p_s <- map_depth(all_projections, 2, ~ .[["p_s"]])


pred_qntls <- map_depth(
  projections,
  2,
  function(mat) {
    dates_pred <- seq(
      from = as.Date(week_ending) + 1,
      length.out = ncol(mat), by = "1 day"
    )
    y <- apply(
      mat, 2, quantile, prob = prob, na.rm = FALSE
    )
    y <- data.frame(y)
    colnames(y) <- dates_pred
    y <- tibble::rownames_to_column(y, var = "qntl")
    y <- tidyr::gather(y, date, val, -qntl)
    tidyr::spread(y, qntl, val)
  }
)

reff_qntls <- map_depth(
  r_effective,
  2,
  function(y) {
    dates_pred <- seq(
      from = as.Date(week_ending) + 1,
      length.out = length(y), by = "1 day"
    )
    names(y) <- dates_pred
    out <- map_dfr(y, ~ quantile(., prob = prob), .id = "date")
  }
)

ps_qntls <- map_depth(
  p_s,
  2,
  function(y) {
    dates_pred <- seq(
      from = as.Date(week_ending) + 1,
      length.out = length(y), by = "1 day"
    )
    names(y) <- dates_pred
    map_dfr(y, ~ quantile(., prob = prob, na.rm = FALSE), .id = "date")
  }
)


walk(
  1:3,
  function(idx) {
    walk(
      countries,
      function(country) {
        pred <- pred_qntls[[idx]][[country]]
        ps <- ps_qntls[[idx]][[country]]
        reff <- reff_qntls[[idx]][[country]]
        obs <- deaths_to_use[, c("dates", country)]
        obs$deaths <- obs[[country]]

        pred$date <- as.Date(pred$date)
        ps$date <- as.Date(ps$date)
        reff$date <- as.Date(reff$date)
        obs$dates <- as.Date(obs$dates)
        p <- combined_plot(obs, pred, ps, reff)
        x <- snakecase::to_title_case(names(pred_qntls)[idx])
        p <- p +
          plot_annotation(
            title = glue::glue("{x} projections for {country}")
          )
        x <- snakecase::to_snake_case(x)
        outfile <- glue::glue("figures/{x}_{country}_{week_ending}.png")
        ggsave(outfile, p)
      }
    )
  }
)

iwalk(
  pred_qntls,
  function(x, y) saveRDS(x, glue::glue("{y}_pred_qntls.rds"))
)


iwalk(
  ps_qntls,
  function(x, y) saveRDS(x, glue::glue("{y}_ps_qntls.rds"))
)

iwalk(
  reff_qntls,
  function(x, y) saveRDS(x, glue::glue("{y}_reff_qntls.rds"))
)
