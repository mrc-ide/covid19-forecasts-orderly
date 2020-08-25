## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"))
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")

population <- readr::read_csv("ecdc_pop2018.csv")
unwtd_rt_estimates <- readRDS("combined_rt_estimates.rds")
wtd_rt_estimates_across_countries <- readRDS(
  "combined_weighted_estimates_across_countries.rds"
)
wtd_rt_estimates_per_country <- readRDS(
  "combined_weighted_estimates_per_country.rds"
)

date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_sim <- 1000

shape <- readRDS("cfr_shape_params.rds")
cfr_samples <- rbeta(n_sim, shape1 = shape[1],shape2 = shape[2])

indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

si_distrs <- readRDS("si_distrs.rds")
si <- si_distrs[[use_si]]

deaths_to_use <- raw_data[["D_active_transmission"]]


tall_deaths <- tidyr::gather(
  deaths_to_use, key = country, value = deaths, -dates
) %>%
  split(.$country) %>%
  purrr::map(
  ~ rincewind:::ts_to_incid(ts = ., date_col = "dates", case_col = "deaths")
)



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
        deaths_so_far <- sum(
          deaths_to_use[deaths_to_use$dates <= week_ending, country]
        )
        pop <- population$pop_data2018[population$countries_and_territories == country]
        deaths_per_capita <- deaths_so_far / pop
        p <- proportion_susceptible(deaths_per_capita, cfr_samples)
        list(
          p_susceptible = p,
          r_eff = rt$rt_samples * p
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
        f <-  function() {
          project_with_saturation(
            deaths = x,
            r_eff = rt,
            p_susceptible = p,
            si = si,
            n_sim = n_sim,
            n_days = n_days,
            cfr = cfr_samples,
            pop
          )
         }
        out <- rerun(sims_per_rt, f())
      }
    )
  }
)

projections <- map(
  all_projections,
  function(pred) {
    map(
      pred,
      function(country_pred) {
        out <- map(country_pred, ~ .[["pred"]])
      }
    )
  }
)

saveRDS(unwtd_projections, "unwtd_projections.rds")
saveRDS(
  wtd_per_country_projections, "wtd_per_country_projections.rds"
)

saveRDS(
  wtd_across_all_projections, "wtd_across_all_projections.rds"
)
