## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"), use_draft = "newer")
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")
prob <- c(0.025, 0.25, 0.50, 0.75, 0.975)
population <- readr::read_csv("ecdc_pop2018.csv")
wtd_rt_estimates_per_country <- readRDS(
  "combined_weighted_estimates_per_country.rds"
)

#######
names(wtd_rt_estimates_per_country)[names(wtd_rt_estimates_per_country) == "Czech_Republic"] <- "Czechia"

######
date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_sim <- 1000

ifr_samples <- readRDS("population_weighted_ifr.rds")
names(ifr_samples) <- snakecase::to_snake_case((names(ifr_samples)))

latest_deaths_wide <- readRDS("latest_deaths_wide_no_filter.rds")
deaths_to_use <- latest_deaths_wide[latest_deaths_wide$dates <= week_ending, ]
colnames(deaths_to_use)[colnames(deaths_to_use) == "Czech_Republic"] <- "Czechia"

si_distrs <- readRDS("si_distrs.rds")
si <- si_distrs[[use_si]]

exclude <- readRDS("exclude.rds")
countries <- setNames(
  names(wtd_rt_estimates_per_country), names(wtd_rt_estimates_per_country)
)
countries <- countries[! countries %in% exclude]
message("################ Including countries ######################")
message(paste(countries, collapse = "\n"))

all_restimates <- list(
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
        list(p_susceptible = p, r_eff = rt$rt_samples / p)
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
        ## n_days <- length(
        ##   unwtd_rt_estimates[[country]]$weeks_combined
        ## ) * 7
        n_days <- 42
        x <- deaths_to_use[[country]]
        ##rt <- unwtd_rt_estimates[[country]]$rt_samples
        rt <- reff[[country]][["r_eff"]]
        p <- reff[[country]][["p_susceptible"]]
        ifr <- ifr_samples[[tolower(country)]]
        project_without_saturation(
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


iwalk(
  pred_qntls,
  function(x, y) saveRDS(x, glue::glue("{y}_pred_qntls.rds"))
)

iwalk(
  projections,
  function(x, y) saveRDS(x, glue::glue("{y}_projections.rds"))
)
