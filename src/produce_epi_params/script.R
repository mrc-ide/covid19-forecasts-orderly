raw_data <- list(
  si_mean = c(4.80, 6.48),
  si_std = c(2.70, 3.83)
)

si_distrs <- map2(
  raw_data[["si_mean"]],
  raw_data[["si_std"]],
  function(mu, sigma) {
    reparams <- epitrix::gamma_mucv2shapescale(
      mu = mu, cv = sigma / mu
    )
    miss_at_most <-0.001
    cutoff <- ceiling(
      qgamma(
        1 - miss_at_most,
        shape = reparams$shape,
        scale = reparams$scale
      )
    )
    EpiEstim::discr_si(k = 0:cutoff, mu = mu, sigma = sigma)
  }
)

names(si_distrs) <- c("si_1", "si_2")

saveRDS(si_distrs, "si_distrs.rds")
#####################################################################
#####################################################################
############### CFR Parameters
#####################################################################
#####################################################################
### Age-disaggregated IFR
### https://www.medrxiv.org/content/10.1101/2020.08.12.20173690v2.full.pdf
### Table 3, page 19
### Other possible sources:
### https://doi.org/10.2807/1560-7917.ES.2020.25.31.2001383

infections <- data.frame(
  age_group = c("[15-44)", "[45-64)", "[65-74)", "75+"),
  mu = c(1536000, 895000, 181000, 166000),
  sigma = 1000 * c(
  (1635 - 1437) / (2 * 1.96),
  (953 - 837) / (2 * 1.96),
  (209 - 153) / (2 * 1.96),
  (201 - 131) / (2 * 1.96)
  )
)
c19_deaths <- data.frame(
  age_group = c("[15-44)", "[45-64)", "[65-74)", "75+"),
  deaths = c(524, 4657, 5663, 19330)
)

ifr_distr <- map(
  infections$age_group,
  function(age_group) {
    mu <- infections$mu[infections$age_group == age_group]
    sigma <- infections$sigma[infections$age_group == age_group]
    x <- rnorm(1e4, mean = mu, sd = sigma)
    while (any(x < 0)) x <- rnorm(1e4, mean = mu, sd = sigma)
    c19_deaths$deaths[c19_deaths$age_group == age_group] / x
  }
)
names(ifr_distr) <- infections$age_group

saveRDS(ifr_distr, "ifr_distr.rds")

pop_by_age <- read_excel(
  "PopulationAgeSex-20200831015935.xlsx", sheet = 2
)
colnames(pop_by_age) <- pop_by_age[1, ]
pop_by_age <- pop_by_age[-1, ]

## First re-organise the population in the desired age brakets
## Fix names so that names in ECDC match with those in population
## estimates excel sheet
pop_by_age$Location[pop_by_age$Location == "Bolivia (Plurinational State of)"] <- "Bolivia"
pop_by_age$Location[pop_by_age$Location == "Côte d'Ivoire"] <- "Cote_dIvoire"
pop_by_age$Location[pop_by_age$Location == "Iran (Islamic Republic of)"] <- "Iran"
pop_by_age$Location[pop_by_age$Location == "Lao People's Democratic Republic"] <- "Laos"
pop_by_age$Location[pop_by_age$Location == "Republic of Moldova"] <- "Moldova"
pop_by_age$Location[pop_by_age$Location == "State of Palestine"] <- "Palestine"
pop_by_age$Location[pop_by_age$Location == "Russian Federation"] <- "Russia"
pop_by_age$Location[pop_by_age$Location == "Republic of Korea"] <- "South Korea"
pop_by_age$Location[pop_by_age$Location == "Syrian Arab Republic"] <- "Syria"
pop_by_age$Location[pop_by_age$Location == "China, Taiwan Province of China"] <- "Taiwan"
pop_by_age$Location[pop_by_age$Location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
pop_by_age$Location[pop_by_age$Location == "Viet Nam"] <- "Vietnam"
## Kosovo is not present in the population estimates
## We use the age distribution of its nearest neighbor
df <- pop_by_age[pop_by_age$Location == "Serbia", ]
df$Location <- "Kosovo"
pop_by_age <- rbind(pop_by_age, df)


locations <- setNames(
  unique(pop_by_age$Location), unique(pop_by_age$Location)
)


pop_pyramid <- map(
  locations,
  function(location) {
    x <- pop_by_age[pop_by_age$Location == location, ]
    x <- mutate_at(x, vars(`0-4`:`100+`), as.numeric)
    out <- data.frame(
      location = location,
      "[0-15)" = rowSums(select(x, `0-4`:`10-14`)),
      "[15-44)" = rowSums(select(x, `15-19`:`40-44`)),
      "[45-64)" = rowSums(select(x, `45-49`:`60-64`)),
      "[65-74)" = rowSums(select(x, `65-69`:`70-74`)),
      "75+" = rowSums(select(x, `75-79`:`100+`)),
      ##total = rowSums(select(x, `0-4`:`100+`)),
      check.names = FALSE
    )
    out <- gather(out, age_group, pop, -location)
    out$prop <- out$pop / sum(out$pop)
    out
  }
)

saveRDS(pop_pyramid, "pop_pyramid.rds")

pop_wtd_ifr <- map(
  pop_pyramid,
  function(pop) {
    out <- slider::slide(
      pop, ~ ifr_distr[[as.character(.x$age_group)]] * .x$prop
    )
    out[[5]] + out[[2]] + out[[3]] + out[[4]]
  }
)

pop_wtd_ifr_qntls <- map_dfr(
  pop_wtd_ifr,
  ~ quantile(., na.rm = TRUE, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)),
  .id = "location"
)

saveRDS(pop_wtd_ifr, "population_weighted_ifr.rds")

continent <- readr::read_csv("country_continent.csv") %>%
  janitor::clean_names()
continent$countries_and_territories <- tolower(continent$countries_and_territories)

pop_wtd_ifr_qntls$location <- snakecase::to_snake_case(pop_wtd_ifr_qntls$location)
pop_wtd_ifr_qntls <- left_join(
  pop_wtd_ifr_qntls, continent, by = c("location" = "countries_and_territories")
)
pop_wtd_ifr_qntls <- na.omit(pop_wtd_ifr_qntls)

saveRDS(pop_wtd_ifr_qntls, "pop_wtd_ifr_qntls.rds")
