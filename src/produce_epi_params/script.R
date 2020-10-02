week_ending <- "2020-08-16"
indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
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
  (1635 - 1437) / 2,
  (953 - 837) / 2,
  (209 - 153) / 2,
  (201 - 131) / 2
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
df$Location <- "kosovo"
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
      "[15-44)" = rowSums(select(x, `5-9`:`40-44`)),
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
  pop_wtd_ifr, ~ quantile(., na.rm = TRUE), .id = "location"
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

pop_wtd_ifr_qntls$color <- case_when(
  pop_wtd_ifr_qntls$continent == "Africa" ~ "#000000",
  pop_wtd_ifr_qntls$continent == "Asia" ~ "#E69F00",
  pop_wtd_ifr_qntls$continent == "Europe" ~ "#56B4E9",
  pop_wtd_ifr_qntls$continent == "North America" ~ "#009E73",
  pop_wtd_ifr_qntls$continent == "South America" ~ "#0072B2",
  pop_wtd_ifr_qntls$continent == "Oceania" ~ "#D55E00"
)

pop_wtd_ifr_qntls$location <- snakecase::to_title_case(pop_wtd_ifr_qntls$location)
pop_wtd_ifr_qntls$label <- glue(
  "<i style='color:{pop_wtd_ifr_qntls$color}'>{pop_wtd_ifr_qntls$location}</i>"
)
pop_wtd_ifr_qntls$label <- forcats::fct_reorder(
  pop_wtd_ifr_qntls$label, pop_wtd_ifr_qntls$continent, min
)

pop_wtd_ifr_qntls1 <- filter(
  pop_wtd_ifr_qntls, continent %in% c("Africa", "Asia")
)
pop_wtd_ifr_qntls1 <- droplevels(pop_wtd_ifr_qntls1)

pop_wtd_ifr_qntls2 <- filter(
  pop_wtd_ifr_qntls, !continent %in% c("Africa", "Asia")
)
pop_wtd_ifr_qntls2 <- droplevels(pop_wtd_ifr_qntls2)

p1 <- ggplot(pop_wtd_ifr_qntls1) +
  geom_point(
    aes(label, `50%`, col = color)
  ) +
  geom_linerange(
    aes(x = label, ymin = `25%`, ymax = `75%`, col = color)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(
      angle = -90, hjust = 0, vjust = 0, size = 6
    ),
    axis.title.y = element_text(size = 6),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  xlab("") +
  ylab("Infection fatality ratio") +
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))



p2 <- ggplot(pop_wtd_ifr_qntls2) +
  geom_point(
    aes(label, `50%`, col = color)
  ) +
  geom_linerange(
    aes(x = label, ymin = `25%`, ymax = `75%`, col = color)
  ) +
  theme_minimal() +
    theme(
    axis.text.x = element_markdown(
      angle = -90, hjust = 0, vjust = 0, size = 6
    ),
    axis.title.y = element_text(size =6),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  xlab("") +
  ylab("Infection fatality ratio") +
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))



p <- p1 + p2 + plot_layout(ncol = 1) #&


ggsave(
  filename = "ifr_per_country.png",
  plot = p,
  device = agg_png,
  width = 25,
  height = 12,
  units = "cm",
  res = 300,
  scaling = 0.9
)

#####################################################################
#####################################################################
############### CFR Parameters
############### No population weighting
#####################################################################
#####################################################################

CFR_esti <- c(1.38, 1.23, 1.53)/100
# function to get parameters
f1 <- function(shape){
  res <- c(
    shape[1]/(shape[1]+ shape[2]),
    qbeta(.025, shape1 = shape[1], shape2 = shape[2]),
    qbeta(.975, shape1 = shape[1], shape2 = shape[2])
  )
  res <- sum((res*100-CFR_esti*100)^2)
  return(res)
}

n <- 5e2
Shape1 <- rep(seq(300,350,length.out = n), each = n)
Shape2 <- rep(seq(22500,23500,length.out = n), n)
res <- rep(NA, n * n)
for (i in 1:(n * n)){
  res[i] <- f1(c(Shape1[i],Shape2[i]))
}
f <- which(res == min(res))
shape <- c(Shape1[f], Shape2[f])

saveRDS(shape, "cfr_shape_params.rds")
