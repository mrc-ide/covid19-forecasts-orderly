## orderly::orderly_develop_start(parameters = list(week_ending = "2022-03-20"), use_draft = "newer")

deaths <- readRDS("latest_deaths.rds")
cases <- readRDS("latest_cases.rds")

ecdc <- read_csv("ECDC-COVID-19-global-data.csv")
ecdc <- distinct(
  select(ecdc, `Countries and territories`, popData2018)
)

cases_tall <- gather(cases, country, cases, -dates)
cases_tall <- left_join(
  cases_tall, ecdc, by = c("country" = "Countries and territories")
)
cases_tall$per_capita_cases <- cases_tall$cases / cases_tall$popData2018
cases_tall$per_1e6_cases <- round(cases_tall$per_capita_cases * 100000)


deaths_tall <- gather(deaths, country, deaths, -dates)
deaths_tall <- left_join(
  deaths_tall, ecdc, by = c("country" = "Countries and territories")
)
deaths_tall$per_capita_deaths <- deaths_tall$deaths / deaths_tall$popData2018
deaths_tall$per_1e6_deaths <- round(deaths_tall$per_capita_deaths * 100000)

cases_per_1e6 <- select(cases_tall, dates, country, per_1e6_cases) %>%
  spread(country, per_1e6_cases)

deaths_per_1e6 <- select(deaths_tall, dates, country, per_1e6_deaths) %>%
  spread(country, per_1e6_deaths)
## Round everything
cases <- mutate_if(cases, is.numeric, as.integer)
deaths <- mutate_if(deaths, is.numeric, as.integer)
cases_per_1e6 <- mutate_if(cases_per_1e6, is.numeric, as.integer)
deaths_per_1e6 <- mutate_if(deaths_per_1e6, is.numeric, as.integer)

write_csv(cases, "cases_all.csv")
write_csv(deaths, "deaths_all.csv")

write_csv(cases_per_1e6, "cases_per_1e6.csv")
write_csv(deaths_per_1e6, "deaths_per_1e6.csv")


## outputs
exclude <- readRDS("exclude.rds")
model_outputs <- readRDS("ensemble_model_predictions.rds")
model_outputs <- mutate_if(model_outputs, is.numeric, as.integer)
model_outputs <- model_outputs[!model_outputs$country %in% exclude, ]
write_csv(model_outputs, "latest_model_outputs.csv")

## weekly
weekly <- readRDS("ensemble_weekly_qntls.rds")
weekly <- mutate_if(weekly, is.numeric, as.integer)
weekly <- weekly[!weekly$country %in% exclude, ]
write_csv(weekly, "weekly_projected_deaths.csv")
file.create("deploy.txt")

x <- read_csv("country_epidemic_phase2.csv")
x <- mutate_if(x, is.numeric, ~ round(., 2))
x <- x[!x$country %in% exclude, ]
write_csv(x, "country_epidemic_phase.csv")
