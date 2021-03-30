## orderly::orderly_develop_start(parameters = list(week_ending = "2021-03-28"), use_draft = "newer")

deaths <- readRDS("latest_deaths.rds")
cases <- readRDS("latest_cases.rds")

ecdc <- read_csv("ECDC-COVID-19-global-data.csv")
ecdc <- distinct(
  select(ecdc, `Countries and territories`, popData2018)
)

cases_tall <- tidyr::gather(cases, country, cases, -dates)
cases_tall <- left_join(
  cases_tall, ecdc, by = c("country" = "Countries and territories")
)
cases_tall$per_capita_cases <- cases_tall$cases / cases_tall$popData2018
cases_tall$per_1e6_cases <- round(cases_tall$per_capita_cases * 100000)


deaths_tall <- tidyr::gather(deaths, country, deaths, -dates)
deaths_tall <- left_join(
  deaths_tall, ecdc, by = c("country" = "Countries and territories")
)
deaths_tall$per_capita_deaths <- deaths_tall$deaths / deaths_tall$popData2018
deaths_tall$per_1e6_deaths <- round(deaths_tall$per_capita_deaths * 100000)

cases_per_1e6 <- select(cases_tall, dates, country, per_1e6_cases) %>%
  tidyr::spread(country, per_1e6_cases)

deaths_per_1e6 <- select(deaths_tall, dates, country, per_1e6_deaths) %>%
  tidyr::spread(country, per_1e6_deaths)

write_csv(cases, "cases_all.csv")
write_csv(deaths, "deaths_all.csv")

write_csv(cases_per_1e6, "cases_per_1e6.csv")
write_csv(deaths_per_1e6, "deaths_per_1e6.csv")


## outputs
model_outputs <- readRDS("ensemble_model_predictions.rds")
write_csv(model_outputs, "latest_model_outputs.csv")
