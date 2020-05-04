week_finishing <- "2020-05-03"
params <- parameters(week_finishing)
raw_data <- read.csv(
  parameters(week_finishing)$infile,
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate_at(
    vars("DateRep"), ~ as.Date(., format = "%d/%m/%Y")
  ) %>%
  ## Manual fixes.
  ## For 2020-03-17, there are two rows for Somalia
  ## one with 0 Cases and one with 1 Cases, Delete one of them
  dplyr::filter(
    !(Countries.and.territories == "Somalia" &
      DateRep == "2020-03-17" & Cases == 0)
  ) %>% dplyr::filter(DateRep <= as.Date(week_finishing))

## 04th May 2020. Manual tweaks against worldometer
raw_data$Deaths[raw_data$DateRep == "2020-05-01" & raw_data$`Countries.and.territories` == "Germany"] <- 156
raw_data$Deaths[raw_data$DateRep == "2020-05-02" & raw_data$`Countries.and.territories` == "Germany"] <- 113
raw_data$Deaths[raw_data$DateRep == "2020-05-03" & raw_data$`Countries.and.territories` == "Ireland"] <- 21
raw_data$Deaths[raw_data$DateRep == "2020-04-27" & raw_data$`Countries.and.territories` == "Spain"] <- 331
raw_data$Deaths[raw_data$DateRep == "2020-04-28" & raw_data$`Countries.and.territories` == "Spain"] <- 301
##raw_data$Deaths[raw_data$DateRep == "2020-05-02" & raw_data$`Countries.and.territories` == "Spain"] <- 276
raw_data$Deaths[raw_data$DateRep == "2020-05-03" & raw_data$`Countries.and.territories` == "Spain"] <- 164

spain_extra <- data.frame(
  DateRep = "2020-05-02",
  day = 2,
  month = 5,
  year = 2020,
  Cases = 2610,
  Deaths = 276,
  `Countries.and.territories` = "Spain",
  geoId = "ES",
  countryterritoryCode = "ESP",
  popData2018 = 46723749,
  continent = "Europe"
)
raw_data <- rbind(raw_data, spain_extra)

## spain_extra <- data.frame(
##   DateRep = "2020-04-26",
##   day = 26,
##   month = 4,
##   year = 2020,
##   Cases = 3995,
##   Deaths = 378,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )
## raw_data <- rbind(raw_data, spain_extra)
## 27th April: Ireland manually fixed in the csv.
## ECDC Reported 234 deaths on 2020-04-26
## which was a massive jump from 35 reported on 2020-04-25.
## Fixed it to match numbers from Worldometer.

## Save before applying theresholds as well so that we can compute
## model performance metrics
by_country_deaths_all <- dplyr::select(
  raw_data, dates = DateRep, Deaths, Countries.and.territories
) %>%
  tidyr::spread(
    key = Countries.and.territories, value = Deaths, fill = 0
  )

saveRDS(
  object = by_country_deaths_all,
  file = "latest_deaths_wide_no_filter.rds"
)

## Excluding China which is included only because of the massive back-fill.
## raw_data <- dplyr::filter(
##   raw_data, !(Countries.and.territories == "China")
## )
## Apply thresholds
pass <- split(raw_data, raw_data$`Countries.and.territories`) %>%
  purrr::keep(deaths_threshold) %>%
  dplyr::bind_rows()

by_country_deaths <- dplyr::select(
  pass, DateRep, Deaths, Countries.and.territories
) %>%
  tidyr::spread(
    key = Countries.and.territories, value = Deaths, fill = 0
  )

## No lines means no cases for that day. That is why fill is 0.
by_country_cases <- dplyr::select(
  pass, DateRep, Cases, Countries.and.territories
) %>%
  tidyr::spread(
    key = Countries.and.territories, value = Cases, fill = 0
  ) %>%
  dplyr::filter(DateRep <= week_finishing)


## For consistency with Pierre's code, rename DateRep to dates
cases_to_use <- dplyr::rename(by_country_cases, dates = "DateRep")

deaths_to_use <- dplyr::rename(by_country_deaths, dates = "DateRep")

Country <- colnames(deaths_to_use)[!colnames(deaths_to_use) == "dates"]

x <- list(
    date_week_finishing = week_finishing,
    Threshold_criterion_4weeks = params$Threshold_criterion_4weeks,
    Threshold_criterion_7days = params$Threshold_criterion_7days,
    I_active_transmission = cases_to_use,
    D_active_transmission = deaths_to_use,
    Country = Country,
    si_mean = params$si_mean,
    si_std = params$si_std
)

out <- saveRDS(
  object = x,
  file = params$outfile
)

## Also save it with a generic name to avoid having to configure the
## downstream tasks

out <- saveRDS(
  object = x,
  file = "latest_model_input.rds"
)
