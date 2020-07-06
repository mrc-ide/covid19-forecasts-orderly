week_finishing <- "2020-07-05"
params <- parameters(week_finishing)
raw_data <- read.csv(
  parameters(week_finishing)$infile,
  stringsAsFactors = FALSE
)

## colnames(raw_data) <- c(
##   "DateRep", "day", "month", "year", "Cases", "Deaths", "Countries and territories",
##   "geoId", "countryterritoryCode", "popData2018", "continent"
## )

raw_data <- dplyr::mutate_at(
    raw_data, vars("DateRep"), ~ as.Date(., format = "%d/%m/%Y")
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
raw_data$Deaths[raw_data$DateRep == "2020-05-22" & raw_data$`Countries.and.territories` == "Spain"] <- 53
raw_data$Deaths[raw_data$DateRep == "2020-05-16" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 17
raw_data$Deaths[raw_data$DateRep == "2020-05-17" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 15
##raw_data$Deaths[raw_data$DateRep == "2020-05-02" & raw_data$`Countries.and.territories` == "Spain"] <- 276
##raw_data$Deaths[raw_data$DateRep == "2020-05-03" & raw_data$`Countries.and.territories` == "Spain"] <- 164

## Update 10th May - this row has been added in ECCDC data
## spain_extra <- data.frame(
##   DateRep = "2020-05-02",
##   day = 2,
##   month = 5,
##   year = 2020,
##   Cases = 2610,
##   Deaths = 276,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )
## raw_data <- rbind(raw_data, spain_extra)

## spain_extra <- data.frame(
##   DateRep = "2020-05-10",
##   day = 10,
##   month = 5,
##   year = 2020,
##   Cases = 1880,
##   Deaths = 143,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )
## raw_data <- rbind(raw_data, spain_extra)


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

## spain_extra <- data.frame(
##   DateRep = "2020-05-17",
##   day = 17,
##   month = 5,
##   year = 2020,
##   Cases = 1214,
##   Deaths = 87,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )

## spain_extra <- data.frame(
##   DateRep = "2020-05-24",
##   day = 24,
##   month = 5,
##   year = 2020,
##   Cases = 482,
##   Deaths = 74,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )

## Spain reports -1918 deaths and -373 cases on 25th May.
## Fixing it to a average of cases/deaths from 22nd to 24th May and
## 26th to 28th.
dates_of_interest <- as.Date(c(
  "2020-05-22", "2020-05-23", "2020-05-24",
  "2020-05-26", "2020-05-27", "2020-05-28"
))

deaths_avg <- round(
  mean(
    raw_data$Deaths[raw_data$DateRep %in% dates_of_interest &
                    raw_data$`Countries.and.territories` == "Spain"]
  )
)

cases_avg <- round(
  mean(
    raw_data$Cases[raw_data$DateRep %in% dates_of_interest &
                    raw_data$`Countries.and.territories` == "Spain"]
  )
)
raw_data$Cases[raw_data$DateRep == "2020-05-25" & raw_data$`Countries.and.territories` == "Spain"] <- cases_avg
raw_data$Deaths[raw_data$DateRep == "2020-05-25" & raw_data$`Countries.and.territories` == "Spain"] <- deaths_avg

## Update 07-06-2020: This is now present in ECDC data
## spain_extra <- data.frame(
##   DateRep = "2020-05-31",
##   day = 31,
##   month = 5,
##   year = 2020,
##   Cases = 201,
##   Deaths = 2,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )

## spain_extra <- data.frame(
##   DateRep = "2020-06-07",
##   day = 7,
##   month = 6,
##   year = 2020,
##   Cases = 240,
##   Deaths = 1,
##   `Countries.and.territories` = "Spain",
##   geoId = "ES",
##   countryterritoryCode = "ESP",
##   popData2018 = 46723749,
##   continent = "Europe"
## )

## raw_data <- rbind(raw_data, spain_extra)

## Corrections for Turkey
raw_data$Deaths[raw_data$DateRep == "2020-05-28" & raw_data$`Countries.and.territories` == "Turkey"] <- 34
raw_data$Deaths[raw_data$DateRep == "2020-05-29" & raw_data$`Countries.and.territories` == "Turkey"] <- 30

## Corrections for Ukraine
raw_data$Deaths[raw_data$DateRep == "2020-05-28" & raw_data$`Countries.and.territories` == "Ukraine"] <- 14
raw_data$Deaths[raw_data$DateRep == "2020-05-29" & raw_data$`Countries.and.territories` == "Ukraine"] <- 11
raw_data$Deaths[raw_data$DateRep == "2020-05-30" & raw_data$`Countries.and.territories` == "Ukraine"] <- 10
raw_data$Deaths[raw_data$DateRep == "2020-05-31" & raw_data$`Countries.and.territories` == "Ukraine"] <- 17


## Brazil: 06th and 07th from worldometers
raw_data$Deaths[raw_data$DateRep == "2020-06-06" & raw_data$`Countries.and.territories` == "Brazil"] <- 910
raw_data$Deaths[raw_data$DateRep == "2020-06-07" & raw_data$`Countries.and.territories` == "Brazil"] <- 542

## Panama: ECDC has -ve deaths
raw_data$Deaths[raw_data$DateRep == "2020-06-03" & raw_data$`Countries.and.territories` == "Panama"] <- 8
raw_data$Deaths[raw_data$DateRep == "2020-06-04" & raw_data$`Countries.and.territories` == "Panama"] <- 5

## Peru. 4th June has 260 deaths, which seems to be a sum of deaths on
## 3rd and 4th
raw_data$Deaths[raw_data$DateRep == "2020-06-03" & raw_data$`Countries.and.territories` == "Peru"] <- 127
raw_data$Deaths[raw_data$DateRep == "2020-06-04" & raw_data$`Countries.and.territories` == "Peru"] <- 137

## 15th June 2020. Colombia mismatch
raw_data$Deaths[raw_data$DateRep == "2020-06-12" & raw_data$`Countries.and.territories` == "Colombia"] <- 55
raw_data$Deaths[raw_data$DateRep == "2020-06-13" & raw_data$`Countries.and.territories` == "Colombia"] <- 57
raw_data$Cases[raw_data$DateRep == "2020-06-12" & raw_data$`Countries.and.territories` == "Colombia"] <- 1530
raw_data$Cases[raw_data$DateRep == "2020-06-13" & raw_data$`Countries.and.territories` == "Colombia"] <- 1646

## 22nd June, corrections for India, Russia and Iraq
raw_data$Deaths[raw_data$DateRep == "2020-06-17" & raw_data$`Countries.and.territories` == "India"] <- 675
raw_data$Deaths[raw_data$DateRep == "2020-06-20" & raw_data$`Countries.and.territories` == "Iraq"] <- 69
raw_data$Deaths[raw_data$DateRep == "2020-06-21" & raw_data$`Countries.and.territories` == "Iraq"] <- 88

raw_data$Deaths[raw_data$DateRep == "2020-06-19" & raw_data$`Countries.and.territories` == "Russia"] <- 182
raw_data$Deaths[raw_data$DateRep == "2020-06-20" & raw_data$`Countries.and.territories` == "Russia"] <- 181
raw_data$Deaths[raw_data$DateRep == "2020-06-21" & raw_data$`Countries.and.territories` == "Russia"] <- 161

######################################################################
######################################################################
######################################################################
########### Corrections 28th June ###################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Argentina"] <- 33

raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "Armenia"] <- 13
raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Armenia"] <- 11
raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Armenia"] <- 14
raw_data$Deaths[raw_data$DateRep == "2020-06-23" & raw_data$`Countries.and.territories` == "Armenia"] <- 12


raw_data$Deaths[raw_data$DateRep == "2020-06-21" & raw_data$`Countries.and.territories` == "Azerbaijan"] <- 5
raw_data$Deaths[raw_data$DateRep == "2020-06-20" & raw_data$`Countries.and.territories` == "Azerbaijan"] <- 4

raw_data$Deaths[raw_data$DateRep == "2020-06-21" & raw_data$`Countries.and.territories` == "Belarus"] <- 6
raw_data$Deaths[raw_data$DateRep == "2020-06-20" & raw_data$`Countries.and.territories` == "Belarus"] <- 6

raw_data$Deaths[raw_data$DateRep == "2020-06-28" & raw_data$`Countries.and.territories` == "Democratic_Republic_of_the_Congo"] <- 4
raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "Democratic_Republic_of_the_Congo"] <- 0
raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Democratic_Republic_of_the_Congo"] <- 7

raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "Egypt"] <- 83
raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Egypt"] <- 85

raw_data$Deaths[raw_data$DateRep == "2020-06-28" & raw_data$`Countries.and.territories` == "Iran"] <- 125
raw_data$Deaths[raw_data$DateRep == "2020-06-27" & raw_data$`Countries.and.territories` == "Iran"] <- 109

raw_data$Deaths[raw_data$DateRep == "2020-06-28" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 15
raw_data$Deaths[raw_data$DateRep == "2020-06-27" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 11

raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Peru"] <- 181
raw_data$Deaths[raw_data$DateRep == "2020-06-23" & raw_data$`Countries.and.territories` == "Peru"] <- 178

raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Ukraine"] <- 16
raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Ukraine"] <- 23

raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "United_States_of_America"] <-
  raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "United_States_of_America"] - 1854


### Replace -31 deaths in Italy with moving average
dates_of_interest <- as.Date(c(
  "2020-06-22", "2020-06-23", "2020-06-24",
  "2020-06-26", "2020-06-27", "2020-06-28"
))

deaths_avg <- round(
  mean(
    raw_data$Deaths[raw_data$DateRep %in% dates_of_interest &
                    raw_data$`Countries.and.territories` == "Italy"]
  )
)

raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Italy"] <- deaths_avg

#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 5th July #####################################
######################################################################
######################################################################
######################################################################
raw_data$Cases[raw_data$DateRep == "2020-07-01" & raw_data$`Countries.and.territories` == "Kazakhstan"] <-
  round(mean(
  c(raw_data$Cases[raw_data$DateRep == "2020-07-02" & raw_data$`Countries.and.territories` == "Kazakhstan"],
    raw_data$Cases[raw_data$DateRep == "2020-07-03" & raw_data$`Countries.and.territories` == "Kazakhstan"],
    raw_data$Cases[raw_data$DateRep == "2020-06-30" & raw_data$`Countries.and.territories` == "Kazakhstan"],
    raw_data$Cases[raw_data$DateRep == "2020-06-29" & raw_data$`Countries.and.territories` == "Kazakhstan"]
    )
  ))

raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 26
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 26

raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Qatar"] <- 3
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Qatar"] <- 2

raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Sudan"] <- 4

raw_data$Deaths[raw_data$DateRep == "2020-07-01" & raw_data$`Countries.and.territories` == "Ukraine"] <- 12
raw_data$Deaths[raw_data$DateRep == "2020-07-02" & raw_data$`Countries.and.territories` == "Ukraine"] <- 14
raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Ukraine"] <- 27
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Ukraine"] <- 15

raw_data$Cases[raw_data$DateRep == "2020-07-03" & raw_data$`Countries.and.territories` == "United_Kingdom"] <-
  round(
    mean(
      c(raw_data$Cases[raw_data$DateRep == "2020-07-01" & raw_data$`Countries.and.territories` == "United_Kingdom"],
        raw_data$Cases[raw_data$DateRep == "2020-07-02" & raw_data$`Countries.and.territories` == "United_Kingdom"],
        raw_data$Cases[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "United_Kingdom"],
        raw_data$Cases[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "United_Kingdom"]
        )
    )
  )

## uk_extra <- data.frame(
##   DateRep = "2020-05-24",
##   day = 24,
##   month = 5,
##   year = 2020,
##   Cases = 0,
##   Deaths = 282,
##   `Countries.and.territories` = "United_Kingdom",
##   geoId = "GB",
##   countryterritoryCode = "GBR",
##   popData2018 = 66488991,
##   continent = "Europe"
## )
## raw_data <- rbind(raw_data, uk_extra)


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
