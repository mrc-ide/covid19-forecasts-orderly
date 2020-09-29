week_finishing <- "2020-09-27"
params <- parameters(week_finishing)
raw_data <- read.csv(
  parameters(week_finishing)$infile,
  stringsAsFactors = FALSE,
  na.strings = ""
)
raw_data <- dplyr::select(raw_data, -`Cumulative_number_for_14_days_of_COVID.19_cases_per_100000`)

raw_data <- dplyr::mutate_at(
    raw_data, vars("DateRep"), ~ as.Date(., format = "%d/%m/%Y")
  ) %>% dplyr::filter(DateRep <= as.Date(week_finishing))


######################################################################
######################################################################
########## Read in WHO data, and combined with ECDC data.
########## Where number of deaths reported by WHO < 0, and deaths
########## reported by ECDC are not, use ECDC data
########## Similarly for case data
######################################################################
######################################################################

who <- readr::read_csv("WHO-COVID-19-global-data.csv") %>%
  janitor::clean_names()

who$date_reported <- lubridate::ymd(who$date_reported)
who$iso3c <- countrycode::countrycode(who$country, "country.name", "iso3c")

raw_data <- select(raw_data, -geoId)
who <- select(who, -country_code)

both <- left_join(
  raw_data, who,
  by = c("DateRep" = "date_reported", "countryterritoryCode" = "iso3c")
)


## Where are the numbers different between the two data sources
## head(both[both$Deaths != both$new_deaths, ])
## First look at non-Nas, and then at Nas.
## Namibia's geoId being Na is being treated as NA by R.
##both <- select(both, -geoId)
both_complete <- na.omit(both)
both_incomplete <- both[!complete.cases(both), ]

## Add Kosovo and Taiwan. They don't match because they don't have a
## country code
both_complete <- filter(
  both_incomplete, `Countries.and.territories` %in% c("Kosovo", "Taiwan")
) %>% rbind(both_complete)


## Replace WHO data with ECDC data if WHO deaths are -ve
both_complete$new_deaths <- ifelse(
  both_complete$new_deaths < 0 & both_complete$Deaths >=0,
  both_complete$Deaths,
  both_complete$new_deaths
)



## Replace WHO data with ECDC data if WHO cases are NA as for Kosovo
## and Taiwan
both_complete$new_cases <- ifelse(
  is.na(both_complete$new_cases),
  both_complete$Cases,
  both_complete$new_cases
)


## Replace WHO data with ECDC data if WHO deaths are NA
both_complete$new_deaths <- ifelse(
  is.na(both_complete$new_deaths),
  both_complete$Deaths,
  both_complete$new_deaths
)

any(both_complete$new_deaths < 0)

## Replace WHO data with ECDC data if WHO cases are -ve
both_complete$new_cases <- ifelse(
  both_complete$new_cases < 0 & both_complete$Cases >=0,
  both_complete$Cases,
  both_complete$new_cases
)
any(both_complete$new_cases < 0)

## Now we can replace ECDC data completely with WHO data, and drop
## extra columns so that the rest of the code works without change
both_complete$Cases <- both_complete$new_cases
both_complete$Deaths <- both_complete$new_deaths
raw_data <- both_complete[ , colnames(raw_data)]


#################### Apply necessary corrections
raw_data$Deaths[raw_data$DateRep == "2020-04-17" & raw_data$`Countries.and.territories` == "China"] <- 0
## 04th May 2020. Manual tweaks against worldometer
raw_data$Deaths[raw_data$DateRep == "2020-05-03" & raw_data$`Countries.and.territories` == "Ireland"] <- 21
raw_data$Deaths[raw_data$DateRep == "2020-05-17" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 15
raw_data$Deaths[raw_data$DateRep == "2020-06-17" & raw_data$`Countries.and.territories` == "India"] <- 675

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

raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Ukraine"] <- 27
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Ukraine"] <- 15


#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 19th July ####################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$`Countries.and.territories` == "Chile" & raw_data$DateRep == "2020-07-18"] <- 98
raw_data$Deaths[raw_data$`Countries.and.territories` == "Chile" & raw_data$DateRep == "2020-07-19"] <- 58

raw_data$Deaths[raw_data$`Countries.and.territories` == "France" & raw_data$DateRep == "2020-07-15"] <- 71
raw_data$Deaths[raw_data$`Countries.and.territories` == "France" & raw_data$DateRep == "2020-07-16"] <- 20

raw_data$Deaths[raw_data$`Countries.and.territories` == "Honduras" & raw_data$DateRep == "2020-07-18"] <- 22
raw_data$Deaths[raw_data$`Countries.and.territories` == "Honduras" & raw_data$DateRep == "2020-07-19"] <- 34
#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 26th July ####################################
######################################################################
######################################################################
######################################################################

raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-07-24"] <- 188


## Soruce Wikipedia; ECDC reports 40 on one day and -12 on the previous day
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-06"] <- 15
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-05"] <- 13


raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-07-31"] <- 26
raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-08-01"] <- 24
raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-08-02"] <- 21


## Negative deaths reported in both WHO and ECDC, fixing against Worldometers
raw_data$Deaths[raw_data$`Countries.and.territories` == "Czechia" & raw_data$DateRep == "2020-07-05"] <- 0
raw_data$Deaths[raw_data$`Countries.and.territories` == "Czechia" & raw_data$DateRep == "2020-07-06"] <- 2
######################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 9th August ¢##################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan"& raw_data$DateRep == "2020-08-01"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan"& raw_data$DateRep == "2020-08-02"] <- 8
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan"& raw_data$DateRep == "2020-08-03"] <- 8

######################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 23rd August ¢##################################
######################################################################
######################################################################
######################################################################
## On 15 August, Emilia-Romagna added 154 deaths from March, April and May to its count.
raw_data$Deaths[raw_data$`Countries.and.territories` == "Italy" & raw_data$DateRep == "2020-08-16"] <- 4

######################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 30th August ¢#################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2020-08-24"] <- 18
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2020-08-25"] <- 19

## source Worldometers
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-29"] <- 12
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-30"] <- 13

raw_data$Cases[raw_data$`Countries.and.territories` == "Dominican_Republic" & raw_data$DateRep == "2020-09-05"] <- 20
raw_data$Cases[raw_data$`Countries.and.territories` == "Dominican_Republic" & raw_data$DateRep == "2020-09-06"] <- 19


dates_to_avg <- as.Date(c(
  "2020-09-05", "2020-09-06",
  "2020-09-07", "2020-09-09", "2020-09-10", "2020-09-11"
))

## Bolivia reports 1610 deaths on 7th september, setting it to the
## average of deaths from 4th, 5th, 6th, 8th, 9th, 10 September

bolivia_avg_cases <- mean(
  raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" &
                 raw_data$DateRep %in% dates_to_avg]
) %>% round

bolivia_avg_deaths <- mean(
  raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" &
                 raw_data$DateRep %in% dates_to_avg]
) %>% round


raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2020-09-08"] <- bolivia_avg_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2020-09-08"] <- bolivia_avg_deaths

## Similarly for Ecuador.
## average of deaths from 4th, 5th, 6th, 8th, 9th, 10 September
dates_to_avg <- as.Date(c(
  "2020-09-04", "2020-09-05", "2020-09-06",
  "2020-09-09", "2020-09-10", "2020-09-11"
))

ecuador_avg_cases <- mean(
  raw_data$Cases[raw_data$`Countries.and.territories` == "Ecuador" &
                 raw_data$DateRep %in% dates_to_avg]
) %>% round
ecuador_avg_deaths <- mean(
  raw_data$Deaths[raw_data$`Countries.and.territories` == "Ecuador" &
                 raw_data$DateRep %in% dates_to_avg]
) %>% round

raw_data$Cases[raw_data$`Countries.and.territories` == "Ecuador" & raw_data$DateRep == "2020-09-07"] <- ecuador_avg_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ecuador" & raw_data$DateRep == "2020-09-07"] <- ecuador_avg_deaths

######################################################################
### Corrections 20th September
### https://rpp.pe/peru/actualidad/minsa-suma-3-658-decesos-a-cifra-de-muertes-por-la-covid-19-y-casos-positivos-ya-superan-el-medio-millon-noticia-1286372?ref=rpp
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-08-15"] <- 277
## From ECDC
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-20"] <- 30
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-19"] <- 27
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-18"] <- 4
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-17"] <- 18
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-16"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-15"] <- 17
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-14"] <- 16
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-13"] <- 13

raw_data <- split(raw_data, raw_data$`Countries.and.territories`) %>%
  map_dfr(
    function(df) {
      if (all(df$Cases > 0)) return(df)
      idx <- which(df$Cases < 0)
      for (i in idx) {
        date_neg <- df$DateRep[i]
        dates_to_avg <- seq(
          from = as.Date(date_neg) - 3,
          to = as.Date(date_neg) + 3,
          by = "1 day"
        )
        dates_to_avg <- dates_to_avg[dates_to_avg != as.Date(date_neg)]
        dates_to_avg <- dates_to_avg[dates_to_avg <= max(as.Date(df$DateRep))]
        df$Cases[i] <- round(
          mean(df$Cases[df$DateRep %in% dates_to_avg])
        )
      }
      df
    }
  )


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

## Apply thresholds
pass <- split(raw_data, raw_data$`Countries.and.territories`) %>%
  purrr::keep(deaths_threshold) %>%
  dplyr::bind_rows()

## Still have some negative cases. Replace the negative case count
## with an average of previous and later 3 days.

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


## exclude <- c(
##   "China", "United_States_of_America",
##   "Ethiopia", "Ghana", "Kazakhstan",
##   "Zambia", "Kyrgyzstan", "Oman", "Zimbabwe"
## )

exclude <- c(
  "Kazakhstan",
  "Oman",
  "United_States_of_America",
  "Syria",
  "Zimbabwe",
  "Israel"
)
saveRDS(exclude, "exclude.rds")
