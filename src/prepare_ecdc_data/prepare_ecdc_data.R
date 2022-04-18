params <- parameters(week_ending)

## We only take the country names from here, because we have used
## them earlier.
ecdc <- readr::read_csv("ECDC-COVID-19-global-data.csv")
ecdc <- select(ecdc, `Countries and territories`, countryterritoryCode)
ecdc <- distinct(ecdc)

raw_data <- readr::read_csv("WHO-COVID-19-global-data.csv") %>%
  janitor::clean_names()

raw_data$date_reported <- lubridate::ymd(raw_data$date_reported)
raw_data <- filter(raw_data, date_reported <= as.Date(week_ending))


raw_data$iso3c <- countrycode(raw_data$country, "country.name", "iso3c")
#### To Attach Kosovo back later to the dataset, it doesn't have an
#### ISO3 code, but is included in the list of countries
raw_data$country[raw_data$country == "Kosovo[1]"] <- "Kosovo"
### code used in ECDC for Kosovo
kosovo <- ecdc$countryterritoryCode[ecdc$`Countries and territories` == "Kosovo"]
raw_data$iso3c[raw_data$country == "Kosovo"] <- kosovo

raw_data <- left_join(
  raw_data, ecdc, by = c("iso3c" = "countryterritoryCode")
)
## Drop country_code, we don't use this column anyway and it causes
## problems as it is NA for Namibia
raw_data <- select(raw_data, -country_code)
## Some values were not matched unambiguously: Bonaire, Kosovo[1],
## Other, Saba, Saint Martin, Sint Eustatius
raw_data <- na.omit(raw_data)


## Rename columns of WHO data, so that we can continue to reuse the
## old code

raw_data <- rename(
  raw_data, Cases = "new_cases", Deaths = "new_deaths",
  DateRep = "date_reported",
  `Countries.and.territories` = "Countries and territories"
)

raw_data$Cases[raw_data$DateRep == "2020-03-01" & raw_data$`Countries.and.territories` == "Spain"] <- 32
raw_data$Cases[raw_data$DateRep == "2020-03-02" & raw_data$`Countries.and.territories` == "Spain"] <- 17
raw_data$Cases[raw_data$DateRep == "2020-03-03" & raw_data$`Countries.and.territories` == "Spain"] <- 31
raw_data$Cases[raw_data$DateRep == "2020-03-08" & raw_data$`Countries.and.territories` == "Spain"] <- 56
raw_data$Cases[raw_data$DateRep == "2020-03-09" & raw_data$`Countries.and.territories` == "Spain"] <- 159
raw_data$Cases[raw_data$DateRep == "2020-03-10" & raw_data$`Countries.and.territories` == "Spain"] <- 615

raw_data$Deaths[raw_data$DateRep == "2020-03-11" & raw_data$`Countries.and.territories` == "Spain"] <- 7
raw_data$Deaths[raw_data$DateRep == "2020-03-12" & raw_data$`Countries.and.territories` == "Spain"] <- 12
raw_data$Deaths[raw_data$DateRep == "2020-03-13" & raw_data$`Countries.and.territories` == "Spain"] <- 37
raw_data$Deaths[raw_data$DateRep == "2020-03-14" & raw_data$`Countries.and.territories` == "Spain"] <- 37
raw_data$Deaths[raw_data$DateRep == "2020-03-15" & raw_data$`Countries.and.territories` == "Spain"] <- 15


raw_data$Deaths[raw_data$DateRep == "2020-04-27" & raw_data$`Countries.and.territories` == "Spain"] <- 331
raw_data$Deaths[raw_data$DateRep == "2020-04-28" & raw_data$`Countries.and.territories` == "Spain"] <- 301
raw_data$Deaths[raw_data$DateRep == "2020-05-22" & raw_data$`Countries.and.territories` == "Spain"] <- 53


## For 4 days in April, ECDC data and WHO data for USA are very different
## with both reporting a lot more outliers
## These numbers are from https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html
raw_data$Deaths[raw_data$DateRep == "2020-04-15" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 2752
raw_data$Deaths[raw_data$DateRep == "2020-04-16" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 2349
raw_data$Deaths[raw_data$DateRep == "2020-04-17" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 2289
raw_data$Deaths[raw_data$DateRep == "2020-04-18" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 1951
raw_data$Deaths[raw_data$DateRep == "2020-04-19" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 1520

## Problems with WHO data for USA for these dates as well, with WHO reporting negative deaths,
## 5000 deaths on one day and 156 on another.
## ECDC data for these dates are used.
raw_data$Deaths[raw_data$DateRep == "2020-05-03" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 1317
raw_data$Deaths[raw_data$DateRep == "2020-05-04" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 1297
raw_data$Deaths[raw_data$DateRep == "2020-05-05" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 1252
raw_data$Deaths[raw_data$DateRep == "2020-05-06" & raw_data$`Countries.and.territories` == "United_States_of_America"] <- 2144


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
######################################################################
######################################################################
######################################################################
## From ECDC, WHO data have a 0 for one day and 387 on the next day, while ECDC
## report  191 and 196 respectively
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-08-02"] <- 191
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-08-03"] <- 196

### Corrections 20th September
### https://rpp.pe/peru/actualidad/minsa-suma-3-658-decesos-a-cifra-de-muertes-por-la-covid-19-y-casos-positivos-ya-superan-el-medio-millon-noticia-1286372?ref=rpp
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-08-15"] <- 277
## From Worldometers. There is a lag of 2 days between the time series on worldometers
## and that reported by WHO/ECDC in this period, with Worldometers being 2 days ahead.
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-09-22"] <- 186
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-09-24"] <- 112
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-09-25"] <- 98

## From ECDC
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-20"] <- 30
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-19"] <- 27
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-18"] <- 4
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-17"] <- 18
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-16"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-15"] <- 17
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-14"] <- 16
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-09-13"] <- 13

## Source: https://www.lanacion.com.ar/sociedad/coronavirus-buenos-aires-cargo-casi-3200-muertos-nid2466920
## Argentina added 2997 deaths to its total
raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina" & raw_data$DateRep == "2020-10-03"] <- 354


## Corrections 12th October; Worldometers
raw_data$Deaths[raw_data$`Countries.and.territories` == "Hungary" & raw_data$DateRep == "2020-10-11"] <- 14
raw_data$Deaths[raw_data$`Countries.and.territories` == "Hungary" & raw_data$DateRep == "2020-10-10"] <- 21
## source Worldometers
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2020-10-10"] <- 180
raw_data$Deaths[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2020-10-09"] <- 160
raw_data$Deaths[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2020-10-08"] <- 145

## Corrections 19th October
## https://www.thejournal.ie/new-covid-19-cases-2-october-5221779-Oct2020/
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ireland" & raw_data$DateRep == "2020-10-03"] <- 1


## Corrections 09th November
raw_data$Deaths[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-08"] <- 199
raw_data$Cases[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-08"] <- 6124
## 1326 new deaths were added after a change in criteria for reporting
## deaths
raw_data$Deaths[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep == "2020-11-05"] <- 297


raw_data$Deaths[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-15"] <- 118
raw_data$Cases[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-15"] <- 4659
## Backlog of deaths reported on this day
raw_data$Deaths[raw_data$`Countries.and.territories` == "France" & raw_data$DateRep == "2020-11-11"] <- 857



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

raw_data <- split(raw_data, raw_data$`Countries.and.territories`) %>%
  map_dfr(
    function(df) {
      if (all(df$Deaths > 0)) return(df)
      idx <- which(df$Deaths < 0)
      for (i in idx) {
        date_neg <- df$DateRep[i]
        dates_to_avg <- seq(
          from = as.Date(date_neg) - 3,
          to = as.Date(date_neg) + 3,
          by = "1 day"
        )
        dates_to_avg <- dates_to_avg[dates_to_avg != as.Date(date_neg)]
        dates_to_avg <- dates_to_avg[dates_to_avg <= max(as.Date(df$DateRep))]
        df$Deaths[i] <- round(
          mean(df$Deaths[df$DateRep %in% dates_to_avg])
        )
      }
      df
    }
  )

## WHO erroneously notes this as 12
raw_data$Deaths[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-02"] <- 112
raw_data$Cases[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-02"] <- 11789

raw_data$Deaths[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-22"] <- 96
raw_data$Cases[raw_data$`Countries.and.territories` == "Belgium" & raw_data$DateRep == "2020-11-22"] <- 1875

raw_data$Deaths[raw_data$`Countries.and.territories` == "Switzerland" & raw_data$DateRep == "2020-11-22"] <- 39
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2020-11-21"] <- 59
## worldometers
raw_data$Cases[raw_data$`Countries.and.territories` == "France" & raw_data$DateRep == "2020-11-18"] <- 14524

raw_data$Deaths[raw_data$`Countries.and.territories` == "Poland" & raw_data$DateRep == "2020-10-05"] <- 34
raw_data$Cases[raw_data$`Countries.and.territories` == "Poland" & raw_data$DateRep == "2020-10-05"] <- 1934

## Corrections from worldometers
raw_data$Cases[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2020-10-10"] <- 5263
raw_data$Deaths[raw_data$`Countries.and.territories` == "Azerbaijan" & raw_data$DateRep == "2020-11-29"] <- 38

## From  ECDC
raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina" & raw_data$DateRep == "2020-11-24"] <- 100

raw_data$Deaths[raw_data$`Countries.and.territories` == "Azerbaijan" & raw_data$DateRep == "2020-11-29"] <- 38

## Corrections 14th Dec from ECDC
raw_data$Cases[raw_data$`Countries.and.territories` == "Portugal" & raw_data$DateRep == "2020-12-10"] <- 4097
raw_data$Deaths[raw_data$`Countries.and.territories` == "Portugal" & raw_data$DateRep == "2020-12-10"] <- 70
raw_data$Cases[raw_data$`Countries.and.territories` == "Portugal" & raw_data$DateRep == "2020-12-09"] <- 2905
raw_data$Deaths[raw_data$`Countries.and.territories` == "Portugal" & raw_data$DateRep == "2020-12-09"] <- 81

raw_data$Cases[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep == "2020-12-13"] <- 15627
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep == "2020-12-13"] <- 249

## Corrections 28th December. Source Worldometers
raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2020-12-27"] <- 163
raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2020-12-26"] <- 81
raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2020-12-25"] <- 122
raw_data$Deaths[raw_data$`Countries.and.territories` == "Croatia" & raw_data$DateRep == "2020-12-27"] <- 58
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-12-25"] <- 55
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-12-26"] <- 44

## Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-01-03" & raw_data$`Countries.and.territories` == "Greece"] <- 40
raw_data$Deaths[raw_data$DateRep == "2021-01-03" & raw_data$`Countries.and.territories` == "South_Africa"] <- 288
raw_data$Deaths[raw_data$DateRep == "2021-01-02" & raw_data$`Countries.and.territories` == "South_Africa"] <- 418
raw_data$Cases[raw_data$DateRep == "2021-01-03" & raw_data$`Countries.and.territories` == "South_Africa"] <- 15002
raw_data$Cases[raw_data$DateRep == "2021-01-02" & raw_data$`Countries.and.territories` == "South_Africa"] <- 16726

raw_data$Deaths[raw_data$DateRep == "2020-12-31" & raw_data$`Countries.and.territories` == "Switzerland"] <- 101
raw_data$Deaths[raw_data$DateRep == "2021-01-01" & raw_data$`Countries.and.territories` == "Switzerland"] <- 51
raw_data$Deaths[raw_data$DateRep == "2021-01-02" & raw_data$`Countries.and.territories` == "Switzerland"] <- 59
raw_data$Deaths[raw_data$DateRep == "2021-01-03" & raw_data$`Countries.and.territories` == "Switzerland"] <- 25

raw_data$Cases[raw_data$DateRep == "2020-12-31" & raw_data$`Countries.and.territories` == "Switzerland"] <- 4197
raw_data$Cases[raw_data$DateRep == "2021-01-01" & raw_data$`Countries.and.territories` == "Switzerland"] <- 5424
raw_data$Cases[raw_data$DateRep == "2021-01-02" & raw_data$`Countries.and.territories` == "Switzerland"] <- 4391

## Corrections for report on 11th Jan 2021
## WHO reports 0 on 9th and 429 on 10th. Corrections from worldometers
raw_data$Deaths[raw_data$DateRep == "2021-01-09" & raw_data$`Countries.and.territories` == "India"] <- 229
raw_data$Deaths[raw_data$DateRep == "2021-01-10" & raw_data$`Countries.and.territories` == "India"] <- 213

## Backlog of 293 deaths reported in Lithuania
## https://www.delfi.lt/news/daily/lithuania/nvsc-vadovas-ministrui-pateike-atsakymus-del-neapskaitytu-mirciu.d?id=86144987
raw_data$Deaths[raw_data$DateRep == "2021-01-04" & raw_data$`Countries.and.territories` == "Lithuania"] <- 307 - 293


## Corrections for report on 18th Jan 2021. Source Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-01-14" & raw_data$`Countries.and.territories` == "Guatemala"] <- 37
raw_data$Deaths[raw_data$DateRep == "2021-01-13" & raw_data$`Countries.and.territories` == "Guatemala"] <- 55

raw_data$Deaths[raw_data$DateRep == "2021-01-16" & raw_data$`Countries.and.territories` == "Latvia"] <- 15
raw_data$Deaths[raw_data$DateRep == "2021-01-17" & raw_data$`Countries.and.territories` == "Latvia"] <- 26

raw_data$Deaths[raw_data$DateRep == "2021-01-14" & raw_data$`Countries.and.territories` == "Peru"] <- 64
raw_data$Deaths[raw_data$DateRep == "2021-01-15" & raw_data$`Countries.and.territories` == "Peru"] <- 74
## Source Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-01-24" & raw_data$`Countries.and.territories` == "Switzerland"] <- 16

## WHO data has 0s for UK. Corrected from Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-01-30" & raw_data$`Countries.and.territories` == "United_Kingdom"] <- 1245
raw_data$Deaths[raw_data$DateRep == "2021-01-31" & raw_data$`Countries.and.territories` == "United_Kingdom"] <- 1200

raw_data$Cases[raw_data$DateRep == "2021-01-30" & raw_data$`Countries.and.territories` == "United_Kingdom"] <- 29079
raw_data$Cases[raw_data$DateRep == "2021-01-31" & raw_data$`Countries.and.territories` == "United_Kingdom"] <- 21088
## from Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-01-31" & raw_data$`Countries.and.territories` == "Bolivia"] <- 53
## Erroneously recorded as 351 in WHO data
raw_data$Deaths[raw_data$DateRep == "2021-01-31" & raw_data$`Countries.and.territories` == "Lebanon"] <- 51

## Corrections 15th March 2021
raw_data$Deaths[raw_data$DateRep == "2021-03-14" & raw_data$`Countries.and.territories` == "Albania"] <- 12
raw_data$Cases[raw_data$DateRep == "2021-03-14" & raw_data$`Countries.and.territories` == "Albania"] <- 698


## Corrections 29th March 2021; Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-03-28" & raw_data$`Countries.and.territories` == "Croatia"] <- 21
raw_data$Cases[raw_data$DateRep == "2021-03-28" & raw_data$`Countries.and.territories` == "Croatia"] <- 1321

## French Government reported 897 new deaths, of which 594 deaths in EMS and EHAPD and 303 deaths in hospitals.
raw_data$Deaths[raw_data$DateRep == "2021-03-27" & raw_data$`Countries.and.territories` == "France"] <- 363
raw_data$Deaths[raw_data$DateRep == "2021-03-28" & raw_data$`Countries.and.territories` == "France"] <- 190
raw_data$Cases[raw_data$DateRep == "2021-03-28" & raw_data$`Countries.and.territories` == "France"] <- 42619

## 12th April 2021 Uruguay. Source worldometers
raw_data$Deaths[raw_data$DateRep == "2021-04-11" & raw_data$`Countries.and.territories` == "Uruguay"] <- 52

## 19th April, source worldometers
raw_data$Deaths[raw_data$DateRep == "2021-04-12" & raw_data$`Countries.and.territories` == "Armenia"] <- 18
raw_data$Deaths[raw_data$DateRep == "2021-04-13" & raw_data$`Countries.and.territories` == "Armenia"] <- 22

## 24th May, source worldometers
raw_data$Deaths[raw_data$DateRep == "2021-05-20" & raw_data$`Countries.and.territories` == "France"] <- 146
raw_data$Deaths[raw_data$DateRep == "2021-05-21" & raw_data$`Countries.and.territories` == "France"] <- 138
raw_data$Deaths[raw_data$DateRep == "2021-05-22" & raw_data$`Countries.and.territories` == "France"] <- 113
raw_data$Deaths[raw_data$DateRep == "2021-05-23" & raw_data$`Countries.and.territories` == "France"] <- 70

raw_data$Cases[raw_data$DateRep == "2021-05-20" & raw_data$`Countries.and.territories` == "France"] <- 17877
raw_data$Cases[raw_data$DateRep == "2021-05-21" & raw_data$`Countries.and.territories` == "France"] <- 15415
raw_data$Cases[raw_data$DateRep == "2021-05-22" & raw_data$`Countries.and.territories` == "France"] <- 12800
raw_data$Cases[raw_data$DateRep == "2021-05-23" & raw_data$`Countries.and.territories` == "France"] <- 9704

raw_data$Deaths[raw_data$DateRep == "2021-05-22" & raw_data$`Countries.and.territories` == "Sri_Lanka"] <- 43
raw_data$Deaths[raw_data$DateRep == "2021-05-23" & raw_data$`Countries.and.territories` == "Sri_Lanka"] <- 46



## 3rd June, more than 4000 deaths in WHO data. Correction source worldometers
raw_data$Deaths[raw_data$DateRep == "2021-06-03" & raw_data$`Countries.and.territories` == "Mexico"] <- 33

## india backlogged deaths
raw_data$Deaths[raw_data$DateRep == "2021-06-10" & raw_data$`Countries.and.territories` == "India"] <- 6148 - 3951


## 2021-06-17,NA,Namibia,AFRO,0,67021,0,1040
## 2021-06-18,NA,Namibia,AFRO,3091,70112,67,1107
## 2021-06-19,NA,Namibia,AFRO,0,70112,0,1107
## 2021-06-20,NA,Namibia,AFRO,1649,71761,27,1134

raw_data$Deaths[raw_data$DateRep == "2021-06-17" & raw_data$`Countries.and.territories` == "Namibia"] <- 34
raw_data$Deaths[raw_data$DateRep == "2021-06-18" & raw_data$`Countries.and.territories` == "Namibia"] <- 27
raw_data$Deaths[raw_data$DateRep == "2021-06-19" & raw_data$`Countries.and.territories` == "Namibia"] <- 27
raw_data$Deaths[raw_data$DateRep == "2021-06-20" & raw_data$`Countries.and.territories` == "Namibia"] <- 3

## Worldometers
raw_data$Deaths[raw_data$DateRep == "2021-07-02" & raw_data$`Countries.and.territories` == "Peru"] <- 122
raw_data$Deaths[raw_data$DateRep == "2021-07-03" & raw_data$`Countries.and.territories` == "Peru"] <- 234
raw_data$Cases[raw_data$DateRep == "2021-07-02" & raw_data$`Countries.and.territories` == "Peru"] <- 3079
raw_data$Cases[raw_data$DateRep == "2021-07-03" & raw_data$`Countries.and.territories` == "Peru"] <- 2410


raw_data$Deaths[raw_data$DateRep == "2021-07-11" & raw_data$`Countries.and.territories` == "Peru"] <- 175
raw_data$Deaths[raw_data$DateRep == "2021-07-12" & raw_data$`Countries.and.territories` == "Peru"] <- 165
raw_data$Cases[raw_data$DateRep == "2021-07-11" & raw_data$`Countries.and.territories` == "Peru"] <- 2344
raw_data$Cases[raw_data$DateRep == "2021-07-12" & raw_data$`Countries.and.territories` == "Peru"] <- 2285

## worldometers
raw_data$Deaths[raw_data$DateRep == "2021-06-15" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 65
raw_data$Deaths[raw_data$DateRep == "2021-06-16" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 94
raw_data$Cases[raw_data$DateRep == "2021-06-15" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 1537
raw_data$Cases[raw_data$DateRep == "2021-06-16" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 1722

## https://www.worldometers.info/coronavirus/country/india/
raw_data$Deaths[raw_data$DateRep == "2021-07-21" & raw_data$`Countries.and.territories` == "India"] <- 3998 - 3509

## Worldometers
raw_data$Cases[raw_data$DateRep == "2021-08-06" & raw_data$`Countries.and.territories` == "Turkey"] <- 24297
raw_data$Cases[raw_data$DateRep == "2021-08-07" & raw_data$`Countries.and.territories` == "Turkey"] <- 23957
raw_data$Deaths[raw_data$DateRep == "2021-08-06" & raw_data$`Countries.and.territories` == "Turkey"] <- 108
raw_data$Deaths[raw_data$DateRep == "2021-08-07" & raw_data$`Countries.and.territories` == "Turkey"] <- 101

raw_data$Deaths[raw_data$DateRep == "2021-08-15" & raw_data$`Countries.and.territories` == "South_Africa"] <- 238
raw_data$Cases[raw_data$DateRep == "2021-08-15" & raw_data$`Countries.and.territories` == "South_Africa"] <- 13020


raw_data$Deaths[raw_data$DateRep == "2021-08-16" & raw_data$`Countries.and.territories` == "Bolivia"] <- 7
raw_data$Deaths[raw_data$DateRep == "2021-08-17" & raw_data$`Countries.and.territories` == "Bolivia"] <- 26

## Corrections 6th September 2021
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2021-08-30"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2021-08-29"] <- 20

raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2021-08-30"] <- 254
raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2021-08-29"] <- 355

raw_data$Deaths[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2021-09-05"] <- 182
raw_data$Cases[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2021-09-05"] <- 8410

########### 13th September 2021
raw_data$Deaths[raw_data$`Countries.and.territories` == "Azerbaijan" & raw_data$DateRep == "2021-09-12"] <- 34
raw_data$Cases[raw_data$`Countries.and.territories` == "Azerbaijan" & raw_data$DateRep == "2021-09-12"] <- 2372

raw_data$Deaths[raw_data$`Countries.and.territories` == "Croatia" & raw_data$DateRep == "2021-09-11"] <- 14
raw_data$Cases[raw_data$`Countries.and.territories` == "Croatia" & raw_data$DateRep == "2021-09-11"] <- 1162

raw_data$Deaths[raw_data$`Countries.and.territories` == "Croatia" & raw_data$DateRep == "2021-09-12"] <- 13
raw_data$Cases[raw_data$`Countries.and.territories` == "Croatia" & raw_data$DateRep == "2021-09-12"] <- 807

raw_data$Deaths[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2021-09-06"] <- 8
raw_data$Cases[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2021-09-06"] <- 1048
raw_data$Cases[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2021-09-07"] <- 1061
raw_data$Deaths[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2021-09-07"] <- 6


raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-06"] <- 80
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-06"] <- 3043
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-07"] <- 1923
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-07"] <- 72


raw_data$Deaths[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2021-09-08"] <- 282
raw_data$Cases[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2021-09-08"] <- 5372
raw_data$Cases[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2021-09-09"] <- 7338
raw_data$Deaths[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2021-09-09"] <- 253


raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-14"] <- 72
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-15"] <- 65
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-16"] <- 46
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-17"] <- 46
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-18"] <- 53
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-19"] <- 48

raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-14"] <- 917
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-15"] <- 2785
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-16"] <- 2642
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-17"] <- 2432
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-18"] <- 2412
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2021-09-19"] <- 2291

raw_data$Cases[raw_data$`Countries.and.territories` == "Serbia" & raw_data$DateRep == "2021-09-19"] <- 6745
raw_data$Deaths[raw_data$`Countries.and.territories` == "Serbia" & raw_data$DateRep == "2021-09-19"] <- 35

raw_data$Deaths[raw_data$`Countries.and.territories` == "Sri_Lanka" & raw_data$DateRep == "2021-10-08"] <- 38
raw_data$Deaths[raw_data$`Countries.and.territories` == "Sri_Lanka" & raw_data$DateRep == "2021-10-09"] <- 29

raw_data$Cases[raw_data$`Countries.and.territories` == "Sri_Lanka" & raw_data$DateRep == "2021-10-08"] <- 1387
raw_data$Cases[raw_data$`Countries.and.territories` == "Sri_Lanka" & raw_data$DateRep == "2021-10-09"] <- 726


## 18th October corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Australia" & raw_data$DateRep == "2021-10-14"] <- 18
raw_data$Deaths[raw_data$`Countries.and.territories` == "Australia" & raw_data$DateRep == "2021-10-15"] <- 17
raw_data$Deaths[raw_data$`Countries.and.territories` == "Colombia" & raw_data$DateRep == "2021-10-16"] <- 35
raw_data$Deaths[raw_data$`Countries.and.territories` == "Cuba" & raw_data$DateRep == "2021-10-16"] <- 24
raw_data$Deaths[raw_data$`Countries.and.territories` == "Cuba" & raw_data$DateRep == "2021-10-17"] <- 19

raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2021-10-16"] <- 381
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2021-10-17"] <- 434

raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2021-10-16"] <- 17
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2021-10-17"] <- 24

raw_data$Deaths[raw_data$`Countries.and.territories` == "Romania" & raw_data$DateRep == "2021-10-17"] <- 298

raw_data$Deaths[raw_data$`Countries.and.territories` == "Turkey" & raw_data$DateRep == "2021-11-20"] <- 218
raw_data$Cases[raw_data$`Countries.and.territories` == "Turkey" & raw_data$DateRep == "2021-11-20"] <- 23810


## 30th November corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Algeria" & raw_data$DateRep == "2021-11-21"] <- 2
raw_data$Deaths[raw_data$`Countries.and.territories` == "Algeria" & raw_data$DateRep == "2021-11-22"] <- 4

raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2021-11-21"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2021-11-22"] <- 10


## 7th December corrections

raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2021-12-05"] <- 5

raw_data$Deaths[raw_data$`Countries.and.territories` == "India" & raw_data$DateRep == "2021-12-05"] <- 372

raw_data$Deaths[raw_data$`Countries.and.territories` == "Paraguay" & raw_data$DateRep == "2021-11-29"] <- 1


## 21st December corrections

raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2021-12-19"] <- 24

## 10th January 2022 corrections

raw_data$Deaths[raw_data$`Countries.and.territories` == "Angola" & raw_data$DateRep == "2022-01-04"] <- 3
raw_data$Deaths[raw_data$`Countries.and.territories` == "Angola" & raw_data$DateRep == "2022-01-05"] <- 3
raw_data$Deaths[raw_data$`Countries.and.territories` == "Angola" & raw_data$DateRep == "2022-01-09"] <- 10

raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina" & raw_data$DateRep == "2022-01-02"] <- 23
raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina" & raw_data$DateRep == "2022-01-03"] <- 12

raw_data$Deaths[raw_data$`Countries.and.territories` == "Australia" & raw_data$DateRep == "2021-12-26"] <- 6
raw_data$Deaths[raw_data$`Countries.and.territories` == "Australia" & raw_data$DateRep == "2021-12-27"] <- 2

raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-01-02"] <- 31
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-01-03"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-01-09"] <- 24

raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2022-01-09"] <- 84

raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2022-01-06"] <- 27
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2022-01-07"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2022-01-08"] <- 27
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2022-01-09"] <- 26

raw_data$Deaths[raw_data$`Countries.and.territories` == "Ethiopia" & raw_data$DateRep == "2021-12-28"] <- 6
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ethiopia" & raw_data$DateRep == "2021-12-29"] <- 0
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ethiopia" & raw_data$DateRep == "2022-01-01"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ethiopia" & raw_data$DateRep == "2022-01-02"] <- 10

raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2022-01-07"] <- 24
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2022-01-08"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2022-01-09"] <- 26

raw_data$Deaths[raw_data$`Countries.and.territories` == "Iraq" & raw_data$DateRep == "2022-01-07"] <- 3
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iraq" & raw_data$DateRep == "2022-01-08"] <- 4
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iraq" & raw_data$DateRep == "2022-01-09"] <- 2

raw_data$Deaths[raw_data$`Countries.and.territories` == "Jamaica" & raw_data$DateRep == "2022-01-04"] <- 1
raw_data$Deaths[raw_data$`Countries.and.territories` == "Jamaica" & raw_data$DateRep == "2022-01-05"] <- 2

raw_data$Deaths[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2022-01-07"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2022-01-08"] <- 21
raw_data$Deaths[raw_data$`Countries.and.territories` == "Jordan" & raw_data$DateRep == "2022-01-09"] <- 25

raw_data$Deaths[raw_data$`Countries.and.territories` == "Kenya" & raw_data$DateRep == "2022-01-09"] <- 12

raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2022-01-06"] <- 20
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2022-01-07"] <- 18
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2022-01-08"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2022-01-09"] <- 17

raw_data$Deaths[raw_data$`Countries.and.territories` == "Libya" & raw_data$DateRep == "2022-01-07"] <- 15
raw_data$Deaths[raw_data$`Countries.and.territories` == "Libya" & raw_data$DateRep == "2022-01-08"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Libya" & raw_data$DateRep == "2022-01-09"] <- 10

raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2022-01-06"] <- 5
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2022-01-07"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2022-01-08"] <- 13
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2022-01-09"] <- 7

raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-20"] <- 2
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-24"] <- 1
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-25"] <- 4
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-26"] <- 2
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-27"] <- 1
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-28"] <- 5
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-30"] <- 12
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2021-12-31"] <- 8
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2022-01-01"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mozambique" & raw_data$DateRep == "2022-01-02"] <- 6

raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2022-01-07"] <- 5
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2022-01-08"] <- 6
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2022-01-09"] <- 7

raw_data$Deaths[raw_data$`Countries.and.territories` == "Palestine" & raw_data$DateRep == "2022-01-06"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Palestine" & raw_data$DateRep == "2022-01-07"] <- 3
raw_data$Deaths[raw_data$`Countries.and.territories` == "Palestine" & raw_data$DateRep == "2022-01-08"] <- 6
raw_data$Deaths[raw_data$`Countries.and.territories` == "Palestine" & raw_data$DateRep == "2022-01-09"] <- 9

raw_data$Deaths[raw_data$`Countries.and.territories` == "South_Africa" & raw_data$DateRep == "2022-01-07"] <- 45

raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2021-12-25"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2021-12-26"] <- 37
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-03"] <- 14
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-04"] <- 31
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-05"] <- 22
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-08"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-09"] <- 18

raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-01"] <- 6
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-02"] <- 3
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-03"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-04"] <- 9
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-05"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-06"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-07"] <- 8
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-08"] <- 8
raw_data$Deaths[raw_data$`Countries.and.territories` == "Zambia" & raw_data$DateRep == "2022-01-09"] <- 14


## 17th January 2022 corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2022-01-16"] <- 127
raw_data$Deaths[raw_data$`Countries.and.territories` == "Libya" & raw_data$DateRep == "2022-01-16"] <- 13

## 24th Jan 2022
## 19/01 - 17101, 59
## 17/01 - 76
## 18/01 - 59
## 16/01 - 227
## 15/01 - 195
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2022-01-16"] <- 227
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2022-01-17"] <- 59
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2022-01-18"] <- 76
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2022-01-19"] <- 59

## 31st Jan 2022 corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-01-24"] <- 38
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-01-25"] <- 22
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-01-30"] <- 41

raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2022-01-30"] <- 140

raw_data$Deaths[raw_data$`Countries.and.territories` == "Jamaica" & raw_data$DateRep == "2022-01-30"] <- 11

raw_data$Deaths[raw_data$`Countries.and.territories` == "Libya" & raw_data$DateRep == "2022-01-30"] <- 14

raw_data$Deaths[raw_data$`Countries.and.territories` == "Panama" & raw_data$DateRep == "2022-01-27"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Panama" & raw_data$DateRep == "2022-01-28"] <- 17

raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-23"] <- 24
raw_data$Deaths[raw_data$`Countries.and.territories` == "Trinidad_and_Tobago" & raw_data$DateRep == "2022-01-24"] <- 8
## worldometers
raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina" & raw_data$DateRep == "2022-02-04"] <- 321
raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina" & raw_data$DateRep == "2022-02-05"] <- 318

raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2022-02-09"] <- 132
raw_data$Cases[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep == "2022-02-09"] <- 6343

## 28th Feb 2022 corrections
raw_data$Cases[raw_data$`Countries.and.territories` == "Sri_Lanka" & raw_data$DateRep == "2022-02-11"] <- 31
raw_data$Cases[raw_data$`Countries.and.territories` == "Sri_Lanka" & raw_data$DateRep == "2022-02-12"] <- 23

raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-02-25"] <- 8
raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-02-26"] <- 5
raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2022-02-27"] <- 7

raw_data$Cases[raw_data$`Countries.and.territories` == "Portugal" & raw_data$DateRep == "2022-02-26"] <- 32
raw_data$Cases[raw_data$`Countries.and.territories` == "Portugal" & raw_data$DateRep == "2022-02-27"] <- 28

raw_data$Cases[raw_data$`Countries.and.territories` == "United_Kingdom" & raw_data$DateRep == "2022-02-27"] <- 45

## 7 March 2022 corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Japan" & raw_data$DateRep == "2022-02-28"] <- 176
raw_data$Deaths[raw_data$`Countries.and.territories` == "Japan" & raw_data$DateRep == "2022-03-01"] <- 187

## 29 March 2022 corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Chile" & raw_data$DateRep == "2022-03-22"] <- 0

raw_data$Deaths[raw_data$`Countries.and.territories` == "India" & raw_data$DateRep == "2022-03-26"] <- 95

## 4 April 2022 corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Japan" & raw_data$DateRep == "2022-03-21"] <- 74
raw_data$Deaths[raw_data$`Countries.and.territories` == "Japan" & raw_data$DateRep == "2022-03-22"] <- 57

raw_data$Deaths[raw_data$`Countries.and.territories` == "Ireland" & raw_data$DateRep == "2022-04-03"] <- 13

## 11 April 2022 corrections
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2022-04-01"] <- 6
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2022-04-02"] <- 4



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

by_country_cases_all <- dplyr::select(
  raw_data, dates = DateRep, Cases, Countries.and.territories
) %>%
  tidyr::spread(
    key = Countries.and.territories, value = Cases, fill = 0
  )

saveRDS(
  object = by_country_cases_all,
  file = "latest_cases_wide_no_filter.rds"
)

## Apply thresholds
pass <- split(raw_data, raw_data$`Countries.and.territories`) %>%
  keep(rincewind::deaths_threshold) %>%
  bind_rows()

## Always keep "United_Kingdom" and Slovenia
if (! "United_Kingdom" %in% unique(pass$Countries.and.territories)) {
  x <- raw_data[raw_data$`Countries.and.territories` == "United_Kingdom", ]
  pass <- rbind(pass, x)
}
if (! "Slovenia" %in% unique(pass$Countries.and.territories)) {
  x <- raw_data[raw_data$`Countries.and.territories` == "Slovenia", ]
  pass <- rbind(pass, x)
}

## Still have some negative cases. Replace the negative case count
## with an average of previous and later 3 days.
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

raw_data <- split(raw_data, raw_data$`Countries.and.territories`) %>%
  map_dfr(
    function(df) {
      if (all(df$Deaths > 0)) return(df)
      idx <- which(df$Deaths < 0)
      for (i in idx) {
        date_neg <- df$DateRep[i]
        dates_to_avg <- seq(
          from = as.Date(date_neg) - 3,
          to = as.Date(date_neg) + 3,
          by = "1 day"
        )
        dates_to_avg <- dates_to_avg[dates_to_avg != as.Date(date_neg)]
        dates_to_avg <- dates_to_avg[dates_to_avg <= max(as.Date(df$DateRep))]
        df$Deaths[i] <- round(
          mean(df$Deaths[df$DateRep %in% dates_to_avg])
        )
      }
      df
    }
  )


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
  dplyr::filter(DateRep <= week_ending)


## For consistency with Pierre's code, rename DateRep to dates
cases_to_use <- dplyr::rename(by_country_cases, dates = "DateRep")

deaths_to_use <- dplyr::rename(by_country_deaths, dates = "DateRep")

Country <- colnames(deaths_to_use)[!colnames(deaths_to_use) == "dates"]

exclude <- c(
  ## 27 deaths in WHO data on 26th June. Time series toally different from worldometers
  # "Australia", # excluded 04/04/22 - huge outlier in deaths, no worldometer match
  "Argentina", # excluded 29/03/22 - all historic data for past few months seems off
  ##"Armenia",
  "Belgium", # excluded 14th/21st dec (weekend data delayed)
  "Bosnia_and_Herzegovina",
  "Botswana",
  # "Brazil", # excluded 1st March (delay and no worldometer match)
  "Burkina_Faso",
  #"Canada", # excluded 1st March (erratic reporting over last week & no worldometer match)
  "Cameroon", # excluded 14th dec (erratic)
  "Cape_Verde",
  # "Costa_Rica",
  "Cuba",
  "Cyprus",
  "Democratic_Republic_of_the_Congo",
  "Ecuador", ## Massive backlog reported & some negative deaths,
  "Egypt", # weekly reporting
  "El_Salvador",
  "Fiji", # excluded 31st Jan
  "Ghana", # exclude 18 Jan (no weekends)
  "Guatemala",
  "Honduras", # excluded 31st Jan
  "Hungary", # excluded 14th dec
  #"Ireland",
  #"Israel", # included again 31st Jan
  "Jordan", # weekly reporting
  "Kazakhstan",
  "Kosovo",
  "Kyrgyzstan",
  "Laos",
  "Lesotho",
  "Libya",
  "Madagascar", # excluded 10th Jan (weekly reporting)
  "Malta",
  "Mauritius",
  "Mauritania",
  "Mexico",
  "Namibia",
  "Nepal",
  "Norway", # weekly reporting
  "Oman",
  "Paraguay", 
  # "Portugal", # delay and can't find matches in worldometer
  "Philippines", ## Erratic data
  "Rwanda",
  "Saint_Lucia",
  "Syria",
  "Sudan",
  # "Spain", # delay and can't find matches in worldometer
  "Sweden",
  "Switzerland", # delay and can't find matches in worldometer
  "Trinidad_and_Tobago", # excluded 1st March (seems to be missing data & no worldometer match)
  "Tunisia", # excluded again 1st March (delay & no match)
  "Uganda",
  "Vietnam",
  "Yemen",
  "Zimbabwe",
  "United_Republic_of_Tanzania",
  "Ukraine",
  "United_Kingdom" # data missing over weekend and reporting delays
  
  ##" United_States_of_America", ## Missing data
  ## Discuss list later
)

saveRDS(exclude, "exclude.rds")

##Country <- Country[!Country %in% exclude]

x <- list(
  date_week_ending = week_ending,
  Threshold_criterion_4weeks = params$Threshold_criterion_4weeks,
  Threshold_criterion_7days = params$Threshold_criterion_7days,
  I_active_transmission = cases_to_use,
  D_active_transmission = deaths_to_use,
  Country = Country,
  si_mean = params$si_mean,
  si_std = params$si_std
)


## Also save it with a generic name to avoid having to configure the
## downstream tasks

out <- saveRDS(object = x, file = "latest_model_input.rds")


## exclude <- c(
##   "China", "United_States_of_America",
##   "Ethiopia", "Ghana", "Kazakhstan",
##   "Zambia", "Kyrgyzstan", "Oman", "Zimbabwe"
## )



################# Check data
pass$DateRep <- as.Date(pass$DateRep)

plots <- split(pass, pass$`Countries.and.territories`) %>%
  map(
    function(df) {
      p <- ggplot(df) +
        geom_point(aes(DateRep, Deaths)) +
        facet_wrap(
          ~`Countries.and.territories`, scales = "free_y", ncol = 1
        ) +
        scale_x_date(limits = c(as.Date("2020-03-01") , NA)) +
        theme_minimal()
    }
  )


pdf("epicurves.pdf")
plots
dev.off()
