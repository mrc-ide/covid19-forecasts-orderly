week_finishing <- "2020-09-13"
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

raw_data$Deaths[raw_data$DateRep == "2020-04-17" & raw_data$`Countries.and.territories` == "China"] <- 0
## 04th May 2020. Manual tweaks against worldometer
raw_data$Deaths[raw_data$DateRep == "2020-05-01" & raw_data$`Countries.and.territories` == "Germany"] <- 156
raw_data$Deaths[raw_data$DateRep == "2020-05-02" & raw_data$`Countries.and.territories` == "Germany"] <- 113
raw_data$Deaths[raw_data$DateRep == "2020-05-03" & raw_data$`Countries.and.territories` == "Ireland"] <- 21
raw_data$Deaths[raw_data$DateRep == "2020-04-27" & raw_data$`Countries.and.territories` == "Spain"] <- 331
raw_data$Deaths[raw_data$DateRep == "2020-04-28" & raw_data$`Countries.and.territories` == "Spain"] <- 301
raw_data$Deaths[raw_data$DateRep == "2020-05-22" & raw_data$`Countries.and.territories` == "Spain"] <- 53
raw_data$Deaths[raw_data$DateRep == "2020-05-16" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 17
raw_data$Deaths[raw_data$DateRep == "2020-05-17" & raw_data$`Countries.and.territories` == "Afghanistan"] <- 15
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

#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 12th July ####################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$DateRep == "2020-07-12" & raw_data$`Countries.and.territories` == "Algeria"] <- 8
raw_data$Deaths[raw_data$DateRep == "2020-07-11" & raw_data$`Countries.and.territories` == "Algeria"] <- 8
raw_data$Deaths[raw_data$DateRep == "2020-07-11" & raw_data$`Countries.and.territories` == "Haiti"] <- 7
raw_data$Deaths[raw_data$DateRep == "2020-07-12" & raw_data$`Countries.and.territories` == "Haiti"] <- 5
raw_data$Deaths[raw_data$DateRep == "2020-07-11" & raw_data$`Countries.and.territories` == "Ukraine"] <- 27
raw_data$Deaths[raw_data$DateRep == "2020-07-12" & raw_data$`Countries.and.territories` == "Ukraine"] <- 11


#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 19th July ####################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$`Countries.and.territories` == "Canada" & raw_data$DateRep == "2020-07-19"] <- 9
raw_data$Deaths[raw_data$`Countries.and.territories` == "Chile" & raw_data$DateRep == "2020-07-18"] <- 98
raw_data$Deaths[raw_data$`Countries.and.territories` == "Chile" & raw_data$DateRep == "2020-07-19"] <- 58

## Egypt
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2020-07-13"] <- 89
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2020-07-14"] <- 77


raw_data$Deaths[raw_data$`Countries.and.territories` == "El_Salvador" & raw_data$DateRep == "2020-07-15"] <- 8
raw_data$Deaths[raw_data$`Countries.and.territories` == "El_Salvador" & raw_data$DateRep == "2020-07-16"] <- 12
raw_data$Deaths[raw_data$`Countries.and.territories` == "El_Salvador" & raw_data$DateRep == "2020-07-19"] <- 11

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
raw_data$Deaths[raw_data$`Countries.and.territories` == "Guatemala" & raw_data$DateRep == "2020-07-26"] <- 30
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-07-24"] <- 188
raw_data$Deaths[raw_data$`Countries.and.territories` == "Serbia" & raw_data$DateRep == "2020-07-26"] <- 8
raw_data$Cases[raw_data$`Countries.and.territories` == "Serbia" & raw_data$DateRep == "2020-07-26"] <- 411

raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-26"] <- 195
raw_data$Cases[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-26"] <- 2316
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-25"] <- 215
raw_data$Cases[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-25"] <- 2489
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-01"] <- 15
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-02"] <- 10

## Soruce Wikipedia; ECDC reports 40 on one day and -12 on the previous day
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-06"] <- 15
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-05"] <- 13


raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-07-31"] <- 26
raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-08-01"] <- 24
raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-08-02"] <- 21

raw_data$Deaths[raw_data$`Countries.and.territories` == "Sudan" & raw_data$DateRep == "2020-08-02"] <- 6
raw_data$Cases[raw_data$`Countries.and.territories` == "Sudan" & raw_data$DateRep == "2020-08-02"] <- 94

raw_data$Deaths[raw_data$`Countries.and.territories` == "Venezuela" & raw_data$DateRep == "2020-07-31"] <- 4
raw_data$Cases[raw_data$`Countries.and.territories` == "Venezuela" & raw_data$DateRep == "2020-07-30"] <- 3
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

raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2020-08-09"] <- 63
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ethiopia"& raw_data$DateRep == "2020-08-03"] <- 26
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ethiopia"& raw_data$DateRep == "2020-08-04"] <- 26
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran"& raw_data$DateRep == "2020-08-07"] <- 174
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran"& raw_data$DateRep == "2020-08-08"] <- 156

raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan"& raw_data$DateRep == "2020-08-01"] <- 19
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan"& raw_data$DateRep == "2020-08-02"] <- 8
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan"& raw_data$DateRep == "2020-08-03"] <- 8
######################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 16th August ¢##################################
######################################################################
######################################################################
######################################################################

raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2020-08-14"] <- 57
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2020-08-15"] <- 55
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep == "2020-08-15"] <- 64
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-08-14"] <- 277
######################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 23rd August ¢##################################
######################################################################
######################################################################
######################################################################

raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2020-08-19"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Egypt" & raw_data$DateRep == "2020-08-20"] <- 13

raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-08-20"] <- 153
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-08-21"] <- 139

## On 15 August, Emilia-Romagna added 154 deaths from March, April and May to its count.
raw_data$Deaths[raw_data$`Countries.and.territories` == "Italy" & raw_data$DateRep == "2020-08-16"] <- 4


raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2020-08-19"] <- 2
raw_data$Deaths[raw_data$`Countries.and.territories` == "Lebanon" & raw_data$DateRep == "2020-08-20"] <- 2

raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-08-18"] <- 23
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-08-19"] <- 33
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-08-20"] <- 29

raw_data$Deaths[raw_data$`Countries.and.territories` == "North_Macedonia" & raw_data$DateRep == "2020-08-21"] <- 3
raw_data$Deaths[raw_data$`Countries.and.territories` == "North_Macedonia" & raw_data$DateRep == "2020-08-22"] <- 3


raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2020-08-19"] <- 11
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2020-08-20"] <- 8

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
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-23"] <- 730
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-24"] <- 1809
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-25"] <- 1998
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-26"] <- 1943
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-27"] <- 2000
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-28"] <- 1597
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-29"] <- 1465
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-30"] <- 555

raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-23"] <- 15
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-24"] <- 13
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-25"] <- 12
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-26"] <- 16
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-27"] <- 9
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-28"] <- 10
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-29"] <- 12
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep == "2020-08-30"] <- 13

raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-08-24"] <- 30
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-08-25"] <- 32

raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2020-08-24"] <- 9
raw_data$Deaths[raw_data$`Countries.and.territories` == "Pakistan" & raw_data$DateRep == "2020-08-25"] <- 11

raw_data$Cases[raw_data$`Countries.and.territories` == "Dominican_Republic" & raw_data$DateRep == "2020-09-05"] <- 20
raw_data$Cases[raw_data$`Countries.and.territories` == "Dominican_Republic" & raw_data$DateRep == "2020-09-06"] <- 19

raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-09-05"] <- 39
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-09-06"] <- 37
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
  "Zimbabwe"
)
saveRDS(exclude, "exclude.rds")
