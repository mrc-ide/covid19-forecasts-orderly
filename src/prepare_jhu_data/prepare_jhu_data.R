params <- parameters(week_ending)


## Read in Johns Hopkins data

## Case data
## Convert to long format, aggregate by state and compute daily case numbers
cases <-  read_csv("covid19_confirmed_US.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-c(
    uid, iso2, iso3, code3, fips, country_region,
    lat, long, combined_key
  )
  )

cases <- cases %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "date_reported",
    names_prefix = "x",
    values_to = "all_cases"
  )

cases$date_reported <- lubridate::mdy(cases$date_reported)

cases <- cases %>%
  dplyr::group_by(province_state, date_reported) %>%
  dplyr::summarise(all_cases = sum(all_cases)) %>%
  dplyr::mutate(new_cases = diff(c(0, all_cases)))

## Deaths data
## Convert to long format, aggregate by state and compute daily death numbers

deaths <- read_csv("covid19_deaths_US.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-c(
    uid, iso2, iso3, code3, fips, country_region,
    lat, long, population, combined_key
  )
  )

deaths <- deaths %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "date_reported",
    names_prefix = "x",
    values_to = "all_deaths"
  )

deaths$date_reported <- lubridate::mdy(deaths$date_reported)

deaths <- deaths %>%
  dplyr::group_by(province_state, date_reported) %>%
  dplyr::summarise(all_deaths = sum(all_deaths)) %>%
  dplyr::mutate(new_deaths = diff(c(0, all_deaths)))

## Join case and death data

raw_data <- dplyr::left_join(cases, deaths,
                             by = c("province_state", "date_reported")) %>%
  dplyr::select(date_reported, everything()) %>%
  dplyr::filter(date_reported <= as.Date(week_ending))


## Identify province_state locations to be removed
remove_location <- c(
  "Diamond Princess",
  "Grand Princess"
)

raw_data <- filter(raw_data, !(province_state %in% remove_location))

## Rename columns of Johns Hopkins data to be consistent with previous code

raw_data <- rename(
  raw_data, Cases = "new_cases", Deaths = "new_deaths",
  DateRep = "date_reported"
) %>%
  select(DateRep, province_state, Cases, Deaths)

## Average out any negative reports over the preceding and subsequent 3 days
## Note this still leaves negative case numbers for three dates in Puerto Rico
## 2020-04-22, 2020-04-23, 2020-04-25
## Might need some manual correction of data

raw_data <- split(raw_data, raw_data$province_state) %>%
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

raw_data <- split(raw_data, raw_data$province_state) %>%
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


## Manual cleaning for days where reported deaths are much higher or lower (e.g due to batch reporting)

##########################################
## Corrections for 12 April 2021 report ##
##########################################

## California
## 25 March 2021 entry corrected with Worldometer value.

raw_data$Deaths[raw_data$DateRep == "2021-03-25" & raw_data$province_state == "California"] <- 242

## Kentucky
## Entries from 25, 27 and 28 March 2021 corrected with Worldometer values.

raw_data$Deaths[raw_data$DateRep == "2021-03-25" & raw_data$province_state == "Kentucky"] <- 19
raw_data$Deaths[raw_data$DateRep == "2021-03-27" & raw_data$province_state == "Kentucky"] <- 15
raw_data$Deaths[raw_data$DateRep == "2021-03-28" & raw_data$province_state == "Kentucky"] <- 8

## New York
## Large entry on 24 March 2021 distributed over 23/24 March as per Worldometer.

raw_data$Deaths[raw_data$DateRep == "2021-03-23" & raw_data$province_state == "New York"] <- 131
raw_data$Deaths[raw_data$DateRep == "2021-03-24" & raw_data$province_state == "New York"] <- 154

## Oklahoma
## Batch upload of ~1700 deaths on 7 April 2021. As per COVID-19 Forecast Hub email from Jeremy Ratcliff (10/4/21)

raw_data$Deaths[raw_data$DateRep == "2021-04-07" & raw_data$province_state == "Oklahoma"] <- 16

## West Virginia
## 165 backlogged deaths published on 12 March 2021. As per COVID-19 Forecast Hub email from Jeremy Ratcliff (14/4/21)

raw_data$Deaths[raw_data$DateRep == "2021-03-12" & raw_data$province_state == "West Virginia"] <- 8


##########################################
## Corrections for 19 April 2021 report ##
##########################################

## Kentucky
## Historical deaths added on March 18th (417 deaths) and 19th (166 deaths) 2021.
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff.
raw_data$Deaths[raw_data$DateRep == "2021-03-18" & raw_data$province_state == "Kentucky"] <- 31
raw_data$Deaths[raw_data$DateRep == "2021-03-19" & raw_data$province_state == "Kentucky"] <- 25

## Michigan
## Ongoing death certificate review taking place. Backlogged deaths have been added since 1 April 2021.
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff.

raw_data$Deaths[raw_data$DateRep == "2021-04-01" & raw_data$province_state == "Michigan"] <- 55 - 33
raw_data$Deaths[raw_data$DateRep == "2021-04-03" & raw_data$province_state == "Michigan"] <- 65 - 51
raw_data$Deaths[raw_data$DateRep == "2021-04-06" & raw_data$province_state == "Michigan"] <- 61 - 16
raw_data$Deaths[raw_data$DateRep == "2021-04-08" & raw_data$province_state == "Michigan"] <- 77 - 43
raw_data$Deaths[raw_data$DateRep == "2021-04-10" & raw_data$province_state == "Michigan"] <- 84 - 57
raw_data$Deaths[raw_data$DateRep == "2021-04-13" & raw_data$province_state == "Michigan"] <- 81 - 37
raw_data$Deaths[raw_data$DateRep == "2021-04-15" & raw_data$province_state == "Michigan"] <- 123 - 81
raw_data$Deaths[raw_data$DateRep == "2021-04-17" & raw_data$province_state == "Michigan"] <- 76 - 60

## Montana
## Backlogged deaths added on 9 April 2021..
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff.

raw_data$Deaths[raw_data$DateRep == "2021-04-09" & raw_data$province_state == "Montana"] <- 27 - 26

## Texas
## Local dashboard failed to update on 31 March. Double upload occurred on 1 April.
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff.
## Replace JHU data for 31 March & 1 April with data from Worldometer for those days.

raw_data$Deaths[raw_data$DateRep == "2021-03-31" & raw_data$province_state == "Texas"] <- 97
raw_data$Deaths[raw_data$DateRep == "2021-04-01" & raw_data$province_state == "Texas"] <- 136


##########################################
## Corrections for 26 April 2021 report ##
##########################################

## Michigan
## Ongoing death certificate review taking place. Backlogged deaths have been added since 1 April 2021.
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff.

raw_data$Deaths[raw_data$DateRep == "2021-04-20" & raw_data$province_state == "Michigan"] <- 90 - 33
raw_data$Deaths[raw_data$DateRep == "2021-04-22" & raw_data$province_state == "Michigan"] <- 121 - 75
raw_data$Deaths[raw_data$DateRep == "2021-04-24" & raw_data$province_state == "Michigan"] <- 126 - 91


##########################################
## Corrections for 10 May 2021 report ##
##########################################

## Michigan
## Ongoing death certificate review taking place. Backlogged deaths have been added since 1 April 2021.
## Source1: COVID-19 Forecast Hub email from Jeremy Ratcliff.
## Source2: https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html

raw_data$Deaths[raw_data$DateRep == "2021-04-27" & raw_data$province_state == "Michigan"] <- 117 - 48
raw_data$Deaths[raw_data$DateRep == "2021-04-29" & raw_data$province_state == "Michigan"] <- 116 - 78
raw_data$Deaths[raw_data$DateRep == "2021-05-01" & raw_data$province_state == "Michigan"] <- 140 - 98
raw_data$Deaths[raw_data$DateRep == "2021-05-04" & raw_data$province_state == "Michigan"] <- 133 - 51
raw_data$Deaths[raw_data$DateRep == "2021-05-06" & raw_data$province_state == "Michigan"] <- 123 - 92
raw_data$Deaths[raw_data$DateRep == "2021-05-08" & raw_data$province_state == "Michigan"] <- 124 - 83


## Montana
## All deaths reported on 7 May were backlogged.
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff (10/5/21).

raw_data$Deaths[raw_data$DateRep == "2021-05-07" & raw_data$province_state == "Montana"] <- 0


##########################################
## Corrections for 17 May 2021 report ##
##########################################

## Michigan
## Ongoing death certificate review taking place. Backlogged deaths have been added since 1 April 2021.
## Source1: COVID-19 Forecast Hub email from Jeremy Ratcliff.
## Source2: https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html

raw_data$Deaths[raw_data$DateRep == "2021-05-11" & raw_data$province_state == "Michigan"] <- 102 - 32
raw_data$Deaths[raw_data$DateRep == "2021-05-13" & raw_data$province_state == "Michigan"] <- 118 - 73
raw_data$Deaths[raw_data$DateRep == "2021-05-15" & raw_data$province_state == "Michigan"] <- 112 - 91


##########################################
## Corrections for 24 May 2021 report ##
##########################################

## Michigan
## Ongoing death certificate review taking place. Backlogged deaths have been added since 1 April 2021.
## Source1: COVID-19 Forecast Hub email from Jeremy Ratcliff.
## Source2: https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html

raw_data$Deaths[raw_data$DateRep == "2021-05-18" & raw_data$province_state == "Michigan"] <- 87 - 27
raw_data$Deaths[raw_data$DateRep == "2021-05-20" & raw_data$province_state == "Michigan"] <- 82 - 61


##########################################
## Corrections for 7 June 2021 report ##
##########################################

## Kentucky
## Some deaths reported on 1 June were backlogged.
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff (6/6/21).

raw_data$Deaths[raw_data$DateRep == "2021-06-01" & raw_data$province_state == "Kentucky"] <- 285 - 260

## Maryland
## Backlogged deaths reported on 27 May
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff (1/6/21).

raw_data$Deaths[raw_data$DateRep == "2021-05-27" & raw_data$province_state == "Maryland"] <- 544 - 538

## Wisconsin
## Backlogged deaths reported on 27 May
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff (1/6/21).

raw_data$Deaths[raw_data$DateRep == "2021-05-27" & raw_data$province_state == "Wisconsin"] <- 49 - 41
raw_data$Deaths[raw_data$DateRep == "2021-05-31" & raw_data$province_state == "Wisconsin"] <- 26 - 15


##########################################
## Corrections for 21 June 2021 report ##
##########################################

## West Virginia
## Some deaths reported on 9 June were from death certificate review
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff (15/6/21).

raw_data$Deaths[raw_data$DateRep == "2021-06-09" & raw_data$province_state == "West Virginia"] <- 24 - 18

## Alaska
## Some deaths reported on 11 June were from death certificate review
## Source: COVID-19 Forecast Hub email from Jeremy Ratcliff (15/6/21).

raw_data$Deaths[raw_data$DateRep == "2021-06-09" & raw_data$province_state == "Alaska"] <- 4 - 4




## Save wide versions of death and case data

by_state_deaths_all <- dplyr::select(
  raw_data, dates = DateRep, Deaths, province_state
) %>%
  tidyr::spread(
    key = province_state, value = Deaths, fill = 0
  )

saveRDS(
  object = by_state_deaths_all,
  file = "latest_deaths_wide_no_filter.rds"
)

by_state_cases_all <- dplyr::select(
  raw_data, dates = DateRep, Cases, province_state
) %>%
  tidyr::spread(
    key = province_state, value = Cases, fill = 0
  )

saveRDS(
  object = by_state_cases_all,
  file = "latest_cases_wide_no_filter.rds"
)


## Apply thresholds
pass <- split(raw_data, raw_data$province_state) %>%
  purrr::keep(rincewind::deaths_threshold) %>%
  dplyr::bind_rows()



by_state_deaths <- dplyr::select(
  pass, DateRep, Deaths, province_state
) %>%
  tidyr::spread(
    key = province_state, value = Deaths, fill = 0
  )

## No lines means no cases for that day. That is why fill is 0.
by_state_cases <- dplyr::select(
  pass, DateRep, Cases, province_state
) %>%
  tidyr::spread(
    key = province_state, value = Cases, fill = 0
  )

## For consistency with Pierre's code, rename DateRep to dates
cases_to_use <- dplyr::rename(by_state_cases, dates = "DateRep")

deaths_to_use <- dplyr::rename(by_state_deaths, dates = "DateRep")

State <- colnames(deaths_to_use)[!colnames(deaths_to_use) == "dates"]


x <- list(
  date_week_ending = week_ending,
  I_active_transmission = cases_to_use,
  D_active_transmission = deaths_to_use,
  State = State,
  si_mean = params$si_mean,
  si_std = params$si_std
)

## Also save it with a generic name to avoid having to configure the
## downstream tasks

out <- saveRDS(object = x, file = "latest_model_input.rds")



################# Check data
pass$DateRep <- as.Date(pass$DateRep)

plots <- split(pass, pass$province_state) %>%
  map(
    function(df) {
      p <- ggplot(df) +
        geom_point(aes(DateRep, Deaths)) +
        facet_wrap(
          ~province_state, scales = "free_y", ncol = 1
        ) +
        scale_x_date(limits = c(as.Date("2020-03-01") , NA)) +
        theme_minimal()
    }
  )


pdf("epicurves.pdf")
plots
dev.off()

