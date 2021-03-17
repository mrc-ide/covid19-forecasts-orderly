params <- parameters(week_ending)


## Read in Johns Hopkins data

## Case data
## Convert to long format, aggregate by state and compute daily case numbers
cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
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

deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
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

