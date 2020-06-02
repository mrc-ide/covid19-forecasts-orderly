##googlesheets4::sheets_deauth()
deaths <- readr::read_csv("InCovid19 - Deaths.csv")
deaths <- janitor::clean_names(deaths)
deaths$state <- tolower(deaths$state)
deaths$date_death <- lubridate::dmy(deaths$date_death)
incid <- incidence::incidence(
  dates = deaths$date_death,
  groups = deaths$state
)

deaths_ts <- as.data.frame(incid)


cases <- readr::read_csv("InCovid19 - Line_listing.csv")
cases <- janitor::clean_names(cases)
cases$state <- tolower(cases$state)

## Manual Fix
idx <- which(cases$date_of_test_positivity == "3/10/2020")
cases$date_of_test_positivity[idx] <- "10 March 2020"
cases$date_of_test_positivity <- lubridate::dmy(cases$date_of_test_positivity)

cases_incid <- incidence::incidence(
  dates = cases$date_of_test_positivity,
  groups = cases$state
  )
cases_ts <- as.data.frame(cases_incid)

#cases_ts <- dplyr::rename(cases_ts, dates = "DateRep")
#deaths_ts <- dplyr::rename(deaths_ts, dates = "DateRep")

keep <- intersect(unique(cases$state), unique(deaths$state))
dates_to_keep <- which(cases_ts$dates %in% deaths_ts$dates)

shape <- 3.16
scale <- 1.52
                                        # hist(rgamma(1e4,shape = shape, scale = scale))
new_params <- epitrix::gamma_shapescale2mucv(
  shape = shape, scale = scale
)
si_mean <- new_params$mu
si_std <- new_params$mu * new_params$cv
## Neil suggested sensitivity analyses with a shorter and a longer time
## window
si_mean <- c(si_mean, 6.48)
si_std <- c(si_std, 3.83)

x <- list(
    I_active_transmission = cases_ts[dates_to_keep, c("dates", keep)],
    D_active_transmission = deaths_ts[, c("dates", keep)],
    Country = keep,
    si_mean = si_mean,
    si_std = si_std
)

out <- saveRDS(object = x, file = "india.rds")

