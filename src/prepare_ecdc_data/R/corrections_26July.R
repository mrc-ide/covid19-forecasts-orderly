#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 26th July ####################################
######################################################################
######################################################################
######################################################################
# For Argentina, we use the data from WHO.
who <- readr::read_csv("WHO-COVID-19-global-data.csv") %>%
  janitor::clean_names()

who$date_reported <- lubridate::ymd(who$date_reported)

who_argentina <- who[who$country == "Argentina", ]
ecdc_argentina <- raw_data[raw_data$`Countries.and.territories` == "Argentina", ]
who_argentina <- who_argentina[who_argentina$date_reported %in% ecdc_argentina$DateRep, ]
df <- dplyr::left_join(who_argentina, ecdc_argentina, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Argentina"] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Argentina"] <- df$new_deaths


who_elsalv <- who[who$country == "El Salvador", ]
ecdc_elsalv <- raw_data[raw_data$`Countries.and.territories` == "El_Salvador", ]
who_elsalv <- who_elsalv[who_elsalv$date_reported %in% ecdc_elsalv$DateRep, ]
df <- dplyr::left_join(who_elsalv, ecdc_elsalv, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "El_Salvador"] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "El_Salvador"] <- df$new_deaths

raw_data$Deaths[raw_data$`Countries.and.territories` == "Guatemala" & raw_data$DateRep == "2020-07-26"] <- 30
raw_data$Deaths[raw_data$`Countries.and.territories` == "Peru" & raw_data$DateRep == "2020-07-24"] <- 188
raw_data$Deaths[raw_data$`Countries.and.territories` == "Serbia" & raw_data$DateRep == "2020-07-26"] <- 8
raw_data$Cases[raw_data$`Countries.and.territories` == "Serbia" & raw_data$DateRep == "2020-07-26"] <- 411

raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-26"] <- 195
raw_data$Cases[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-26"] <- 2316
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-25"] <- 215
raw_data$Cases[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep == "2020-07-25"] <- 2489

# For Russia, we use the data from WHO.

who_russia <- who[who$country == "Russian Federation", ]
ecdc_russia <- raw_data[raw_data$`Countries.and.territories` == "Russia", ]
who_russia <- who_russia[who_russia$date_reported %in% ecdc_russia$DateRep, ]
df <- dplyr::left_join(who_russia, ecdc_russia, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Russia" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Russia" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

last_week <- seq(from = as.Date("2020-07-12"), to = as.Date("2020-07-19"), by = "1 day")
who_ukraine <- who[who$country == "Ukraine" & who$date_reported %in% last_week, ]
ecdc_ukraine <- raw_data[raw_data$`Countries.and.territories` == "Ukraine", ]
ecdc_ukraine <- ecdc_ukraine[ecdc_ukraine$DateRep %in% last_week, ]
df <- dplyr::left_join(who_ukraine, ecdc_ukraine, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths