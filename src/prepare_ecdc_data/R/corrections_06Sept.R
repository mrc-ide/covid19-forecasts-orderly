######################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 6th September ################################
######################################################################
######################################################################
######################################################################
raw_data$Cases[raw_data$`Countries.and.territories` == "Dominican_Republic" & raw_data$DateRep == "2020-09-05"] <- 20
raw_data$Cases[raw_data$`Countries.and.territories` == "Dominican_Republic" & raw_data$DateRep == "2020-09-06"] <- 19

raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-09-05"] <- 39
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep == "2020-09-06"] <- 37


last_2months <- seq(from = as.Date("2020-07-01"), to = as.Date("2020-09-06"), by = "1 day")
who_India <- who[who$country == "India" & who$date_reported %in% last_2months, ]
ecdc_India <- raw_data[raw_data$`Countries.and.territories` == "India", ]
ecdc_India <- ecdc_India[ecdc_India$DateRep %in% last_2months, ]
df <- dplyr::left_join(who_India, ecdc_India, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "India" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "India" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

who_Mexico <- who[who$country == "Mexico" & who$date_reported %in% last_2months, ]
ecdc_Mexico <- raw_data[raw_data$`Countries.and.territories` == "Mexico", ]
ecdc_Mexico <- ecdc_Mexico[ecdc_Mexico$DateRep %in% last_2months, ]
df <- dplyr::left_join(who_Mexico, ecdc_Mexico, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Mexico" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths
