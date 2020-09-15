######################################################################
######################################################################
######################################################################
######################################################################
########## Corrections 13th September ################################
######################################################################
######################################################################
######################################################################


who_Bangladesh <- who[who$country == "Bangladesh" & who$date_reported %in% last_2months, ]
ecdc_Bangladesh <- raw_data[raw_data$`Countries.and.territories` == "Bangladesh", ]
ecdc_Bangladesh <- ecdc_Bangladesh[ecdc_Bangladesh$DateRep %in% last_2months, ]
df <- dplyr::left_join(who_Bangladesh, ecdc_Bangladesh, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Bangladesh" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bangladesh" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

who_Morocco <- who[who$country == "Morocco" & who$date_reported %in% last_2months, ]
ecdc_Morocco <- raw_data[raw_data$`Countries.and.territories` == "Morocco", ]
ecdc_Morocco <- ecdc_Morocco[ecdc_Morocco$DateRep %in% last_2months, ]
df <- dplyr::left_join(who_Morocco, ecdc_Morocco, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Morocco" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths



## WHO report 0 new cases and 0 new deaths for Israel from 10th to 13th
## Sept. We take these values from ECDC
last_2monthsa <- head(last_2months, -4)
who_Israel <- who[who$country == "Israel" & who$date_reported %in% last_2monthsa, ]
ecdc_Israel <- raw_data[raw_data$`Countries.and.territories` == "Israel", ]
ecdc_Israel <- ecdc_Israel[ecdc_Israel$DateRep %in% last_2monthsa, ]
df <- dplyr::left_join(who_Israel, ecdc_Israel, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

last_2monthsb <- head(last_2months, -1)
who_Bolivia <- who[who$country == "Bolivia (Plurinational State of)" & who$date_reported %in% last_2monthsb, ]
ecdc_Bolivia <- raw_data[raw_data$`Countries.and.territories` == "Bolivia", ]
ecdc_Bolivia <- ecdc_Bolivia[ecdc_Bolivia$DateRep %in% last_2monthsb, ]
df <- dplyr::left_join(who_Bolivia, ecdc_Bolivia, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

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
