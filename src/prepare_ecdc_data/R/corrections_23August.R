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


who_Bolivia <- who[who$country == "Bolivia (Plurinational State of)", ]
ecdc_Bolivia <- raw_data[raw_data$`Countries.and.territories` == "Bolivia", ]
who_Bolivia <- who_Bolivia[who_Bolivia$date_reported %in% ecdc_Bolivia$DateRep, ]
df <- dplyr::left_join(who_Bolivia, ecdc_Bolivia, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Bolivia" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths


who_Israel <- who[who$country == "Israel", ]
ecdc_Israel <- raw_data[raw_data$`Countries.and.territories` == "Israel", ]
who_Israel <- who_Israel[who_Israel$date_reported %in% ecdc_Israel$DateRep, ]
df <- dplyr::left_join(who_Israel, ecdc_Israel, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Israel" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths
