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



who_Honduras <- who[who$country == "Honduras", ]
ecdc_Honduras <- raw_data[raw_data$`Countries.and.territories` == "Honduras", ]
who_Honduras <- who_Honduras[who_Honduras$date_reported %in% ecdc_Honduras$DateRep, ]
df <- dplyr::left_join(who_Honduras, ecdc_Honduras, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Honduras" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Honduras" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

who_Iran <- who[who$country == "Iran (Islamic Republic of)", ]
ecdc_Iran <- raw_data[raw_data$`Countries.and.territories` == "Iran", ]
who_Iran <- who_Iran[who_Iran$date_reported %in% ecdc_Iran$DateRep, ]
df <- dplyr::left_join(who_Iran, ecdc_Iran, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Iran" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

who_Spain <- who[who$country == "Spain", ]
ecdc_Spain <- raw_data[raw_data$`Countries.and.territories` == "Spain", ]
who_Spain <- who_Spain[who_Spain$date_reported %in% ecdc_Spain$DateRep, ]
df <- dplyr::left_join(who_Spain, ecdc_Spain, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

who_Ukraine <- who[who$country == "Ukraine", ]
ecdc_Ukraine <- raw_data[raw_data$`Countries.and.territories` == "Ukraine", ]
who_Ukraine <- who_Ukraine[who_Ukraine$date_reported %in% ecdc_Ukraine$DateRep, ]
df <- dplyr::left_join(who_Ukraine, ecdc_Ukraine, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths
