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


last_week <- seq(from = as.Date("2020-07-24"), to = as.Date("2020-08-09"), by = "1 day")
who_Spain <- who[who$country == "Spain" & who$date_reported %in% last_week, ]
ecdc_Spain <- raw_data[raw_data$`Countries.and.territories` == "Spain", ]
ecdc_Spain <- ecdc_Spain[ecdc_Spain$DateRep %in% last_week, ]
df <- dplyr::left_join(who_Spain, ecdc_Spain, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
## ECDC data does not have data for Spain for 9th August, add a row
last_row <- raw_data[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep == "2020-08-08", ]
last_row$DateRep <- "2020-08-09"
raw_data <- rbind(raw_data, last_row)
raw_data$Cases[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths


who_Kosovo <- who[who$country == "Kosovo[1]", ]
ecdc_Kosovo <- raw_data[raw_data$`Countries.and.territories` == "Kosovo", ]
who_Kosovo <- who_Kosovo[who_Kosovo$date_reported %in% ecdc_Kosovo$DateRep, ]
df <- dplyr::left_join(who_Kosovo, ecdc_Kosovo, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths
