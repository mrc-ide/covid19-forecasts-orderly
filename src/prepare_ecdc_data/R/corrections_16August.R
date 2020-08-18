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


last_week <- seq(from = as.Date("2020-08-08"), to = as.Date("2020-08-16"), by = "1 day")
who_ukraine <- who[who$country == "Ukraine" & who$date_reported %in% last_week, ]
ecdc_ukraine <- raw_data[raw_data$`Countries.and.territories` == "Ukraine", ]
ecdc_ukraine <- ecdc_ukraine[ecdc_ukraine$DateRep %in% last_week, ]
df <- dplyr::left_join(who_ukraine, ecdc_ukraine, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Ukraine" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths


last_week <- seq(from = as.Date("2020-07-24"), to = as.Date("2020-08-16"), by = "1 day")
who_Spain <- who[who$country == "Spain" & who$date_reported %in% last_week, ]
ecdc_Spain <- raw_data[raw_data$`Countries.and.territories` == "Spain", ]
ecdc_Spain <- ecdc_Spain[ecdc_Spain$DateRep %in% last_week, ]
df <- dplyr::left_join(who_Spain, ecdc_Spain, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
## ECDC data does not have data for Spain for 9th August, add a row
##last_row <- raw_data[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep == "2020-08-08", ]
##last_row$DateRep <- "2020-08-09"
##raw_data <- rbind(raw_data, last_row)
raw_data$Cases[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Spain" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths