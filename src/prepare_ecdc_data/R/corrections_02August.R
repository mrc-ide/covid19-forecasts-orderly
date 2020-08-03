#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 02nd August ##################################
######################################################################
######################################################################
######################################################################

last_week <- seq(from = as.Date("2020-07-26"), to = as.Date("2020-08-02"), by = "1 day")
who_Guatemala <- who[who$country == "Guatemala" & who$date_reported %in% last_week, ]
ecdc_Guatemala <- raw_data[raw_data$`Countries.and.territories` == "Guatemala", ]
ecdc_Guatemala <- ecdc_Guatemala[ecdc_Guatemala$DateRep %in% last_week, ]
df <- dplyr::left_join(who_Guatemala, ecdc_Guatemala, by = c("date_reported" = "DateRep"))
df <- dplyr::arrange(df, desc(date_reported))
raw_data$Cases[raw_data$`Countries.and.territories` == "Guatemala" & raw_data$DateRep %in% df$date_reported] <- df$new_cases
raw_data$Deaths[raw_data$`Countries.and.territories` == "Guatemala" & raw_data$DateRep %in% df$date_reported] <- df$new_deaths

raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-01"] <- 15
raw_data$Deaths[raw_data$`Countries.and.territories` == "Kosovo" & raw_data$DateRep == "2020-08-02"] <- 10

raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-07-31"] <- 26
raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-08-01"] <- 24
raw_data$Deaths[raw_data$`Countries.and.territories` == "Saudi_Arabia" & raw_data$DateRep == "2020-08-02"] <- 21

raw_data$Deaths[raw_data$`Countries.and.territories` == "Sudan" & raw_data$DateRep == "2020-08-02"] <- 6
raw_data$Cases[raw_data$`Countries.and.territories` == "Sudan" & raw_data$DateRep == "2020-08-02"] <- 94

raw_data$Deaths[raw_data$`Countries.and.territories` == "Venezuela" & raw_data$DateRep == "2020-07-31"] <- 4
raw_data$Cases[raw_data$`Countries.and.territories` == "Venezuela" & raw_data$DateRep == "2020-07-30"] <- 3
