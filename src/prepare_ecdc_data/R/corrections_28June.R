raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Argentina"] <- 33

raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "Armenia"] <- 13
raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Armenia"] <- 11
raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Armenia"] <- 14
raw_data$Deaths[raw_data$DateRep == "2020-06-23" & raw_data$`Countries.and.territories` == "Armenia"] <- 12


raw_data$Deaths[raw_data$DateRep == "2020-06-21" & raw_data$`Countries.and.territories` == "Azerbaijan"] <- 5
raw_data$Deaths[raw_data$DateRep == "2020-06-20" & raw_data$`Countries.and.territories` == "Azerbaijan"] <- 4

raw_data$Deaths[raw_data$DateRep == "2020-06-21" & raw_data$`Countries.and.territories` == "Belarus"] <- 6
raw_data$Deaths[raw_data$DateRep == "2020-06-20" & raw_data$`Countries.and.territories` == "Belarus"] <- 6

raw_data$Deaths[raw_data$DateRep == "2020-06-28" & raw_data$`Countries.and.territories` == "Democratic_Republic_of_the_Congo"] <- 4
raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "Democratic_Republic_of_the_Congo"] <- 0
raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Democratic_Republic_of_the_Congo"] <- 7

raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "Egypt"] <- 83
raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Egypt"] <- 85

raw_data$Deaths[raw_data$DateRep == "2020-06-28" & raw_data$`Countries.and.territories` == "Iran"] <- 125
raw_data$Deaths[raw_data$DateRep == "2020-06-27" & raw_data$`Countries.and.territories` == "Iran"] <- 109

raw_data$Deaths[raw_data$DateRep == "2020-06-28" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 15
raw_data$Deaths[raw_data$DateRep == "2020-06-27" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 11

raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Peru"] <- 181
raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Peru"] <- 178

raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Ukraine"] <- 16
raw_data$Deaths[raw_data$DateRep == "2020-06-24" & raw_data$`Countries.and.territories` == "Ukraine"] <- 23

raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "United_States_of_America"] <-
  raw_data$Deaths[raw_data$DateRep == "2020-06-26" & raw_data$`Countries.and.territories` == "United_States_of_America"] - 1854


### Replace -31 deaths in Italy with moving average
dates_of_interest <- as.Date(c(
  "2020-06-22", "2020-06-23", "2020-06-24",
  "2020-06-26", "2020-06-27", "2020-06-28"
))

deaths_avg <- round(
  mean(
    raw_data$Deaths[raw_data$DateRep %in% dates_of_interest &
                    raw_data$`Countries.and.territories` == "Italy"]
  )
)

raw_data$Deaths[raw_data$DateRep == "2020-06-25" & raw_data$`Countries.and.territories` == "Italy"] <- deaths_avg


