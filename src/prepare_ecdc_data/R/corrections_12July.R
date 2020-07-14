#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 12th July ####################################
######################################################################
######################################################################
######################################################################
raw_data$Deaths[raw_data$DateRep == "2020-07-12" & raw_data$`Countries.and.territories` == "Algeria"] <- 8
raw_data$Deaths[raw_data$DateRep == "2020-07-11" & raw_data$`Countries.and.territories` == "Algeria"] <- 8
raw_data$Deaths[raw_data$DateRep == "2020-07-11" & raw_data$`Countries.and.territories` == "Haiti"] <- 7
raw_data$Deaths[raw_data$DateRep == "2020-07-12" & raw_data$`Countries.and.territories` == "Haiti"] <- 5
raw_data$Deaths[raw_data$DateRep == "2020-07-11" & raw_data$`Countries.and.territories` == "Ukraine"] <- 27
raw_data$Deaths[raw_data$DateRep == "2020-07-12" & raw_data$`Countries.and.territories` == "Ukraine"] <- 11
