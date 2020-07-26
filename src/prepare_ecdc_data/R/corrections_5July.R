#####################################################################
######################################################################
######################################################################
######################################################################
########### Corrections 5th July #####################################
######################################################################
######################################################################
######################################################################
raw_data$Cases[raw_data$DateRep == "2020-07-01" & raw_data$`Countries.and.territories` == "Kazakhstan"] <-
  round(mean(
  c(raw_data$Cases[raw_data$DateRep == "2020-07-02" & raw_data$`Countries.and.territories` == "Kazakhstan"],
    raw_data$Cases[raw_data$DateRep == "2020-07-03" & raw_data$`Countries.and.territories` == "Kazakhstan"],
    raw_data$Cases[raw_data$DateRep == "2020-06-30" & raw_data$`Countries.and.territories` == "Kazakhstan"],
    raw_data$Cases[raw_data$DateRep == "2020-06-29" & raw_data$`Countries.and.territories` == "Kazakhstan"]
    )
  ))

raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 26
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Kazakhstan"] <- 26

raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Qatar"] <- 3
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Qatar"] <- 2

raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Sudan"] <- 4

raw_data$Deaths[raw_data$DateRep == "2020-07-01" & raw_data$`Countries.and.territories` == "Ukraine"] <- 12
raw_data$Deaths[raw_data$DateRep == "2020-07-02" & raw_data$`Countries.and.territories` == "Ukraine"] <- 14
raw_data$Deaths[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "Ukraine"] <- 27
raw_data$Deaths[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "Ukraine"] <- 15

raw_data$Cases[raw_data$DateRep == "2020-07-03" & raw_data$`Countries.and.territories` == "United_Kingdom"] <-
  round(
    mean(
      c(raw_data$Cases[raw_data$DateRep == "2020-07-01" & raw_data$`Countries.and.territories` == "United_Kingdom"],
        raw_data$Cases[raw_data$DateRep == "2020-07-02" & raw_data$`Countries.and.territories` == "United_Kingdom"],
        raw_data$Cases[raw_data$DateRep == "2020-07-04" & raw_data$`Countries.and.territories` == "United_Kingdom"],
        raw_data$Cases[raw_data$DateRep == "2020-07-05" & raw_data$`Countries.and.territories` == "United_Kingdom"]
        )
    )
  )
