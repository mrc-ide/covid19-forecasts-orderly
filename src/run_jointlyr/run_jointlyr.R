## orderly::orderly_develop_start(use_draft = "newer", parameters = list(week_ending = "2021-01-10", location = "Arizona"))
set.seed(1)

model_input <- readRDS("model_input.rds")
deaths_to_use <- model_input$D_active_transmission

incid <- tail(deaths_to_use[[location]], 10)

si_distrs <- readRDS("si_distrs.rds")


## Generate stan fit
fit <- jointlyr::jointly_estimate(10, 100, incid, si_distr = si_distrs[[2]],
                                  seed = 42, iter = 20000, chains = 4)
