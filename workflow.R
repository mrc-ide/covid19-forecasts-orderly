library(orderly)
library(purrr)
library(glue)

week <- "2021-03-28"
a <- orderly_run(
  "prepare_jhu_data/",
  parameters = list(week_ending = as.character(week))
)
## a <- "20210330-173949-f6ba30b8"
model_input <- readRDS(
  glue("draft/prepare_jhu_data/{a}/latest_model_input.rds")
)
locations <- model_input$State


## Debugging
## locations <- c("Alabama", "California", "Montana", "Texas", "Michigan")

walk(
  locations, function(location) {
    orderly_run(
      "src/run_jointlyr",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

walk(
  locations, function(location) {
    orderly_run(
      "src/run_apeestim/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

walk(
  locations, function(location) {
    orderly_run(
      "src/run_deca/",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)

walk(
  locations, function(location) {
    orderly_run(
      "src/produce_ensemble_outputs",
      parameters = list(
        location = location, week_ending = as.character(week)
      ), use_draft = "newer"
    )
  }
)


source("orderly-helper-scripts/dependancies_collate_weekly.R")

orderly_run("collate_weekly_outputs", use_draft = "newer")

orderly_run(
  "produce_weekly_figs", parameters = list(week_ending = week),
  use_draft = "newer"
)




## > quantile(x$R_last[[2]])
##        0%       25%       50%       75%      100%
## 0.5139567 0.5258963 0.5330751 0.5452551 0.5843861

## quantile(x$R_last[[2]])
##        0%       25%       50%       75%      100%
## 0.5024188 0.6403970 0.6690439 0.6990609 0.8596034

## > quantile(x$R_last[[2]])
##        0%       25%       50%       75%      100%
## 0.6679605 0.8052921 0.8390534 0.8730171 1.0515063

## tidyr::spread(x, quantile, out2)
##     si forecast_date   state        1%       10%       15%      2.5%       20%
## 1 si_1    2021-01-01 Alabama 0.5971384 0.6200558 0.6225175 0.6122602 0.6252226
## 2 si_2    2021-01-01 Alabama 0.5198694 0.5273670 0.5316987 0.5215673 0.5380033
##         25%       30%       35%       40%       45%        5%       50%
## 1 0.6283630 0.6324280 0.6375466 0.6453725 0.6565795 0.6169837 0.6739133
## 2 0.5472537 0.5614672 0.6082428 0.6360542 0.6532103 0.5237146 0.6701638
##         55%       60%       65%       70%       75%       80%       85%
## 1 0.6899202 0.7113741 0.7542444 0.8541258 0.8835275 0.9066038 0.9281001
## 2 0.6870688 0.7068288 0.7368671 0.7800645 0.8070288 0.8277964 0.8466299
##         90%       95%     97.5%       99%
## 1 0.9490748 0.9780495 1.0018999 1.0314771
## 2 0.8668866 0.8934858 0.9128488 0.9394665
