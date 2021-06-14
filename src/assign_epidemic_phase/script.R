## orderly::orderly_develop_start(parameters = list(week_ending = "2021-06-13"))

ensb_rt <- readRDS("unwtd_ensemble_model_rt.rds")
phase <- split(ensb_rt, ensb_rt$country) %>%
  map_dfr(function(x) assign_epidemic_phase2(x[[use_si]]),
    .id = "country")

phase$model <- week_ending
