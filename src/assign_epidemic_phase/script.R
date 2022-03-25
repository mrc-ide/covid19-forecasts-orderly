## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"), use_draft = "newer")
## assign_epidemic_phase2 <- function(rt) {

##   med <- quantile(rt, prob = 0.5)
##   iqr <- quantile(rt, prob = 0.975) - quantile(rt, prob = 0.025)
##   ratio <- iqr / med
##   if (med > 1) {
##     ## could be growing, but need to take into account the width of the
##     ## distribution
##     if (ratio < med/3) phase <- "growing"
##     else if (med/3 <= ratio & ratio < 2 * med/3) phase <- "likely growing"
##     else if (2 * med/3 <= ratio & ratio < med)  phase <- "likely stable"
##     else phase <- "indeterminate"
##   } else {
##     if (ratio < med/3) phase <- "definitely declining"
##     else if (med/3 <= ratio & ratio < 2 * med/3) phase <- "likely declining"
##     else if (2 * med/3 <= ratio & ratio < med)  phase <- "likely stable"
##     else phase <- "indeterminate"
##   }
##   phase
## }

rti0 <- readRDS("rti0.rds")
apeestim <- readRDS("apeestim.rds")
deca <- readRDS("deca.rds")
countries <- rti0$Country

indvdl_phase <- map_dfr(
  countries, function(country) {
    m1_rt <- rti0[["R_last"]][[country]][[2]]
    m2_rt <- apeestim[["R_last"]][[country]][[2]]
    m3_rt <- deca[["R_last"]][[country]][[2]]

    data.frame(
      model_name = c("RtI0", "apeestim", "DeCa"),
      country = country,
      model = week_ending,
      phase = c(
        assign_epidemic_phase2(m1_rt)[["phase"]],
        assign_epidemic_phase2(m2_rt)[["phase"]],
        assign_epidemic_phase2(m3_rt)[["phase"]]
      )
    )
  }
)


ensb_rt <- readRDS("unwtd_ensemble_model_rt.rds")
ensb_phase <- split(ensb_rt, ensb_rt$country) %>%
  map_dfr(function(x) {
    out <- assign_epidemic_phase2(x[[use_si]])
    data.frame(
      model_name = "ensemble",
      country = x$country[1],
      model = x$model[1],
      phase = out[["phase"]],
      rt_cv = out[["cv"]],
      less_than_1 = out[["less_than_1"]]
    )
  })

shortterm_phase <- ensb_phase

longer_rs <- readRDS("weighted_per_country_rsaturation.rds")


longer_phase <- map_dfr(longer_rs, function(country_rs) {
  imap_dfr(country_rs, function(day_rs, day) {
    out <- assign_epidemic_phase2(day_rs)
    data.frame(
      phase = out[["phase"]],
      rt_cv = out[["cv"]],
      less_than_1 = out[["less_than_1"]],
      low = quantile(day_rs, prob = 0.025),
      med = quantile(day_rs, prob = 0.5),
      high = quantile(day_rs, prob = 0.975)
    )
  }, .id = "day")
}, .id = "country")

longer_phase$model <- week_ending


saveRDS(shortterm_phase, "short_term_phase.rds")
saveRDS(longer_phase, "medium_term_phase.rds")
