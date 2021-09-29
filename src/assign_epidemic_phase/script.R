## orderly::orderly_develop_start(parameters = list(week_ending = "2020-11-29", use_si = "si_2"), use_draft = "newer")

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
        assign_epidemic_phase2(m1_rt),
        assign_epidemic_phase2(m2_rt),
        assign_epidemic_phase2(m3_rt)
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
      phase = out
    )
  })

shortterm_phase <- rbind(indvdl_phase, ensb_phase)

longer_rs <- readRDS("weighted_per_country_rsaturation.rds")
longer_phase <- map_dfr(longer_rs, function(country_rs) {
  imap_dfr(country_rs, function(day_rs, day) {
    data.frame(
      phase = assign_epidemic_phase2(day_rs)
    )
  }, .id = "day")
}, .id = "country")

longer_phase$model <- week_ending


saveRDS(shortterm_phase, "short_term_phase.rds")
saveRDS(longer_phase, "medium_term_phase.rds")
