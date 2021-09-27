## orderly::orderly_develop_start(parameters = list(latest_week = "2021-02-28"))
weekly_incid <- readRDS("weekly_incidence.rds")
weekly_incid <- select(
  weekly_incid, week_starting, weekly_incid, country
)
weekly_incid <- distinct(weekly_incid)

weekly_phase <- split(
  weekly_incid, weekly_incid$country
) %>%
  map_dfr(
    function(x) {
      x$week_starting <- as.Date(x$week_starting)
      x <- arrange(x, week_starting)
      x$change_from_prev_week <- c(NA, diff(x$weekly_incid))
      ## Change over previous week
      x$rel_change <- x$change_from_prev_week / lag(x$weekly_incid)
      x
    }, .id = "country"
  )

weekly_phase$phase <- case_when(
  weekly_phase$change_from_prev_week > 0 ~ "Growing",

)
