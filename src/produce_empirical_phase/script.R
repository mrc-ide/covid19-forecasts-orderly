## orderly::orderly_develop_start(parameters = list(latest_week = "2020-12-06", week_starting =  "2020-02-22"), use_draft = "newer")
weekly_incid <- readRDS("weekly_incidence.rds")

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

india$phase <- case_when(
(lead(india$weekly_incid) > india$weekly_incid - 0.5 * india$sigma) &
(lead(india$weekly_incid) < india$weekly_incid + 0.5 * india$sigma) ~ "stable",

(lead(india$weekly_incid) < india$weekly_incid - 0.5 * india$sigma) &
(lead(india$weekly_incid) > india$weekly_incid -  india$sigma) ~ "declining",


(lead(india$weekly_incid) > india$weekly_incid + 0.5 * india$sigma) &
(lead(india$weekly_incid) < india$weekly_incid + india$sigma) ~ "growing"

)
