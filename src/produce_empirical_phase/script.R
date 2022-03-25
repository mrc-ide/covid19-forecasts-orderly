## orderly::orderly_develop_start(parameters = list(latest_week = "2020-12-06", week_starting =  "2020-02-22"), use_draft = "newer")
weekly_incid <- readRDS("weekly_incidence.rds")

weekly_phase <- split(
  weekly_incid, weekly_incid$country
) %>%
  map_dfr(
    function(x) {
      x$week_starting <- as.Date(x$week_starting)
      x <- arrange(x, week_starting)
      x$phase <- case_when(
      (lead(x$weekly_incid) > x$weekly_incid - 0.5 * x$sigma) &
      (lead(x$weekly_incid) < x$weekly_incid + 0.5 * x$sigma) ~ "likely stable",

      (lead(x$weekly_incid) < x$weekly_incid - 0.5 * x$sigma) &
      (lead(x$weekly_incid) > x$weekly_incid -  x$sigma) ~ "likely decreasing",

      (lead(x$weekly_incid) < x$weekly_incid - x$sigma) ~ "definitely decreasing",


      (lead(x$weekly_incid) > x$weekly_incid + 0.5 * x$sigma) &
      (lead(x$weekly_incid) < x$weekly_incid + x$sigma) ~ "likely growing",

      lead(x$weekly_incid) > (x$weekly_incid + x$sigma) ~ "definitely growing"
      )
      x

    }, .id = "country"
  )

## Sanity check; looks alright.
## x <- weekly_incid[weekly_incid$country == "India", ]
## y <- weekly_phase[weekly_phase$country == "India", ]
## x$week_starting <- as.Date(x$week_starting)
## z <- left_join(x, y, by = "week_starting")
## ggplot(z, aes(week_starting, weekly_incid.x, col = phase)) +
##   geom_point()


saveRDS(
  weekly_phase, "empirical_epidemic_phase.rds"
)
