## orderly::orderly_develop_start(use_draft = "newer",
## parameters = list(week_ending = "2020-10-04"))
dir.create("figures")

reff_overlaps_weekly <- function(reff, weekly) {
  x <- left_join(
    reff, weekly, by = c("date" = "date"),
    suffix = c("_reff", "_weekly")
  )
  x <- na.omit(x)
  x$day <- seq_len(nrow(x))
  x$reff_overlaps_weekly <-
    (x$`2.5%_weekly` > x$`2.5%_reff`) &
    (x$`97.5%_weekly` < x$`97.5%_reff`)

  x
}

reff_qntls <- readRDS("weighted_per_country_reff_qntls.rds")
## Add day
reff_qntls <- map_dfr(reff_qntls, function(df) {
  df$day <- seq_len(nrow(df))
  df
}, .id = "country")


reff_qntls <- rincewind::assign_epidemic_phase(reff_qntls)
## We assume transmissibility remains constant for 1 week.
## Therefore, for each country, for each week, repeat the rows for 7
## days and increment dates
weekly_rt <- readRDS("weekly_rt.rds")
weekly_rt <- na.omit(weekly_rt)


######################################################################
######### Compare phase assigned by the two Rts ######################
######################################################################
reff_qntls$date <- as.Date(reff_qntls$date)
compare_phase <- dplyr::left_join(
  reff_qntls, weekly_rt,
  by = c("country", "date"),
  suffix = c("_eff", "_weekly")
  )

compare_phase$week_of_forecast <- case_when(
  compare_phase$day <= 7 ~ "Week 1",
  7 < compare_phase$day & compare_phase$day <= 14 ~ "Week 2",
  14 < compare_phase$day & compare_phase$day <= 21 ~ "Week 3",
  21 < compare_phase$day  ~ "Week 4"
)
######################################################################
####### Option 1: Compare the phase assigned #########################
######################################################################
x <- select(
  compare_phase, day, week_of_forecast, phase_eff, phase_weekly
)

tabyl(x, phase_weekly, phase_eff, week_of_forecast) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2) %>%
  bind_rows(.id = "Week of forecast")

ggplot(compare_phase, aes(`50%_weekly`, `50%_eff`)) +
  geom_point() +
  geom_smooth(method = "lm") +
facet_wrap(~week_of_forecast, nrow = 2, scales = "free")

##compare_phase <- na.omit(compare_phase)

saveRDS(compare_phase, "compare_phase_weekly_effevtive.rds")

compare_phase$flag <- case_when(
  compare_phase$phase_weekly == compare_phase$phase_eff ~ "same",
  TRUE ~ "different"
)

country_groups <- readRDS("country_groups.rds")

palette <- c(
  decline = "018571", growing = "#a6611a", unclear = "#dfc27d",
  `stable/growing slowly` = "#80cdc1"
)


plots <- map(
  country_groups,
  function(countries) {
    x <- compare_phase[compare_phase$country %in% countries, ]
    x$country <- rincewind::nice_country_name(x$country)
    ##x <- select(x, country, date, phase_eff, phase_weekly)
    ## This might work!
    ## p <- ggplot(x, aes(date, country, fill = phase_eff)) +
    ##   geom_tile() +
    ##   facet_grid(week_of_forecast ~ phase_weekly, scales = "free_x")

    ggplot(x) +
      geom_line(
        aes(date, country, col = phase_eff), size = 1.2
      ) +
      scale_color_manual(values = palette) +
      scale_x_date(
        date_breaks = date_breaks, date_labels = date_labels
      ) +
      facet_grid(phase_weekly ~ week_of_forecast) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank()
      )
  }
)

iwalk(plots, function(p, index) {
  outfile <- glue("compare_phase_{index}.tiff")
  rincewind::save_multiple(p, outfile)
})

compare_phase <- na.omit(compare_phase)
x <- count(compare_phase, phase_weekly, phase_eff, day)
## x <- tidyr::spread(x, phase_eff, n, 0)
## x$total <- rowSums(x[, -c(1, 2)])
## x <- mutate_at(
##   x,
##   vars("decline", "growing", "stable/growing slowly", "unclear"),
##   ~ . / total
## )

x <- split(compare_phase, compare_phase$country) %>%
  map_dfr(
    function(by_country) {
      split(by_country, by_country$day) %>%
          map_dfr(function(df) {
            Hmisc::rcorr(df$`50%_weekly`, df$`50%_eff`) %>% broom::tidy()
          }, .id = "day"
       )
    }, .id = "country"
  )


ggplot(x, aes(as.integer(day), country, fill = estimate)) + geom_tile()
