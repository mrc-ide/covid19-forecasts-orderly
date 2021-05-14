## orderly::orderly_develop_start(use_draft = "newer", parameters = list(location = "Arizona"))
## infiles <- list.files(pattern = "*.rds")
##

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

model_input <- readRDS("model_input.rds")
infiles <- infiles[infiles != "model_input.rds"]
forecast_weeks <- gsub("ensemble_daily_qntls_", "", infiles)
forecast_weeks <- gsub(".rds", "", forecast_weeks)
names(infiles) <- forecast_weeks
daily_qntls <- map_dfr(infiles, readRDS, .id = "forecast_week")


daily_qntls <- split(daily_qntls, daily_qntls$si)

obs <- model_input[, c("dates", location)]
obs$deaths <- obs[[location]]

## Also plot the 7 day sliding average
obs$rolling_mean <- slide_dbl(
  obs$deaths, mean, .before = 3, .after = 2
)

plots <- map(
  daily_qntls, function(proj) {
    proj$date <- as.Date(proj$date)
    ggplot() +
      geom_point(
        data = obs, aes(dates, deaths), alpha = 0.5
      ) +
      geom_line(
        data = obs, aes(dates, rolling_mean)
      ) +
      geom_ribbon(
        data = proj,
        aes(
          x = date, ymin = `2.5%`, ymax = `97.5%`, group = forecast_week
        ),
        alpha = 0.3, fill = "#4a8c6f"
      ) +
      geom_ribbon(
        data = proj,
        aes(x = date, ymin = `25%`, ymax = `75%`, group = forecast_week),
        alpha = 0.5, fill = "#4a8c6f"
      ) +
      scale_date_manuscript(
        date_breaks = "2 weeks", date_labels = "%d - %b",
        xmin = "2021-01-01"
      ) +
      ylab("Daily Deaths") +
      theme_minimal() +
      theme(axis.title.x = element_blank()) +
      ggtitle(label = rincewind::nice_country_name(location))
  }
)


iwalk(
  plots,
  function(p, si) save_plot(glue("{location}_{si}.pdf"), p)
)


iwalk(
  plots,
  function(p, si) save_plot(glue("{si}.pdf"), p)
)

dev.off()
