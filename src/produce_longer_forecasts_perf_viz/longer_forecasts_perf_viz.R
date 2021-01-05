## orderly::orderly_develop_start(use_draft = "newer")

daily_error <- readRDS("long_projections_error_daily.rds")
weekly_error <- readRDS("long_projections_error_weekly.rds")
weekly_incid <- readRDS("weekly_incidence.rds")
exclude <- readRDS("exclude.rds")
country_groups <- readRDS("country_groups.rds")
weekly_error <- weekly_error[!weekly_error$country %in% exclude, ]

by_strategy <- split(weekly_error, weekly_error$strategy)

plots <-  map(
  by_strategy,
  function(x) {
    x$week_of_projection <- factor(x$week_of_projection)
    p <- ggplot(x, aes(week_of_projection, log(rel_mae))) +
      geom_boxplot() +
      xlab("Week of projection") +
      ylab("(log) Relative mean error") +
      theme_minimal()
    p
  }
)

iwalk(plots, function(p, y) ggsave(glue("rel_mae_{y}.png"), p))

rel_mae_plots <- map(
  by_strategy,
  function(df) {
    map(
      country_groups,
      function(countries) {
        local <- df[df$country %in% countries, ]
        local <- weekly_summary(local)
        local$country <- rincewind::nice_country_name(local$country)
        out <- augment_data(local, 2)
        long_relative_error_heatmap(
          out[[1]], high1 = 1, high2 = 3, out$x_labels, out$y_labels) +
          facet_wrap(~week_of_projection, nrow = 2)
      }
    )
  }
)

iwalk(
  rel_mae_plots,
  function(plots, strategy) {
    iwalk(plots, function(p, page) {
      outfile <- glue("rel_mae_{strategy}_{page}.tiff")
      rincewind::save_multiple(p, outfile)
    }
    )
  }
)

date_labeller <- function(x) {
  strftime(as.Date(x), format = "%d-%b")
}

by_week_plots <- map(
  by_strategy,
  function(df) {
    df$week_of_projection <- factor(df$week_of_projection)

    ggplot(df) +
      geom_boxplot(
        aes(forecast_week, log(rel_mae), fill = week_of_projection),
        position = "dodge", alpha = 0.5
      ) +
      scale_x_discrete(labels = date_labeller) +
      ylab("(log) Relative mean error") +
      guides(fill = guide_legend(title = "Week of forecast")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "top",
        axis.title.x = element_blank()
      )
  }
)

iwalk(by_week_plots, function(p, strategy) {
  outfile <- glue("rel_mae_by_forecast_week_{strategy}.tiff")
  rincewind::save_multiple(p, outfile)
})

by_country_plots <- map(
  by_strategy,
  function(df) {
    df$week_of_projection <- factor(df$week_of_projection)
    x <- group_by(df, country) %>%
      summarise(mu = mean(rel_mae)) %>%
      ungroup() %>%
      arrange(desc(mu))

    df$country <- factor(
      df$country, levels = x$country, ordered = TRUE
    )

    df$flag <- ifelse(
      df$country %in% levels(df$country)[1:50], 1, 2
    )

    ggplot(df) +
      geom_boxplot(
        aes(country, log(rel_mae)),
        position = "dodge", alpha = 0.5
      ) +
      scale_x_discrete(labels = rincewind::nice_country_name) +
      facet_wrap(~flag, ncol = 1, scales = "free") +
      ylab("(log) Relative mean error") +
      guides(fill = guide_legend(title = "Week of forecast")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "top",
        axis.title.x = element_blank(),
        strip.text = element_blank()
      )

  }
)

iwalk(by_country_plots, function(p, strategy) {
  outfile <- glue("rel_mae_by_country_{strategy}.tiff")
  rincewind::save_multiple(p, outfile)
})
