## orderly::orderly_develop_start(use_draft = "newer")


chosen_strategy <- "weighted_per_country"
dir.create("figures")
weekly_error <- readRDS("long_projections_error_weekly.rds")
weekly_error <- weekly_error[weekly_error$strategy == chosen_strategy, ]
weekly_incid <- readRDS("weekly_incidence.rds")
exclude <- readRDS("exclude.rds")

weekly_error <- weekly_error[!weekly_error$country %in% exclude, ]
country_groups <- readRDS("country_groups.rds")

weeks <- seq(
  from  = as.Date("2020-03-29"), to = as.Date("2020-11-29"),
  by = "7 days"
)

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

iwalk(plots, function(p, y) ggsave(glue("figures/rel_mae_{y}.png"), p))

rel_mae_plots <- map(
  by_strategy,
  function(df) {
    map(
      country_groups,
      function(countries) {
        local <- df[df$country %in% countries, ]
        local <- weekly_summary(local)
        local$country <- rincewind::nice_country_name(local$country)
        out <- augment_data(local, weeks, 2)
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
    plots <- customise_for_rows(plots, c(2, 3, 4))
    iwalk(plots, function(p, page) {
      outfile <- glue("figures/rel_mae_{strategy}_{page}.tiff")
      rincewind::save_multiple(p, outfile)
    }
    )
  }
)

prop_50_plots <- map(
  by_strategy,
  function(df) {
    map(
      country_groups,
      function(countries) {
        local <- df[df$country %in% countries, ]
        local$country <- rincewind::nice_country_name(local$country)
        out <- augment_data(local, weeks, 2)
        df <- out$df
        df$fill <- df$prop_in_50
        prop_in_ci_heatmap(df, out$x_labels, out$y_labels) +
          facet_wrap(~week_of_projection, nrow = 2)
      }
    )
  }
)

iwalk(
  prop_50_plots,
  function(plots, strategy) {
    plots <- customise_for_rows(plots, c(1, 2, 3, 4))
    iwalk(plots, function(p, page) {
      outfile <- glue("figures/prop_50_{strategy}_{page}.tiff")
      rincewind::save_multiple(p, outfile, one_col = FALSE)
    }
    )
  }
)


prop_95_plots <- map(
  by_strategy,
  function(df) {
    map(
      country_groups,
      function(countries) {
        local <- df[df$country %in% countries, ]
        local$country <- rincewind::nice_country_name(local$country)
        out <- augment_data(local, weeks, 2)
        df <- out$df
        df$fill <- df$prop_in_975
        prop_in_ci_heatmap(df, out$x_labels, out$y_labels, "95%") +
          facet_wrap(~week_of_projection, nrow = 2)
      }
    )
  }
)

iwalk(
  prop_95_plots,
  function(plots, strategy) {
    plots <- customise_for_rows(plots, c(1, 2, 3, 4))
    iwalk(plots, function(p, page) {
      outfile <- glue("figures/prop_95_{strategy}_{page}.tiff")
      rincewind::save_multiple(p, outfile, one_col = FALSE)
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
  outfile <- glue("figures/rel_mae_by_forecast_week_{strategy}.tiff")
  rincewind::save_multiple(p, outfile)
})

