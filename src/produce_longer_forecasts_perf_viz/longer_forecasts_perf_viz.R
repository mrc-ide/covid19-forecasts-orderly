## orderly::orderly_develop_start(use_draft = "newer")


chosen_strategy <- "weighted_per_country"
dir.create("figures")
weekly_error <- readRDS("long_projections_error_weekly.rds")
weekly_incid <- readRDS("weekly_incidence.rds")
exclude <- readRDS("exclude.rds")
better_than_null <- readRDS("better_than_null.rds")
country_groups <- readRDS("country_groups.rds")

weekly_error <- weekly_error[weekly_error$strategy == chosen_strategy, ]
weekly_error <- weekly_error[!weekly_error$country %in% exclude, ]
weekly_error$country <- factor(
  weekly_error$country,
  levels = better_than_null$country, ordered = TRUE
)

weeks <- seq(
  from  = as.Date("2020-03-29"), to = as.Date("2020-11-29"),
  by = "7 days"
)

facet_labels <- c("1" = "1-week ahead", "2" = "2-weeks ahead",
                  "3" = "3-weeks ahead", "4" = "4-weeks ahead")

by_strategy <- split(weekly_error, weekly_error$strategy)

date_labeller <- function(x) {
  strftime(as.Date(x), format = "%d-%b")
}
## First make the plot to show relative error grows over
## weeks. Then drop everything greater than 4 weeks.
plots <-  map(
  by_strategy,
  function(x) {
    x$week_of_projection <- factor(x$week_of_projection)
    p <- ggplot(x, aes(week_of_projection, log(rel_mae))) +
      geom_boxplot() +
      xlab("Week of projection") +
      ylab("(log) Mean relative error") +
      theme_minimal() +
      theme(
        text = element_text(family = "CMU Sans Serif", size = 16),
        legend.position = "top", legend.title = element_blank()
      )

    p
  }
)

iwalk(plots, function(p, y) ggsave(glue("figures/rel_mae_{y}.png"), p))

by_strategy <- map(
  by_strategy,
  function(x) x[x$week_of_projection %in% 1:4, ]
)


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
      ylab("(log) Mean relative error") +
      guides(fill = guide_legend(title = "Week of forecast")) +
      theme_minimal() +
      theme(
        text = element_text(family = "CMU Sans Serif", size = 16),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = "top", legend.title = element_blank()
      )

  }
)

iwalk(by_week_plots, function(p, strategy) {
  outfile <- glue("figures/rel_mae_by_forecast_week_{strategy}")
  rincewind::save_multiple(p, outfile)
})



rel_mae_plots <- map(
  by_strategy,
  function(df) {
    map(
      country_groups,
      function(countries) {
        local <- df[df$country %in% countries, ]
        local <- droplevels(local)
        ##local <- weekly_summary(local)
        ##local$country <- rincewind::nice_country_name(local$country)
        out <- augment_data(local, weeks, 2.2)
        long_relative_error_heatmap(
          out[[1]], high1 = 1, high2 = 2, out$x_labels, out$y_labels) +
          facet_wrap(
            ~week_of_projection, nrow = 2,
            labeller = as_labeller(facet_labels)
          )
      }
    )
  }
)

iwalk(
  rel_mae_plots,
  function(plots, strategy) {
    plots <- customise_for_rows(plots, c(2, 3, 4))
    iwalk(plots, function(p, page) {
      outfile <- glue("figures/rel_mae_{strategy}_{page}")
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
        local <- droplevels(local)
        out <- augment_data(local, weeks, 2.2)
        df <- out$df
        df$fill <- df$prop_in_50
        prop_in_ci_heatmap(df, out$x_labels, out$y_labels) +
          facet_wrap(
            ~week_of_projection, nrow = 2,
            labeller = as_labeller(facet_labels)
          )
      }
    )
  }
)

iwalk(
  prop_50_plots,
  function(plots, strategy) {
    plots <- customise_for_rows(plots, c(1, 2, 3, 4))
    iwalk(plots, function(p, page) {
      outfile <- glue("figures/prop_50_{strategy}_{page}")
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
        local <- droplevels(local)
        out <- augment_data(local, weeks, 2.2)
        df <- out$df
        df$fill <- df$prop_in_975
        prop_in_ci_heatmap(df, out$x_labels, out$y_labels, "95%") +
          facet_wrap(
            ~week_of_projection, nrow = 2,
            labeller = as_labeller(facet_labels)
          )
      }
    )
  }
)

iwalk(
  prop_95_plots,
  function(plots, strategy) {
    plots <- customise_for_rows(plots, c(1, 2, 3, 4))
    iwalk(plots, function(p, page) {
      outfile <- glue("figures/prop_95_{strategy}_{page}")
      rincewind::save_multiple(p, outfile, one_col = FALSE)
    }
    )
  }
)


