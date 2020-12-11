## orderly::orderly_develop_start(use_draft = "newer")

daily_error <- readRDS("long_projections_error_daily.rds")
weekly_error <- readRDS("long_projections_error_weekly.rds")
weekly_incid <- readRDS("weekly_incidence.rds")
exclude <- readRDS("exclude.rds")
country_groups <- readRDS("country_groups.rds")
weekly_error <- weekly_error[weekly_error$week_of_projection %in% c(1, 2, 3, 4), ]
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


df <- by_strategy[[3]]
df <- df[df$country %in% country_groups[[1]], ]
df <- weekly_summary(df)

df$country <- rincewind::nice_country_name(df$country)
df <- augment_data(df)
p <- long_relative_error_heatmap(df$df, 1, 2, df$x_labels, df$y_labels)
