## orderly::orderly_develop_start(use_draft = "newer")
daily <- readRDS("long_projections_error_daily.rds")
weekly <- group_by(daily, strategy, country, week_of_projection) %>%
  summarise_if(is.numeric, mean)


compare <- ggplot(
  weekly, aes(factor(week_of_projection), rel_mae, fill = strategy)
) +
  geom_boxplot(position = "dodge") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Week of projection") +
  ylab("log relative error")


country_groups <- readRDS("country_groups.rds")

by_strategy <- split(weekly, weekly$strategy)

by_strategy_week <- map(
  by_strategy,
  function(df) {
    group_by(df, strategy, week_of_projection) %>%
      summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
      ungroup()
  }
)

by_strategy_country <- map(
  by_strategy,
  function(df) {
    group_by(df, strategy, country) %>%
      summarise_if(is.numeric, list(mu = mean, sd = sd)) %>%
      ungroup()
  }
)

df <- weekly[weekly$country %in% country_groups[[1]], ]


ggplot() +
  geom_tile(
    data = by_country[by_country$rel_mae <= 5, ],
    aes(week_of_projection, country, fill = rel_mae)
  ) +
  scale_fill_distiller(
    palette = "Spectral", na.value = "white", direction = -1,
    guide = guide_colourbar(
      title = "Relative Error",
      title.position = "left",
      title.vjust = 0.5,
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(
    data = by_country[by_country$rel_mae > 5 & by_country$rel_mae < 10, ],
    aes(week_of_projection, country, fill = rel_mae)
  ) +
  scale_fill_distiller(
    palette = "YlOrRd", na.value = "white", direction = 1,
    guide = guide_colourbar(
      title = "2 < Model Error <= 5",
      title.position = "top",
      title.hjust = 0.5,
      order = 2
    )
  ) +
  theme_minimal()

nice_country_name <- function(x) snakecase::to_title_case(as.character(x))


long_relative_error_heatmap(by_country)

quantile()

ggplot(by_country) +
  geom_histogram(aes(rel_mae), binwidth = 0.1) +
  facet_wrap(~week_of_projection, ncol = 2, scales = "free_x")
