## orderly::orderly_develop_start(use_draft = "newer")
daily <- readRDS("long_projections_error_daily.rds")
weekly <- group_by(daily, strategy, country, week_of_projection) %>%
  summarise_if(is.numeric, list(mu = mean, sd = sd))

weekly$country <- factor(
    weekly$country,
    levels = better_than_null$country, ordered = TRUE
  )


weekly_incid <- readRDS("weekly_incidence.rds")

compare <- ggplot(
  weekly, aes(factor(week_of_projection), rel_mae, fill = strategy)
) +
  geom_boxplot(position = "dodge") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Week of projection") +
  ylab("log relative error")

cbbPalette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7"
)

names(cbbPalette) <- 1:8

weekly$week_of_projection <- factor(weekly$week_of_projection)

f <- scales::trans_breaks("log10", function(x) 10^x)
g <- scales::trans_format("log10", scales::math_format(10^.x))
breaks <- union(f(weekly$obs_mu), f(weekly$rel_mae_mu))
labels <- g(breaks)

p <- ggplot(
  weekly,
  aes(log(obs_mu, 10), log(rel_mae_mu, 10), col = week_of_projection)
) +
  geom_point() +
  expand_limits(x = 0, y = 0) +
  scale_color_manual(values = cbbPalette, name = "Week of projection") +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
  xlab("(log) Mean weekly incidence") +
  ylab("(log) Mean relative error")

ggsave("incid_vs_relative_error.png", p)

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

x <- by_strategy[[1]]
df <- x[x$country %in% country_groups[[1]], ]
out <- augment_data(df)

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
