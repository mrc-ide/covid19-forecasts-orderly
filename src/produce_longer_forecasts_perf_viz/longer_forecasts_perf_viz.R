## orderly::orderly_develop_start(use_draft = "newer")
daily <- readRDS("long_projections_error_daily.rds")
weekly <- group_by(daily, strategy, country, week_of_projection) %>%
  summarise_if(is.numeric, mean)

better_than_null <- readRDS("better_than_null.rds")

weekly$country <- factor(
  weekly$country, levels = better_than_null$country, ordered = TRUE
)

weekly$week_of_projection <- factor(weekly$week_of_projection)

country_groups <- readRDS("country_groups.rds")

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



by_strategy <- split(weekly, weekly$strategy)

x <- by_strategy[[1]]

df <- x[x$country %in% country_groups[[1]], ]
df <- weekly_summary(df)

out <- augment_data(df)

df <- out[[1]]
