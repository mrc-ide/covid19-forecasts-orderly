## orderly::orderly_develop_start(use_draft = "newer")
daily_error <- readRDS("long_projections_error_daily.rds")
weekly_error <- readRDS("long_projections_error_weekly.rds")

weekly_incid <- group_by(
  daily_error, strategy, country, forecast_week, week_of_projection
) %>%
  summarise(weekly_incid = sum(obs)) %>%
  ungroup()

weekly_incid$category <- case_when(
  weekly_incid$weekly_incid <= 10 ~ "less_than_10",
  weekly_incid$weekly_incid > 10 &
  weekly_incid$weekly_incid <= 100 ~ "less_than_100",
  weekly_incid$weekly_incid > 100 &
  weekly_incid$weekly_incid <= 500 ~ "less_than_500",
  weekly_incid$weekly_incid > 500  ~ "greater_than_500",
)

weekly_error <- left_join(weekly_error, weekly_incid)
weekly_error$category <- factor(
  weekly_error$category,
  levels = c("less_than_10", "less_than_100",
             "less_than_500", "greater_than_500"),
  ordered = TRUE
)

by_strategy <- split(weekly_error, weekly_error$strategy)

cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
names(cb_palette) <- levels(weekly_error$category)

plots_by_incid <- map(
  by_strategy,
  function(df) {
    df$week_of_projection <- factor(df$week_of_projection)
    df$rel_mae <- log(df$rel_mae, 10)
    p <- ggplot(
      df, aes(week_of_projection, rel_mae, fill = category),
      ) +
      geom_boxplot(position = "dodge", alpha = 0.5) +
      scale_fill_manual(
        values = cb_palette,
        name = "Weekly incidence",
        breaks = c("less_than_10", "less_than_100",
                   "less_than_500", "greater_than_500"),
        labels = c(" <= 10", " <= 100", " <= 500", "> 500")
      ) +
      theme_minimal() +
      xlab("Week of projection") +
      ylab("(log) Relative mean error") +
      theme(
        legend.position = "bottom"
      )
    p
  }
)

iwalk(
  plots_by_incid,
  function(p, strategy) {
    outfile <- glue("error_by_incid_level_{strategy}.tiff")
    rincewind::save_multiple(p, outfile, two_col = FALSE)
  }
)

weeks_combined <- readRDS("length_weeks_combined.rds")

x <- left_join(weekly_error, weeks_combined)

x$flag <- case_when(
  x$week_of_projection <= x$weeks_combined ~ "within",
  x$week_of_projection > x$weeks_combined ~ "outside"
)

x <- split(x, x$strategy)

plots <- map(
  x,
  function(df) {

    df$week_of_projection <- factor(df$week_of_projection)

    p <- ggplot() +
      geom_half_violin(
        data = df[df$flag == "within", ],
        aes(week_of_projection, log(rel_mae), fill = "red"),
        draw_quantiles = c(0.25, 0.5, 0.75),
        side = "l", alpha = 0.3
      ) +
      geom_half_violin(
        data = df[df$flag != "within", ],
        aes(week_of_projection, log(rel_mae), fill = "blue"),
        draw_quantiles = c(0.25, 0.5, 0.75),
        side = "r", alpha = 0.3
      ) +
      theme_minimal() +
      xlab("Week of projection") +
      ylab("(log) Relative mean error") +
      scale_fill_identity(
        guide = "legend",
        breaks = c("red", "blue"),
        labels = c("<= weeks combined", "> weeks combined"),
        name = "Projection horizon"
      ) +
      theme(legend.position = "bottom")

    p
  }
)

iwalk(
  plots, function(p, strategy) {
    outfile <- glue("within_or_without_window_{strategy}.tiff")
    rincewind::save_multiple(p, outfile, two_col = FALSE)
  }
)

better_than_null <- readRDS("better_than_null.rds")

weekly$country <- factor(
  weekly$country, levels = better_than_null$country, ordered = TRUE
)

weekly$week_of_projection <- factor(weekly$week_of_projection)

country_groups <- readRDS("country_groups.rds")

compare <- ggplot(
  weekly, aes(factor(week_of_projection), log(rel_mae), fill = strategy)
) +
  geom_boxplot(position = "dodge") +
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
x_labels <- out[["x_labels"]]
y_labels <- out[["y_labels"]]
