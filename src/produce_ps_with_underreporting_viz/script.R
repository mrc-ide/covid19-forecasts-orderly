## orderly::orderly_develop_start(parameters = list(week_ending = "2020-11-29"), use_draft = "newer")

ifr <- readRDS("pop_wtd_ifr_qntls.rds")
ifr$country <- snakecase::to_title_case(ifr$location)

reff <- readRDS("weighted_per_country_reff.rds")
reff$country <- snakecase::to_title_case(reff$country)
reff$underreporting <- as.integer(reff$underreporting)

ps <- reff[reff$var == "p_s", ]
##ps$underreporting <- factor(ps$underreporting, levels = 1:100)
ps$country <- forcats::fct_reorder(ps$country, ps$`50%`, max)
##ps$underreporting <- as.numeric(ps$underreporting)
ps <- ps[ps$underreporting < 51, ]

p <- ggplot(ps, aes(underreporting, country, fill = `50%`)) +
  geom_tile() +
  scale_fill_distiller(
    palette = "Greens",
    guide = guide_colourbar(
      title = "Median proportion susceptible",
      title.position = "left",
      title.vjust = 0.5
    ),
    direction = -1,
    breaks = c(0, 0.5, 1),
    limits = c(0, 1)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "lines")
  ) +
  xlab("Underreporting") +
  ylab("")

save_multiple(p, "proportion_susceptible_underreporting.tiff")

######### Proportion susceptible at the last day of analysis
#########
observed_ps <- readRDS("ps_qntls.rds")
ps_last_day <- map_dfr(
  observed_ps, function(x)tail(x, 1), .id = "country"
)

ps_last_day <- country_to_continent(ps_last_day, "country")

ps_last_day$color <- case_when(
  ps_last_day$continent == "Africa" ~ "#000000",
  ps_last_day$continent == "Asia" ~ "#E69F00",
  ps_last_day$continent == "Europe" ~ "#56B4E9",
  ps_last_day$continent == "North America" ~ "#009E73",
  ps_last_day$continent == "South America" ~ "#0072B2",
  ps_last_day$continent == "Oceania" ~ "#D55E00"
)


ps_last_day$country <- nice_country_name(ps_last_day$country)

ps_last_day$label <- glue(
  "<i style='color:{ps_last_day$color}'>{ps_last_day$country}</i>"
)

ps_last_day <- arrange(ps_last_day, continent, `50%`)
ps_last_day$label <- factor(
  ps_last_day$label, unique(ps_last_day$label), ordered = TRUE
)

ps_last_day1 <- filter(ps_last_day, continent %in% c("Africa", "Asia"))
ps_last_day1 <- droplevels(ps_last_day1)

ps_last_day2 <- filter(
  ps_last_day, !continent %in% c("Africa", "Asia")
)
ps_last_day2 <- droplevels(ps_last_day2)


p1 <- ggplot(ps_last_day1) +
  geom_point(aes(label, `50%`, col = color)) +
  geom_linerange(
    aes(x = label, ymin = `2.5%`, ymax = `97.5%`, col = color)
  )


p2 <- ggplot(ps_last_day2) +
  geom_point(aes(label, `50%`, col = color)) +
  geom_linerange(
    aes(x = label, ymin = `2.5%`, ymax = `97.5%`, col = color)
  )


label <- textGrob(
  "Population susceptible (%)", rot = 90, gp = gpar(fontsize = 7)
)

p <- p1 + p2 + plot_layout(ncol = 1) &
  theme_minimal() &
  theme(
    axis.text.x = element_markdown(
      angle = -90, hjust = 0, vjust = 0, size = 6
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  ) &
  scale_color_identity() &
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0, 1)
  )


pfinal <- wrap_elements(label) + wrap_elements(p) +
  plot_layout(ncol = 2, widths = c(0.03, 1))


save_multiple(pfinal, "proportion_susceptible.tiff")
