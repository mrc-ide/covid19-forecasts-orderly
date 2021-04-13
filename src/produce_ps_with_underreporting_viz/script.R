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
    limits = c(0, 1),
    labels = percent
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.key.width = unit(2, "lines"),
    axis.title.y = element_blank()
  ) +
  xlab("Underreporting")

##save_multiple(p, "proportion_susceptible_underreporting.tiff")
ggsave("proportion_susceptible_underreporting.png", p)
######### Proportion susceptible at the last day of analysis
#########
observed_ps <- readRDS("ps_qntls.rds")
ps_last_day <- map_dfr(
  observed_ps, function(x) tail(x, 1), .id = "country"
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

p1 <- ggplot(ps_last_day) +
  geom_point(aes(`50%`, label, col = color)) +
  geom_linerange(
    aes(xmin = `2.5%`, xmax = `97.5%`, y = label, col = color)
  ) +
  scale_color_identity(
    guide = "legend",
    breaks = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2",
                "#D55E00"),
    labels = c("Africa", "Asia", "Europe",  "North America",
               "South America", "Oceania"),
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0.75, 1)
  ) +
  xlab("Population susceptible (%)") +
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(hjust = 0, vjust = 0, size = 6),
    axis.title.y = element_blank(),
    ##axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )


ggsave("2col_wider_proportion_susceptible.png", p1)
