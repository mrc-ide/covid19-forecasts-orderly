## orderly::orderly_develop_start(parameters = list(week_ending = "2020-04-19"), use_draft = "newer")

ifr <- readRDS("pop_wtd_ifr_qntls.rds")
ifr$country <- snakecase::to_title_case(ifr$location)

reff <- readRDS("unweighted_reff.rds")
reff$country <- snakecase::to_title_case(reff$country)

reff <- left_join(
  reff, ifr, by = "country", suffix = c("_underrep", "_ifr")
)
reff$underreporting <- as.integer(reff$underreporting)

ps <- reff[reff$var == "p_s", ]
##ps$underreporting <- factor(ps$underreporting, levels = 1:100)
ps$country <- forcats::fct_reorder(ps$country, ps$`50%_underrep`, max)
##ps$underreporting <- as.numeric(ps$underreporting)

p <- ggplot(ps, aes(underreporting, country, fill = `50%_underrep`)) +
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

ggsave(
  filename = "proportion_susceptible_underreporting.png", p
)

reff <- reff[reff$var != "p_s", ]
reff_lim <- reff[reff$`50%_underrep` < 10, ]

reff_lim <-  select(
  reff_lim, country, underreporting,
  `50%_underrep`, var, continent, `50%_ifr`, deaths_per_million
) %>%
  tidyr::spread(var, `50%_underrep`)

reff_lim$`50%_ifr` <- signif(reff_lim$`50%_ifr`, digits = 2)


labels <- reff_lim[reff_lim$`r_eff` > 3, ]
labels <- na.omit(labels)
labels <- group_by(labels, country) %>%
  slice_max(underreporting) %>% ungroup()
labels$ifr <- scales::percent(labels$`50%_ifr`, accuracy = 0.1)

## Setting labels by hand
labels$r_eff[labels$country == "Belgium"] <- 8.90
labels$underreporting[labels$country == "France"] <- 46
labels$r_eff[labels$country == "France"] <- 9.20
labels$underreporting[labels$country == "Ireland"] <- 45
labels$underreporting[labels$country == "Italy"] <- 38
labels$r_eff[labels$country == "United Kingdom"] <- 8.8
labels$underreporting[labels$country == "United Kingdom"] <- 45
labels$country[labels$country == "United Kingdom"] <- "UK"
labels$country[labels$country == "United States of America"] <- "USA"

labels$label <- glue::glue(
  "{labels$country} ({labels$deaths_per_million}, {labels$ifr})"
  )


palette <- c(
 "Africa" = "#000000",
 "Asia" = "#E69F00",
 "Europe" = "#56B4E9",
 "North America" = "#009E73",
 "South America" = "#0072B2",
 "Oceania" = "#D55E00"
)


p <- ggplot() +
  geom_line(
    data = reff_lim,
    aes(underreporting, r_eff, group = country, col = continent)
  ) +
  theme_minimal() +
  scale_color_manual(values = palette) +
  expand_limits(y = 0) +
  geom_hline(
    yintercept = 1, linetype = "dashed"
  ) +
  ggtext::geom_richtext(
    data = labels, aes(underreporting, r_eff, label = label),
    size = 3,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
  xlim(1, 50) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x.bottom = element_text(
      angle = 90, hjust = 0.5, vjust = 0.5
    )
  ) +
  ylab("Effective Reproduction Number") +
  xlab("Underreporting")

ggsave(
  "reffective_underreporting.png", p
)
 ## facet_wrap(~continent, ncol = 2)
