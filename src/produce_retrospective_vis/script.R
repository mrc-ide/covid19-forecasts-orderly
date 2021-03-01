week_starting <- as.Date("2020-03-08")
week_ending <- as.Date(week_ending)
weeks <- seq(from = week_starting, to = week_ending, by = "7 days")
names(weeks) <- weeks

model_input <- readRDS("model_input.rds")
tall <- tidyr::gather(model_input, country, Deaths, -dates)
tall <- rename(tall, DateRep = "dates")


included <- map_dfr(
  weeks,
  function(week) {
    input_in_week <- tall[tall$DateRep <= week, ]
    wide <- split(input_in_week, input_in_week$country)
    out <- bind_rows(keep(wide, rincewind::deaths_threshold))
    data.frame(country = unique(out$country), week_starting = week)
  }
)


## These are now included in the ECDC data. ECDC doesn;t distuish
## N and S America
## continents <- readr::read_csv(
##   "COVID-19-geographic-disbtribution-worldwide-2020-06-14.csv"
## ) %>%
##   dplyr::select(`Countries and territories`, popData2018, continent) %>%
## dplyr::distinct() %>%
## janitor::clean_names()

continents <- readr::read_csv("country-and-continent-codes-list.csv")
continents <- janitor::clean_names(continents)
## Some are dups, not that it matters..
dups <- duplicated(continents$three_letter_country_code)
continents <- continents[!dups, ]
continents <- continents[, c(
  "continent_name",
  "three_letter_country_code"
)]
included$country <- as.character(included$country)
included$iso3c <- countrycode::countrycode(
  snakecase::to_title_case(included$country), "country.name", "iso3c"
)
included$country[included$country == "Czech_Republic"] <- "Czechia"

included <- left_join(
  included, continents, by = c("iso3c" = "three_letter_country_code")
)

palette <- rincewind::continent_colorscale()


x <- count(included, week_starting, continent_name)
x$week_starting <- as.Date(x$week_starting)


pline <- ggplot() +
  geom_line(
    data = x, aes(week_starting, n, col = continent_name), size = 1.5
  ) +
  scale_x_date(
    breaks = seq(
      from = as.Date("2020-03-01"),
      to = week_ending,
      by = "8 weeks"
    ),
    limits = c(as.Date("2020-03-01"), week_ending)
  ) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = palette) +
  ylab("Countries with active transmission") +
  xlab("Week Starting")


ggsave("n_included_line.png", pline)

## One version for SI.

pline2 <- ggplot() +
  geom_line(
    data = x, aes(week_starting, n, col = continent_name), size = 1.5
  ) +
  scale_x_date(
    breaks = seq(
      from = as.Date("2020-03-01"),
      to = week_ending,
      by = "8 weeks"
    ),
    limits = c(as.Date("2020-03-01"), as.Date("2020-12-10")),
    date_labels = "%d - %b"
  ) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_manual(values = palette) +
  ylab("Countries with active transmission") +
  xlab("Week Starting")

ggsave("n_included_line_si.png", pline2)

#####################################################################
#####################################################################
############# Epicurve by continent #################################
#####################################################################
#####################################################################

model_input <- tall[tall$country %in% included$country, ]
model_input$iso3c <- countrycode::countrycode(
  snakecase::to_title_case(model_input$country), "country.name", "iso3c"
)

model_input <- left_join(
  model_input, continents, by = c("iso3c" = "three_letter_country_code")
)

by_continent <- group_by(model_input, DateRep, continent_name) %>%
  summarise(deaths = sum(Deaths)) %>%
  ungroup()


epicurve <- ggplot() +
  geom_line(
    data = by_continent,
    aes(DateRep, deaths, col = continent_name),
    size = 1.2
  ) +
  scale_color_manual(values = palette) +
  scale_x_date(
    breaks = seq(
      from = as.Date("2020-03-01"),
      to = week_ending,
      by = "8 weeks"
    ),
    limits = c(as.Date("2020-03-01"), week_ending)
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  xlab("") +
  ylab("Deaths")


## labels <- dplyr::left_join(labels, y, by = c("label" = "continent_name"))
## epicurve <- epicurve +
##   geom_text_repel(
##     data = labels, aes(x = x, y = deaths, label = label, col = label)
##   ) + coord_cartesian(clip = "off")

ggsave("epicurve_by_continent.png", epicurve)


## Side by side:
p <- cowplot::plot_grid(epicurve, pline, nrow = 2, labels = "AUTO", align = "hv")
ggsave("epicurve_pline.png", p)

readr::write_csv(included, "countries_included_each_week.csv")
