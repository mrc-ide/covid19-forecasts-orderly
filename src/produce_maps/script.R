# Produce maps of deaths by continent

# need to set this in the more recent versions of sf package
sf_use_s2(FALSE)

# loading the map data (simple features format)
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$geounit != "Antarctica", ]
## 24.01.2022 Weirdly, iso_a3 is NA only for France in world.
## not sure why! setting it explicitly
world$iso_a3[world$name_en == "France"] <- "FRA"

rt_estimates <- readRDS("ensemble_model_rt.rds")
exclude <- readRDS("exclude.rds")
rt_estimates <- rt_estimates[! rt_estimates$country %in% exclude, ]
rt_estimates$iso_a3 <- countrycode(
  to_title_case(rt_estimates$country),
  "country.name",
  "iso3c"
)
rt_estimates <- spread(rt_estimates, quantile, out2)
rt_estimates <- assign_epidemic_phase(rt_estimates)
rt_estimates <- left_join(world, rt_estimates)
# For healthmap
out <- select(
  rt_estimates,
  country, phase, continent, si,
  `2.5%`, `50%`, `97.5%`
)
out <- st_drop_geometry(out)
out$week_of_forecast <- week_ending
out <- na.omit(out)
write_csv(out, "country_epidemic_phase.csv")

x <- split(rt_estimates, rt_estimates$si)

maps <- iwalk(
  x,
  function(x_si, si) {
    world_df_pts <- sf::st_centroid(x_si)
    coords <- as.data.frame(st_coordinates(world_df_pts))
    coords$label <- x_si$geounit
    p <- rt_map(world, x_si, coords)
    outfile <- glue::glue("rt_phase_{si}.png")
    ggplot2::ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)
