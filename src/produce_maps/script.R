# Produce maps of deaths by continent

# need to set this in the more recent versions of sf package
sf_use_s2(FALSE)

# loading the map data (simple features format)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$geounit != "Antarctica", ]
## 24.01.2022 Weirdly, iso_a3 is NA only for France in world.
## not sure why! setting it explicitly
world$iso_a3[world$name_en == "France"] <- "FRA"
weekly_qntls <- readRDS("ensemble_weekly_qntls.rds")

weekly_qntls$iso_a3 <-countrycode::countrycode(
  snakecase::to_title_case(weekly_qntls$country),
  "country.name",
  "iso3c"
)
if (any(is.na(weekly_qntls$iso_a3))) {
  warning(print("Country names need matching to ISO 3 codes"))
}

weekly_qntls <- dplyr::left_join(world, weekly_qntls)
# generating the ISO3 code for the model predictions to link with the simple features map data

rt_estimates <- readRDS("ensemble_model_rt.rds")
exclude <- readRDS("exclude.rds")
rt_estimates <- rt_estimates[! rt_estimates$country %in% exclude, ]
rt_estimates$iso_a3 <-countrycode::countrycode(
  snakecase::to_title_case(rt_estimates$country),
  "country.name",
  "iso3c"
)
rt_estimates <- tidyr::spread(rt_estimates, quantile, out2)
rt_estimates <- rincewind::assign_epidemic_phase(rt_estimates)

rt_estimates <- dplyr::left_join(world, rt_estimates)
# filling in the missing values

x <- split(weekly_qntls, weekly_qntls$si)

maps <- purrr::iwalk(
  x,
  function(x_si, si) {
    world_df_pts <- sf::st_centroid(x_si)
    coords <- as.data.frame(st_coordinates(world_df_pts))

    coords$label <- paste0(
      x_si$geounit, "\n",
      prettyNum(
        signif(x_si$`50%`, 3),
        big.mark = ","
      )
    )
    p <- deaths_map(world, world_df_pts, coords)
    outfile <- glue::glue("median_deaths_{si}.png")
    ggplot2::ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)


x <- split(rt_estimates, rt_estimates$si)

maps <- purrr::iwalk(
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
