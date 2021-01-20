## List of continents-coutnry mapping
continents <- readr::read_csv("country_continent.csv")
continents <- janitor::clean_names(continents)


## Observations in tall format
model_input <- readRDS("model_input.rds")
obs_deaths <- model_input [["D_active_transmission"]]
obs_deaths <- tidyr::gather(obs_deaths, country, deaths, -dates)

obs_deaths <- add_continents(obs_deaths, continents)


ensb_pred <- readr::read_rds("ensemble_daily_qntls.rds")
ensb_pred <- na.omit(ensb_pred)
ensb_pred$week_ending <- ensb_pred$proj
ensb_pred$proj <- "Ensemble"
## exclude <- c(
##   "Cameroon", "United_States_of_America",
##   "Yemen", "Democratic_Republic_of_the_Congo", "Mauritania",
##   "Ethiopia", "Ghana", "Kazakhstan",
##   "Zambia", "Kyrgyzstan", "Sudan", "Haiti"
## )
exclude <- readRDS("exclude.rds")
ensb_pred <- ensb_pred[! ensb_pred$country %in% exclude, ]

## Read in the model specific outputs here so that we can construct
## nice names
daily_predictions_qntls <- readRDS("daily_predictions_qntls.rds")
daily_predictions_qntls$model <- glue::glue(
  "{daily_predictions_qntls$model}_{week_ending}"
)
daily_predictions_qntls <- daily_predictions_qntls[! daily_predictions_qntls$country %in% exclude, ]
daily_predictions_qntls <- tidyr::separate(
  daily_predictions_qntls,
  col = "model",
  into = c("proj", NA, NA, "week_ending"),
  sep = "_"
)

sbsm_countries <- dplyr::filter(
  daily_predictions_qntls, proj == "sbsm"
) %>% pull(country) %>% unique
##ensb_pred$proj[ensb_pred$country == "United_Kingdom"] <- "sbsm"

## Make plots for only the latest week.
ensb_pred <- ensb_pred[ensb_pred$week_ending == max(as.Date(ensb_pred$week_ending)), ]
ensb_pred <- add_continents(ensb_pred, continents)

by_continent_si <- split(
  ensb_pred, list(ensb_pred$continent, ensb_pred$si),
  sep = "_"
)
n_cntrs <- map_int(by_continent_si, ~ length(unique(.$country)))

nrows <- 3
ncols <- 1
npanels <- nrows * ncols

n_pages <- ceiling(n_cntrs / npanels)
nice_names <- snakecase::to_title_case(unique(ensb_pred$country))
names(nice_names) <- unique(ensb_pred$country)
nice_names[names(nice_names) %in% sbsm_countries] <-
  glue::glue("{nice_names[names(nice_names) %in% sbsm_countries]}**")

nice_names[! names(nice_names) %in% sbsm_countries] <-
  glue::glue("{nice_names[! names(nice_names) %in% sbsm_countries]}*")

plots <- imap(
  by_continent_si,
  function(pred, continent_si) {
    pred$date <- as.Date(pred$date)
    obs <- obs_deaths[obs_deaths$country %in% pred$country, ]
    countries <- sort(unique(pred$country))
    here_pages <- n_pages[[continent_si]]
    out <- map(seq_len(here_pages), function(page) {
      idx <- seq(to = page * npanels, length.out = npanels, by = 1)
      idx <- idx[idx <= length(countries)]
      cntry_local <- countries[idx]
      obs_local <- obs[obs$country %in% cntry_local, ]
      pred_local <- pred[pred$country %in% cntry_local, ]
      p <- projection_plot(obs_local, pred_local) +
        theme(legend.position = "none") +
         facet_wrap(
           ~country, scales = "free_y", ncol = ncols, nrow = nrows,
           labeller = as_labeller(nice_names),
           )
      if (length(cntry_local) < npanels) {
        ## if the number of countries is smaller, then the facets are
        ## bigger. Add empty grobs to fix the size
        p2 <- ggplot() + theme_void()
        ##p3 <- ggplot() + theme_void()
        p <- cowplot::plot_grid(p, p2, nrow = 2, rel_heights = c(1, 0.25, 0.25))
      }
      p
    }
    )
    out
  }
)


iwalk(
  plots,
  function(ps, continent_si) {
    iwalk(
      ps,
      function(p, page_num) {
        outfile <- glue::glue("ensmbl_pred_{continent_si}_page_{page_num}.png")
        message("Saving ", outfile)
        ggsave(
          filename = outfile,
          plot = p,
          width = fig_size$fig.width,
          height = fig_size$fig.height,
          unit = fig_size$units
        )
      }
    )
  }
)


##### Individual Model Projection Plots

daily_predictions_qntls <- add_continents(
  daily_predictions_qntls, continents
)

daily_predictions_qntls$proj <- dplyr::case_when(

  daily_predictions_qntls$proj == "RtI0" ~ "Model 1",
  daily_predictions_qntls$proj == "sbkp" ~ "Model 2",
  daily_predictions_qntls$proj == "DeCa" ~ "Model 3",
  daily_predictions_qntls$proj == "sbsm" ~ "Model 4"
)

by_model_si <- split(
  daily_predictions_qntls,
  list(
    ##daily_predictions_qntls$proj,
    daily_predictions_qntls$continent,
    daily_predictions_qntls$si
  ),
  sep = "_"
)

by_model_si <- keep(by_model_si, ~ nrow(.) >= 1)

plots <- imap(
  by_model_si,
  function(pred, continent_si) {
    pred$date <- as.Date(pred$date)
    obs <- obs_deaths[obs_deaths$country %in% pred$country, ]
    countries <- sort(unique(pred$country))
    here_pages <- n_pages[[continent_si]]
    out <- map(seq_len(here_pages), function(page) {
      idx <- seq(to = page * npanels, length.out = npanels, by = 1)
      idx <- idx[idx <= length(countries)]
      cntry_local <- countries[idx]
      obs_local <- obs[obs$country %in% cntry_local, ]
      pred_local <- pred[pred$country %in% cntry_local, ]
      p <- projection_plot(obs_local, pred_local) +
        theme(legend.position = "none") +
         facet_wrap(
           ~country, scales = "free_y", ncol = ncols, nrow = nrows,
           labeller = as_labeller(nice_names),
           )
      if (length(cntry_local) < npanels) {
        ## if the number of countries is smaller, then the facets are
        ## bigger. Add empty grobs to fix the size
        p2 <- ggplot() + theme_void()
        ##p3 <- ggplot() + theme_void()
        p <- cowplot::plot_grid(p, p2, nrow = 2, rel_heights = c(1, 0.25, 0.25))
      }
      p
    })
    out
  }
)

#nice_names <- snakecase::to_title_case(unique(daily_predictions_qntls$country))
#names(nice_names) <- unique(daily_predictions_qntls$country)
## 6 pages in each plot so.
iwalk(
  plots,
  function(ps, model_si) {
    iwalk(
      ps,
      function(p, page_num) {
        outfile <- glue::glue("{model_si}_page_{page_num}.png")
        message("Saving ", outfile)
        ggsave(
          filename = outfile,
          plot = p,
          width = fig_size$fig.width,
          height = fig_size$fig.height,
          unit = fig_size$units
        )
      }
    )
  }
)

## Model Rt quantiles
ensemble_rt <- readRDS("ensemble_model_rt.rds")
ensemble_rt <- ensemble_rt[! ensemble_rt$country %in% exclude,]
### For ensemble plots, need to split by date
###
ensemble_rt_wide <- tidyr::spread(
  ensemble_rt, quantile, out2
)
ensemble_rt_wide <- add_continents(ensemble_rt_wide, continents)
ensemble_rt_wide <- ensemble_rt_wide[ensemble_rt_wide$model == max(as.Date(ensemble_rt_wide$model)), ]
ensemble_rt_wide$proj <- "Ensemble"


plots <- split(
  ensemble_rt_wide,
  list(
    ensemble_rt_wide$continent,
    ensemble_rt_wide$si
  ),
  sep = "_"
) %>% map(~ rt_boxplot(., nice_names) + theme(legend.position = "none"))

iwalk(
  plots,
  function(p, date_si) {
    outfile <- glue::glue("ensemble_rt_{date_si}_boxplot.png")
    message(outfile)
    ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)


model_rt <- readRDS("model_rt_qntls.rds")
model_rt <- model_rt[! model_rt$country %in% exclude,]
model_rt$model <- gsub(
  x = model_rt$model,
  pattern = "Std_results_week_end_",
  replacement = ""
)
## Re-shuffle columns so that we can rbind
model_rt <- model_rt[, colnames(ensemble_rt)]
##rt_both <- rbind(ensemble_rt, model_rt)
rt_both <- model_rt
rt_both <- tidyr::separate(
  rt_both,
  col = "model",
  into = c("proj", "date"),
  sep = "_"
)
rt_both <- tidyr::spread(rt_both, key = quantile, value = out2)

rt_both$proj <- dplyr::case_when(

  rt_both$proj == "RtI0" ~ "Model 1",
  rt_both$proj == "sbkp" ~ "Model 2",
  rt_both$proj == "DeCa" ~ "Model 3",
  rt_both$proj == "sbsm" ~ "Model 4",
  rt_both$proj == "Ensemble" ~ "Ensemble"
)

rt_both <- add_continents(rt_both, continents)

plots <- split(
  rt_both,
  list(rt_both$si, rt_both$continent),
  sep = "_"
) %>%
  map(~ rt_lineplot(., nice_names))

iwalk(
  plots,
  function(p, model_si) {
    outfile <- glue::glue("rt_{model_si}.png")
    ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)
