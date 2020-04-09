## Observations in tall format
model_input <- readr::read_rds("model_input.rds")
obs_deaths <- model_input [["D_active_transmission"]]
obs_deaths <- tidyr::gather(
  obs_deaths,
  country,
  deaths,
  -dates
)

ensb_pred <- readr::read_rds("ensemble_daily_qntls.rds")
ensb_pred <- na.omit(ensb_pred)

plots <- split(ensb_pred, ensb_pred$si) %>%
  purrr::map(
    function(pred) {
      pred$date <- as.Date(pred$date)
      nice_names <- snakecase::to_title_case(pred$country)
      names(nice_names) <- pred$country

      projection_plot(obs_deaths, pred)
    }
  )


## 6 pages in each plot so.
n_pages <- ceiling(length(unique(ensb_pred1$country)) / 6)
purrr::iwalk(
  plots,
  function(p, si) {
    purrr::walk(
      seq_len(n_pages),
      function(page_num) {
        p <- p +
          ggforce::facet_wrap_paginate(
            ~country,
            scales = "free_y",
            labeller = as_labeller(
              nice_names
            ),
            ncol = 2,
            nrow = 3,
            page = page_num
          )

        outfile <- glue::glue("ensmbl_pred_{si}_page_{page_num}.png")

        ggplot2::ggsave(filename = outfile, plot = p)
      }
    )
  }
)

##### Model 1 Projection Plots

