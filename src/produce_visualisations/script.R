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
ensb_pred$week_ending <- ensb_pred$proj

plots <- split(ensb_pred, ensb_pred$si) %>%
  purrr::map(
    function(pred) {
      pred$date <- as.Date(pred$date)
      projection_plot(obs_deaths, pred)
    }
  )

nice_names <- snakecase::to_title_case(unique(ensb_pred$country))
names(nice_names) <- unique(ensb_pred$country)
## 6 pages in each plot so.
n_pages <- ceiling(length(unique(ensb_pred$country)) / 6)
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
daily_predictions_qntls <- readRDS("daily_predictions_qntls.rds")

daily_predictions_qntls <- tidyr::separate(
  daily_predictions_qntls,
  col = "model",
  into = c("proj", NA, NA, NA, NA, "week_ending"),
  sep = "_"
  )



by_model_si <- split(
  daily_predictions_qntls,
  list(daily_predictions_qntls$proj, daily_predictions_qntls$si),
  sep = "_"
)

plots <- purrr::map(
    by_model_si,
    function(pred) {
      pred$date <- as.Date(pred$date)
      projection_plot(obs_deaths, pred)
    }
  )

nice_names <- snakecase::to_title_case(unique(daily_predictions_qntls$country))
names(nice_names) <- unique(daily_predictions_qntls$country)
## 6 pages in each plot so.
n_pages <- ceiling(length(unique(daily_predictions_qntls$country)) / 6)
purrr::iwalk(
  plots,
  function(p, model_si) {
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

        outfile <- glue::glue("{model_si}_page_{page_num}.png")

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
ensemble_rt$model <- paste0("ensemble_", ensemble_rt$model)
model_rt <- readRDS("model_rt_qntls.rds")
model_rt$model <- gsub(
  x = model_rt$model,
  pattern = "Std_results_week_end_",
  replacement = ""

)
## Re-shuffle columns so that we can rbind
model_rt <- model_rt[, colnames(ensemble_rt)]
rt_both <- rbind(ensemble_rt, model_rt)
rt_both <- tidyr::separate(
  rt_both,
  col = "model",
  into = c("model", "date"),
  sep = "_"
)
rt_both <- tidyr::spread(rt_both, key = quantile, value = out2)

plots <- split(
  rt_both,
  list(rt_both$model, rt_both$si, rt_both$date), sep = "_"
) %>% purrr::map(rt_plot)

purrr::iwalk(
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

### For ensemble plots, need to split by date

plots <- split(
  rt_both,
  rt_both$date,
) %>% purrr::map(rt_plot)

purrr::iwalk(
  plots,
  function(p, date) {
    outfile <- glue::glue("ensemble_rt_{date}.png")
    ggsave(
      filename = outfile,
      plot = p,
      width = fig_size$fig.width,
      height = fig_size$fig.height,
      unit = fig_size$units
    )
  }
)
