## orderly::orderly_develop_start(parameters = list(week_ending = "2021-03-28"), use_draft = "newer")
palette <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
names(palette) <- c("Model 4", "Model 2", "Model 1", "Model 3", "Ensemble")

rt_boxplot <- function(rt, nice_names, position = "left") {

  #nice_names <- snakecase::to_title_case(rt$state)
  #names(nice_names) <- rt$state
  ##rt$state <- reorder(rt$state, -rt$`50%`)
  rt$state <- reorder(rt$state, -rt$`50%`)
  p <- ggplot(rt) +
  geom_boxplot(
    aes(
      y = state,
      xmin = `2.5%`,
      xmax = `97.5%`,
      xmiddle = `50%`,
      xlower = `25%`,
      xupper = `75%`,
      fill = proj
    ),
    alpha = 0.3,
    stat = "identity"
  ) +
    xlab("Effective Reproduction Number") +
    ylab("") +
    scale_y_discrete(labels = nice_names, position = "right") +
    geom_vline(
      xintercept = 1,
      linetype = "dashed"
    ) + theme_minimal() +
    scale_fill_manual(values = palette) +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )

  p
}

projection_plot <- function(obs, pred, date_min = "2021-01-01") {
  ## Plot only the latest projections.
  palette <- c("#56B4E9", "#009E73", "#D55E00", "#CC79A7")
  names(palette) <- c(
    "Model 2", "Model 1", "Model 3", "Ensemble"
  )
  date_min <- as.Date(date_min)
  date_max <- max(pred$date) + 2
  dates_to_mark <- seq(
    from = date_min,
    to = date_max,
    by = "1 day"
  )
  dates_to_mark <- dates_to_mark[weekdays(dates_to_mark) == "Monday"]
  idx <- seq(from = length(dates_to_mark), to = 1, by = -3)
  dates_to_mark <- dates_to_mark[rev(idx)]
  ## Get dates of adding vlines.
  window_eps <- group_by(pred, proj) %>%
    summarise(date = min(date)) %>%
    ungroup()

  window_eps$xintercepts <- as.numeric(window_eps$date - 1) + 0.5
  ## To get nice labels
  ## https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }

  p <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
    geom_line(
      data = pred,
      aes(date, `50%`, col = proj, group = proj),
      size = 1.1
    ) +
    geom_ribbon(
      data = pred,
      aes(x = date,
          ymin = `2.5%`,
          ymax = `97.5%`,
          fill = proj,
          group = proj),
      alpha = 0.4) +
    scale_color_manual(
      values = palette,
      aesthetics = c("color", "fill"),
    ) +
    theme_minimal() +
    theme(legend.position = "top", legend.title = element_blank()) +
    scale_x_date(breaks = dates_to_mark, limits = c(date_min, date_max)) +
    scale_y_continuous(breaks = integer_breaks()) +
    geom_vline(
      xintercept = c(
        window_eps$xintercepts
      ),
      linetype = "dashed"
    ) + xlab("") +
    ylab("Deaths") +
    theme(
      axis.text.x = element_text(angle = -90)
    )

  p
}



## ensemble projections
ensemble_forecasts_qntls <- readRDS("us_ensemble_forecasts_qntls.rds")
ensemble_forecasts_qntls <- ensemble_forecasts_qntls[ensemble_forecasts_qntls$si == "si_2", ]
model_inputs <- readRDS("latest_model_input.rds")
tall <- tidyr::gather(model_inputs$D_active_transmission, state, deaths, -dates)
ensemble_forecasts_qntls$date <- as.Date(ensemble_forecasts_qntls$date)
ensemble_forecasts_qntls$proj <- "Ensemble"

nrows <- 3
ncols <- 1
pbase <- projection_plot(tall, ensemble_forecasts_qntls)
p <- pbase +
  facet_wrap_paginate(
    ~state, ncol = ncols, nrow = nrows, page = 1, scales = "free_y"
  )

npages <- n_pages(p)


for (page in seq_len(npages)) {
  p <- pbase +
    facet_wrap_paginate(
      ~state, ncol = ncols, nrow = nrows, page = page, scales = "free_y"
    )
  ggsave(glue::glue("us_ensemble_forecasts_{page}.png"), p)

}

######################## Rt Line graph

ensemble_rt <- readRDS("us_ensemble_rt_qntls.rds")
ensemble_rt_wide <- tidyr::spread(
  ensemble_rt, quantile, out2
)
ensemble_rt_wide$proj <- "Ensemble"
ensemble_rt_wide <- ensemble_rt_wide[ensemble_rt_wide$si == "si_2", ]

## Divide the list of states in roughly half
## so that plot is not cluttered

nstates <- length(unique(ensemble_rt_wide$state))
states_to_draw <- unique(ensemble_rt_wide$state)[seq_len(ceiling(nstates / 2))]

x <- ensemble_rt_wide[ensemble_rt_wide$state %in% states_to_draw, ]

p1 <- rt_boxplot(x, rincewind::nice_country_name(unique(x$state)))

last_drawn <- ceiling(nstates / 2)
states_to_draw <- unique(ensemble_rt_wide$state)[seq(last_drawn + 1, nstates)]
x <- ensemble_rt_wide[ensemble_rt_wide$state %in% states_to_draw, ]
p2 <- rt_boxplot(x, rincewind::nice_country_name(unique(x$state)), "right")

p <- cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)

ggsave("us_ensemble_rt.png", p)

