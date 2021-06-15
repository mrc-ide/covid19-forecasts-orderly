## orderly::orderly_develop_start(parameters = list(week_ending = "2021-04-18"), use_draft = "newer")
dir.create("figures")
palette <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
names(palette) <- c("Model 4", "Model 2", "Model 1", "Model 3", "Ensemble")

dates_forecast <- seq(
  as.Date(week_ending) + 1, length.out = 7, by = "1 day"
)

## exclude some states due to one-off data anomalies or because they only report weekly
exclude <- c(# "Montana",
             # "Missouri", # numbers reported on 13/14th April inconsistent with trend
              "Florida", # report weekly
             "Ohio",  
             "Oklahoma" #, # report weekly
             # "Oregon" # anomalously high reported deaths on 6 April
             )


## ensemble projections
ensemble_forecasts_qntls <- readRDS("us_ensemble_forecasts_qntls.rds")
ensemble_forecasts_qntls <- ensemble_forecasts_qntls[ensemble_forecasts_qntls$si == "si_2", ]
model_inputs <- readRDS("latest_model_input.rds")

ensemble_forecasts_qntls <- ensemble_forecasts_qntls[! ensemble_forecasts_qntls$state %in% exclude, ]

tall <- gather(model_inputs$D_active_transmission, state, deaths, -dates)
tall <- tall[tall$state %in% unique(ensemble_forecasts_qntls$state), ]
tall <- tall[! tall$state %in% exclude, ]
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
    ) + theme(legend.position = "none")
  ggsave(glue("figures/us_ensemble_forecasts_{page}.png"), p)

}

#############################################################
############ Forecasts from individual models
#############################################################

m1_forecasts <- readRDS("rti0_qntls.rds") %>%
  filter(! state %in% exclude) %>%
  pivot_longer(cols = as.character(dates_forecast), names_to = "date") %>%
  pivot_wider(names_from = "qntl", values_from = "value")

m2_forecasts <- readRDS("apeestim_qntls.rds") %>%
  filter(! state %in% exclude) %>%
    pivot_longer(cols = as.character(dates_forecast), names_to = "date") %>%
  pivot_wider(names_from = "qntl", values_from = "value")

m3_forecasts <- readRDS("deca_qntls.rds") %>%
  filter(! state %in% exclude) %>%
  pivot_longer(cols = as.character(dates_forecast), names_to = "date") %>%
  pivot_wider(names_from = "qntl", values_from = "value")

m1_forecasts$proj <- "Model 1"
m2_forecasts$proj <- "Model 2"
m3_forecasts$proj <- "Model 3"

x <- rbind(m1_forecasts, m2_forecasts, m3_forecasts)
x$date <- as.Date(x$date)
pbase <- projection_plot(tall, x)
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
  if (page > 1) p <-  p + theme(legend.position = "none")
  ggsave(glue("figures/us_indvdl_forecasts_{page}.png"), p)

}

#############################################################
######################## Rt Line graph
#############################################################

ensemble_rt <- readRDS("us_ensemble_rt_qntls.rds") %>%
  filter(! state %in% exclude)
ensemble_rt_wide <- spread(ensemble_rt, quantile, out2)
ensemble_rt_wide$proj <- "Ensemble"
ensemble_rt_wide <- ensemble_rt_wide[ensemble_rt_wide$si == "si_2", ]

## Divide the list of states in roughly half
## so that plot is not cluttered

ensemble_rt_wide$state <- reorder(ensemble_rt_wide$state, -ensemble_rt_wide$`50%`)
ensemble_rt_wide <- ensemble_rt_wide[order(ensemble_rt_wide$state),]
nstates <- length(unique(ensemble_rt_wide$state))
states_to_draw <- unique(ensemble_rt_wide$state)[seq_len(ceiling(nstates / 2))]
max_rt_to_draw <- max(ensemble_rt_wide$`97.5%`)

x <- ensemble_rt_wide[ensemble_rt_wide$state %in% states_to_draw, ]

p1 <- rt_boxplot(x, rincewind::nice_country_name(unique(x$state))) +
  theme(legend.position = "none") +
  xlim(0, ceiling(max_rt_to_draw))

pline1 <- rt_lineplot(x, rincewind::nice_country_name(unique(x$state))) +
  ylim(0, ceiling(max_rt_to_draw))

last_drawn <- ceiling(nstates / 2)
states_to_draw <- unique(ensemble_rt_wide$state)[seq(last_drawn + 1, nstates)]
x <- ensemble_rt_wide[ensemble_rt_wide$state %in% states_to_draw, ]
p2 <- rt_boxplot(x, rincewind::nice_country_name(unique(x$state))) +
  theme(legend.position = "none") +
  xlim(0, ceiling(max_rt_to_draw))
pline2 <- rt_lineplot(x, rincewind::nice_country_name(unique(x$state))) +
  ylim(0, ceiling(max_rt_to_draw))

p <- cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)

ggsave("figures/us_ensemble_rt_line_1.png", pline1)
ggsave("figures/us_ensemble_rt_line_2.png", pline2)
ggsave("figures/us_ensemble_rt_box_1.png", p1)
ggsave("figures/us_ensemble_rt_box_2.png", p2)
ggsave("figures/us_ensemble_rt_box.png", p)

#############################################################
############ Rt plots for individual models
#############################################################
#############################################################

m1_rt <- readRDS("rti0_rt_qntls.rds") %>%
  filter(! state %in% exclude) %>%
  spread(qntl, out2)

m2_rt <- readRDS("apeestim_rt_qntls.rds") %>%
  filter(! state %in% exclude) %>%
  spread(qntl, out2)

m3_rt <- readRDS("deca_rt_qntls.rds") %>%
  filter(! state %in% exclude) %>%
    spread(qntl, out2)

m1_rt$proj <- "Model 1"
m2_rt$proj <- "Model 2"
m3_rt$proj <- "Model 3"
##ensemble_rt_wide <- ensemble_rt_wide[, colnames(m1_rt)]

x <- rbind(m1_rt, m2_rt, m3_rt)
states_to_draw <- unique(ensemble_rt_wide$state)[seq_len(ceiling(nstates / 2))]
max_rt_to_draw <- max(x$`97.5%`)
x1 <- x[x$state %in% states_to_draw, ]
x1$state <- factor(x1$state, levels = states_to_draw, ordered = TRUE)
p1 <- rt_lineplot(x1, rincewind::nice_country_name(levels(x1$state))) +
  ylim(0, ceiling(max_rt_to_draw))

last_drawn <- ceiling(nstates / 2)
states_to_draw <- unique(ensemble_rt_wide$state)[seq(last_drawn + 1, nstates)]
x1 <- x[x$state %in% states_to_draw, ]
x1$state <- factor(x1$state, levels = states_to_draw, ordered = TRUE)
p2 <- rt_lineplot(x1, rincewind::nice_country_name(levels(x1$state))) +
  theme(legend.position = "none") +
  ylim(0, ceiling(max_rt_to_draw))

p <- cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2, align = "hv")

ggsave("figures/us_indvdl_rt_line_1.png", p1)
ggsave("figures/us_indvdl_rt_line_2.png", p2)
ggsave("figures/us_indvdl_rt_line.png", p)

