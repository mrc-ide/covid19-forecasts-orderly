## orderly::orderly_develop_start(parameters = list(week_ending = "2021-04-18"), use_draft = "newer")
dir.create("figures")
palette <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
names(palette) <- c("Model 4", "Model 2", "Model 1", "Model 3", "Ensemble")

dates_forecast <- seq(
  as.Date(week_ending) + 1, length.out = 7, by = "1 day"
)

## exclude some states due to one-off data anomalies or because they only report weekly
exclude <- c("Alabama", # 3 report/week
             "Alaska", # 2 report/week
             "Arizona", # 6 report/week # Appear to be reporting daily again (09 Aug)
                        # Exclude again: no reports 4, 11, 17/18 October, 15 November
                        # Include again 06/12/21
                        # Exclude again 10/01/22 - no reports 20th/27th/28th Dec, 3rd/9th Jan
             #"Arkansas", # Now reporting at weekends again (Update from JHU email 03 August)
                         # exclude again: anomalous large report 10/11 October
                         # JHU email 12 October states that these are from throughout pandemic, no indication of size of backlog
                         # no reports 26th/28th November - Thanksgiving
                        # Include again 13th Dec
                        # Exclude again 10th Jan
             # "California", # 5 report/week according to JHU email,
                         # but still seems to have daily reports
                         # anomalous large report 19 October. Include again 22 Nov.
             "Colorado", # 5 report / week
             "Connecticut", # < 7 report / week (unclear on exact reporting freq.)
             "Delaware", # < 7 report / week
             "Florida", # 1 report/week
             "Georgia", # 5 report/week
             "Guam", # < 7 report / week
             "Hawaii", # < 7 report/week
             "Idaho", # 5 report/week
             "Illinois", # 5 report/week
             "Indiana", # 6 report/week
             "Iowa", # 1 report / week
             "Kansas", # 3 report/week
             "Kentucky", # 5 report/week
             "Louisiana", # 5 report/week
             "Maine", # 5 report / week
             "Maryland", # no report 19 September
                         # no report 25/26 November (Thanksgiving)
             "Massachusetts", # 5 report / week
             "Michigan", # 2 report/week
             "Minnesota", # 5 report / week
             "Mississippi", # 5 report / week
             "Missouri", # 6 report / week
                           # seems to be back to reporting every day (23/08/21)
                           # no report 25 October, 1st or 7th/8th/11th/15th November
                           # no report 25/26 Nov - Thanksgiving
                           # v large spike from addition of probable deaths on 02/12/21
             "Montana", # 5 report / week
             "Nebraska", # 5 report / week
             "Nevada", # 5 report/week
             "New Hampshire", # 5 report / week
             # "New Jersey", # no report 31 October, 4 November
             "New Mexico", # 3 report/week
             "North Carolina", # 5 report / week
             "North Dakota", # <7 report/day
             "Ohio", # 2 report / week
             "Oklahoma", # 1 report / week
             "Oregon", # 5 report / week
             #"Pennsylvania", # reporting daily again 
                            # no reports weekend 9/10 Oct or 16/17 Oct, 7 Nov
                            # no report 26 Nov - Thanksgiving
                            # Included again 13th Dec
             "Puerto Rico", # gets shown in the country level forecasts
             "Rhode Island", # <7 reports / week
             "South Carolina", # 5 report / week
             "South Dakota", # 1 report/week
             "Tennessee", # 5 report / week
             "Texas", # reports for 9/10 Oct are anomalously low (only 2/3)
                      # 13 Oct figure v large (backlog from previous days?)
                      # 08/11/21: include again
                      # no report 25-28 Nov - Thanksgiving
                      # Included again 20 Dec
                      # Excluded again 10th Jan - no reporting for 6 days (Xmas and NY)
             "Utah", # 5 report/week
             "Vermont", # not daily reports
             "Virginia", # 5 report / week
             "Washington", # 5 report / week
             "West Virginia", # 5 report / week
             "Wisconsin", # 5 report / week
             "Wyoming" # 5 report / week
             
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
##states_to_draw <- unique(ensemble_rt_wide$state)[seq_len(ceiling(nstates / 2))]
## Can put all on the same graph.
states_to_draw <- unique(ensemble_rt_wide$state)
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

p <- cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2, align = "v", axis = "l")

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
##states_to_draw <- unique(ensemble_rt_wide$state)[seq_len(ceiling(nstates / 2))]
max_rt_to_draw <- max(x$`97.5%`)
states_to_draw <- unique(ensemble_rt_wide$state)
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

p <- cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2, align = "hv", axis = "l")

ggsave("figures/us_indvdl_rt_line_1.png", p1)
ggsave("figures/us_indvdl_rt_line_2.png", p2)
ggsave("figures/us_indvdl_rt_line.png", p)

files2zip <- dir('figures', full.names = TRUE)
zip::zip(zipfile = 'us-figs.zip', files = files2zip)
