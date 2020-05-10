###
model_input <- readRDS("model_input.rds")
deaths_tall <- tidyr::gather(model_input, country, deaths, -dates)
deaths_tall$dates <- as.Date(deaths_tall$dates)

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
infiles <- infiles[infiles != "model_input.rds"]

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)

## All unweighted ensemble outputs
unweighted_qntls <- purrr::map(
  infiles[grep("unwtd", infiles)], readRDS
)

unweighted_qntls <- dplyr::bind_rows(unweighted_qntls) %>%
    dplyr::filter(si == "si_2")
unweighted_qntls$model <- "Unweighted Ensemble"

## Weighted using weights from previous weeks forecasts only
wtd_prev_week <- purrr::map(
  grep("wtd_prev_week", infiles, value = TRUE), readRDS
)
wtd_prev_week <- dplyr::bind_rows(wtd_prev_week) %>%
  dplyr::filter(si == "si_2")
wtd_prev_week$model <- "Weighted Ensemble (weights previous week)"

## Weighted using weights from all previous forecasts
wtd_all_prev_weeks <- purrr::map(
  grep("wtd_all_prev_weeks", infiles, value = TRUE), readRDS
)
wtd_all_prev_weeks  <- dplyr::bind_rows(wtd_all_prev_weeks) %>%
    dplyr::filter(si == "si_2")
wtd_all_prev_weeks$model <- "Weighted Ensemble (weights all weeks)"

unweighted_qntls$date <- as.Date(unweighted_qntls$date)
wtd_prev_week$date <- as.Date(wtd_prev_week$date)
wtd_all_prev_weeks$date <- as.Date(wtd_all_prev_weeks$date)

##deaths_tall <- deaths_tall[deaths_tall$country %in% unweighted_qntls$country, ]


## levels <- unique(interaction(both$proj, both$model))
## unwtd <- grep(pattern = "Unweighted", x = levels, value = TRUE)
## wtd <- grep(pattern = "Unweighted", x = levels, value = TRUE, invert = TRUE)
## palette <- c(rep("#b067a3", nlevels(levels) / 2),
##              rep("#9c954d", nlevels(levels) / 2))
## names(palette) <- c(unwtd, wtd)


deaths_tall <- deaths_tall[deaths_tall$country %in% unweighted_qntls$country, ]
##deaths_tall <- droplevels(deaths_tall)


p <- ggplot() +
  geom_point(data = deaths_tall, aes(dates, deaths), col = "black") +
  ## geom_ribbon(
  ##   data = unweighted_qntls,
  ##   aes(
  ##     x = date,
  ##     ymin = `2.5%`,
  ##     ymax = `97.5%`,
  ##     group = proj,
  ##     fill = "#0072B2"
  ##   ),
  ##   alpha = 0.3
  ## ) +
  ## geom_ribbon(
  ##   data = unweighted_qntls,
  ##   aes(x = date, ymin = `25%`, ymax = `75%`, group = proj),
  ##   fill = "#0072B2",
  ##   alpha = 0.5
  ## ) +
  geom_line(
    data = unweighted_qntls,
    aes(x = date, `50%`, group = proj, col = "#0072B2"),
    size = 1
  ) +

  geom_ribbon(
    data = wtd_prev_week,
    aes(
      x = date,
      ymin = `2.5%`,
      ymax = `97.5%`,
      group = proj,
      fill = "#D55E00"
    ),
    alpha = 0.3
  ) +
  ## geom_ribbon(
  ##   data = wtd_prev_week,
  ##   aes(x = date, ymin = `25%`, ymax = `75%`, group = proj),
  ##   fill = "#D55E00",
  ##   alpha = 0.5
  ## ) +
  geom_line(
    data = wtd_prev_week,
    aes(x = date, `50%`, group = proj, col = "#D55E00"),
    size = 1
  ) +

  geom_ribbon(
    data = wtd_all_prev_weeks,
    aes(
      x = date,
      ymin = `2.5%`,
      ymax = `97.5%`,
      group = proj,
      fill = "#CC79A7"
    ),
    alpha = 0.3
  ) +
  ## geom_ribbon(
  ##   data = wtd_all_prev_weeks,
  ##   aes(x = date, ymin = `25%`, ymax = `75%`, group = proj),
  ##   fill = "#CC79A7",
  ##   alpha = 0.5
  ## ) +
  geom_line(
    data = wtd_all_prev_weeks,
    aes(x = date, `50%`, group = proj, col = "#CC79A7"),
    size = 1
  ) +
  scale_fill_identity(
    breaks = c("#0072B2", "#D55E00", "#CC79A7"),
    labels = c("Unweighted", "Previous Week", "All previous weeks"),
    guide = "legend"
  ) +
  scale_color_identity(
    breaks = c("#0072B2", "#D55E00", "#CC79A7"),
    labels = c("Unweighted", "Previous Week", "All previous weeks"),
    guide = "legend"
  ) +

  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank()) +
  xlab("") + ylab("") +
  scale_x_date(limits = c(as.Date("2020-03-01"), NA))

npages <- ceiling(length(unique(unweighted_qntls$country)) / 2)

for (page in seq_len(npages)) {
  p <- p +
    ggforce::facet_wrap_paginate(
      ~country, ncol = 1, nrow = 2, page = page, scales = "free_y"
    )
  ggsave(glue::glue("ensemble_comparison_{page}.png"), p)

}

######
allthree <- rbind(wtd_prev_week, wtd_all_prev_weeks, unweighted_qntls)
allthree$date <- as.Date(allthree$date)

p <- ggplot() +
  geom_point(data = deaths_tall, aes(dates, deaths), col = "black") +
  geom_ribbon(
    data = allthree,
    aes(x = date, ymin = `2.5%`, ymax = `97.5%`, group = proj),
    alpha = 0.3) +
  geom_line(
    data = allthree,
    aes(x = date, y = `50%`, group = proj), size = 1.1) +
  theme_classic() +
  xlab("") + ylab("") +
  scale_x_date(limits = c(as.Date("2020-03-01"), NA)) +

  ggforce::facet_grid_paginate(
    country ~ model, nrow = 2, ncol = 3, page = 1, scales = "free_y"
  )

npages <- ggforce::n_pages(p)

for (page in seq_len(npages)) {
  p <- p +
   ggforce::facet_grid_paginate(
    country ~ model, nrow = 2, ncol = 3, page = page, scales = "free_y"
  )
  ggsave(glue::glue("grid_ensemble_comparison_{page}.png"), p)

}
