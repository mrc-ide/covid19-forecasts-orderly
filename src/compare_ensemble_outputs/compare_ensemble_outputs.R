rel_mse2 <- function(obs, pred) {
  nsims <- ncol(pred)
  obs <- matrix(
    rep(obs, each = nsims),
    ncol = nsims,
    byrow = TRUE
  )
  log_l <- rowSums(
    dpois(
      x = obs,
      lambda = pred + .5,
      log = TRUE
    )
  )
  log_l <- log_l / nsims# (nsims * (obs^2 + 1))
  log_l
}

model_input <- readRDS("model_input.rds")

run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as
infiles <- infiles[infiles != "model_input.rds"]

names(infiles) <- gsub(
  pattern = ".rds",
  replacement = "",
  x = infiles
)


weighted <- infiles[grep("unwtd", infiles, invert = TRUE)] %>%
  .[grep(pattern = "qntls", x = ., invert = TRUE)] %>%
  purrr::map(readRDS)

unweighted <- infiles[grep("unwtd", infiles)] %>%
    .[grep("qntls", ., invert = TRUE)] %>%
  purrr::map(readRDS)

model_predictions_error <- purrr::map_dfr(
  weighted,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred,
          function(y_si) {
            y_si <- t(y_si) ## Need T X N matrix for assessr
            dates2 <- as.Date(rownames(y_si))
            x <- dplyr::filter(
              model_input, dates %in% dates2) %>%
              pull(cntry)
            rel_mae <- assessr::rel_mae(obs = x, pred = y_si)
            rel_mse <- assessr::rel_mse(obs = x, pred = y_si)
            avg_likelhd <- rel_mse2(obs = x, pred = y_si)
            bias <- assessr::bias(obs = x, pred = y_si)
            metrics <- data.frame(
              rel_abs = rel_mae,
              rel_sq = rel_mse,
              avg_likelhd = avg_likelhd,
              bias = bias
            )
            metrics <- tibble::rownames_to_column(metrics, var = "date")
            metrics
          }, .id = "si"
        )
        out
      },
      .id = "country"
    )
  }, .id = "forecast_date"
)


unwtd_model_predictions_error <- purrr::map_dfr(
  unweighted,
  function(wtd) {
    countries <- names(wtd[[1]])
    names(countries) <- countries
    purrr::map_dfr(
      countries,
      function(cntry) {
        message(cntry)
        wtd_pred <- wtd[[1]][[cntry]]
        out <- purrr::map_dfr(
          wtd_pred,
          function(y_si) {
            y_si <- t(y_si) ## Need T X N matrix for assessr
            dates2 <- as.Date(rownames(y_si))
            x <- dplyr::filter(
              model_input, dates %in% dates2) %>%
              pull(cntry)
            rel_mae <- assessr::rel_mae(obs = x, pred = y_si)
            rel_mse <- assessr::rel_mse(obs = x, pred = y_si)
            avg_likelhd <- rel_mse2(obs = x, pred = y_si)
            bias <- assessr::bias(obs = x, pred = y_si)
            metrics <- data.frame(
              rel_abs = rel_mae,
              rel_sq = rel_mse,
              avg_likelhd = avg_likelhd,
              bias = bias
            )
            metrics <- tibble::rownames_to_column(metrics, var = "date")
            metrics
          }, .id = "si"
        )
        out
      },
      .id = "country"
    )
  },   .id = "forecast_date"
)


model_predictions_error$date <- as.Date(model_predictions_error$date)
unwtd_model_predictions_error$date <- as.Date(unwtd_model_predictions_error$date)
model_predictions_error <- model_predictions_error[model_predictions_error$si == "si_2", ]
unwtd_model_predictions_error <- unwtd_model_predictions_error[unwtd_model_predictions_error$si == "si_2", ]

model_predictions_error$forecast_date <- gsub(
  x = model_predictions_error$forecast_date,
  pattern = "wtd_",
  replacement = ""
)
model_predictions_error$proj <- "Weighted"
unwtd_model_predictions_error$forecast_date <- gsub(
  x = unwtd_model_predictions_error$forecast_date,
  pattern = "unwtd_",
  replacement = ""
)
unwtd_model_predictions_error$proj <- "Unweighted"
df <- rbind(model_predictions_error, unwtd_model_predictions_error)
readr::write_csv(x = df, path = "weighted_vs_unweighted_metrics.csv")
df <- split(df, df$forecast_date)
purrr::iwalk(
  df,
  function(x, forecast_date) {
     p <- ggplot(x, aes(date, avg_likelhd, col = proj)) +
       geom_point() +
       theme_pubr() +
       theme(legend.position = "top", legend.title = element_blank())

     ggsave(
       glue::glue("weighted_unweighted_{forecast_date}.pdf"), p
     )
  }
)



###
deaths_tall <- tidyr::gather(model_input, country, deaths, -dates)
deaths_tall$dates <- as.Date(deaths_tall$dates)

weighted_qntls <- infiles[grep("unwtd", infiles, invert = TRUE)] %>%
  .[grep(pattern = "qntls", x = ., invert = FALSE)] %>%
  purrr::map(readRDS)
weighted_qntls <- dplyr::bind_rows(weighted_qntls)
weighted_qntls$model <- "Weighted Ensemble"

unweighted_qntls <- infiles[grep("unwtd", infiles)] %>%
    .[grep("qntls", ., invert = FALSE)] %>%
  purrr::map(readRDS)
unweighted_qntls <- dplyr::bind_rows(unweighted_qntls)
unweighted_qntls$model <- "Unweighted Ensemble"


both <- rbind(unweighted_qntls, weighted_qntls)
both <- both[both$si == "si_2", ]
both$date <- as.Date(both$date)
deaths_tall <- deaths_tall[deaths_tall$country %in% both$country, ]


levels <- unique(interaction(both$proj, both$model))
unwtd <- grep(pattern = "Unweighted", x = levels, value = TRUE)
wtd <- grep(pattern = "Unweighted", x = levels, value = TRUE, invert = TRUE)
palette <- c(rep("#b067a3", nlevels(levels) / 2),
             rep("#9c954d", nlevels(levels) / 2))
names(palette) <- c(unwtd, wtd)

npages <- ceiling(length(unique(both$country)) /6)

for (page in seq_len(npages)) {
  p <- ggplot() +
  geom_point(data = deaths_tall, aes(dates, deaths), col = "black") +
  geom_ribbon(
    data = both,
    aes(
      date,
      ymin = `2.5%`,
      ymax = `97.5%`,
      group = interaction(proj, model),
      fill = interaction(proj, model)
    ), alpha = 0.3
  ) +
  geom_line(
    data = both,
    aes(
      date, y = `50%`,
      col = interaction(proj, model),
      group = interaction(proj, model)
    )
  ) +
    theme_classic() +
    theme(legend.position = "top") +
  scale_x_date(limits = c(as.Date("2020-03-01"), as.Date("2020-04-05"))) +
    scale_fill_manual(
      values = palette,
      aesthetics=c("col", "fill"),
      labels = c("Weighted", "Unweighted"),
      breaks = c("Weighted", "Unweighted"),
    ) +
  ggforce::facet_wrap_paginate(~country, ncol = 2, nrow = 2, page = page, scales = "free_y") +
  xlab("") + ylab("")

  ggsave(glue::glue("ensemble_comparison_{page}.png"), p)

}

