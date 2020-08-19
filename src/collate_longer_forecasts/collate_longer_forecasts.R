dir.create("figures")
run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

unwtd <- grep("unwtd_projections", infiles, value = TRUE)

names(unwtd) <- gsub(
  x = unwtd, pattern = "unwtd_projections_", replacement = ""
) %>% gsub(x = ., pattern = ".rds", replacement = "")

wtd_across_all <- grep("wtd_across_all", infiles, value = TRUE)

names(wtd_across_all) <- gsub(
  x = wtd_across_all, pattern = "wtd_across_all_projections_", replacement = ""
) %>% gsub(x = ., pattern = ".rds", replacement = "")


wtd_per_country <- grep("wtd_per_country", infiles, value = TRUE)

names(wtd_per_country) <- gsub(
  x = wtd_per_country, pattern = "wtd_per_country_projections_", replacement = ""
) %>% gsub(x = ., pattern = ".rds", replacement = "")


infiles <- list(
  unwtd = unwtd,
  wtd_across_all = wtd_across_all,
  wtd_per_country = wtd_per_country
)

pred_qntls <- map(
  infiles,
  function(infile) {
    projections <- purrr::map(infile, readRDS)
    map_depth(
      projections, 2,
      function(pred) {
        pred <- data.frame(pred, check.names = FALSE)
        pred <- tidyr::gather(pred, dates, val)
        qntls <- dplyr::group_by(pred, dates) %>%
          ggdist::median_qi(.width = 0.95)
        qntls$dates <- as.Date(qntls$dates)
        qntls
      }
    )
  }
)

deaths_to_use <- readRDS("latest_deaths_wide_no_filter.rds")
main_text_countries <- c(
  "Brazil", "India", "Italy", "South_Africa", "United_States_of_America"
)

iwalk(
  pred_qntls,
  function(qntls, strategy) {
    imap(
      qntls,
      function(week, forecast_week) {
        imap(
          week,
          function(df, country) {
            if (! country %in% main_text_countries) return(NULL)
            obs <- deaths_to_use[, c("dates", country)]
            obs$dates <- as.Date(obs$dates)
            obs <- obs[obs$dates <= max(df$dates), ]
            obs$deaths <- obs[[country]]

            ymax <- 10 * (ceiling(max(obs$deaths, na.rm = TRUE)/10) * 10)
            df$val[df$val > ymax] <- NA
            df$`.lower`[df$`.lower` > ymax] <- NA
            df$`.upper`[df$`.upper` > ymax] <- NA

            p <- rincewind::plot_projections(obs, df)
            p <- p +
              ggtitle(
                glue::glue("Projections for {country} for week starting {forecast_week}")
              )

            outfile <- glue("figures/{strategy}_{country}_{forecast_week}.png")
            message(outfile)
            ggsave(outfile, p)
      }
    )
  }
)})

iwalk(
  pred_qntls,
  function(qntls, strategy) {
    out <- map_dfr(qntls, ~ bind_rows(., .id = "country"), .id = "forecast_week")
    saveRDS(out, glue("{strategy}_projections_qntls.rds"))
  }
)



