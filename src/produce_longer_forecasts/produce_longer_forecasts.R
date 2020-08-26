## orderly::orderly_develop_start(parameters = list(week_ending = "2020-03-29", use_si = "si_2"))
## infiles <- list.files(pattern = "*.rds")
dir.create("figures")
prob <- c(0.025, 0.25, 0.50, 0.75, 0.975)
population <- readr::read_csv("ecdc_pop2018.csv")
unwtd_rt_estimates <- readRDS("combined_rt_estimates.rds")
wtd_rt_estimates_across_countries <- readRDS(
  "combined_weighted_estimates_across_countries.rds"
)
wtd_rt_estimates_per_country <- readRDS(
  "combined_weighted_estimates_per_country.rds"
)

date_to_project_from <- as.Date(week_ending)
sims_per_rt <- 10
n_sim <- 1000

shape <- readRDS("cfr_shape_params.rds")
cfr_samples <- rbeta(n_sim, shape1 = shape[1],shape2 = shape[2])

indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

si_distrs <- readRDS("si_distrs.rds")
si <- si_distrs[[use_si]]

deaths_to_use <- raw_data[["D_active_transmission"]]


tall_deaths <- tidyr::gather(
  deaths_to_use, key = country, value = deaths, -dates
) %>%
  split(.$country) %>%
  purrr::map(
  ~ rincewind:::ts_to_incid(ts = ., date_col = "dates", case_col = "deaths")
)



## rt_estimates <- purrr::keep(
##   rt_estimates, ~ length(.$weeks_combined) > 1
## )

countries <- setNames(
  names(unwtd_rt_estimates), names(unwtd_rt_estimates)
)

all_restimates <- list(
  unweighted = unwtd_rt_estimates,
  weighted_across_countries = wtd_rt_estimates_across_countries,
  weighted_per_country = wtd_rt_estimates_per_country
)


all_reff <- map(
  all_restimates,
  function(restimate) {
    imap(
      restimate,
      function(rt, country) {
        deaths_so_far <- sum(
          deaths_to_use[deaths_to_use$dates <= week_ending, country]
        )
        pop <- population$pop_data2018[population$countries_and_territories == country]
        deaths_per_capita <- deaths_so_far / pop
        p <- proportion_susceptible(deaths_per_capita, cfr_samples)
        list(
          p_susceptible = p,
          r_eff = rt$rt_samples * p
        )
      }
    )
  }
)


all_projections <- map(
  all_reff,
  function(reff) {
    map(
      countries,
      function(country) {
        message(country)
        pop <- population$pop_data2018[population$countries_and_territories == country]
        n_days <- length(
          unwtd_rt_estimates[[country]]$weeks_combined
        ) * 7
        x <- deaths_to_use[[country]]
        ##rt <- unwtd_rt_estimates[[country]]$rt_samples
        rt <- reff[[country]][["r_eff"]]
        p <- reff[[country]][["p_susceptible"]]
        f <-  function() {
          project_with_saturation(
            deaths = x,
            r_eff = rt,
            p_susceptible = p,
            si = si,
            n_sim = n_sim,
            n_days = n_days,
            cfr = cfr_samples,
            pop
          )
         }
        out <- rerun(sims_per_rt, f())
      }
    )
  }
)

projections <- map(
  all_projections,
  function(pred) {
    map(
      pred,
      function(country_pred) {
        out <- map(country_pred, ~ .[["pred"]])
        do.call(what = 'rbind', args = out)
      }
    )
  }
)

r_effective <- map(
  all_projections,
  function(pred) {
    map(
      pred,
      function(country_pred) {
        out <- map(country_pred, ~ .[["r_effective"]])
        len <- length(out[[1]])
        reff <- vector(mode = "list", length = len)
        for (idx in 1:len) {
          reff[[idx]] <- unlist(map(out, ~ .[[idx]]))
        }
        reff
      }
    )
  }
)

p_s <- map(
  all_projections,
  function(pred) {
    map(
      pred,
      function(country_pred) {
        out <- map(country_pred, ~ .[["p_s"]])
        len <- length(out[[1]])
        ps <- vector(mode = "list", length = len)
        for (idx in 1:len) {
          ps[[idx]] <- unlist(map(out, ~ .[[idx]]))
        }
        ps
      }
    )
  }
)

pred_qntls <- map_depth(
  projections,
  2,
  function(mat) {
    dates_pred <- seq(
      from = as.Date(week_ending) + 1,
      length.out = ncol(mat), by = "1 day"
    )
    y <- apply(
      mat, 2, quantile, prob = prob
    )
    y <- data.frame(y)
    colnames(y) <- dates_pred
    y <- tibble::rownames_to_column(y, var = "qntl")
    y <- tidyr::gather(y, date, val, -qntl)
    tidyr::spread(y, qntl, val)
  }
)

reff_qntls <- map_depth(
  r_effective,
  2,
  function(y) {
    dates_pred <- seq(
      from = as.Date(week_ending) + 1,
      length.out = length(y), by = "1 day"
    )
    names(y) <- dates_pred
    out <- map_dfr(y, ~ quantile(., prob = prob), .id = "date")
  }
)

ps_qntls <- map_depth(
  p_s,
  2,
  function(y) {
    dates_pred <- seq(
      from = as.Date(week_ending) + 1,
      length.out = length(y), by = "1 day"
    )
    names(y) <- dates_pred
    map_dfr(y, ~ quantile(., prob = prob), .id = "date")
  }
)

combined_plot <- function(obs, pred, ps, reff) {

  p1 <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
    geom_ribbon(
      data = pred, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) +
    geom_line(
      data = pred, aes(x = date, y = `50%`)
    ) +
    ylab("Daily Deaths") +
    xlab("") +
    theme(axis.text.x = element_blank())

  p2 <- ggplot() +
    geom_ribbon(
      data = reff, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) +
    geom_line(
      data = reff, aes(x = date, y = `50%`)
    ) +
    ylim(0, NA) +
    geom_hline(yintercept = 1, col = "red", linetype = "dashed") +
    ylab("Effective reproduction number") +
    xlab("") +
    theme(axis.ticks.x = element_blank())


  p3 <- ggplot() +
    geom_ribbon(
      data = ps, aes(x = date, ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.3
    ) +
    geom_line(
      data = ps, aes(x = date, y = `50%`)
    ) + ylim(0, 1) +
    xlab("") +
    ylab("Proportion susceptible")

  p <- p1 + p2 + p3 + plot_layout(nrow = 3) +
    plot_annotation(tag_levels = 'A') &
        scale_x_date(
      date_breaks = "1 week", limits = c(as.Date("2020-03-01"), NA)
    ) &
    theme_minimal()
  p

}

saveRDS(unwtd_projections, "unwtd_projections.rds")
saveRDS(
  wtd_per_country_projections, "wtd_per_country_projections.rds"
)

saveRDS(
  wtd_across_all_projections, "wtd_across_all_projections.rds"
)
