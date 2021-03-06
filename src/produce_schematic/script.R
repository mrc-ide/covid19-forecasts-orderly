## orderly::orderly_develop_start(use_draft = "newer")
forecast_text <- deparse(bquote("Forecasts with \n constant"~R[t]))
linesize <- 1.2
theme_schematic <- function() {
  theme_classic() %+replace%
    theme(
      text = element_text(size = 15),
      line = element_line(size = 1.2),
      axis.text = element_blank(), axis.ticks = element_blank()
    )
}

######################################################################
###### Common bits
######################################################################
######################################################################
model_outputs <- readRDS("DeCa_Std_results.rds")
## Anyone will do, for illustration
obs_deaths <- model_outputs$D_active_transmission[ , c("dates", "Peru")]
## Get a really smooth curve
obs_deaths$deaths <- slide_dbl(
  obs_deaths$Peru, mean, .before = 5, .after = 5
)

earliest <- as.Date("2020-03-31")
latest <- as.Date("2020-08-22")
now <- as.Date("2020-07-15")
now_minus_tau <- as.Date("2020-05-25")
obs_deaths <- obs_deaths[obs_deaths$dates <= now, ]
obs_deaths <- obs_deaths[obs_deaths$dates >= earliest, ]

######################################################################
########### RtI0 Model ###############################################
######################################################################
###### Model 1 jointly estimates Rt and incidence prior to window
## Generate dummy data
x <- obs_deaths[21:30, ]
x$deaths <- ceiling(x$deaths)
incid <- rincewind::ts_to_incid(x, "dates", "deaths")
si <- readRDS("si_distrs.rds")[[2]]

proj <- project(
  incid, 1.5, si, model = "poisson", n_sim = 10000, n_days = 35
)
proj <- data.frame(proj, check.names = FALSE)
## First 14 days, back-calculated incidence
back <- filter(proj, dates < proj$dates[15]) %>%
  tidyr::gather(sim, val, -dates)
## Next few days obs incidence
obs <- proj[, c("dates", "sim_1")]
obs <- filter(
  obs, dates >= proj$dates[15], dates < proj$dates[30]
) %>%   tidyr::gather(sim, val, -dates)
## future
future <- filter(proj, dates >= proj$dates[30]) %>%
  tidyr::gather(sim, val, -dates)

qlow <- function(x) quantile(x, 0.025)
qhigh <- function(x) quantile(x, 0.975)
qmed <- function(x) quantile(x, 0.5)

back <- group_by(back, dates) %>%
  summarise_if(
    is.numeric, list(low = qlow, med = qmed, high = qhigh)
  ) %>% ungroup()

future <- group_by(future, dates) %>%
  summarise_if(
    is.numeric, list(low = qlow, med = qmed, high = qhigh)
  ) %>% ungroup()


pobs <- ggplot() +
  geom_ribbon(
    data = back,
    aes(dates, ymin = low, ymax = high),
    alpha = 0.3, fill = "red"
  ) +
  geom_line(
    data = back, aes(dates, med),
    alpha = 0.3, fill = "red"
  ) +
  geom_point(data = obs, aes(dates, val)) +
  geom_ribbon(
    data = future,
    aes(dates, ymin = low, ymax = high),
    alpha = 0.3, fill = "red"
  ) +
  geom_line(
    data = future, aes(dates, med),
    alpha = 0.3, fill = "red"
  ) +
  xlab("Time") +
  ylab("Daily Deaths") +
  theme_schematic()





obs_m1 <- pobs +
  geom_vline(
    xintercept = c(
      as.numeric(min(obs$dates)) - 0.5,
      as.numeric(max(obs$dates)) + 0.5
    ),
    linetype = "dashed", size = linesize
  )



m1_right <- obs_m1 +
  geom_segment(
    aes(
      x = min(obs$dates) - 0.5,
      xend = max(obs$dates) + 0.5,
      y = max(future$high) * 0.8,
      yend = max(future$high) * 0.8
    ), arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    size = linesize
  ) +
  geom_segment(
    aes(
      x = min(proj$dates),
      xend = min(obs$dates) - 1,
      y = max(future$high) * 0.7, yend = max(future$high) * 0.7
    ), arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    size = linesize
  ) + coord_trans(clip = "off")


cowplot::save_plot("m1_right.pdf", m1_right)


######################################################################
######################################################################
######## Model 3: Death to Cases
######################################################################
######################################################################
report_to_death_distr <- function(mu, std, trunc) {
  out <- EpiEstim::discr_si(seq(0, trunc), mu, std)
  out / sum(out)
}

mu_delta <- 10
s_delta <- 2
SItrunc <- 30
report_to_death <- report_to_death_distr(
  mu = mu_delta, std = s_delta, trunc = SItrunc
)
obs_cases <- model_outputs$I_active_transmission[ , c("dates", "Peru")]
## Get a really smooth curve
obs_cases$cases <- slide_dbl(
  obs_cases$Peru, mean, .before = 5, .after = 5
)
obs_cases <- obs_cases[obs_cases$dates <= now, ]
obs_cases <- obs_cases[obs_cases$dates >= earliest, ]
## To kind of bring cases and deaths on same scale
obs_cases$cases <-  obs_cases$cases / 10

x <- matrix(obs_cases$cases, ncol = 1)
wtd_cases <- ascertainr::weighted_incid(x, report_to_death, SItrunc)
wtd_cases <- data.frame(
  dates = seq(
    from = earliest + 10, length.out = nrow(wtd_cases),
    by = "1 day"
  ),
  wtd_cases = wtd_cases[, 1]
)

rho <- tail(obs_deaths$deaths, 1) / tail(wtd_cases$wtd_cases, 1)
## Simulate from binomial but keep increasing the
## weighted cases
wtd_mu <- mean(tail(wtd_cases$wtd_cases, 7))
wtd_sd <- sd(tail(wtd_cases$wtd_cases, 7))
params <- epitrix::gamma_mucv2shapescale(wtd_mu, wtd_sd / wtd_mu)
future_wtd <- rgamma(21, shape = params$shape, scale = params$scale)
future <- map_dfr(
  future_wtd, function(wtd) {
    deaths <- rbinom(1e4, size = ceiling(wtd), prob = rho)
    data.frame(
      low = qlow(deaths), med = qmed(deaths),
      high = qhigh(deaths)
    )
  }
)

future$dates <- seq(
  from = as.Date(max(obs_deaths$dates)),
  length.out = 21, by = "1 day"
)

m3_left <- ggplot() +
  geom_line(
    data = obs_deaths, aes(dates, deaths), size = linesize
  ) +
  geom_line(
    data = obs_cases, aes(dates, cases), col = "blue",
    size = linesize
  ) +
  geom_vline(
    xintercept = as.numeric(now), linetype = "dashed", size = linesize
  ) +
  scale_x_date(limits = c(earliest, latest)) +
  xlab("Time") + ylab("Daily Cases/Deaths") +
  theme_schematic()

idx <- which.max(wtd_cases$wtd_cases)
xmax <- wtd_cases$dates[idx]
ymax <- wtd_cases$wtd_cases[idx]

m3_right <- m3_left +
  geom_line(
    data = wtd_cases, aes(dates, wtd_cases), col = "#6666ff",
    linetype = "longdash", size = linesize
  ) +
  geom_line(
    data = future, aes(dates, med),
    linetype = "dashed", col = "red"
  ) +
  geom_ribbon(
    data = future, aes(dates, ymin = low, ymax = high),
    alpha = 0.3, fill = "red"
  ) +
  ## segment above rho
  geom_segment(
    aes(
      x = xmax, y = obs_deaths$deaths[idx] + 30,
      yend = (ymax / 2) - 15, xend = xmax
    ), arrow = arrow(length = unit(0.2, "cm"), ends = "last"),
    size = linesize
  ) +
  ## segment below rho
  geom_segment(
    aes(
      x = xmax, y = (ymax / 2) + 25, yend = ymax - 5, xend = xmax
    ), arrow = arrow(length = unit(0.2, "cm"), ends = "first"),
    size = linesize
  ) +
  ## segment left and right of mu
  geom_segment(
    aes(
      x = earliest + 28, y = 200, yend = 200, xend = earliest + 32
    ), arrow = arrow(length = unit(0.2, "cm"), ends = "last"),
    size = linesize
  ) +
  geom_segment(
    aes(
      x = earliest + 39, y = 200, yend = 200, xend = earliest + 46
    ), arrow = arrow(length = unit(0.2, "cm"), ends = "first"),
    size = linesize
  ) +
  geom_text(
    aes(xmax, ymax / 2 , label = "rho"), parse = TRUE,
    size = 6, fontface = "bold"
  ) +
  geom_text(
    aes(earliest + 35, 200, label = "mu"), parse = TRUE,
    size = 6, fontface = "bold"
  )

cowplot::save_plot("m3_right.pdf", m3_right)

######################################################################
######################################################################
################# Model 2 ############################################
######################################################################
######################################################################
x <- tail(obs_deaths[obs_deaths$dates <= latest, ], 7)
incid <- rincewind::ts_to_incid(x, "dates", "Peru")
si <- readRDS("si_distrs.rds")[[2]]

proj <- project(
  incid, 1.2, si[-1], model = "poisson", n_sim = 10000, n_days = 15
)

future <- data.frame(proj, check.names = FALSE) %>%
  tidyr::gather(sim, val, -dates) %>%
  group_by(dates) %>%
  summarise_if(
    is.numeric,
    list(low = qlow, med = qmed, high = qhigh)
  ) %>% ungroup()


future <- mutate_if(future, is.numeric, ~ . + 30)

m2_left <- ggplot() +
  geom_line(
    data = obs_deaths, aes(dates, deaths),
    size = linesize
  ) +
  geom_vline(
    xintercept = as.numeric(now), linetype = "dashed", size = linesize
  ) +
  ##scale_x_date(limits = c(earliest, latest)) +
  xlab("Time") + ylab("Daily Deaths") +
  theme_schematic()

m2_right <- m2_left +
  geom_vline(
    xintercept = as.numeric(now_minus_tau), linetype = "dashed",
    size = linesize
  ) +
  geom_segment(
    aes(
      x = now_minus_tau, xend = now, y = 209, yend = 209
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    size = linesize
  ) +
  ## Various window segments
  geom_segment(
    aes(x = now - 15, xend = now, y = 70, yend = 70),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    size = linesize
  ) +
  geom_segment(
    aes(x = now - 20, xend = now, y = 85, yend = 85),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    size = linesize
  ) +
  geom_segment(
    aes(x = now - 25, xend = now, y = 100, yend = 100),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    size = linesize
  ) +
  geom_ribbon(
    data = future,
    aes(dates, ymin = low, ymax = high),
    alpha = 0.3, fill = "red"
  ) +
  geom_line(
    data = future, aes(dates, med),
    alpha = 0.3, col = "red", size = linesize
  ) +
  ## So that text is not chopped off
  coord_trans(clip = "off")


cowplot::save_plot("m2_right.pdf", m2_right)


########### Medium-term forecasts schematic
###########
mu_weeks <- c(3, 1.4, 1.6, 1.8)
cv_weeks <- 0.1
params <- epitrix::gamma_mucv2shapescale(mu_weeks, cv_weeks)
weeks <- seq_along(mu_weeks)
rt <- pmap_dfr(
  list(x = params$shape, y = params$scale, week = weeks),
  function(x, y, week) {
    data.frame(
      rt = rgamma(1e4, shape = x, scale = y),
      week = week,
      stringsAsFactors = FALSE
    )
  }
)

## weighted version : take actual weighted version of
## the above with beta 1.
weights <- rev(
  ceiling(1e4 * exp(-seq_along(mu_weeks)))
)
wtd_rt <- split(rt, rt$week)
wtd_rt <- map2(wtd_rt, weights, function(x, wt) sample(x$rt, wt))

wtd_rt <- data.frame(
  rt = unlist(wtd_rt), week = 5, stringsAsFactors = FALSE
)

rt <- rbind(rt, wtd_rt)
rt$fill <- case_when(
  rt$week == 1 ~ "gray",
  rt$week %in% c(2, 3, 4) ~ "blue",
  rt$week == 5 ~ "red"
)

rt$alpha <- 0.3
rt$alpha[rt$week == 4] <- 0.6
## Horizontal Lines
week4_qntls <- rt[rt$week == 4, ]
ci_high <- quantile(week4_qntls$rt, probs = 0.975)
ci_low <- quantile(week4_qntls$rt, probs = 0.025)
rt$week <- glue::glue("Week {rt$week}")


## Starting point for each arrow
## Manual placement!
y <- data.frame(
  x = c("Week 2", "Week 3", "Week 4"), xend = "Week 5",
  y = c(1.5, 2, 2.2), yend = c(3, 2.8, 2.5)
)

rt_plot <- ggplot(rt) +
  geom_half_violin(
    aes(week, rt, fill = fill, alpha = alpha),
    draw_quantiles = c(0.025, 0.975)
  ) +
  ## Horizontal lines to show 95% CrI of the most recent estimate
  geom_hline(yintercept = c(ci_low, ci_high), linetype = "dashed") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_x_discrete(
    position = "top",
    breaks = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
    labels = c("Week of (T - 21)", "Week of (T - 14)",
               "Week of (T - 7)", "Week of T", expression({R^{"w"}}(T)))
  ) +
  theme_schematic() +
  theme(legend.position = "none") +
  ylab("Reproduction number") +
  theme(
    axis.line = element_blank(), axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  ## Arrows to show sampling
  annotate(
    "curve", x = "Week 2", xend = 4.8, y = 2, yend = 2.8,
    curvature = -0.3, alpha = 0.3, linetype = "dashed",
    arrow = arrow(length = unit(0.5, "cm"), ends = "last"),
    size = linesize
  ) +
  annotate(
    "curve", x = "Week 3", xend = 4.8, y = 2, yend = 2.6,
    curvature = -0.3, alpha = 0.5, linetype = "dashed",
    arrow = arrow(length = unit(0.5, "cm"), ends = "last"),
    size = linesize
  ) +
  annotate(
    "curve", x = "Week 4", xend = 4.8, y = 2.2, yend = 2.4,
    curvature = -0.2, alpha = 1, linetype = "dashed",
    arrow = arrow(length = unit(0.5, "cm"), ends = "last"),
    size = linesize
  )

cowplot::save_plot("rt_plot.pdf", rt_plot)
dev.off()
