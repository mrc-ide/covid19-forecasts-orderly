## orderly::orderly_develop_start(use_draft = "newer")
## Random
fontsize <- 12 / .pt
forecast_text <- paste("Forecasts with constant", expression(R[t]))

theme_schematic <- function() {
  theme_classic() %+replace%
    theme(axis.text = element_blank(), axis.ticks = element_blank())
}

######################################################################
###### Common bits
######################################################################
si <- rgamma(1e4, shape = 2.3, rate = 1.28)
psi <- ggplot() +
  geom_density(aes(si), fill = "red", col = NA, alpha = 0.2) +
  xlab("Serial interval") +
  theme_schematic() +
  theme(axis.title.y = element_blank())
######################################################################
model_outputs <- readRDS("DeCa_Std_results.rds")
## Anyone will do, for illustration
obs_deaths <- model_outputs$D_active_transmission[ , c("dates", "Peru")]
## Get a really smooth curve
obs_deaths$deaths <- slide_dbl(
  obs_deaths$Peru, mean, .before = 5, .after = 5
)

earliest <- as.Date("2020-03-31")
now <- as.Date("2020-07-15")
now_minus_tau <- as.Date("2020-05-25")
obs_deaths <- obs_deaths[obs_deaths$dates <= now, ]
obs_deaths <- obs_deaths[obs_deaths$dates >= earliest, ]

######################################################################
########### RtI0 Model ###############################################
######################################################################
###### Model 1 jointly estimates Rt and incidence prior to window
## Generate dummy data
dates <- seq(from = earliest,  to = now_minus_tau, by = "1 day")
i0_est <- map_dfr(
  1:1000,
  function(index) {
    lambda <- floor(
      slider::slide_dbl(obs_deaths$deaths, mean, .before = 3, .after = 3)
    )
    deaths <- rpois(length(dates), lambda)
    deaths <- floor(
      slider::slide_dbl(deaths, mean, .before = 3, .after = 3)
    )
    data.frame(dates = dates, deaths = deaths)
  }, .id = "sim"
)

i0_est <- group_by(i0_est, dates) %>%
  summarise(
    val = quantile(deaths, probs = seq(0.1, 1, 0.05)),
    probs = seq(0.1, 1, 0.05)
  )
i0_est <- ungroup(i0_est)


dates_future <- seq(from = now, length.out = 21, by = "1 day")
i0_future <- map_dfr(
  1:1000,
  function(index) {
    deaths <- rpois(
      length(dates_future),
      seq(185, length.out = length(dates_future), by = 1)
    )
    deaths <- floor(
      slider::slide_dbl(deaths, mean, .before = 3, .after = 3)
    )
    data.frame(dates = dates_future, deaths = deaths)
  }, .id = "sim"
)

i0_future <- group_by(i0_future, dates) %>%
  summarise(
    val = quantile(deaths, probs = seq(0.1, 1, 0.05)),
    probs = seq(0.1, 1, 0.05)
  ) %>% ungroup()


obs_deaths$seen <- ifelse(obs_deaths$dates < now_minus_tau, 0.3, 1)
obs <- ggplot() +
  geom_line(data = obs_deaths, aes(dates, deaths, alpha = seen)) +
  scale_x_date(
    limits = c(as.Date("2020-03-31"), as.Date("2020-07-31"))
  ) +
  scale_alpha_identity() +
  xlab("Time") +
  ylab("Daily Deaths") +
  theme_schematic()

obs_m1 <- obs +
  geom_vline(
    xintercept = c(as.numeric(now), as.numeric(now_minus_tau)),
    linetype = "dashed"
  ) +
  geom_text(aes(x = now + 4, y = 220, label = "Now"), size = fontsize) +
  geom_text(
    aes(x = now_minus_tau - 7, y = 220,
        label = paste("Now -", expression(tau))),
    size = fontsize, parse = TRUE
  )

m1_left <- obs_m1 +
  geom_text(
    aes(x = now_minus_tau - 38, y = 150,
        label = "Data not used for model calibration"
    ), size = fontsize
  ) +
  ## Arrow below the label "Data not used for model calibration"
  geom_segment(
    aes(
      x = as.Date("2020-04-01"), xend = now_minus_tau, y = 144,
      yend = 144
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) +
  ## Arrow below the label "Assume constant Rt in this window"
  geom_text(
    aes(
      x = now_minus_tau + 20, y = 212,
      label = paste("Assume constant", expression(R[t]),"\n in window")
    ), size = fontsize
  ) +
  geom_segment(
    aes(
      x = now_minus_tau, xend = now, y = 195, yend = 195
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) + coord_trans(clip = "off")

m1_right <- obs_m1 +
  geom_line(
    data = i0_est, aes(dates, val, group = probs),
    linetype = "dashed", alpha = 0.2, col = "red"
  ) +
  geom_line(
    data = i0_future, aes(dates, val, group = probs),
    linetype = "dashed", alpha = 0.2, col = "red"
  ) +
  geom_segment(
    aes(
      x = as.Date("2020-04-01"), xend = now_minus_tau, y = 144,
      yend = 144
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) +
  ## Arrow below the label "Assume constant Rt in this window"
  geom_text(
    aes(x = now_minus_tau - 30, y = 150,
        label = "Data not used for model calibration"
    ), size = fontsize
  ) +
  geom_text(
    aes(x = now_minus_tau - 35, y = 140,
        label = "Jointly estimated with Rt"
    ), size = fontsize, col = "red"
  ) +
  geom_text(
    aes(
      x = now_minus_tau + 25, y = 200,
      label = "Assume constant Rt in window"
    ), size = fontsize
  ) +
  geom_text(
    aes(
      x = now + 11, y = 170,
      label = paste("Forecasts with", "constant Rt", sep = "\n")
    ), size = fontsize, col = "red"
  ) +
  geom_segment(
    aes(
      x = now_minus_tau, xend = now, y = 195, yend = 195
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) + coord_trans(clip = "off")


left <- plot_grid(
  m1_left + ggtitle("Model 1"),
  psi, nrow = 2, rel_heights = c(0.7, 0.3),
  align = "hv", axis = "l"
)


ggsave("m1_left.png", left)
ggsave("m1_right.png", m1_right)


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

m3_left <- ggplot() +
  geom_line(data = obs_deaths, aes(dates, deaths)) +
  geom_line(data = obs_cases, aes(dates, cases), col = "blue") +
  geom_vline(xintercept = as.numeric(now), linetype = "dashed") +
  geom_text(aes(x = now + 5, y = 500, label = "Now"), size = fontsize) +
  geom_text(
    aes(x = as.Date("2020-07-05"), y = 360, label = "Cases"),
    color = "blue"
  ) +
  geom_text(
    aes(x = as.Date("2020-07-04"), y = 200, label = "Deaths")
  ) +
  ##scale_x_date(limits = earliest, as.Date("2020-07-31")) +
  xlab("Time") + ylab("Daily Cases/Deaths") +
  theme_schematic()

m3_right <- m3_left +
  geom_line(
    data = wtd_cases, aes(dates, wtd_cases), col = "#6666ff",
    linetype = "longdash"
  ) +
  geom_line(
    data = i0_future, aes(dates, val, group = probs),
    linetype = "dashed", alpha = 0.2, col = "red"
  ) +
  geom_text(
    aes(x = now + 15, y = 330, label = "Weighted cases"),
    size = fontsize, col = "#6666ff"
  ) +
  geom_text(
    aes(
      x = now + 12, y = 150,
      label = "Forecasts \n assuming constant \n Rt"
    ), size = fontsize, col = "red"
  ) +
  ## Midway between deaths and weighted cases on this day
  geom_text(
    aes(now - 5, 260, label = "rho"), parse = TRUE,
    size = fontsize, fontface = "bold"
  ) +
  ## Arrows above and below
  geom_segment(
    aes(
      x = now - 5, y = 183, yend = 250, xend = now - 5
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "last")
  ) +
  geom_segment(
    aes(
      x = now - 5, y = 270, yend = 345, xend = now - 5
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "first")
  ) +
  ## Delay from report to death
  geom_text(
    aes(earliest + 35, 200, label = "gamma"), parse = TRUE,
    size = fontsize, fontface = "bold"
  ) +
  ## Arrows left and right
  geom_segment(
    aes(
      x = earliest + 28, y = 200, yend = 200, xend = earliest + 33
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "last")
  ) +
  geom_segment(
    aes(
      x = earliest + 37, y = 200, yend = 200, xend = earliest + 46
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "first")
  )



delay <- rgamma(1e4, shape = 4, scale = 2.5)
pdelay <- ggplot() +
  geom_density(aes(delay), fill = "black", col = NA, alpha = 0.2) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +
  geom_text(
    aes(x = 8.5, 0.1, label = "gamma"), parse = TRUE
  ) +
  xlab("Days since case reported") +
  ylab("Probability of death") +
  theme_schematic()

m3_left <- plot_grid(
  m3_left + ggtitle("Model 3"), pdelay, nrow = 2,
  rel_heights = c(0.7, 0.3), align = "hv", axis = "l"
)

ggsave("m3_left.png", m3_left)
ggsave("m3_right.png", m3_right)

######################################################################
######################################################################
################# Model 2 ############################################
######################################################################
######################################################################

m2_left <- ggplot() +
  geom_line(data = obs_deaths, aes(dates, deaths)) +
  geom_vline(
    xintercept = as.numeric(now), linetype = "dashed"
  ) +
  geom_text(aes(x = now + 4, y = 220, label = "Now"), size = fontsize) +
  xlab("Time") + ylab("Daily Deaths") +
  theme_schematic()

m2_right <- m2_left +
  geom_vline(
    xintercept = as.numeric(now_minus_tau), linetype = "dashed"
  ) +
  geom_text(
    aes(x = now_minus_tau - 7, y = 220, label = "Now - k*"),
    size = fontsize
  ) +
  geom_text(
    aes(
      x = now_minus_tau + 25, y = 212,
      label = "Assume constant Rt in window"
    ), size = fontsize
  ) +
  geom_segment(
    aes(
      x = now_minus_tau, xend = now, y = 209, yend = 209
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) +
  geom_segment(
    aes(x = now - 15, xend = now, y = 90, yend = 90),
    linetype = "dotted"
  ) +
  geom_segment(
    aes(x = now - 20, xend = now, y = 100, yend = 100),
    linetype = "dotted"
  ) +
  geom_segment(
    aes(x = now - 25, xend = now, y = 110, yend = 110),
    linetype = "dotted"
  ) +
  geom_segment(
    aes(x = now - 35, xend = now, y = 120, yend = 120),
    linetype = "dotted"
  ) +
  geom_text(
    aes(
      x = now + 15, y = 120,
      label = "Different possible"
    ), size = fontsize
  ) +
  geom_text(
    aes(
      x = now + 15, y = 110,
      label = "windows. \n Choose best (k*)"
    ), size = fontsize
  ) +
  geom_line(
    data = i0_future, aes(dates, val, group = probs),
    linetype = "dashed", alpha = 0.2, col = "red"
  ) +
  geom_text(
    aes(
      x = now + 12, y = 170,
      label = "Forecasts \n assuming \n constant Rt"
    ), size = fontsize, col = "red"
  ) +
  ## So that text is not chopped off
  expand_limits(x = as.Date(c(NA, "2020-08-10"))) +
  coord_trans(clip = "off")


m2_left <- plot_grid(
  m2_left +  ggtitle("Model 2"), psi, nrow = 2,
  rel_heights = c(0.7, 0.3), align = "hv", axis = "l"
)

ggsave("m2_left.png", m2_left)
ggsave("m2_right.png", m2_right)


########### Medium-term forecasts schematic
###########

## Rt distributions
rt <- data.frame(
  week = rep(paste("Week", 1:5), each = 1e3),
  rt = c(
    rnorm(1e3, 8, 2), rnorm(1e3, 4, 2), rnorm(1e3, 2, 2),
    rnorm(1e3, 0, 2), rnorm(1e3, 0, 1)
  ),
  fill = rep(c(rep("gray", 4), "blue"), each = 1e3),
  stringsAsFactors = FALSE
)

rt_plot <- ggplot(rt) +
  geom_half_violin(
    aes(week, rt, fill = fill), draw_quantiles = c(0.25, 0.5, 0.75),
    alpha = 0.3
  ) +
  scale_fill_identity() +
  scale_x_discrete(
    position = "top",
    breaks = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
    labels = c("Week (K - 4)", "Week (K - 3)", "Week (K - 2)",
               "Week (K - 1)", "Week K")
  ) +
  theme_schematic() +
  ylab("Reproduction number") +
  theme(
    axis.line.x = element_blank(), axis.title.x = element_blank(),
    axis.text.x = element_text(size = 6, angle = 90),
    axis.title.y = element_text(size = 8)
  ) +
  ## Arrows to show sampling
  annotate(
    "curve", x = "Week 2", xend = "Week 5", y = 12, yend = 5,
    curvature = -0.5, alpha = 0.2, linetype = "dashed",
    arrow = arrow(length = unit(0.03, "npc")
  )
  ) +
  annotate(
    "curve", x = "Week 3", xend = "Week 5", y = 9, yend = 4,
    curvature = -0.3, alpha = 0.5, linetype = "dashed",
    arrow = arrow(length = unit(0.03, "npc")
  )
) +
  annotate(
    "curve", x = "Week 4", xend = "Week 5", y = 7, yend = 3,
    curvature = -0.2, alpha = 1, linetype = "dashed",
    arrow = arrow(length = unit(0.03, "npc")
  )
)


medium_algo <- ggdraw() +
  draw_image("medium-term-forecasts-algo.png", scale = 1.5)


final <- plot_grid(
  rt_plot, medium_algo, nrow = 2, rel_heights = c(0.7, 0.3)
)

ggsave("medium-term-schematic.png", final)
