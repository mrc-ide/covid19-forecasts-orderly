## orderly::orderly_develop_start(use_draft = "newer")
## Random
si <- rgamma(1e4, shape = 2.3, rate = 1.28)

theme_schematic <- function() {
  theme_classic() %+replace%
    theme(axis.text = element_blank(), axis.ticks = element_blank())
}

psi <- ggplot() +
  geom_density(aes(si), fill = "red", col = NA, alpha = 0.2) +
  xlab("Serial interval") +
  theme_schematic() +
  theme(axis.title.y = element_blank())


model_outputs <- readRDS("DeCa_Std_results.rds")
## Anyone will do, for illustration
obs_deaths <- model_outputs$D_active_transmission[ , c("dates", "Peru")]
## Get a really smooth curve
obs_deaths$deaths <- slide_dbl(
  obs_deaths$Peru, mean, .before = 5, .after = 5
)

earliest <- as.Date("2020-03-31")
now <- as.Date("2020-07-15")
now_minus_tau <- as.Date("2020-06-05")
obs_deaths <- obs_deaths[obs_deaths$dates <= now, ]
obs_deaths <- obs_deaths[obs_deaths$dates >= earliest, ]


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




## RtI0 Model
obs_m1 <- obs +
  geom_vline(
    xintercept = c(as.numeric(now), as.numeric(now_minus_tau)),
    linetype = "dashed"
  ) +
  geom_text(aes(x = now + 3, y = 190, label = "now"), size = 8 / .pt) +
  geom_text(
    aes(x = now_minus_tau - 6, y = 190, label = "now - tau"),
    size = 8 / .pt
  )

m1_left <- obs_m1 +
  geom_text(
    aes(x = now_minus_tau - 38, y = 150,
        label = "Data not used for model calibration"
    ), size = 8 / .pt
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
      x = now_minus_tau + 20, y = 200,
      label = "Assume constant Rt in window"
    ), size = 8 / .pt
  ) +
  geom_segment(
    aes(
      x = now_minus_tau, xend = now, y = 195, yend = 195
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) + coord_trans(clip = "off")


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


###### Model 1 jointly estimates Rt and incidence prior to window
dates_future <- seq(from = now, length.out = 21, by = "1 day")
i0_future <- map_dfr(
  1:1000,
  function(index) {
    deaths <- rpois(
      length(dates), seq(185, length.out = length(dates), by = 1)
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
    aes(x = now_minus_tau - 38, y = 150,
        label = "Jointly estimated with Rt"
    ), size = 8 / .pt, col = "red"
  ) +
  geom_text(
    aes(
      x = now_minus_tau + 20, y = 200,
      label = "Assume constant Rt in window"
    ), size = 8 / .pt
  ) +
  geom_text(
    aes(
      x = now + 11, y = 220,
      label = "Forecasts assuming \n constant Rt"
    ), size = 8 / .pt, col = "red"
  ) +
  geom_segment(
    aes(
      x = now_minus_tau, xend = now, y = 195, yend = 195
    ), arrow = arrow(length = unit(0.15, "cm"), ends = "both")
  ) + coord_trans(clip = "off")


## https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave
x <- seq(0, 1, 0.1)
dev.new()
p <- ggplot() + geom_point(aes(x, x), alpha = 0) + theme_void()
grid.brackets(52, 24, 49, 350, h = 0.1, lwd = 4)
dev.copy(png, "paren.png", height = 1000, width = 1000, res = 200)
dev.off()

######################################################################
######## Model 3: Death to Cases
obs_cases <- model_outputs$I_active_transmission[ , c("dates", "Peru")]
## Get a really smooth curve
obs_cases$cases <- slide_dbl(
  obs_cases$Peru, mean, .before = 5, .after = 5
)
obs_cases <- obs_cases[obs_cases$dates <= now, ]
obs_cases <- obs_cases[obs_cases$dates >= as.Date("2020-03-31"), ]
## To kind of bring cases and deaths on same scale
obs_cases$cases <-  obs_cases$cases / 10


obs <- ggplot() +
  geom_line(data = obs_deaths, aes(dates, deaths)) +
  geom_line(data = obs_cases, aes(dates, cases), col = "blue") +
  geom_vline(xintercept = as.numeric(now), linetype = "dashed") +
  geom_text(aes(x = now + 3, y = 190, label = "now"), size = 8 / .pt) +
  geom_text(
    aes(x = now_minus_tau - 6, y = 190, label = "now - tau"),
    size = 8 / .pt
  ) +
  geom_text(
    aes(x = as.Date("2020-07-10"), y = 375, label = "Cases"),
    color = "blue"
  ) +
  geom_text(
    aes(x = as.Date("2020-07-10"), y = 200, label = "Deaths")
  ) +
  scale_x_date(
    limits = c(as.Date("2020-03-31"), as.Date("2020-07-31"))
  ) +
  xlab("Time") + ylab("cases / deaths") +
  theme_schematic() +
  theme(axis.title.y = element_blank())


delay <- rgamma(1e4, shape = 4, scale = 2.5)
pdelay <- ggplot() +
  geom_density(aes(delay), fill = "black", col = NA, alpha = 0.2) +
  geom_vline(xintercept = 7.5, linetype = "dashed") +
  xlab("Days since case reported") +
  ylab("Probability of death") +
  theme_schematic()
