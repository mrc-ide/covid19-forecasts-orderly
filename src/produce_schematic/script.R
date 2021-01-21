##orderly::orderly_develop_start(use_draft = "newer")
model_outputs <- readRDS("DeCa_Std_results.rds")
## Anyone will do, for illustration
obs_deaths <- model_outputs$D_active_transmission[ , c("dates", "Peru")]
## Get a really smooth curve
obs_deaths$deaths <- slide_dbl(
  obs_deaths$Peru, mean, .before = 5, .after = 5
)

now <- as.Date("2020-07-15")
now_minus_tau <- as.Date("2020-06-05")
obs_deaths <- obs_deaths[obs_deaths$dates <= now, ]
obs_deaths <- obs_deaths[obs_deaths$dates >= as.Date("2020-03-31"), ]


obs_deaths$seen <- ifelse(obs_deaths$dates < now_minus_tau, 0.3, 1)

obs <- ggplot(obs_deaths, aes(dates, deaths, alpha = seen)) +
  geom_line() +
  scale_x_date(
    limits = c(as.Date("2020-03-31"), as.Date("2020-07-31"))
  ) +
  scale_alpha_identity() +
  xlab("Time") +
  ylab("Daily Deaths") +
  theme_classic() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())



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
  ) +
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
    aes(x = now_minus_tau - 38, y = 150,
        label = "Data not used for model calibration"
    ), size = 8 / .pt
  ) +
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


