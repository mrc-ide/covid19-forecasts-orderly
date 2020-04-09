projection_plot <- function(obs, pred) {

    ## Number of projections
    nprojs <- length(unique(pred$proj))

    ## Latest projections get a different color
    palette <- c(
        rep("#98984d", nprojs - 1),
        "#b3669e"
    )
    names(palette) <- unique(pred$proj)

    date_min <- as.Date("2020-03-01")
    date_max <- max(pred$date) + 2
    dates_to_mark <- seq(
        from = date_min,
        to = date_max,
        by = "1 day"
    )
    dates_to_mark <- dates_to_mark[weekdays(dates_to_mark) == "Monday"]
    ## Get dates of adding vlines.
    window_eps <- dplyr::group_by(pred, proj) %>%
        dplyr::summarise(date = min(date)) %>%
        dplyr::ungroup()

    window_eps$xintercepts <- as.numeric(
        window_eps$date - 1
    ) + 0.5
    ## To get nice labels
    ## https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
    integer_breaks <- function(n = 5, ...) {
        fxn <- function(x) {
            breaks <- floor(pretty(x, n, ...))
            names(breaks) <- attr(breaks, "labels")
            breaks
        }
        return(fxn)
    }

    p <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
        geom_line(
            data = pred,
            aes(date, `50%`, col = proj, group = proj)
        ) +
        geom_ribbon(
            data = pred,
            aes(x = date,
                ymin = `2.5%`,
                ymax = `97.5%`,
                fill = proj,
                group = proj),
            alpha = 0.4) +
    scale_color_manual(
        values = palette,
        aesthetics = c("color", "fill")
    ) +
    theme_pubr() +
    theme(legend.position = "none") +
        scale_x_date(breaks = dates_to_mark, limits = c(date_min, date_max)) +
        scale_y_continuous(breaks = integer_breaks()) +
    geom_vline(
        xintercept = c(
           window_eps$xintercepts
        ),
        linetype = "dashed"
    ) + xlab("") +
        ylab("Deaths") +
        theme(
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.text.x =
                element_text(angle = -90, hjust = 0, size = 20),
            strip.text.x = element_text(
                margin = margin(2,0,2,0, "pt"),
                size = 20
            )
          )

    p
}


rt_plot <- function(rt) {


    nice_names <- snakecase::to_any_case(
        rt$country,
        "title"
    )
    names(nice_names) <- rt$country
    rt$country <- reorder(rt$country, -rt$`50%`)
    if (length(unique(rt$model)) == 1) width <- 0.1
    else width <- 0.7

    p <- ggplot() +
        geom_errorbar(
            data = rt,
            aes(x = country, ymin = `2.5%`, ymax = `97.5%`, col = model),
            position = position_dodge(width = width),
            size = 1.1
        ) +
        geom_point(
            data = rt,
            aes(x = country, y = `50%`, col = model),
            position = position_dodge(width = 0.5),
            size = 4
            ) +
        theme_pubr() +
        xlab("") +
        ylab("Effective Reproduction Number") +
        scale_x_discrete(labels = nice_names) +
        theme(
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.text.x =
                element_text(angle = -90, hjust = 0, size = 20),
            legend.position = "none"
        ) +
        geom_hline(
            yintercept = 1,
            linetype = "dashed"
        )

    p
}


