format_weekly_pred <- function(x) {

    x <- dplyr::mutate_if(x, is.numeric, ~ signif(., 3))
    x <- dplyr::mutate_if(x, is.numeric, ~prettyNum(., big.mark = ","))

    out <- data.frame(
        Country = x$country,
        `Week Ending` = x$week_ending,
        `Predicted Deaths` = glue::glue(
             "{x$`50%`} ({x$`2.5%`} - {x$`97.5%`})",
        ),
        check.names = FALSE
    )

    out
}

format_last_rt <- function(rt) {

    rt <- dplyr::select(rt, -si, `2.5%`, `50%`, `97.5%`)
    ##rt <- rt[rt$quantile %in% c("2.5%", "50%", "97.5%"), ]
    ##rt <- tidyr::spread(rt, quantile, out2)
    rt <- dplyr::arrange(rt, `50%`)
    rt <- dplyr::mutate_if(
        rt,
        is.numeric,
        ~ format(round(., 2), nsmall = 1)
    )
    rt$`R_t` <- glue::glue(
        "{rt$`50%`} ({rt$`2.5%`} - {rt$`97.5%`})"
    )
    rt <- dplyr::select(rt, model, Country = country, `R_t`)
    rt

}
