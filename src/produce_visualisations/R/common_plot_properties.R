theme_project <- function(font_size = 22) {

  ggpubr::theme_pubr() %+replace%
    theme(
      axis.title.y = element_text(size = font_size, angle = 90),
      axis.title.x = element_text(size = font_size),
      axis.text.y = element_text(size = font_size),
      axis.text.x =
        element_text(hjust = 0, size = font_size),
      strip.text.x = element_text(
        margin = margin(2,0,2,0, "pt"),
        size = font_size
      )
    )

}

fig_size <- list(
  fig.width = 18, fig.height = 12, units = "in"
)
