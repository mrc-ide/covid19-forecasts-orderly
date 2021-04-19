theme_project <- function(font_size = 22) {

  theme_minimal() %+replace%
    theme(
      axis.title.y = element_text(size = font_size, angle = 90),
      axis.title.x = element_text(size = font_size),
      axis.text.y = element_text(size = font_size),
      axis.text.x =
        element_text(hjust = 0, size = font_size),
      strip.text.x = element_text(
        margin = margin(2,0,2,0, "pt"),
        size = font_size
      ),
      legend.title = element_blank(),
      legend.text = element_text(size = font_size),
      legend.position = "top"
    )

}

fig_size <- list(
  fig.width = 18, fig.height = 12, units = "in"
)
