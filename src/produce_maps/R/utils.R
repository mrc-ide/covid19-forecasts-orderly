rt_map <- function(world, df, coords) {

  palette <- c(
    decline = "018571",
    growing = "#a6611a",
    unclear = "#dfc27d",
    `stable/growing slowly` = "#80cdc1"
  )
  p <- ggplot() + ggthemes::theme_map() + xlab("") + ylab("")
  p <- p +
    geom_sf(data = world) +
      geom_sf(data = df, aes(fill = phase), alpha = 0.7) +
      ##geom_text_repel(data = coords, aes(x = X, y = Y, label = label)) +
      guides(size = F) +
    theme(
      panel.background = element_rect("white"),
      panel.grid = element_blank()
    ) +
    scale_fill_manual(
      values = palette,
      na.value = "gray",
      breaks = c("decline", "stable/growing slowly", "growing", "unclear"),
      labels = c("Declining", "Stable/Growing Slowly", "Growing", "Unclear Trend")
    ) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.position = "top") +
  coord_sf()

  p

}

deaths_map <- function(world, world_df_pts, coords) {

    p <- ggplot() + ggthemes::theme_map() + xlab("") + ylab("")
    p <- p +
      geom_sf(data = world, fill = "grey", col = "white", size = 0.1) +
      geom_sf(
        data = world_df_pts,
        aes(size = `50%`),
        alpha = 0.4,
        shape = 21,
        stroke = 2,
        col = "red",
        fill = "red"
      ) +
      ##geom_sf(data = world_df_pts, aes(size = `97.5%`), shape = 1, col = "red") +
      geom_text_repel(data = coords, aes(x = X, y = Y, label = label)) +
      guides(size = F) +
      theme(panel.background = element_rect("white"), panel.grid = element_blank()) +
      coord_sf()

  p

}

fig_size <- list(
  fig.width = 18, fig.height = 12, units = "in"
)

