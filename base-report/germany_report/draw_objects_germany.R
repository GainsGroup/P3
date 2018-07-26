# Helper function to draw a page layout grid given desired dimensions
get_grid <- function(width = 8.5, # page width in inches
                     height = 11, # page height in inches
                     ncols = 12, # number of cols for content
                     nrows = 6, # number of rows for content
                     title_height = 1.4, # height in inches of title area
                     margins = c(.75,.75,.75,.75) # top, right, bottom, left margin
                     ) {
  heights = c(margins[1], title_height, rep((height - margins[1] - margins[3] - title_height) / nrows, nrows), margins[3])
  widths = c(margins[2], rep((width - margins[2] - margins[4]) / ncols, ncols), margins[4])
  grid <- grid.layout(nrows + 3, ncols + 2, heights = heights, widths = widths)

}

vplayout <-
  function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)

newpage <- function(grid, bgcolor = ltgrey) {
  grid.newpage()
  pushViewport(viewport(layout = grid))
  grid.rect(gp = gpar(fill = bgcolor, col = bgcolor))
}

# Draw text on the page
drawtext <- function(s, title = NULL, header = FALSE) {
  grob1 <-
    splitTextGrob(s, gp = gpar(
      fontfamily = "Abel",
      col = dkgrey,
      alpha = 0.8,
      cex = .8
    ))
  p <- ggplot() + xlim(0, 1) + ylim(0, 1.1) +
    annotation_custom(
      grob = grob1,
      xmin = 0,
      xmax = .9,
      ymin = .1,
      ymax = .9
    ) +
    theme_p3_text() + labs(x = NULL, y = NULL)
  if (!is.null(title)) {
    border <- linesGrob(
      x = unit(c(0, 1), "npc"),
      y = unit(c(1, 1), "npc"),
      default.units = "npc",
      arrow = NULL,
      name = NULL,
      gp = gpar(col = dkred, lwd = 2),
      vp = NULL
    )
    p <-
      p + ggtitle(title) + annotation_custom(
        grob = border
      )
  }
  if(header) {
    p <- p + theme_p3_title()
  }
  p <- p + theme(axis.ticks.length = unit(0, "pt"))+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(plot.margin = unit(c(0, .5, 0.5, 0), "lines"))
  return(p)
}


find_cell <- function(table, row, col, name="core-bg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

drawtable <- function(df, fill_col = NULL, fill = dkgrey, width=NULL) {
  g2 <- tableGrob(df, rows = NULL, theme = p3_table_theme())
  if (!is.null(fill_col)){
    col_index <- grep(fill_col, colnames(df))
    color_func <- colorRampPalette(c('white', fill))
    pal <- color_func(100)
    for (n in 1:nrow(df)) {
      val <- df$Percentile[n]
      print(val)
      fill <- pal[val]
      print(fill)
      ind <- find_cell(g2, n+1, col_index)
      print(ind)
      g2$grobs[ind][[1]][["gp"]] <- gpar(fill=fill, col = NA)
      print(g2$grobs[ind][[1]][["gp"]])
    }
  }
  if (!is.null(width)){ 
    g2$widths <- unit(rep(1/ncol(g2), ncol(g2)), "npc")
  }
  g2$heights <- unit(rep(1/nrow(g2), nrow(g2)), "npc")

  ggplot() + xlim(0, 1) + ylim(0, 1) +
    annotation_custom(
      g2,
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1
    ) +
    theme_p3_fig() +
    theme(
      #core=list(bg_params=list(col="black")),
      plot.background = element_blank(),
      panel.background = element_blank()
    ) + theme(axis.ticks.length = unit(0, "pt"))+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(plot.margin = unit(c(0, .5, 0.5, 0), "lines"))
}

drawscore <- function(score, caption, bgcolor = blue) {
  ggplot() + xlim(0, 1) + ylim(0, 1) +
    geom_rect(
      aes(
        xmin = 0,
        xmax = 1,
        ymin = 0,
        ymax = 1
      ),
      color = bgcolor,
      fill = bgcolor,
      alpha = .8
    ) +
    theme_p3_fig() +
    theme(plot.background = element_blank(),
          panel.background = element_blank()) +
    annotate(
      'text',
      x = .5,
      y = .6,
      label = score,
      color = white,
      family = 'Abel',
      size = rel(22),
      fontface = 2
    ) +
    annotate(
      'text',
      x = .5,
      y = .2,
      label = caption,
      color = ltgrey,
      family = 'Abel',
      size = rel(4)
    ) + theme(axis.ticks.length = unit(0, "pt"))+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(plot.margin = unit(c(0, .5, 0.5, 0), "lines"))
}
