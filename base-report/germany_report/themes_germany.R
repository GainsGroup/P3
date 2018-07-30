library(ggplot2)
library(dplyr)
library(extrafont)

loadfonts()

dkred <<- '#b11a21'
ltred <<- '#e0474c'
blue <<- '#7acfd6'
ltgrey <<- '#F0F0F0'
dkgrey <<- '#656565'
white <<- '#ffffff'
pal <<- c(dkred, ltred, blue, ltgrey, dkgrey)


theme_p3 <- function(base_size = 10,
                     base_family = "Abel") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Base elements which are not used directly but inherited by others
      line =              element_line(
        colour = '#DADADA',
        size = 0.75,
        linetype = 1,
        lineend = "butt"
      ),
      rect =              element_rect(
        fill = "#F0F0F0",
        colour = "#F0F0F0",
        size = 0.5,
        linetype = 1
      ),
      text =              element_text(
        family = base_family,
        face = "plain",
        colour = "#656565",
        size = base_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 1,
        margin = margin(),
        debug = FALSE
      ),

      # Modified inheritance structure of text element
      plot.title =        element_text(
        size = rel(1.7),
        family = "Abel" ,
        face = 'bold',
        hjust = 0.5,
        vjust = 1,
        colour = '#3B3B3B',
        margin = margin(
          t = 0,
          r = 0,
          b = 5,
          l = 0
        )
      ),
      plot.subtitle = element_text(
        hjust=0.5
      ),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),

      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),

      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),

      # Modifiying legend.position
      legend.position = 'none',

      complete = TRUE
    )
}

theme_p3_tran <- function(base_size = 10,
                     base_family = "Abel") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Base elements which are not used directly but inherited by others
      line =              element_line(
        colour = '#DADADA',
        size = 0.75,
        linetype = 1,
        lineend = "butt"
      ),
      rect =              element_rect(
        fill = "transparent",
        colour = "NA",
        size = 0.5,
        linetype = 1
      ),
      text =              element_text(
        family = base_family,
        face = "plain",
        colour = "#656565",
        size = base_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 1,
        margin = margin(),
        debug = FALSE
      ),

      # Modified inheritance structure of text element
      plot.title =        element_text(
        size = rel(1.7),
        family = "Abel" ,
        face = 'bold',
        hjust = 0,
        vjust = 1,
        colour = '#3B3B3B',
        margin = margin(
          t = 0,
          r = 0,
          b = 5,
          l = 0
        )
      ),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),

      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),

      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),

      # Modifiying legend.position
      legend.position = 'none',

      complete = TRUE
    )
}


theme_p3_text <- function(base_size = 18,
                          base_family = "Abel") {
  theme_nothing() %+replace%
    theme(
      text =              element_text(
        family = base_family,
        face = "plain",
        colour = "#656565",
        size = base_size,
        hjust = 0,
        vjust = 0,
        angle = 0,
        lineheight = 1,
        margin = margin(
          t = 0,
          r = 0,
          b = 5,
          l = 0),
        debug = FALSE
      ),
      plot.title =        element_text(
        size = rel(1.5),
        family = "Abel" ,
        face = 'bold',
        hjust = 0,
        vjust = 0,
        colour = dkgrey,
        margin = margin(
          t = 0,
          r = 0,
          b = 5,
          l = 0)),
      axis.ticks.length = unit(0, "mm")
    )
}

theme_p3_title <- function(base_size = 18,
                           base_family = "Abel") {
  theme_p3_text() %+replace%
    theme(plot.title =        element_text(
      size = rel(1.8),
      family = "Abel" ,
      face = 'bold',
      hjust = 0,
      vjust = 0,
      colour = dkgrey,
      margin = margin(
        t = 0,
        r = 0,
        b = 8,
        l = 0)))
}

theme_p3_fig <- function(base_size = 18,
                         base_family = "Abel") {
  theme_classic() %+replace%
    theme(
      line = element_blank(),
      text = element_blank(),
      title = element_blank(),
      plot.background = element_rect(fill = dkgrey, color = dkgrey),
      panel.background = element_rect(fill = dkgrey, color = dkgrey)
    )
}


p3_table_theme <- function(){
  theme <- ttheme_minimal(
    base_size = 11,
    base_colour = "black",
    padding = unit(c(4, 3), "mm"),
    core = list(fg_params=list(hjust = 0.5
                               #, x=.05
                               , fontface=c("plain","plain")
                               ),
                bg_params = list(fill = c(white, ltgrey))),
    colhead = list(fg_params=list(col=c(white, white)
                                  , fontface="plain"
                                  , cex = 1
                                  , hjust = 0.5
                                  #, x=.05
                                  ),
                   bg_params=list(fill=dkred))
  )
  return(theme)}
