#### P3 NBA Combine Cover Page #####


# Draw text on the page
drawtitle <- function(title = "NULL") {
  
  p <- ggplot() + xlim(0, 1) + ylim(0, .5)
  
  border <- linesGrob(
    x = unit(c(0, 1), "npc"),
    y = unit(c(1, 1), "npc"),
    default.units = "npc",
    arrow = NULL,
    name = NULL,
    gp = gpar(col = dkgrey, lwd = 2),
    vp = NULL
  )
  p <-
    p + ggtitle(title) + 
    annotation_custom(grob = border) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  p <- p + theme_p3_title()
  p <- p + theme(axis.ticks.length = unit(0, "pt"))+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))+
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}
draw_preamble <- function(preamble) {
  
  
  
  grob1 <- splitTextGrob(preamble,
                         x = unit(0, "npc"),
                         y = unit(1, "npc"),
                         just = c("left", "top"),
                         vjust = 1.2,
                         gp = gpar(
                           fontfamily = "Abel",
                           col = "black",
                           cex =1
                         ))
  
  p <- ggplot() + xlim(0, 1) + ylim(0, 1) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank())+
    annotation_custom(grob = grob1,
                      ymax = 1,
                      xmin = 0)+
    theme(axis.line = element_blank())+
    panel_border(colour = "black", size = 1, linetype = 1,
                 remove = TRUE)+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(0,1),expand = c(0,0)) +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
  
}
title_height_coverpage = .6

##################
#### TESTING #####
##################

width = 8.5 # page width in inches
height = 11 # page height in inches
ncols = 24 # number of cols for content
nrows = 25 # number of rows for content
margins = c(.75,.4,.75,.75) # top, right, bottom, left margin


grid_cover <- get_grid(width, height, ncols, nrows, title_height_coverpage, margins)


preamble_text1 <- 
  "P3 is an applied sports science company located in Santa Barbara, CA, that has pioneered the use of advanced sports science technologies for performance and injury prevention in professional athletes. Over the past 12-years, P3 has assessed over 400 NBA athletes and become a trusted resource for NBA players, agents, and teams looking to improve individual player development and performance – all references to NBA averages herein refer to this aforementioned population. By taking a sports-science and data-driven approach to optimizing NBA athletes’ careers, P3 has established itself as a leading authority on basketball player biomechanics and performance."

preamble_text2 <- 
  "At the 2018 NBA Draft Combine, P3 utilized a pair of force plates and a 3D motion capture system to track thousands of data points on each athlete from a series of three performance tests. This data provides insight into athlete kinetics (power output, velocity, rate of force development) and kinematics (joint angles, angular velocities, angular accelerations, etc.). P3 then analyzes this information to identify potential risk factors for injury and inefficiencies that limit performance. This insight can be utilized to design individualized exercise and movement prescriptions, and to compare to clinical impressions from organizational and athlete support staffs. The goal of this assessment is to help optimize each athlete’s NBA career"

preamble_text3 <- 
  "This information from the 2018 NBA Draft Combine is not shared with NBA teams or third parties, but instead is distributed only to the NBA League Office and to the players, who are then free to share this information with their team, agents, or other support staff as they see fit"

############################
##### FOR THE GLOSSARY######
############################
get_glossary <- function(){
  being_img <-
    rasterGrob(readPNG("pdfimg.png"))
  
  fig <- ggplot() +
    annotation_custom(being_img)+
    scale_colour_manual(values = c(dkred, blue)) +
    guides(colour = FALSE, size = FALSE) +
    theme_p3_fig()
  
}
  
