print("hi we made it to clustering")
library(caret)
library(dplyr)
library(cluster)
library(ggfortify)
library(ggplot2)
library(civis)
library(grid)

Sys.setenv(CIVIS_API_KEY = 'cdd57e940062ebda689520c4882a3024de788987e77709ab94f88c2f0883919c')   


### Get all Data and models needed for new athlete
new_cluster<- read_civis("public.v2_plotting_cluster_data", database = "P3")
cluster_raw_data <- read.csv("report_v2/p3clustering_single.csv", stringsAsFactors = FALSE) %>%
  select(-hawks)
completed_cluster <- readRDS("report_v2/production_cluster_data_v1.RDS")
model <- readRDS("report_v2/cluster_sorting.RDS")


### Pull in the athlete whos data were generating the report for, make sure column names match up
single_athlete <- new_cluster %>%
  filter(name == playername) %>%
  filter(assessmentdate == date) 

names(single_athlete) <- names(cluster_raw_data)  

### Sort the athlete and pull out his predicted cluster
athlete_prediction <- predict(model, single_athlete)


### Input the cluster the athlete is in into the funciton, which returns a "lighted up" cluster for his, and gray for others 
clusterplot <- function(athlete_label){
 print("doing cluster plot")
  placement <- ifelse(athlete_label == "Traditional Big", 1,
                                ifelse(athlete_label == "Specimen", 2,
                                       ifelse(athlete_label == "Minus Perimeter", 3,
                                              ifelse(athlete_label == "Kinematic Movers", 4,
                                                     ifelse(athlete_label == "Bigs Plus", 5,
                                                            ifelse(athlete_label == "Force Movers", 6,
                                                                   ifelse(athlete_label == "Hyper-Athletic Guards",7,"Error")))))))
  fill_colors <- rep("gray", 7)
  outline_colors <- rep("gray", 7)
  
  fill_colors[placement] <- "black"
  outline_colors[placement] <- "black"
  
  labsize <- 5
  
  plot <- autoplot(completed_cluster , data = cluster_raw_data, 
           label = FALSE, #label.label = 'names', ncol =14, label.size = 7, 
           shape = FALSE,
           frame = TRUE) +
  scale_fill_manual(values = fill_colors) +
  scale_color_manual(values = outline_colors) +
  xlab("") +
  ylab("") +
  annotate("text", x = -.12, y = .07, label = "Traditional Bigs", color = "black", angle = 30, fontface = "bold", size =labsize) +  ## cluster 1
  annotate("text", x = .08, y = -.12, label = "Specimens", color = "black", angle = 35, fontface = "bold", size =labsize) + ## cluster 2
  annotate("text", x = -.05, y = .09, label = "Minus Perimeter", color = "black", angle = 35, fontface = "bold", size =labsize) +## cluster 3
  annotate("text", x = .01, y = .17, label = "Kinematic Movers", color = "black", fontface = "bold", size =labsize) +## cluster 4
  annotate("text", x = -.05, y = -.17, label = "Bigs Plus", color = "black", fontface = "bold", size =labsize) +## cluster 5
  annotate("text", x = 0, y = 0.01, label = "Force Movers", color = "black",angle = 15, fontface = "bold", size =labsize) +## cluster 6
  annotate("text", x = .095, y = .06, label = "Hyper-Athletic Guards", color = "black", angle = -25, fontface = "bold", size =labsize) + ## cluster 7 
  theme_void() +
  theme(legend.position="none")
       #panel.border = element_rect(colour = "black", fill = NA, size =1)) 
   
  return(plot)
}


cluster_text <- data.frame(
    Cluster = c("Traditional Bigs","Specimens","Minus Perimeter","Kinematic Movers","Bigs Plus", "Force Movers", "Hyper-Athletic Guards"),
    Description = c("Traditional Big: Size tends to be the dominant physical characteristic of athletes in this cluster. Both Height and Length considerably outpace the typical NBA athlete. Improved power output - either vertically or laterally - can push an athlete into the Big Plus category.",
                    "Specimen: Generally, athletes in this cluster possess an impressive mix of size for their position and power output. Force production in all planes can be classified as 'elite', and this quality is paired with above-average kinematic efficiency - suggesting that athletes in this cluster are able to take advantage of their endemic power.",
                    "Minus Perimeter: Athletes in this cluster possess adequate Height and Length measures relative to the NBA population, but fail to pair these metrics with 'plus' movement skills. Both force production and kinematic variables grade-out at as below-average in all planes.",
                    "Kinematic Movers: While these athletes possess middling traditional performance  measures (and force output), they possess elite kinematic measures. Lateral plane movement efficiency generally stands out as a strength for athletes in this cluster.",
                    "Big Plus: While anthropometric measures tend to be the dominant physical traits of this group, Big Plus athletes pair impressive size with league-average vertical and lateral force production. 'Plus' size alongside adequate movement skills can lead to physical advantages for the athletes in this category.",
                    "Force Mover: Force production - in all planes of movement - separate this group from the other clusters. While these athletes do possess impressive power output - leading to elite traditional performance measures - they lack kinematic efficiency.",
                    "Hyper-Athletic Guards: Athletes in this cluster tend to lack the typical Height and Length of the typical NBA athlete. That said, they make up for these anthropometric measures with elite force production and kinematic efficiency. The rare athlete in this cluster with NBA-average Height and Length will possess a series of physical advantages"))

