print("hi we made it to clustering")
library(caret)
#library(useful)
library(dplyr)
library(cluster)
#library(fpc)
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
 
  placement <- ifelse(athlete_label == "Traditional Big", 1,
                                ifelse(athlete_label == "Specimen", 2,
                                       ifelse(athlete_label == "Minus Perimeter", 3,
                                              ifelse(athlete_label == "Kinematic Movers", 4,
                                                     ifelse(athlete_label == "Bigs Plus", 5,
                                                            ifelse(athlete_label == "Force Movers", 6,
                                                                   ifelse(athlete_label == "Hyper-Athletic Guards",7,"Error")))))))
  fill_colors <- rep("gray", 7)
  outline_colors <- rep("gray", 7)
  
  fill_colors[placement] <- "red"
  outline_colors[placement] <- "red"
  
  labsize <- 5
  
  plot <- autoplot(completed_cluster , data = cluster_raw_data, 
           #label = TRUE, label.label = 'names', ncol =14, label.size = 7, 
           shape = TRUE,
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




