library(ggplot2)
library(ggrepel)
library(dplyr)
library(extrafont)
library(reshape2)
library(png)

loadfonts()

dkred <<- '#b11a21'
ltred <<- '#e0474c'
blue <<- '#7acfd6'
ltgrey <<- '#F0F0F0'
midgrey <<- '#b4b4b4'
middkgrey <<- '#969696'
dkgrey <<- '#656565'
white <<- '#ffffff'
yellow <<- "#FFFF66"
green <<- "#228b22"
pal <<- c(dkred, ltred, blue, ltgrey, dkgrey)


athlete_table <- 'public.soccer_page_1_bio_info'

get_stats <- function(playername, date) {
  print("Retrieving Bio Info")
  query <- paste("select name as playername, assessmentdate as date, height as playerht, bwkg as playerwt, position as position from "
        ,athlete_table, " where \"name\" = '",playername,
                 "' and assessmentdate = '",date, "'",sep="")
  stats_df <- read_civis(sql(query),'P3')
  colnames(stats_df) <- c("Name","Date","Height","Weight", "Position")
  return(stats_df)
}   ### UPDATED 


get_athl_cluster_data <- function(playername,date) {
  print("Retrieving Athleticism Cluster Data")
  cluster_athlete_query <- paste("select *
    from public.soccer_spider_plot_data
    where name = '",playername,"' and assessmentdate = '",date,"'",sep="")

  cluster_athlete <- read_civis(sql(cluster_athlete_query),"P3")

  cluster_avg_sql <- paste("select *
    from public.soccer_spider_plot_data
    where name = 'Joel Ward' and assessmentdate = '2018-06-08'",sep="")

  cluster_avg <- read_civis(sql(cluster_avg_sql),"P3")
  
  cluster_avg <- cluster_avg[,-c(1:2)]
  cluster_athlete_compare <- cluster_athlete[,-c(1:2)]

  t_athl_compare <- data.frame(t(cluster_athlete_compare))
  t_athl_compare <- add_rownames(t_athl_compare,"metric")
  colnames(t_athl_compare) <- c("metric","score")
  t_athl_compare$cluster <- 'athlete'
  t_athl_compare <- t_athl_compare[order(t_athl_compare$metric),]
  t_cluster_avg <- data.frame(t(cluster_avg))
  t_cluster_avg <- add_rownames(t_cluster_avg,"metric")
  colnames(t_cluster_avg) <- c("metric","score")
  t_cluster_avg$cluster <- 'comparison'
  t_cluster_avg <- t_cluster_avg[order(t_cluster_avg$metric),]
  cluster_radar <- bind_rows(t_athl_compare,t_cluster_avg)

  metric <- c("avg_total_movement_time",
              "conc_rel_ff",
              "ecc_rel_ff",                         
              "lateralforceleftbw",
              "lateralforcerightbw",
              "relativepower",                      
              "slmaxhipabduction",
              "slmaxhipextensionvelocity",
              "srmaxhipabduction",                  
              "srmaxhipextensionvelocity",
              "vertmaxkneeextensionaccelerationavg",
              "vertmaxkneeextensionvelocityavg")

  label <- c("Avg. Total Mvmt. Time"
             ,"Conc Rel FF"
             ,"Ecc Rel FF"
             ,"Lateral Drive (L)"
             ,"Lateral Drive (R)"
             ,"Relative Power"
             ,"Peak Hip Abduction (L)"
             ,"Hip Extension Velocity (L)"
             ,"Peak Hip Abduction (R)"
             ,"Hip Extension Velocity (R)"
             ,"Knee Ext. Accel."
             ,"Knee Ext. Velocity")

  cl <- data.frame(metric,label)
  cluster_radar <- merge(cluster_radar,cl,by="metric")
  cluster_radar$metric <- cluster_radar$label
  cluster_radar$metric <- factor(cluster_radar$metric,levels = c("Relative Power"
                                                                 ,"Conc Rel FF"
                                                                 ,"Ecc Rel FF"
                                                                 ,"Lateral Drive (R)"
                                                                 ,"Hip Extension Velocity (R)"
                                                                 ,"Peak Hip Abduction (R)"
                                                                 ,"Knee Ext. Velocity"
                                                                 ,"Avg. Total Mvmt. Time"
                                                                 ,"Knee Ext. Accel."
                                                                 ,"Peak Hip Abduction (L)"
                                                                 ,"Hip Extension Velocity (L)"
                                                                 ,"Lateral Drive (L)"))
  cluster_radar <- cluster_radar[order(cluster_radar$metric),]
  return(cluster_radar)

}   ### DONE



graph_page_2_2x2 <- function(playername, assessmentdate){
  print("Retrieving 2x2 Scatter Cluster Data")
  ### Extract the player's position to allow for comparison and to allow for dynamic graph labeling (used below)
  data <- read_civis('public.soccer_page_1_scatter','P3')
  players_position <- data %>%
    filter(name == as.character(playername)) %>%
    select(position) %>%
    pull("position")


  graph <- ggplot(data, aes(x = xaxis, y = yaxis, label=playername)) +
    geom_vline(xintercept = mean(data$xaxis)) + ### position mean, x
    geom_hline(yintercept = mean(data$yaxis)) + ### position mean, y
    geom_point(aes(fill=position),size=7, pch =21, alpha=0.5) +  ### overall points
    geom_point(data = data[data[,1] == playername & data[,2] == assessmentdate , ], size = 11, fill = "black", pch=21, show.legend = FALSE) +  ## point for target athlete
    geom_text(data = data[data[,1] == playername & data[,2] == assessmentdate , ]
              , aes(xaxis,yaxis,label = paste(substr(word(name),1,1),substr(word(name,2),1,1),sep="")), size = 5, fontface="bold",color="white") +  ## text for target athlete
    xlab("Lateral Acceleration") +
    ylab("Vertical Acceleration") +
    labs(title="Vertical-Lateral Acceleration",
         subtitle="Axes represent average for all P3 tested athletes\nFill color indicates position") +
    theme_minimal() +
    theme_p3() +
    theme(#axis.text = element_text(size=10, face = "bold"),
          axis.title.x = element_text(size=11,face="bold"),
          axis.title.y = element_text(size=11,face="bold",angle=90,vjust = 10),
          legend.position = 'bottom',
          legend.key = element_rect(linetype = "blank"),
          legend.key.size = unit(1,"line"))  +
    scale_fill_manual(values=c(dkgrey, white, dkred)) +
    guides(fill=guide_legend("Position"))

  return(graph)

}   ### UPDATED - WILL NEED TO FIX 


get_kpis <- function(playername, date) {
  print("Retrieving Page 1 Percentiles")
  std_sql <- paste("select * from public.soccer_page_1_summary_percentiles where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
  std <- read_civis(sql(std_sql),"P3")
  std <- add_rownames(data.frame(t(std[,3:length(std)])),"metric")
  colnames(std) <- c("metric","percentile")
  metric <- c("vertmaxkneeextensionvelocityavg", 
              "conc_rel_ff", 
              "relativepower", 
              "vertmaxkneeextensionaccelerationavg", 
              "lateralforceleftbw", 
              "lateralforcerightbw", 
              "slmaxhipabduction",
              "srmaxhipabduction",
              "stancesldvjumpheight", 
              "kickingsldvjumpheight", 
              "svbbjumpheight")
  label <- factor(c("Knee Ext Vel (SV)"
             ,"Conc Rel FF (SV)"
             ,"Relative Power (SV)"
             ,"Knee Ext Accel (SV)"
             ,"Lateral Drive (L SK)"
             ,"Lateral Drive (R SK)"
             ,"Max Hip Abd. (L SK)"
             ,"Max Hip Abd. (R SK)"
             ,"SL - Stance"
             ,"SL - Kick"
             ,"Vertical Jump"))
  type <- c("vertical"
            ,"vertical"
            ,"vertical"
            ,"vertical"
            ,"lateral"
            ,"lateral"
            ,"lateral"
            ,"lateral"
            ,"performance"
            ,"performance"
            ,"performance")
  std_merge <- data.frame(metric,label,type)
  kpis <- merge(std,std_merge,by="metric")
  kpis$percentile <- as.numeric(as.character(kpis$percentile))
  kpis$metric <- factor(kpis$label,levels=rev(label))
  kpis$pos <- FALSE
  over_50 <- function(x) {
    ifelse(is.na(x),FALSE,x>50)
  }
  kpis$pos <- as.character(lapply(kpis$percentile, function(x) over_50(x)))
  return(kpis)
} ## DONE


get_accel_decel_2 <- function(playername,date) {
  print("Retrieving Page 2 Accel Decel Data")
  accel_decel_sql <- paste("select * from public.soccer_page_2_accel_decel
                           where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  accel_decel <- read_civis(sql(accel_decel_sql),"P3")
  accel_decel_1 <- accel_decel[,3:8]
  colnames(accel_decel_1)[which(names(accel_decel_1) == "ecc_rel_ff")] <- "Ecc. Force"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "conc_rel_ff")] <- "Rel Conc Force"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "average_lateralforcebw")] <- "Lat. Drive - Avg."
  colnames(accel_decel_1)[which(names(accel_decel_1) == "vertmaxkneeextensionaccelerationavg")] <- "Knee Ext Accel"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "drop_kickmaxkneeactivedecelerationkicking")] <- "Knee Active Decel\n(Kick)"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "drop_stancemaxkneeactivedecelerationstance")] <- "Knee Active Decel\n(Stance)"
  accel_decel_1 <- data.frame(t(accel_decel_1))
  accel_decel_1 <- add_rownames(accel_decel_1,"metric")
  colnames(accel_decel_1) <- c("metric","value")
  accel_decel_1$metric <- factor(accel_decel_1$metric,levels = c("Lat. Drive - Avg.","Ecc. Force","Knee Ext Accel","Knee Active Decel\n(Kick)","Rel Conc Force","Knee Active Decel\n(Stance)"))
  return(accel_decel_1)
} ## DONE


get_percentiles_page_2 <- function(playername, date) {
  print("Retrieving Page 2 Percentiles Data")
  percentile_sql <- paste("select * from public.soccer_page_2_percentiles where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  full_table <- read_civis(sql(percentile_sql),"P3")
  percentiles <- c("drop_stancetotalmovementtimestance","drop_stanceankletotalromstance","drop_stancemaxkneeactivedecelerationstance",
                   "concstanceraw","drop_kicktotalmovementtimekicking","drop_kickankletotalromkicking",             
                   "drop_kickmaxkneeactivedecelerationkicking","conckickingraw","conc_rel_ff",                                
                   "load_rel_ff","vertmaxkneeextensionvelocityavg","vertmaxkneeextensionaccelerationavg",       
                   "relativepower","lateralforceleftbw","slmaxhipextensionvelocity",                 
                   "slmaxhipabduction","lateralforcerightbw","srmaxhipextensionvelocity","srmaxhipabduction")
  label <- c("Contact Time ","Ankle ROM","Knee Active Decel",
             "Peak Conc Force","Contact Time","Ankle ROM ",
             "Knee Active Decel ","Peak Conc Force ","Conc Rel FF",
             "Load Rel FF","Knee Ext Velocity","Knee Ext Accel",
             "Relative Power","L - Lateral Drive","L - Hip Ext. Velocity",
             "L - Hip Abduction","R - Lateral Drive","R - Hip Ext. Velocity","R - Hip Abduction")
  percentile_labels <- data.frame(percentiles,label)
  athlete_percentiles <- full_table[,3:length(full_table)]
  athlete_percentiles <- data.frame(t(athlete_percentiles))
  athlete_percentiles <- add_rownames(athlete_percentiles,"metric")
  colnames(athlete_percentiles) <- c("metric","value")
  percentiles_frame <- merge(percentile_labels,athlete_percentiles,by.x=c("percentiles"),by.y=c("metric"))

  tests <- c("SL Drop Stance","SL Drop Stance","SL Drop Stance","SL Drop Stance",
             "SL Drop Kick","SL Drop Kick","SL Drop Kick","SL Drop Kick",
             "Standing Vertical","Standing Vertical","Standing Vertical","Standing Vertical","Standing Vertical",
             "Skater","Skater","Skater","Skater","Skater","Skater")
  var_tests <- data.frame(percentiles,tests)


  ff2 <- merge(percentiles_frame,var_tests,by.x="percentiles")
  ff2 <- ff2[,-1]
  colnames(ff2) <- c("metric","Percentile","test_type")
  ff2$metric <- factor(ff2$metric,levels = label)
  ff2 <- ff2[order(ff2$metric),]
  return(ff2)
}  ## DONE

get_percentiles_page_3 <- function(playername,date) {
  print("Retrieving Page 3 Dot Plot Percentiles")
  percentile_sql <- paste("select * from public.soccer_page_3_percentiles
   where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  full_table <- read_civis(sql(percentile_sql),"P3")
  low_back <- full_table[,c("average_ankle_dorsi", 
                            "trunkl", 
                            "trunkr", 
                            "average_delta_hip_flex" )]
  low_back <- data.frame(t(low_back))
  low_back <- add_rownames(low_back, "metric")
  colnames(low_back) <- c("metric","value")
  low_back$type <- 'low back'
  low_back$order <- c(1:4)
  low_back$metric <- factor(low_back$metric, levels = low_back$metric[order(low_back$order)])

  stance_knee <- full_table[,c("drop_stancemaxrelativerotationstance", 
                             "translationstance", 
                             "drop_stancedeltahipflexionstance")]
  stance_knee <- data.frame(t(stance_knee))
  stance_knee <- add_rownames(stance_knee,"metric")
  colnames(stance_knee) <- c("metric","value")
  stance_knee$type <- ifelse(full_table$drop_stanceleg == "L", 'left knee', 'right knee')
  stance_knee$order <- c(1:3)
  stance_knee$metric <- factor(stance_knee$metric, levels = stance_knee$metric[order(stance_knee$order)])


  kick_knee <- full_table[,c("drop_kickmaxrelativerotationkicking",
                              "translationkicking", 
                              "drop_kickdeltahipflexionkicking")]
  kick_knee <- data.frame(t(kick_knee))
  kick_knee <- add_rownames(kick_knee,"metric")
  colnames(kick_knee) <- c("metric","value")
  kick_knee$type <- ifelse(full_table$drop_kickleg == "L", 'left knee', 'right knee')
  kick_knee$order <- c(1:3)
  kick_knee$metric <- factor(kick_knee$metric, levels = kick_knee$metric[order(kick_knee$order)])

  stance_foot <- full_table[,c("drop_stanceankleactivedecelerationstance",
                             "drop_stanceankleflexionatt0stance",
                             "inversionstance",
                             "eversionstance" )]
  stance_foot <- data.frame(t(stance_foot))
  stance_foot <- add_rownames(stance_foot,"metric")
  colnames(stance_foot) <- c("metric","value")
  stance_foot$type <- ifelse(full_table$drop_stanceleg == "L", 'left foot', 'right foot')
  stance_foot$order <- c(1:4)
  stance_foot$metric <- factor(stance_foot$metric, levels = stance_foot$metric[order(stance_foot$order)])


  kick_foot <- full_table[,c("drop_kickankleactivedecelerationkicking",
                              "drop_kickankleflexionatt0kicking",
                              "inversionkicking",
                              "eversionkicking" )]
  kick_foot <- data.frame(t(kick_foot))
  kick_foot <- add_rownames(kick_foot,"metric")
  colnames(kick_foot) <- c("metric","value")
  kick_foot$type <- ifelse(full_table$drop_kickleg == "L", 'left foot', 'right foot')
  kick_foot$order <- c(1:4)
  kick_foot$metric <- factor(kick_foot$metric, levels = kick_foot$metric[order(kick_foot$order)])

  percent_frame <- bind_rows(low_back,stance_knee, stance_foot, kick_knee, kick_foot)
  colnames(percent_frame) <- c("metric","percentile","type","order")
  percent_frame$pos <- FALSE
  percent_frame$percentile <- percent_frame$percentile
  over_50 <- function(x) {
    ifelse(is.na(x),FALSE,x>50)
  }
  percent_frame$pos <- as.character(lapply(percent_frame$percentile, function(x) over_50(x)))

  metric <- c("average_ankle_dorsi", 
              "trunkl", 
              "trunkr", 
              "average_delta_hip_flex",
              "drop_stancemaxrelativerotationstance", 
              "translationstance", 
              "drop_stancedeltahipflexionstance",
              "drop_kickmaxrelativerotationkicking",
              "translationkicking", 
              "drop_kickdeltahipflexionkicking",
              "drop_stanceankleactivedecelerationstance",
              "drop_stanceankleflexionatt0stance",
              "inversionstance",
              "eversionstance", 
              "drop_kickankleactivedecelerationkicking",
              "drop_kickankleflexionatt0kicking",
              "inversionkicking",
              "eversionkicking")

  label <- c("Avg. Ankle Dorsi"
             ,"Trunk Stability-L"
             ,"Trunk Stability-R"
             ,"Avg. Delta Hip Flex"
             ,"Rel. Rotation"
             ,"Translation"
             ,"Delta Hip Flex"
             ,"Rel Rotation"
             ,"Translation"
             ,"Delta Hip Flex"
             ,"Ankle Act. Decel"
             ,"Active Dorsi"
             ,"Inversion"
             ,"Eversion"
             ,"Ankle Act. Decel"
             ,"Active Dorsi"
             ,"Inversion"
             ,"Eversion"
             )

  column_labels <- unique(data.frame(metric,label))
  percent_merged_frame <- merge(percent_frame,column_labels,by="metric")
  return(percent_merged_frame)

}

summary_plot <- function(overall) {
  ggplot(overall  %>% arrange(desc(metric)),
           aes(
             x = metric,
             y = value,
             color = pos,
             label = " "
           )) +
    geom_rect(xmin=0, xmax=4.5, ymin=-Inf, ymax=Inf, linetype=0, aes(fill='vertical')) +
    geom_rect(xmin=4.5, xmax=Inf, ymin=-Inf, ymax=Inf, linetype=0, aes(fill='horizontal')) +
    scale_fill_manual("", values = alpha(c('vertical' = white, 'horizontal' = midgrey), .1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    geom_point(
      stat = 'identity',
      fill = dkgrey,
      size = 10
    )  +
    scale_colour_manual(values=c("FALSE"=dkred,"TRUE"=dkgrey)) +
    geom_text(color = white, size = 3) +
    ylim(0, 100) +
    #scale_alpha_manual(values = c('vertical' = .2, 'horizontal' = .2)) +
    coord_flip() +
    theme_p3() +
    theme(axis.title.x = element_text()) +
    #theme(legend.justification=c(1,0), legend.position=c(1,0), legend.background =  element_rect(fill = ltgrey)) +
    labs(title = "Player Summary",
         subtitle = "Normalized performance indicator scores") +
    ylab("Percentile related to P3 Athletes")
}

get_fig <- function(playername,date){
  being_img <- ifelse(GENDER = true,rasterGrob(readPNG("p3_woman.png")),rasterGrob(readPNG("p3 man.png")))
  print("Retrieving Flag Diagram Data")
  color_sql <- paste("select * from public.soccer_flags where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
  color_frame <- read_civis(sql(color_sql),"P3")
  df <- data.frame(
    # L Ankle, L Knee, R Ankle, R Knee, M Back
    x = c(-0.22, -0.23, 0.05,  0.02, -0.13),
    y = c(-0.73, -0.41, -0.81, -0.45, 0.15),
    color = c(as.character(color_frame[,"rightankle_flag"]),as.character(color_frame[,"rightknee_flag"]),as.character(color_frame[,"leftankle_flag"]),as.character(color_frame[,"leftknee_flag"]),as.character(color_frame[,"lowback_flag"]))
  )
  color_map <- c("red"=dkred,"green"=green,"yellow"=yellow)
  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    geom_point(data = df,
               aes(x, y, size = 30, color = color,fill=color),
               alpha = .4,stroke=1,shape=21,show_guide=FALSE) +
    scale_colour_manual(values = color_map) +
    scale_fill_manual(values=color_map) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(8,12)) +
    theme_p3_man()
}

get_logo <- function(){
  being_img <-
    rasterGrob(readPNG("P3 logo.png"))

  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-.25, .25) +
    ylim(-1, 1) +
    scale_colour_manual(values = c(dkgrey, blue)) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(5, 8)) +
    theme_p3_fig()
}

get_eagle <- function(){
  being_img <-
    rasterGrob(readPNG("germany_eagle.png"))
  
  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-.25, .25) +
    ylim(-1, 1) +
    scale_colour_manual(values = c(dkgrey, blue)) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(5, 8)) +
    theme_p3_fig()
}

history_plot <- function(history) {
  ggplot(history, aes(x=label,y=value,group=period,color=period)) +
    geom_point(aes(col=period), size=4) +
    #geom_line(size=2) +
    scale_color_manual(values=c(dkred,dkgrey),labels=c("Current","Previous")) +
    labs(title = "Performance Summary",
         subtitle = "Scores from previous assessments") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)
                     ,limits=rev(levels(history$label))) +
    theme_p3() +
    coord_flip() +
    theme(legend.position = "right"
          ,legend.key = element_rect(linetype = "blank"))
}

acceleration_bars <- function(df,subtitle) {
  ggplot(df,aes(x=metric,y=value,fill=metric)) +
    geom_bar(stat = 'identity') +
    labs(title="Acceleration / Deceleration",
         subtitle=subtitle) +
    scale_fill_manual("legend",values = c(dkred,dkgrey,dkred,dkgrey,dkred,dkgrey)) +
    coord_flip() +
    theme_p3() +
    ylim(-100,100)
}

radar_plot <- function(df.rad) {
  radar_plot <- ggplot(df.rad, aes(x = metric, y = score/100, color = cluster, group = cluster)) +
    geom_polygon(aes(color = cluster, fill = cluster), alpha = .2) + geom_point(aes(color = cluster)) +
    coord_polar(start=-pi/4) +
    theme_p3() +
    scale_color_manual(values=c(dkgrey, dkred), guide = FALSE) +
    scale_fill_manual(values=c(dkgrey, dkred), labels = c("Athlete", 'EPL CB'), name = NULL) +
    labs(title="Performance Factor Comparison",
         subtitle="Athlete Relative to EPL CB") +
    scale_y_continuous(limits = c(0,1.20),expand=c(0,0.0)) +
    geom_text(aes(x=metric, y=1.20,
                  label=str_wrap(metric,width=10)),
              color=dkgrey,size=2) +
    theme(
          legend.key = element_blank(),
          legend.position = 'right',
          axis.text.x=element_blank(),
          plot.subtitle = element_text(vjust=12),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    )
  return(radar_plot)

}

dot_plot <-  function(overall, type = 'lateral',title='Graph') {
  df <- overall %>% filter_(paste0('type == "', type, '"'))
  ggplot(df, aes(x=label, y=percentile)) +
    geom_point(aes(col=pos), size=4) +   # Draw points
    geom_segment(aes(x=label,
                     xend=label,
                     y=0,
                     yend=100),
                 linetype="dashed",
                 size=0.1) +   # Draw dashed lines
    geom_hline(yintercept=50, size = .5) +
    labs(title=title,
         subtitle="Percent Rank") +
    scale_colour_manual(values=c("FALSE"=dkred,"TRUE"=dkgrey)) +
    theme_p3() +
    coord_flip() +
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.title = element_text(vjust=20),
          plot.subtitle = element_text(vjust=20)
    )
}

dot_plot_right <-  function(overall, type = 'lateral',title='Graph') {
  df <- overall %>% filter_(paste0('type == "', type, '"'))
  ggplot(df, aes(x=label, y=percentile)) +
    geom_point(aes(col=pos), size=4) +   # Draw points
    geom_segment(aes(x=label,
                     xend=label,
                     y=0,
                     yend=100),
                 linetype="dashed",
                 size=0.1) +   # Draw dashed lines
    geom_hline(yintercept=50, size = .5) +
    labs(title=title,
         subtitle="Percent Rank") +
    scale_colour_manual(values=c(dkred, dkgrey)) +
    theme_p3() +
    coord_flip() +
    scale_x_discrete(position="top") +
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
}
