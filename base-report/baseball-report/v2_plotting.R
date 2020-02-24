library(ggplot2)
library(ggrepel)
library(dplyr)
library(extrafont)
library(reshape2)
library(png)

loadfonts()


black<- "#000000"
dkred <<- '#EE0000'
ltred <<- '#e0474c'
blue <<- '#7acfd6'
ltgrey <<- '#F0F0F0'
midgrey <<- '#b4b4b4'
middkgrey <<- '#969696'
dkgrey <<- '#656565'
white <<- '#ffffff'
yellow <<- "#FFE600"
green <<- "#00FF41"
pal <<- c(dkred, ltred, blue, ltgrey, dkgrey)

get_table_stats <- function(playername, date) {
  print("Retrieving Bio Info")
  athlete_table <- 'public.baseball_page_1_bio_info'
  
  ### Table 1 - bio info  
  df <- read_civis(athlete_table, database = "P3") %>%
    filter(name == playername) %>%
    filter(assessmentdate == date) %>%
    mutate(perc_vert = as.numeric(perc_vert),
           perc_l_lat = as.numeric(perc_l_lat),
           perc_r_lat = as.numeric(perc_r_lat))
  stats_df <- df[,c(10,12,13,14,3)]
  colnames(stats_df) <- c("Name","Height", "Reach", "Weight", "Pos.")
  
  ### Table 2 - performance info
  performance_df_player <- df %>%
    select(display_name, display_vert, display_l_latforce, display_r_latforce, perc_vert, perc_l_lat, perc_r_lat) %>%
    mutate(perc_vert = ifelse(perc_vert >0, paste0("+", perc_vert),perc_vert),
           perc_l_lat = ifelse(perc_l_lat >0, paste0("+", perc_l_lat),perc_l_lat),
           perc_r_lat = ifelse(perc_r_lat >0, paste0("+", perc_r_lat),perc_r_lat)) %>%
    mutate_all(as.character) %>%
    mutate(display_vert = paste(display_vert, 
                                paste0("(",perc_vert,"%",")")),
           display_l_latforce = paste(display_l_latforce, 
                                    paste0("(",perc_l_lat,"%",")")),
          display_r_latforce = paste(display_r_latforce, 
                                    paste0("(",perc_r_lat,"%",")"))) %>%
    rename("Name" = display_name, "Vert Jump" = display_vert,"Lat Force - L" = display_l_latforce,
          "Lat Force - R" = display_r_latforce) %>%
    select(-c(perc_vert, perc_l_lat, perc_r_lat))
  
  performance_df_average <- df %>%
    select(display_name, average_vert,average_l_latforce, average_r_latforce) %>%
    mutate_all(as.character) %>%
    mutate(display_name = "MLB Avg.") %>%
    rename("Name" = display_name, "Vert Jump" = average_vert,"Lat Force - L" = average_l_latforce,
          "Lat Force - R" = average_r_latforce)
  
  performance_df <- rbind(performance_df_player, performance_df_average)
  
  overall_df <- left_join(stats_df, performance_df, by = "Name")%>%
    rename("Vert Jump\n(+/- MLB Avg)" = "Vert Jump", "Lat Force L\n(+/- MLB Avg)" = "Lat Force - L",
          "Lat Force R\n(+/- MLB Avg)" = "Lat Force - R")
  
  return(overall_df)
}
  


get_percentiles_page_2 <- function(playername, date) {
  print("Retrieving Page 2 Percentiles Data")
  percentile_sql_v2 <- paste("select * from public.baseball_page_2_percentiles where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  full_table_v2 <- read_civis(sql(percentile_sql_v2),"P3")
  percentiles <- c("percvertmaxkneeextensionvelocityavg","percvertmaxkneeextensionaccelerationavg","percconc_rel_ff","percload_rel_ff","percpeakpowercmj",
                   "perclateralforceleftbw","percslmaxhipextensionvelocity","percslmaxhipabduction",
                   "perclateralforcerightbw","percsrmaxhipextensionvelocity","percsrmaxhipabduction","perclpeakpowerrotary",
                   "percrpeakpowerrotary","percpeakfzleft_ash","percpeakfzright_ash","perctbcmj")
  label <- c("Knee Ext Velocity", "Knee Ext Accel", "Conc Rel FF", "Load Rel FF", "Peak Power",
             "L - Lateral Drive", "L - Hip Ext. Velocity","L - Hip Abduction",
             "R - Lateral Drive", "R - Hip Ext. Velocity","R - Hip Abduction", "L - Rot.",
             "R - Rot.", "L - ASH", "R - ASH", "TBCMJ")
  percentile_labels <- data.frame(percentiles,label)
  athlete_percentiles <- full_table_v2[,3:length(full_table_v2)]
  athlete_percentiles <- data.frame(t(athlete_percentiles))
  athlete_percentiles <- add_rownames(athlete_percentiles,"metric")
  colnames(athlete_percentiles) <- c("metric","value")
  percentiles_frame <- merge(percentile_labels,athlete_percentiles,by.x=c("percentiles"),by.y=c("metric"))
  tests <- c("Standing Vertical","Standing Vertical","Standing Vertical","Standing Vertical","Standing Vertical",
             "1 Off Skater","1 Off Skater","1 Off Skater","1 Off Skater","1 Off Skater","1 Off Skater",
             "Upper Extremity","Upper Extremity","Upper Extremity","Upper Extremity",
             "Strength")
  var_tests <- data.frame(percentiles,tests)
  
  ff2 <- merge(percentiles_frame,var_tests,by.x="percentiles")
  ff2 <- ff2[c("label","value","tests")]
  colnames(ff2) <- c("metric","Percentile","test_type")
  ff2$metric <- factor(ff2$metric,levels = c("Knee Ext Accel"
                                             ,"Knee Ext Velocity"
                                             ,"Conc Rel FF"
                                             ,"Load Rel FF"
                                             ,"Peak Power"
                                             ,"L - Lateral Drive"
                                             ,"L - Hip Abduction"
                                             ,"L - Hip Ext. Velocity"
                                             ,"R - Lateral Drive"
                                             ,"R - Hip Abduction"
                                             ,"R - Hip Ext. Velocity"
                                             ,"L - Rot."
                                             ,"R - Rot."
                                             ,"L - ASH"
                                             ,"R - ASH"
                                             ,"TBCMJ"))
  ff2 <- ff2[order(ff2$metric),]
  return(ff2)
}  ### This is page 2 kpis -- dont touch

get_logo <- function(){
  being_img <-
    rasterGrob(readPNG("P3 logo.png"))
  
  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-.25, .25) +
    ylim(-1, 1) +
    scale_colour_manual(values = c(dkred, blue)) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(5, 8)) +
    theme_p3_fig()
}




training_recs <- data.frame(
  Parameter_Name = c(Sys.getenv("HIP_STABILITY"),
                     Sys.getenv("TRUNK_STABILITY"),
                     Sys.getenv("INVERSION"),
                     Sys.getenv("TRANSLATION"),
                     Sys.getenv("EVERSION"),
                     Sys.getenv("ACTIVE_DORSI"),
                     Sys.getenv("ANKLE_STIFF"),
                     Sys.getenv("YIELDING"),
                     Sys.getenv("POST_YIELDING"),
                     Sys.getenv("IMPULSE_ASYM"),
                     Sys.getenv("HIP_MOBILITY"),
                     Sys.getenv("ANKLE_MOBILITY"),
                     Sys.getenv("LE_POWER"),
                     Sys.getenv("LAT_DRIVE"),
                     Sys.getenv("LE_STRENGTH")),
  Rec = c("Hip Stability",
          "Trunk Stability",
          "Inversion",
          "Translation",
          "Eversion",
          "Active Dorsiflexion", 
          "Ankle Stiffness",
          "Yielding",
          "Posterior Yielding",
          "Impulse Asym.",
          "Hip Mobility",
          "Ankle Mobility",
          "LE Power",
          "Lateral Drive",
          "LE Strength"),
  Description = c("- The athlete exhibited notable internal rotation of the femur during double-leg movements.",
                  "- During lateral plane actions, the athlete struggled to control the trunk in the sagittal plane.",
                  "- During the Drop Jump, the athlete contacts the ground with the bottom of the foot rotated inward.",
                  "- During the Drop Jump, the athlete rotates excessively through the foot.",
                  "- During the Drop Jump, the athlete reaches excessive Eversion.",
                  "- During the Drop Jump, the athlete contacts the ground in an excessively plantar flexed position.",
                  "-The athlete struggles to appropriately control the ankle as it moves into dorsi flexion.",
                  "- During a series of vertical movement, the athlete passes through only minimal range of motion at the hip and knee.",
                  "- When completing a series of vertical movements, the athlete passes through only minimal range of motion at the hip.",
                  "- The athlete demonstrates the tendency to pass considerably more force across one limb during double-leg movements.",
                  "- The Mobility Screen revealed notable limitations in the musculature surrounding the athlete's hip.",
                  "- The Mobility Screen revealed notable limitations in the athlete's ankle-joint mobility.",
                  "- The athlete's vertical plane power output can be improved with consistent training.",
                  " - The athlete's lateral plane force production warrants development.",
                  "- The athlete's strength properties warrant development."))

training_recs <- training_recs %>%
  filter(Parameter_Name == "true")

rec_one <- paste0("1. ", training_recs$Rec[1], training_recs$Description[1])                                           
rec_two <- paste0("2. ", training_recs$Rec[2], training_recs$Description[2])                                           
rec_three <- ifelse(nrow(training_recs) >2,paste0("3. ", training_recs$Rec[3], training_recs$Description[3]),"         ")     


get_glossary <- function(){
  being_img <-
    rasterGrob(readPNG("p3_glossary_2.0.png"))
  
  fig <- ggplot() +
    annotation_custom(being_img)+
    scale_colour_manual(values = c(dkred, blue)) +
    guides(colour = FALSE, size = FALSE) +
    theme_p3_fig()
  
}

get_athl_cluster_data <- function(playername,date) {
  print("Retrieving Athleticism Cluster Data")
  cluster_athlete_query <- paste("select *
    from public.baseball_spider_data
    where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  
  cluster_athlete <- read_civis(sql(cluster_athlete_query),"P3")
  
  cluster_avg_sql <- paste("select *
    from public.baseball_spider_data
    where name = 'Average' and assessmentdate = '2019-02-19'",sep="")
  
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
  
  metric <-   c("peakpowercmj",
                "vertmaxkneeextensionaccelerationavg",
                "srmaxhipextensionvelocity",
                "lateralforcerightbw",
                "rpeakpowerrotary",
                "conc_rel_ff",
                "tbcmj",
                "ecc_rel_ff",
                "lpeakpowerrotary",
                "lateralforceleftbw",
                "slmaxhipextensionvelocity",
                "vertmaxkneeextensionvelocityavg")
  
  
  label <- c("Peak Pwr."
             ,"Knee Ext. Accel."
             ,"Hip Ext. Velocity (R)"
             ,"Lateral Drive (R)"
             ,"Rot. (R)"
             ,"Conc Rel FF"
             ,"TBCMJ"
             ,"Ecc Rel FF"
             ,"Rot. (L)"
             ,"Lateral Drive (L)"
             ,"Hip Ext. Velocity (L)"
             ,"Knee Ext. Velocity")
  
  cl <- data.frame(metric,label)
  cluster_radar <- merge(cluster_radar,cl,by="metric")
  cluster_radar$metric <- cluster_radar$label
  cluster_radar$metric <- factor(cluster_radar$metric,levels = c("Knee Ext. Velocity"
                                                                 ,"Peak Pwr."
                                                                 ,"Knee Ext. Accel."
                                                                 ,"Hip Ext. Velocity (R)"
                                                                 ,"Lateral Drive (R)"
                                                                 ,"Rot. (R)"
                                                                 ,"Conc Rel FF"
                                                                 ,"TBCMJ"
                                                                 ,"Ecc Rel FF"
                                                                 ,"Rot. (L)"
                                                                 ,"Lateral Drive (L)"
                                                                 ,"Hip Ext. Velocity (L)"))
  cluster_radar <- cluster_radar[order(cluster_radar$metric),]
  return(cluster_radar)
  
}   ### DONE






radar_plot <- function(df.rad) {
  radar_plot <- ggplot(df.rad, aes(x = metric, y = score/100, color = cluster, group = cluster)) +
    geom_polygon(aes(color = cluster, fill = cluster), alpha = .2) + geom_point(aes(color = cluster)) +
    coord_polar(start=-pi/4) +
    theme_p3() +
    scale_color_manual(values=c(dkgrey, dkred), guide = FALSE) +
    scale_fill_manual(values=c(dkgrey, dkred), labels = c("Athlete", 'P3 Baseball Avg.'), name = NULL) +
    labs(title="Performance Factor Comparison",
         subtitle="Athlete Relative to P3 Baseball Average") +
    scale_y_continuous(limits = c(0,1.20),expand=c(0,0.0)) +
    geom_text(aes(x=metric, y=1.20,
                  label=str_wrap(metric,width=10)),
              color=dkgrey,size=3) +
    theme(
      legend.key = element_blank(),
      legend.position = 'bottom',
      axis.text.x=element_blank(),
      plot.subtitle = element_text(vjust=1, size=12),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  return(radar_plot)
  
}
