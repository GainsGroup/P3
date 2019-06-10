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
  athlete_table <- 'public.v2_page_1_bio_info'
  ### Table 1 - bio info  
  df <- read_civis(athlete_table, database = "P3") %>%
    filter(name == playername) %>%
    filter(date == assessmentdate) %>%
    mutate(perc_vert = as.numeric(perc_vert),
           perc_drop = as.numeric(perc_drop),
           perc_lat = as.numeric(perc_lat))
  stats_df <- df[,c(9,12,14,13)]
  colnames(stats_df) <- c("Name","Height","Weight","Reach")
  ### Table 2 - performance info
  performance_df_player <- df %>%
    select(display_name, display_vert, display_drop,display_latforce, perc_vert, perc_drop, perc_lat) %>%
    mutate(perc_vert = ifelse(perc_vert >0, paste0("+", perc_vert),perc_vert),
           perc_drop = ifelse(perc_drop >0, paste0("+", perc_drop),perc_drop),
           perc_lat = ifelse(perc_lat >0, paste0("+", perc_lat),perc_lat)) %>%
    mutate_all(as.character) %>%
    mutate(display_vert = paste(display_vert, 
                                paste0("(",perc_vert,"%",")")),
           display_drop = paste(display_drop, 
                                paste0("(",perc_drop,"%",")")),
           display_latforce = paste(display_latforce, 
                                paste0("(",perc_lat,"%",")"))) %>%
    rename("Name" = display_name, "Vert Jump" = display_vert, "Drop Jump" = display_drop, "Lat Force" = display_latforce) %>%
    select(-c(perc_vert, perc_drop, perc_lat))

  performance_df_average <- df %>%
    select(display_name, average_vert,average_drop,average_latforce) %>%
    mutate_all(as.character) %>%
    mutate(display_name = "NBA Avg.") %>%
    rename("Name" = display_name, "Vert Jump" = average_vert, "Drop Jump" = average_drop, "Lat Force" = average_latforce)
  
  performance_df <- rbind(performance_df_player, performance_df_average)
  
  overall_df <- left_join(stats_df, performance_df, by = "Name")%>%
    rename("Vert Jump\n(+/- NBA Avg)" = "Vert Jump", "Drop Jump\n(+/- NBA Avg)" = "Drop Jump", "Lat Force\n(+/- NBA Avg)" = "Lat Force")
    
  return(overall_df)
}

graph_page_2_2x2 <- function(playername, assessmentdate){
  print("Retrieving 2x2 Scatter Cluster Data")
  ### Extract the player's position to allow for comparison and to allow for dynamic graph labeling (used below)
  data <- read_civis('public.v2_scatter_model_data','P3')
  players_position <- data %>%
    filter(name == as.character(playername)) %>%
    select(position) %>%
    pull("position")
  
  
  graph <- ggplot(data, aes(x = average_lateralforcebw, y = predicted, label=playername)) +
    geom_vline(xintercept = 50) + ### position mean, x
    geom_hline(yintercept = 50) + ### position mean, y
    geom_point(aes(fill=position),size=5, pch =21, alpha=0.5) +  ### overall points
    geom_point(data = data[data[,1] == playername & data[,2] == assessmentdate , ], size = 12, fill = "black", pch=21, show.legend = FALSE) +  ## point for target athlete
    geom_text(data = data[data[,1] == playername & data[,2] == assessmentdate , ]
              , aes(average_lateralforcebw,predicted,label = paste(substr(word(name),1,1),substr(word(name,2),1,1),sep="")), size = 5, fontface="bold",color="white") +  ## text for target athlete
    xlab("Lateral Ability") +
    ylab("Vertical Ability") +
    labs(title="Vertical/Lateral Ability",
         subtitle="Axes represent average for all P3 tested athletes\nFill color indicates position") +
    theme_minimal() +
    theme_p3() +
    theme(#axis.text = element_text(size=10, face = "bold"),
      axis.title.x = element_text(size=11,face="bold"),
      axis.title.y = element_text(size=11,face="bold",angle=90),
      legend.position = 'bottom',
      legend.key = element_rect(linetype = "blank"),
      legend.key.size = unit(1,"line"))  +
    scale_fill_manual(values=c(white, dkred, dkgrey)) +
    guides(fill=guide_legend("Position"))
  
  
  return(graph)
  
}


get_kpis <- function(playername, date) {
  print("Retrieving Page 1 Percentiles")
  std_sql <- paste("select * from public.v2_page_1_percentiles where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
  std <- read_civis(sql(std_sql),"P3")
  std <- add_rownames(data.frame(t(std[,3:length(std)])),"metric")
  colnames(std) <- c("metric","percentile")
  metric <- c("vertmaxkneeextensionvelocityavg"
              ,"conc_rel_ff"
              ,"dropmaxkneeextensionvelocityavg"
              ,"imp_1_avg"
              ,"slmaxhipabduction"
              ,"srmaxhipabduction"
              ,"slmaxhipextensionvelocity"
              ,"srmaxhipextensionvelocity")
  label <- factor(c("Knee Ext Vel (SV)"
                    ,"Conc Rel FF (SV)"
                    ,"Knee Ext Vel (DV)"
                    ,"Net Imp 1 (DV)"
                    ,"Max Hip Abd. (L SK)"
                    ,"Max Hip Abd. (R SK)"
                    ,"Hip Ext Vel (L SK)"
                    ,"Hip Ext Vel (R SK)"))
  type <- c("vertical"
            ,"vertical"
            ,"vertical"
            ,"vertical"
            ,"lateral"
            ,"lateral"
            ,"lateral"
            ,"lateral")
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
}   ### This is page 1 percentiles 

get_accel_decel_2 <- function(playername,date) {
  print("Retrieving Page 2 Accel Decel Data")
  accel_decel_sql <- paste("select * from public.page_2_accel_decel
                           where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  accel_decel <- read_civis(sql(accel_decel_sql),"P3")
  accel_decel_1 <- accel_decel[,3:7]
  colnames(accel_decel_1)[which(names(accel_decel_1) == "ecc_rel_ff")] <- "Ecc. Force"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "conc_rel_ff")] <- "Rel Conc Force"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "skater_lat_drive_avg")] <- "Lat. Drive - Avg."
  colnames(accel_decel_1)[which(names(accel_decel_1) == "vertmaxkneeextensionaccelerationavg")] <- "Knee Ext Accel"
  colnames(accel_decel_1)[which(names(accel_decel_1) == "ankle_act_dec_avg")] <- "Ank. Active Decel"
  accel_decel_1 <- data.frame(t(accel_decel_1))
  accel_decel_1 <- add_rownames(accel_decel_1,"metric")
  colnames(accel_decel_1) <- c("metric","value")
  accel_decel_1$metric <- factor(accel_decel_1$metric,levels = c("Lat. Drive - Avg.","Ecc. Force","Knee Ext Accel","Ank. Active Decel","Rel Conc Force"))
  return(accel_decel_1)
}  ### This is page 2 accel decel


get_percentiles_page_2 <- function(playername, date) {
  print("Retrieving Page 2 Percentiles Data")
  percentile_sql <- paste("select * from public.page_2_percentiles where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  full_table <- read_civis(sql(percentile_sql),"P3")
  values <- c("imp_1_avg","imp2lraw","imp2rraw","conc_rel_ff","dropmaxkneeextensionvelocityavg","dropmaxkneeextensionaccelerationavg","load_rel_ff","vertmaxankleplantarflexionaccelerationavg","vertmaxkneeextensionvelocityavg","vertmaxkneeextensionaccelerationavg","vertrelativefreefallforceleft","vertrelativefreefallforceright","lateralforceleftbw","slmaxhipextensionvelocity","slmaxhipabduction","lateralforcerightbw","srmaxhipextensionvelocity","srmaxhipabduction","net_rel_conc_force")
  percentiles <- c("percimp_1_avg","percimp2lraw","percimp2rraw","percconc_rel_ff","percdropmaxkneeextensionvelocityavg","percdropmaxkneeextensionaccelerationavg","percload_rel_ff","percvertmaxankleplantarflexionaccelerationavg","percvertmaxkneeextensionvelocityavg","percvertmaxkneeextensionaccelerationavg","percvertrelativefreefallforceleft","percvertrelativefreefallforceright","perclateralforceleftbw","percslmaxhipextensionvelocity","percslmaxhipabduction","perclateralforcerightbw","percsrmaxhipextensionvelocity","percsrmaxhipabduction","percnet_rel_conc_force")
  label <- c("Net Impact 1","Net Impact 2(L)","Net Impact 2(R)","Conc Rel FF","Knee Ext Velocity","Knee Ext Accel","Load Rel. FF","Ankle Ext Accel","Knee Ext Velocity","Knee Ext Accel","L - Load Rel. FF ","R - Load Rel FF","L - Lateral Drive","L - Hip Ext. Velocity","L - Hip Abduction","R - Lateral Drive","R - Hip Ext. Velocity","R - Hip Abduction","Net Rel. Conc Force")
  percentile_labels <- data.frame(values,percentiles,label)
  athlete_percentiles <- full_table[,3:length(full_table)]
  athlete_percentiles <- data.frame(t(athlete_percentiles))
  athlete_percentiles <- add_rownames(athlete_percentiles,"metric")
  colnames(athlete_percentiles) <- c("metric","value")
  values_frame <- merge(percentile_labels,athlete_percentiles,by.x=c("values"),by.y=c("metric"))
  percentiles_frame <- merge(percentile_labels,athlete_percentiles,by.x=c("percentiles"),by.y=c("metric"))
  
  vars <- c("percconc_rel_ff","percdropmaxkneeextensionaccelerationavg","percdropmaxkneeextensionvelocityavg","percdroppeakconcentricforceleft","percdroppeakconcentricforceright","percimp_1_avg","percimp2lraw","percimp2rraw","perclateralforceleftbw","percslmaxhipabduction","percslmaxhipextensionvelocity","perclateralforcerightbw","percsrmaxhipabduction","percsrmaxhipextensionvelocity","percvertmaxankleplantarflexionaccelerationavg","percvertmaxkneeextensionaccelerationavg","percvertmaxkneeextensionvelocityavg","percvertrelativefreefallforceleft","percvertrelativefreefallforceright","percnet_rel_conc_force","percload_rel_ff")
  tests <- c("Standing Vertical","Drop Jump","Drop Jump","Drop Jump","Drop Jump","Drop Jump","Drop Jump","Drop Jump","1 Off Skater","1 Off Skater","1 Off Skater","1 Off Skater","1 Off Skater","1 Off Skater","Standing Vertical","Standing Vertical","Standing Vertical","Standing Vertical","Standing Vertical","Drop Jump","Standing Vertical")
  var_tests <- data.frame(vars,tests)
  
  final_frame <- merge(values_frame[,3:4],percentiles_frame[,3:4],by="label")
  final_frame <- merge(values_frame,percentiles_frame[,2:4],by="values")
  ff2 <- merge(final_frame,var_tests,by.x="percentiles",by.y="vars")
  ff2 <- ff2[c("label.x","value.x","value.y","tests")]
  colnames(ff2) <- c("metric","value","Percentile","test_type")
  ff2$value <- round(ff2$value,2)
  ff2$metric <- factor(ff2$metric,levels = c("Knee Ext Accel"
                                             ,"Knee Ext Velocity"
                                             ,"Net Impact 1"
                                             ,"Net Impact 2(L)"
                                             ,"Net Impact 2(R)"
                                             ,"Net Rel. Conc Force"
                                             ,"Conc Rel FF"
                                             ,"Load Rel. FF"
                                             ,"Ankle Ext Accel"
                                             ,"L - Lateral Drive"
                                             ,"L - Hip Abduction"
                                             ,"L - Hip Ext. Velocity"
                                             ,"R - Lateral Drive"
                                             ,"R - Hip Abduction"
                                             ,"R - Hip Ext. Velocity"))
  ff2 <- ff2[order(ff2$metric),]
  return(ff2)
}  ### This is page 2 kpis -- dont touch

get_percentiles_page_3 <- function(playername,date) {
  print("Retrieving Page 3 Dot Plot Percentiles")
  percentile_sql <- paste("select * from public.v2_page_3_percentiles
                          where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  full_table <- read_civis(sql(percentile_sql),"P3")
  low_back <- full_table %>%
    select("dropdeltahip_average", "imp2lraw", "imp2rraw", "droptotalmovementimpulseasymmetry")
  low_back <- data.frame(t(low_back))
  low_back <- add_rownames(low_back, "metric")
  colnames(low_back) <- c("metric","value")
  low_back$type <- 'low back'
  low_back$order <- c(1:4)
  low_back$metric <- factor(low_back$metric, levels = low_back$metric[order(low_back$order)])
  
  left_knee <- full_table %>%
    select("dropmaxrelativerotationleft", "translationl", "dropmaxhipactivedecelerationleft", "dropdeltafemoralrotationleft")
  left_knee <- data.frame(t(left_knee))
  left_knee <- add_rownames(left_knee,"metric")
  colnames(left_knee) <- c("metric","value")
  left_knee$type <- 'left knee'
  left_knee$order <- c(1:4)
  left_knee$metric <- factor(left_knee$metric, levels = left_knee$metric[order(left_knee$order)])
  
  
  right_knee <-full_table %>%
    select("dropmaxrelativerotationright", "translationr", "dropmaxhipactivedecelerationright", "dropdeltafemoralrotationright")
  right_knee <- data.frame(t(right_knee))
  right_knee <- add_rownames(right_knee,"metric")
  colnames(right_knee) <- c("metric","value")
  right_knee$type <- 'right knee'
  right_knee$order <- c(1:4)
  right_knee$metric <- factor(right_knee$metric, levels = right_knee$metric[order(right_knee$order)])
  
  left_foot <- full_table %>% 
    select("inversionl", "translationl", "dropankleactivedecelerationleft", "dropankleflexionatt0left")
  left_foot <- data.frame(t(left_foot))
  left_foot <- add_rownames(left_foot,"metric")
  colnames(left_foot) <- c("metric","value")
  left_foot$type <- 'left foot'
  left_foot$order <- c(1:4)
  left_foot$metric <- factor(left_foot$metric, levels = left_foot$metric[order(left_foot$order)])
  
  
  right_foot <- full_table %>%
    select("inversionr", "translationr", "dropankleactivedecelerationright", "dropankleflexionatt0right")
  right_foot <- data.frame(t(right_foot))
  right_foot <- add_rownames(right_foot,"metric")
  colnames(right_foot) <- c("metric","value")
  right_foot$type <- 'right foot'
  right_foot$order <- c(1:4)
  right_foot$metric <- factor(right_foot$metric, levels = right_foot$metric[order(right_foot$order)])
  
  percent_frame <- bind_rows(low_back,left_knee, left_foot, right_knee, right_foot)
  colnames(percent_frame) <- c("metric","percentile","type","order")
  percent_frame$pos <- FALSE
  percent_frame$percentile <- percent_frame$percentile
  over_50 <- function(x) {
    ifelse(is.na(x),FALSE,x>50)
  }
  percent_frame$pos <- as.character(lapply(percent_frame$percentile, function(x) over_50(x)))
  
  metric <- c("dropdeltahip_average"
              ,"imp2lraw"
              ,"imp2rraw"
              ,"droptotalmovementimpulseasymmetry"
              ,"dropmaxrelativerotationleft"
              ,"dropdeltarelativerotationleft"
              ,"translationl"
              ,"dropankleactivedecelerationleft"
              ,"droptotalmovementimpulseasymmetry"
              ,"dropmaxrelativerotationright"
              ,"dropdeltarelativerotationright"
              ,"translationr"
              ,"dropankleactivedecelerationright"
              ,"dropankleflexionatt0left"
              ,"inversionl"
              ,"translationl"
              ,"dropankleactivedecelerationleft"
              ,"dropankleflexionatt0right"
              ,"inversionr"
              ,"translationr"
              ,"dropankleactivedecelerationright"
              ,"droptotalmovementimpulseasymmetry"
              ,"dropmaxhipactivedecelerationleft"
              ,"dropmaxhipactivedecelerationright"
              ,"dropdeltafemoralrotationright"
              ,"dropdeltafemoralrotationleft")
  
  label <- c("Delta Hip Flex."
             ,"Impact 2 (L)"
             ,"Impact 2 (R)"
             ,"Impulse Asym"
             ,"Rel. Rotation"
             ,"Delta Rel. Rot."
             ,"Translation"
             ,"Ankle Act Decel"
             ,"Impulse Asym"
             ,"Rel. Rotation"
             ,"Delta Rel. Rot."
             ,"Translation"
             ,"Ankle Act Decel"
             ,"Active Dorsi."
             ,"Inversion"
             ,"Translation"
             ,"Ankle Act Decel"
             ,"Active Dorsi."
             ,"Inversion"
             ,"Translation"
             ,"Ankle Act Decel"
             ,"Impulse Asym"
             ,"Hip Decel."
             ,"Hip Decel."
             ,"Femoral Rotation"
             ,"Femoral Rotation")
  
  column_labels <- unique(data.frame(metric,label))
  percent_merged_frame <- merge(percent_frame,column_labels,by="metric")
  return(percent_merged_frame)
  
}  ### This is page 3 

get_fig_page_one <- function(playername,date){
  being_img <-
    rasterGrob(readPNG("p3_black_wireman.png"))
  print("Retrieving Flag Diagram Data")
  color_sql <- paste("select * from public.v2_page_3_percentiles where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
  color_frame <- read_civis(sql(color_sql),"P3")[,c(1,2,24:33)]
  df <- data.frame(
    # R Ankle, R Knee, L Ankle, L Knee, M Back
    x = c(-0.15, -0.13, 0.14,  0.10, -0.04),
    y = c(-.81, -0.38, -.82, -0.39, 0.16),
    color = c(as.character(color_frame[,"flag_rightankle"]),as.character(color_frame[,"flag_rightknee"]),as.character(color_frame[,"flag_leftankle"]),as.character(color_frame[,"flag_leftknee"]),as.character(color_frame[,"flag_back"]))
  )
  color_map <- c("red"=dkred,"green"=green,"yellow"=yellow)
  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    geom_point(data = df,
               aes(x, y, size = 28, color = color,fill=color),
               alpha = .4,stroke=1,shape=21,show_guide=FALSE) +
    #geom_text(aes(x = -.51, y=-.91), color = dkgrey, label = paste0('Right Foot: ', round(color_frame$rightankle,0)), size = 3 ) +
    #geom_text(aes(x = -.49, y=-.17), color = dkgrey, label = paste0('Right Knee: ', round(color_frame$rightknee,0)), size =3 ) +
    #geom_text(aes(x = .57, y=-.93), color = dkgrey, label = paste0('Left Foot: ', round(color_frame$leftankle,0)), size = 3 ) +
    #geom_text(aes(x = .51, y=-.17), color = dkgrey, label = paste0('Left Knee: ', round(color_frame$leftknee,0)), size = 3 ) +
    #geom_text(aes(x = -.51, y= .64), color = dkgrey, label = paste0('Lower Back: ', round(color_frame$lowback,0)), size =3 ) +
    scale_colour_manual(values = color_map) +
    scale_fill_manual(values=color_map) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(8,12)) +
    labs(title ="Injury Risk Stratification", subtitle = "Injury risk factor by location")+
    theme_p3_fig_two() #+
    #theme(panel.background = element_rect(colour = "black", fill = NA, size =.5))
}
                                           
get_fig_page_three <- function(playername,date){
 print("drawing page 3 image")
  being_img <-
    rasterGrob(readPNG("p3_black_wireman.png"))
  print("Retrieving Flag Diagram Data")
  color_sql <- paste("select * from public.v2_page_3_percentiles where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
  color_frame <- read_civis(sql(color_sql),"P3")[,c(1,2,24:33)]
  df <- data.frame(
    # R Ankle, R Knee, L Ankle, L Knee, M Back
    x = c(-0.13, -0.13, 0.11,  0.09, -0.03),
    y = c(-0.8, -0.45, -0.81, -0.45, 0.13),
    color = c(as.character(color_frame[,"flag_rightankle"]),as.character(color_frame[,"flag_rightknee"]),as.character(color_frame[,"flag_leftankle"]),as.character(color_frame[,"flag_leftknee"]),as.character(color_frame[,"flag_back"]))
  )
  color_map <- c("red"=dkred,"green"=green,"yellow"=yellow)
  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    geom_point(data = df,
               aes(x, y, size = 28, color = color,fill=color),
               alpha = .4,stroke=1,shape=21,show_guide=FALSE) +
    labs(title="Injury Risk Stratification", subtitle = "Injury risk factors by location") +
    #geom_text(aes(x = -.47, y=-.91),color = dkgrey, label = paste0('Right Foot: ', round(color_frame$rightankle,0)), size = 3 ) +
    #geom_text(aes(x = -.45, y=-.18),color = dkgrey, label = paste0('Right Knee: ', round(color_frame$rightknee,0)), size =3 ) +
    #geom_text(aes(x = .49, y=-.93),color = dkgrey, label = paste0('Left Foot: ', round(color_frame$leftankle,0)), size = 3 ) +
    #geom_text(aes(x = .49, y=-.18),color = dkgrey, label = paste0('Left Knee: ', round(color_frame$leftknee,0)), size = 3 ) +
    #geom_text(aes(x = -.47, y= .64),color = dkgrey, label = paste0('Lower Back: ', round(color_frame$lowback,0)), size =3 ) +
    scale_colour_manual(values = color_map) +
    scale_fill_manual(values=color_map) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(8,12)) +
    theme_p3_fig_two()
}
                                           

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

acceleration_bars <- function(df,subtitle) {
  ggplot(df,aes(x=metric,y=value,fill=metric)) +
    geom_bar(stat = 'identity') +
    labs(title="Deceleration / Acceleration",
         subtitle=subtitle) +
    scale_fill_manual("legend",values = c(dkred,dkgrey,dkred,dkgrey,dkred)) +
    coord_flip() +
    theme_p3() +
    ylim(-100,100)
}


main_page_1_dot_plot <-  function(title='Overall Performance Factors') {
  vert_lat_table <- 'public.v2_scatter_model_data'
  accel_decel_table <- 'public.v2_page_2_accel_decel'

  ### Vert_Lat
  vl_df <- read_civis(vert_lat_table, database = "P3") %>%
    filter(name == playername) %>%
    filter(date == assessmentdate) %>%
    select(-position)
  ### Accel Decel
  ad_df <- read_civis(accel_decel_table, database = "P3") %>%
    filter(name == playername) %>%
    filter(date == assessmentdate) %>%
    select(name, assessmentdate, accel_total, decel_total)
  
  overall <- full_join(vl_df, ad_df, by = c("name", "assessmentdate")) %>%
    rename("Vertical" = predicted,"Lateral" = average_lateralforcebw,  "Acceleration" = accel_total, "Deceleration" = decel_total) %>%
    melt() %>%
    select(variable, value) %>%
    rename("label" = variable, "percentile" = value) %>%
    mutate(pos = ifelse(percentile >= 50, TRUE, FALSE))
  
  overall$label <-   factor(overall$label, levels = levels(overall$label)[c(4,3,1,2)])
  
  
  plot <- ggplot(overall, aes(x=label, y=percentile)) +
    geom_point(aes(col = pos, fill = pos), size=4, pch=21) +   # Draw points
    geom_segment(aes(x=label,
                     xend=label,
                     y=0,
                     yend=100),
                 linetype="dashed",
                 size=0.1) +   # Draw dashed lines
    geom_hline(yintercept=50, size = .5) +
    labs(title=title,
         subtitle="NBA Percentile Rank") +
    scale_fill_manual(values=c("FALSE"=ltgrey,"TRUE"=dkgrey)) +
    scale_color_manual(values=c("FALSE"=black,"TRUE"=black)) +
    theme_p3() +
    coord_flip() +
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          #plot.title = element_text(vjust=20),
          #plot.subtitle = element_text(vjust=20)
    )
  
  return(plot)
}

dot_plot <-  function(overall, type = 'lateral',title='Graph') {
  df <- overall %>% filter_(paste0('type == "', type, '"'))
  ggplot(df, aes(x=label, y=percentile)) +
    geom_point(aes(col=pos, fill=pos), size=4, pch=21) +   # Draw points
    geom_segment(aes(x=label,
                     xend=label,
                     y=0,
                     yend=100),
                 linetype="dashed",
                 size=0.1) +   # Draw dashed lines
    geom_hline(yintercept=50, size = .5) +
    labs(title=title,
         subtitle="NBA Percentile Rank") +
    scale_fill_manual(values=c("FALSE"=ltgrey,"TRUE"=dkgrey)) +
    scale_color_manual(values=c("FALSE"=black,"TRUE"=black)) +
    theme_p3() +
    coord_flip() +
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
       #   plot.title = element_text(vjust=20),
       #   plot.subtitle = element_text(vjust=20)
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
         subtitle="NBA Percentile Rank") +
    scale_colour_manual(values=c(dkred, dkgrey)) +
    theme_p3() +
    coord_flip() +
    scale_x_discrete(position="top") +
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
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
                                           
