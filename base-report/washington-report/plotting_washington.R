library(ggplot2)
library(ggrepel)
library(dplyr)
library(extrafont)
library(reshape2)
library(png)

loadfonts()

purple <<- '#4b2e83'
ltred <<- '#e0474c'
blue <<- '#7acfd6'
ltgrey <<- '#F0F0F0'
midgrey <<- '#b4b4b4'
middkgrey <<- '#969696'
gold <<- '#85754d'
white <<- '#ffffff'
yellow <<- "#FFFF66"
green <<- "#228b22"
pal <<- c(purple, ltred, blue, ltgrey, gold)


athlete_table <- 'public.page_1_bio_info'
get_stats <- function(playername, date) {
  print("Retrieving Bio Info")
  query <- paste("select name as playername, assessmentdate as date, height as playerht, bodyweightkg as playerwt, reach as playerrch from "
        ,athlete_table, " where \"name\" = '",playername,
                 "' and assessmentdate = '",date, "'",sep="")
  stats_df <- read_civis(sql(query),'P3')
  colnames(stats_df) <- c("Name","Date","Height","Weight","Reach")
  return(stats_df)
}


get_athl_cluster_data <- function(playername,date) {
  print("Retrieving Athleticism Cluster Data")
  cluster_athlete_query <- paste("select cluster
    ,avg(imp_1_avg) as imp_1_avg
    ,avg(conc_raw_avg) as conc_raw_avg
    ,avg(dropmaxkneeextensionvelocityavg) as dropmaxkneeextensionvelocityavg
    ,avg(ecc_rel_ff) as ecc_rel_ff
    ,avg(conc_rel_ff) as conc_rel_ff
    ,avg(vertmaxkneeextensionvelocityavg) as vertmaxkneeextensionvelocityavg
    ,avg(slaveragelateralrfd) as slaveragelateralrfd
    ,avg(slmaxhipextensionvelocity) as slmaxhipextensionvelocity
    ,avg(slmaxhipabduction) as slmaxhipabduction
    ,avg(sraveragelateralrfd) as sraveragelateralrfd
    ,avg(srmaxhipextensionvelocity) as srmaxhipextensionvelocity
    ,avg(srmaxhipabduction) as srmaxhipabduction
    from public.spider_plot_data
    where name = '",playername,"' and assessmentdate = '",date,"' group by 1",sep="")

  cluster_athlete <- read_civis(sql(cluster_athlete_query),"P3")
  cluster_athlete$cluster
  cluster_avg_sql <- paste("select avg(imp_1_avg) as imp_1_avg
      ,avg(conc_raw_avg) as conc_raw_avg
      ,avg(dropmaxkneeextensionvelocityavg) as dropmaxkneeextensionvelocityavg
      ,avg(ecc_rel_ff) as ecc_rel_ff
      ,avg(conc_rel_ff) as conc_rel_ff
      ,avg(vertmaxkneeextensionvelocityavg) as vertmaxkneeextensionvelocityavg
      ,avg(slaveragelateralrfd) as slaveragelateralrfd
      ,avg(slmaxhipextensionvelocity) as slmaxhipextensionvelocity
      ,avg(slmaxhipabduction) as slmaxhipabduction
      ,avg(sraveragelateralrfd) as sraveragelateralrfd
      ,avg(srmaxhipextensionvelocity) as srmaxhipextensionvelocity
      ,avg(srmaxhipabduction) as srmaxhipabduction
    from public.spider_plot_data
    where cluster = ",cluster_athlete$cluster,sep="")

  cluster_avg <- read_civis(sql(cluster_avg_sql),"P3")
  cluster_athlete_compare <- cluster_athlete[,-1]

  t_athl_compare <- data.frame(t(cluster_athlete_compare))
  t_athl_compare <- add_rownames(t_athl_compare,"metric")
  colnames(t_athl_compare) <- c("metric","score")
  t_athl_compare$cluster <- 'athlete'
  t_athl_compare <- t_athl_compare[order(t_athl_compare$metric),]
  t_cluster_avg <- data.frame(t(cluster_avg))
  t_cluster_avg <- add_rownames(t_cluster_avg,"metric")
  colnames(t_cluster_avg) <- c("metric","score")
  t_cluster_avg$cluster <- 'cluster'
  t_cluster_avg <- t_cluster_avg[order(t_cluster_avg$metric),]
  cluster_radar <- bind_rows(t_athl_compare,t_cluster_avg)

  metric <- c("imp_1_avg"
              ,"dropmaxkneeextensionvelocityavg"
              ,"conc_raw_avg"
              ,"ecc_rel_ff"
              ,"conc_rel_ff"
              ,"vertmaxkneeextensionvelocityavg"
              ,"slaveragelateralrfd"
              ,"slmaxhipextensionvelocity"
              ,"slmaxhipabduction"
              ,"sraveragelateralrfd"
              ,"srmaxhipextensionvelocity"
              ,"srmaxhipabduction")

  label <- c("Net Impact 1"
             ,"Drop Knee Ext. Velocity"
             ,"Concentric Force"
             ,"Ecc Rel FF"
             ,"Conc Rel FF"
             ,"Vert Knee Ext. Velocity"
             ,"Lateral Drive (L)"
             ,"Hip Extension Velocity (L)"
             ,"Peak Hip Abduction (L)"
             ,"Lateral Drive (R)"
             ,"Hip Extension Velocity (R)"
             ,"Peak Hip Abduction (R)")

  cl <- data.frame(metric,label)
  cluster_radar <- merge(cluster_radar,cl,by="metric")
  cluster_radar$metric <- cluster_radar$label
  cluster_radar$metric <- factor(cluster_radar$metric,levels = c("Vert Knee Ext. Velocity"
                                                                 ,"Conc Rel FF"
                                                                 ,"Ecc Rel FF"
                                                                 ,"Lateral Drive (R)"
                                                                 ,"Hip Extension Velocity (R)"
                                                                 ,"Peak Hip Abduction (R)"
                                                                 ,"Net Impact 1"
                                                                 ,"Concentric Force"
                                                                 ,"Drop Knee Ext. Velocity"
                                                                 ,"Peak Hip Abduction (L)"
                                                                 ,"Hip Extension Velocity (L)"
                                                                 ,"Lateral Drive (L)"))
  cluster_radar <- cluster_radar[order(cluster_radar$metric),]
  return(cluster_radar)

}

get_mech_cluster_data <- function(playername,date) {
  print("Retrieving Mechanics Cluster Data")
  cluster_athlete_query <- paste("select cluster
                                 ,avg(imp2lraw) as imp2lraw
                                 ,avg(imp2lraw) as imp2rraw
                                 ,avg(inversionl) as inversionl
                                 ,avg(inversionr) as inversionr
                                 ,avg(eversionl) as eversionl
                                 ,avg(translationl) as translationl
                                 ,avg(translationr) as translationr
                                 ,avg(dropankleactivedecelerationleft) as dropankleactivedecelerationleft
                                 ,avg(dropankleactivedecelerationright) as dropankleactivedecelerationright
                                 ,avg(droptibialrotationatmaxrelativerotationleft) as dropmaxrelativerotationleft
                                 ,avg(droptibialrotationatmaxrelativerotationright) as dropmaxrelativerotationright
                                 ,avg(droptotalmovementimpulseasymmetry) as droptotalmovementimpulseasymmetry
                                 ,avg(dropdeltahip_average) as dropdeltahip_average
                                 from public.page_3_mech_spider
                                 where name = '",playername,"' and assessmentdate = '",date,"' group by 1",sep="")

  cluster_athlete <- read_civis(sql(cluster_athlete_query),"P3")
  cluster_athlete$cluster
  cluster_avg_sql <- paste("select avg(imp2lraw) as imp2lraw
                           ,avg(imp2lraw) as imp2rraw
                           ,avg(inversionl) as inversionl
                           ,avg(inversionr) as inversionr
                           ,avg(eversionl) as eversionl
                           ,avg(translationl) as translationl
                           ,avg(translationr) as translationr
                           ,avg(dropankleactivedecelerationleft) as dropankleactivedecelerationleft
                           ,avg(dropankleactivedecelerationright) as dropankleactivedecelerationright
                           ,avg(dropmaxrelativerotationleft) as dropmaxrelativerotationleft
                           ,avg(dropmaxrelativerotationright) as dropmaxrelativerotationright
                           ,avg(droptotalmovementimpulseasymmetry) as droptotalmovementimpulseasymmetry
                           ,avg(dropdeltahip_average) as dropdeltahip_average
                           from public.page_3_mech_spider
                           where cluster = ",cluster_athlete$cluster,sep="")

  cluster_avg <- read_civis(sql(cluster_avg_sql),"P3")
  cluster_athlete_compare <- cluster_athlete[,-1]

  t_athl_compare <- data.frame(t(cluster_athlete_compare))
  t_athl_compare <- add_rownames(t_athl_compare,"metric")
  colnames(t_athl_compare) <- c("metric","score")
  t_athl_compare$cluster <- 'athlete'
  t_athl_compare <- t_athl_compare[order(t_athl_compare$metric),]
  t_cluster_avg <- data.frame(t(cluster_avg))
  t_cluster_avg <- add_rownames(t_cluster_avg,"metric")
  colnames(t_cluster_avg) <- c("metric","score")
  t_cluster_avg$cluster <- 'cluster'
  t_cluster_avg <- t_cluster_avg[order(t_cluster_avg$metric),]
  cluster_radar <- bind_rows(t_athl_compare,t_cluster_avg)

  metric <- c("imp2lraw"
              ,"imp2rraw"
              ,"inversionl"
              ,"inversionr"
              ,"eversionl"
              ,"eversionr"
              ,"translationl"
              ,"translationr"
              ,"dropankleactivedecelerationleft"
              ,"dropankleactivedecelerationright"
              ,"dropmaxrelativerotationleft"
              ,"dropmaxrelativerotationright"
              ,"droptotalmovementimpulseasymmetry"
              ,"dropdeltahip_average")

  label <- c("Net Imp 2 (L)"
             ,"Net Imp 2 (R)"
             ,"Inversion (L)"
             ,"Inversion (R)"
             ,"Eversion (L)"
             ,"Eversion (R)"
             ,"Translation (L)"
             ,"Translation (R)"
             ,"Drop Ank. Decel (L)"
             ,"Drop Ank. Decel (R)"
             ,"Drop Rel. Rot. (L)"
             ,"Drop Rel Rot. (R)"
             ,"Tot. Impulse Asym"
             ,"Drop Delta Hip Avg.")

  cl <- data.frame(metric,label)
  cluster_radar <- merge(cluster_radar,cl,by="metric")
  cluster_radar$metric <- cluster_radar$label
  cluster_radar$metric <- factor(cluster_radar$metric,levels = c("Drop Delta Hip Avg."
                                                                 ,"Tot. Impulse Asym"
                                                                 ,"Net Imp 2 (R)"
                                                                 ,"Translation (R)"
                                                                 ,"Drop Rel Rot. (R)"
                                                                 ,"Inversion (R)"
                                                                 ,"Eversion (R)"
                                                                 ,"Drop Ank. Decel (R)"
                                                                 ,"Net Imp 2 (L)"
                                                                 ,"Translation (L)"
                                                                 ,"Drop Rel. Rot. (L)"
                                                                 ,"Inversion (L)"
                                                                 ,"Eversion (L)"
                                                                 ,"Drop Ank. Decel (L)"))

  cluster_radar <- cluster_radar[order(cluster_radar$metric),]
  return(cluster_radar)

}

graph_page_2_2x2 <- function(playername, assessmentdate){
  print("Retrieving 2x2 Scatter Cluster Data")
  ### Extract the player's position to allow for comparison and to allow for dynamic graph labeling (used below)
  data <- read_civis('public.page2_2x2','P3')
  players_position <- data %>%
    filter(name == as.character(playername)) %>%
    select(position) %>%
    pull("position")


  graph <- ggplot(data, aes(x = xaxis, y = yaxis, label=playername)) +
    geom_vline(xintercept = mean(data$xaxis)) + ### position mean, x
    geom_hline(yintercept = mean(data$yaxis)) + ### position mean, y
    geom_point(aes(fill=position),size=7, pch =21, alpha=0.5) +  ### overall points
    geom_point(data = data[data[,1] == playername & data[,2] == assessmentdate , ], size = 12, fill = "black", pch=21, show.legend = FALSE) +  ## point for target athlete
    geom_text(data = data[data[,1] == playername & data[,2] == assessmentdate , ]
              , aes(xaxis,yaxis,label = paste(substr(word(name),1,1),substr(word(name,2),1,1),sep="")), size = 5, fontface="bold",color="white") +  ## text for target athlete
    xlab("Lateral Acceleration") +
    ylab("Vertical Acceleration") +
    labs(title="Vertical-Lateral Kinematics",
         subtitle="Axes represent average for all P3 tested athletes\nFill color indicates position") +
    theme_minimal() +
    theme_p3() +
    theme(#axis.text = element_text(size=10, face = "bold"),
          axis.title.x = element_text(size=11,face="bold"),
          axis.title.y = element_text(size=11,face="bold",angle=90,vjust = 10),
          legend.position = 'bottom',
          legend.key = element_rect(linetype = "blank"),
          legend.key.size = unit(1,"line"))  +
    scale_fill_manual(values=c(white, purple, gold)) +
    guides(fill=guide_legend("Position"))


  return(graph)

}


get_kpis <- function(playername, date) {
  print("Retrieving Page 1 Percentiles")
  std_sql <- paste("select * from public.page_1_summary_percentiles where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
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
}

get_accel_decel_1 <- function(playername,date) {
  print("Retrieving Page 1 Accel Decel Data")
  accel_decel_sql <- paste("select * from public.page_1_accel_decel
                           where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  accel_decel <- read_civis(sql(accel_decel_sql),"P3")
  accel_decel_1 <- accel_decel[,3:4]
  colnames(accel_decel_1) <- c("Rel Conc. Force","Rel. Ecc Force")
  accel_decel_1 <- data.frame(t(accel_decel_1))
  accel_decel_1 <- add_rownames(accel_decel_1,"metric")
  colnames(accel_decel_1) <- c("metric","value")
  accel_decel_1$metric <- factor(accel_decel_1$metric)
  return(accel_decel_1)
}

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
}

get_history <- function(playername,date) {
  print("Retrieving Performance Summary Data")
  hist_sql <- paste("select * from public.page_1_player_history
                           where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  hist <- read_civis(sql(hist_sql),"P3")
  hist <- hist[,3:6]
  hist <- data.frame(t(hist))
  metric <- factor(c("delta_vert",
              "delta_drop",
              "lateralforceleftbw",
              "lateralforcerightbw"))
  label <- factor(c("Standing Vert",
              "Drop Jump",
              "Lateral Drive (L)",
              "Lateral Drive (R)"))
  hist <- add_rownames(hist,"metric")

    colnames(hist) <- c("metric","current")
    labelframe <- data.frame(metric,label)
    hist <- merge(hist,labelframe,by="metric")
    hist <- hist[,2:3]
  
  hist$label <- factor(hist$label,levels=label)
  hist <- melt(hist,id='label')
  colnames(hist) <- c("label","type","percentile")
  over_50 <- function(x) {
    ifelse(is.na(x),FALSE,x>50)
  }
  hist$pos <- as.character(lapply(hist$percentile, function(x) over_50(x)))
  return(hist)
}

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
}

get_percentiles_page_3 <- function(playername,date) {
  print("Retrieving Page 3 Dot Plot Percentiles")
  percentile_sql <- paste("select * from public.page_3_percentiles
   where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
  full_table <- read_civis(sql(percentile_sql),"P3")
  low_back <- full_table[,c("dropdeltahip_average"
    , "imp2lraw"
    , "imp2rraw"
    , "droptotalmovementimpulseasymmetry")]
  low_back <- data.frame(t(low_back))
  low_back <- add_rownames(low_back, "metric")
  colnames(low_back) <- c("metric","value")
  low_back$type <- 'low back'
  low_back$order <- c(1:4)
  low_back$metric <- factor(low_back$metric, levels = low_back$metric[order(low_back$order)])

  left_knee <- full_table[,c("dropmaxrelativerotationleft"
    , "droptibialrotationatmaxrelativerotationleft"
    , "translationl"
    , "dropankleactivedecelerationleft"
    , "droptotalmovementimpulseasymmetry")]
  left_knee <- data.frame(t(left_knee))
  left_knee <- add_rownames(left_knee,"metric")
  colnames(left_knee) <- c("metric","value")
  left_knee$type <- 'left knee'
  left_knee$order <- c(1:5)
  left_knee$metric <- factor(left_knee$metric, levels = left_knee$metric[order(left_knee$order)])


  right_knee <- full_table[,c("dropmaxrelativerotationright"
    , "droptibialrotationatmaxrelativerotationright"
    , "translationr"
    , "dropankleactivedecelerationright"
    , "droptotalmovementimpulseasymmetry")]
  right_knee <- data.frame(t(right_knee))
  right_knee <- add_rownames(right_knee,"metric")
  colnames(right_knee) <- c("metric","value")
  right_knee$type <- 'right knee'
  right_knee$order <- c(1:5)
  right_knee$metric <- factor(right_knee$metric, levels = right_knee$metric[order(right_knee$order)])

  left_foot <- full_table[,c("inversionl"
    , "eversionl"
    , "dropankleactivedecelerationleft"
    , "droptotalmovementimpulseasymmetry")]
  left_foot <- data.frame(t(left_foot))
  left_foot <- add_rownames(left_foot,"metric")
  colnames(left_foot) <- c("metric","value")
  left_foot$type <- 'left foot'
  left_foot$order <- c(1:4)
  left_foot$metric <- factor(left_foot$metric, levels = left_foot$metric[order(left_foot$order)])


  right_foot <- full_table[,c("inversionr"
    , "eversionr"
    , "dropankleactivedecelerationright"
    , "droptotalmovementimpulseasymmetry")]
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
              ,"droptotalmovementimpulseasymmetry"
              ,"inversionl"
              ,"eversionl"
              ,"dropankleactivedecelerationleft"
              ,"droptotalmovementimpulseasymmetry"
              ,"inversionr"
              ,"eversionr"
              ,"dropankleactivedecelerationright"
              ,"droptotalmovementimpulseasymmetry")

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
             ,"Impulse Asym"
             ,"Inversion"
             ,"Eversion"
             ,"Ankle Act Decel"
             ,"Impulse Asym"
             ,"Inversion"
             ,"Eversion"
             ,"Ankle Act Decel"
             ,"Impulse Asym")

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
      fill = gold,
      size = 10
    )  +
    scale_colour_manual(values=c("FALSE"=purple,"TRUE"=gold)) +
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
  being_img <-
    rasterGrob(readPNG("p3 man.png"))
  print("Retrieving Flag Diagram Data")
  color_sql <- paste("select * from public.page_1_and_3_flags where name = '",playername,"'and assessmentdate = '",date,"'",sep="")
  color_frame <- read_civis(sql(color_sql),"P3")
  df <- data.frame(
    # L Ankle, L Knee, R Ankle, R Knee, M Back
    x = c(-0.22, -0.23, 0.05,  0.02, -0.13),
    y = c(-0.73, -0.41, -0.81, -0.45, 0.15),
    color = c(as.character(color_frame[,"leftankle_flag"]),as.character(color_frame[,"leftknee_flag"]),as.character(color_frame[,"rightankle_flag"]),as.character(color_frame[,"rightknee_flag"]),as.character(color_frame[,"lowback_flag"]))
  )
  color_map <- c("red"=purple,"green"=green,"yellow"=yellow)
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
    theme_p3_fig()
}

get_logo <- function(){
  being_img <-
    rasterGrob(readPNG("P3 logo.png"))

  fig <- ggplot() +
    annotation_custom(being_img, -1, 1, -1, 1) +
    xlim(-.25, .25) +
    ylim(-1, 1) +
    scale_colour_manual(values = c(purple, blue)) +
    guides(colour = FALSE, size = FALSE) +
    scale_size(range = c(5, 8)) +
    theme_p3_fig()
}

history_plot <- function(history) {
  ggplot(history, aes(x=label,y=value,group=period,color=period)) +
    geom_point(aes(col=period), size=4) +
    #geom_line(size=2) +
    scale_color_manual(values=c(purple,gold),labels=c("Current","Previous")) +
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
    scale_fill_manual("legend",values = c(purple,gold,purple,gold,purple)) +
    coord_flip() +
    theme_p3() +
    ylim(-100,100)
}

radar_plot <- function(df.rad) {
  radar_plot <- ggplot(df.rad, aes(x = metric, y = score/100, color = cluster, group = cluster)) +
    geom_polygon(aes(color = cluster, fill = cluster), alpha = .2) + geom_point(aes(color = cluster)) +
    coord_polar(start=-pi/4) +
    theme_p3() +
    scale_color_manual(values=c(gold, purple), guide = FALSE) +
    scale_fill_manual(values=c(gold, purple), labels = c("Athlete", 'Cluster'), name = NULL) +
    labs(title="Performance Factor Comparison",
         subtitle="Athlete Relative to Cluster") +
    scale_y_continuous(limits = c(0,1.20),expand=c(0,0.0)) +
    geom_text(aes(x=metric, y=1.20,
                  label=str_wrap(metric,width=10)),
              color=gold,size=2) +
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
    scale_colour_manual(values=c("FALSE"=purple,"TRUE"=gold)) +
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
    scale_colour_manual(values=c(purple, gold)) +
    theme_p3() +
    coord_flip() +
    scale_x_discrete(position="top") +
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
}
