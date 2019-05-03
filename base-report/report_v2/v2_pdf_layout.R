library(ggplot2)
library(dplyr)
library(civis)
library(extrafont)
library(gridExtra)
library(grid)
library(cowplot)
library(stringr)
library(grImport)
library(RGraphics)
source('report_v2/v2_themes.R')
source('report_v2/v2_draw_objects.R')
source('report_v2/v2_plotting.R')


extrafont::font_import(prompt=FALSE,pattern='Abel')

loadfonts()

dkred <- '#b11a21'
ltred <- '#e0474c'
blue <- '#656565'
ltgrey <- '#F0F0F0'
dkgrey <- '#656565'
white <- '#ffffff'
pal <- c(dkred, ltred, blue, ltgrey, dkgrey)

#####################
## REPORT INPUTS  ##
###################

output <- 'example_p3_report.pdf'
pagetitle <- 'P3 ASSESSMENT SUMMARY'
playername <- Sys.getenv("PLAYER_NAME")
date = Sys.getenv("ASSESSMENT_DATE")
print(paste('Report for ',playername))
print(paste('Assessment Date',date))

## Bring in the clustering
source('report_v2/v2_production_clustering.R')


accel_subtitle_1 <- "The graph below provides a brief snapshot of the athlete's \nacceleration and deceleration capabilities"
accel_subtitle_2 <- "The graph below contains a series of metrics related to the \nathlete's acceleration and deceleration capabilities"

intro_page_one <- paste(playername, "was taken through an assessment by P3 on",date, "to capture his biomechanical profile.  The report contains metrics that have been derived from P3's dataset of over 600 NBA players.  For additional information please reach out to P3 at any time.",sep =" ")
intro_page_two <- paste("This page provides a detailed breakdown of the athlete's movement output from a vertical, lateral, acceleration, and deceleration persepective.  Additionally, we show which movement archetype the athletes falls into based on P3's database and research.")
intro_page_three <- paste("This page examines the athlete's movement efficiency with relation to key injury risk factors identified by P3's kinematic research.  Each area of the body is scored for injury risk based on how the athlete tests in the top four metrics for that area.")

page_2_detail <- cluster_text %>%
  filter(Cluster == as.character(athlete_prediction)) %>%
  pull(Description)
training_recs <- Sys.getenv("TRAINING_RECS")

################
## LOAD DATA  ##
################
bio_stats_df <- get_table_stats(playername, date)
#performance_stats_df <- get_table_stats(playername, date)[[2]]
kpis <- get_kpis(playername, date)
ad2 <- get_accel_decel_2(playername,date)
percentiles_page2 <- get_percentiles_page_2(playername,date)
percentiles_page3 <- get_percentiles_page_3(playername,date)

athl_score_sql <- paste("select * from public.v2_athl_score where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
athletecism_score <- round(read_civis(sql(athl_score_sql),"P3")$scaled_athl_score)
mech_score_sql <- paste("select * from public.v2_mech_score where name = '",playername,"' and assessmentdate = '",date,"'",sep="")
mechanics_score <- round(read_civis(sql(mech_score_sql),"P3")$scaled_mech_score)

################
## GET PLOTS ##
################
dot_plot6 <- dot_plot(kpis, type = 'vertical', title='Vertical Performance Factors')
dot_plot7 <- dot_plot(kpis, type = 'lateral', title='Lateral Performance Factors')
fig_one <- get_fig_page_one(playername,date)
fig_three <- get_fig_page_three(playername,date)
accel_plot2 <- acceleration_bars(ad2 %>% arrange(desc(metric)),accel_subtitle_2)
cluster_scatter <- graph_page_2_2x2(playername,date)
dot_plot1 <- dot_plot(percentiles_page3, type = 'low back', title='Low Back Mechanics')
dot_plot2 <- dot_plot(percentiles_page3, type = 'left knee', title='Left Knee Mechanics')
dot_plot3 <- dot_plot(percentiles_page3, type = 'right knee', title='Right Knee Mechanics')
dot_plot4 <- dot_plot(percentiles_page3, type = 'left foot', title='Left Foot Mechanics')
dot_plot5 <- dot_plot(percentiles_page3, type = 'right foot', title='Right Foot Mechanics')


################
## PAGE SETUP ##
################

width = 8.5 # page width in inches
height = 11 # page height in inches
ncols = 24 # number of cols for content
nrows = 25 # number of rows for content
title_height = 1.5 # height in inches of title area
margins = c(.75,.4,.75,.75) # top, right, bottom, left margin

grid <- get_grid(width, height, ncols, nrows, title_height, margins)

pdf(
  output,
  family = "Abel",
  width = width,
  height = height
)

###########
## PAGE 1##
###########
newpage(grid)

## Page header: date
print(drawtext(paste0('Assessment Date: ', date)), vp = vplayout(1, 3:25))

## ROW 2: page title
print(drawtext(intro_page_one, pagetitle, header = TRUE), vp = vplayout(2, 3:20))
print(get_logo(), vp = vplayout(2, 21:23))

## ROW 3/4: summary text and scores
print(drawtable_pageone(bio_stats_df), vp = vplayout(3:6, 6:20))
#print(drawtable(performance_stats_df), vp = vplayout(3:9, 13:24)) #this is directionally right, placement wise 
print(drawscore(athletecism_score, 'Athleticism', dkgrey), vp = vplayout(8:12, 15:19)) #move this guy down??
print(drawscore(mechanics_score, 'Mechanics', dkred), vp = vplayout(8:12, 20:24)) #move this guy down ??

## ROW 5-6: lollipop plot and figure
print(main_page_1_dot_plot(), vp = vplayout(7:13, 2:12))  #pull from right df, move to right area

### The V man
print(fig_one, vp = vplayout(14:27, 13:25))  ### move mroe bottom right 

## ROW 7-8: 2x2 scatter
print(cluster_scatter, vp = vplayout(15:26, 2:12)) 



###########
## PAGE 2##
###########
newpage(grid)

## Page header: date
print(drawtext(paste0('Assessment Date: ', date)), vp = vplayout(1, 2:25))
## ROW 2: page title
print(drawtext(intro_page_two, 'P3 ATHLETICISM SUMMARY', header = TRUE), vp = vplayout(2, 2:16))
print(drawscore(athletecism_score, 'Athleticism', dkgrey), vp = vplayout(2, 18:22))
print(get_logo(), vp = vplayout(2, 23:25))

## ROW 3-6: Table
drop_jump <- percentiles_page2 %>% filter(test_type=="Drop Jump") %>% select(metric, Percentile)
print(drawtable(drop_jump %>% dplyr::rename("Vertical: Drop Jump"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(3:10, 2:11), newpage=FALSE)
st_vert <- percentiles_page2 %>% filter(test_type=="Standing Vertical") %>% select(metric, Percentile)
print(drawtable(st_vert %>% dplyr::rename("Vertical: Standing"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(11:18, 2:11), newpage=FALSE)
skater <- percentiles_page2 %>% filter(test_type=="1 Off Skater") %>% select(metric, Percentile)
print(drawtable(skater %>% dplyr::rename("Lateral: Skater"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(19:26, 2:11), newpage=FALSE)


## ROW 3-5: Accel/Decel plot
print(accel_plot2, vp = vplayout(3:11, 12:25))
print(drawtext(page_2_detail, header = FALSE), vp = vplayout(12:15, 12:25))

## ROW 7-8: radar and cluster plots
#print(radar_plot_athl, vp = vplayout(16:27, 12:26)) 
print(clusterplot(athlete_label = athlete_prediction), vp = vplayout(16:26, 12:25))

###########
## PAGE 3##
###########
newpage(grid)

## Page header: date
print(drawtext(paste0('Assessment Date: ', date)), vp = vplayout(1, 2:25))

## ROW 2: page title
print(drawtext(intro_page_three, 'P3 MECHANICS SUMMARY', header = TRUE), vp = vplayout(2, 2:16))
print(drawscore(mechanics_score, 'Mechanics', dkred), vp = vplayout(2, 18:22))
print(get_logo(), vp = vplayout(2, 23:25))

## ROW 3-6 LEFT

## Man Figure
print(fig_three, vp = vplayout(4:20, 10:26))
## Low Back
print(dot_plot1, vp = vplayout(3:7, 2:10))
## Left Knee
print(dot_plot2, vp = vplayout(8:12, 2:10))
## Left Foot
print(dot_plot4, vp = vplayout(13:17, 2:10))
## Right Knee
print(dot_plot3, vp = vplayout(18:22, 2:10))
## Right Foot
print(dot_plot5, vp = vplayout(23:27, 2:10))

## ROW 7-8 LEFT: training targets and cluster plots
print(drawtext(training_recs, 'Training Targets', header = FALSE), vp = vplayout(23:27, 12:24))

dev.off()

filename = paste(playername,": ",date,".pdf",sep="")
file_id <- write_civis_file(output,name = filename)
job_id <- Sys.getenv("CIVIS_JOB_ID")
run_id <- Sys.getenv("CIVIS_RUN_ID")
civis::scripts_post_containers_runs_outputs(job_id, run_id,"File",file_id)

