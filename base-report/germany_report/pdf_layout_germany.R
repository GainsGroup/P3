
library(ggplot2)
library(dplyr)
library(civis)
library(extrafont)
library(gridExtra)
library(grid)
library(cowplot)
library(ReporteRs)
library(stringr)
library(grImport)
library(RGraphics)
source('germany_report/themes_germany.R')
source('germany_report/draw_objects_germany.R')
source('germany_report/plotting_germany.R')

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

accel_subtitle_1 <- "The graph below provides a brief snapshot of the athlete's \nacceleration and deceleration capabilities"
accel_subtitle_2 <- "The graph below contains a series of metrics related to the \nathlete's acceleration and deceleration capabilities"

intro <- Sys.getenv("INTRO")
page_2_detail <- Sys.getenv("PAGE_2_DETAIL")
training_recs <- Sys.getenv("TRAINING_RECS")

################
## LOAD DATA  ##
################
stats_df <- get_stats(playername, date)
kpis <- get_kpis(playername, date)
ad2 <- get_accel_decel_2(playername,date)
cluster_athl <- get_athl_cluster_data(playername,date)
percentiles_page2 <- get_percentiles_page_2(playername,date)
percentiles_page3 <- get_percentiles_page_3(playername,date)



################
## GET PLOTS ##
################
#p <- summary_plot(kpis)
dot_plot6 <- dot_plot(kpis, type = 'vertical', title='Vertical Performance Factors')
dot_plot7 <- dot_plot(kpis, type = 'lateral', title='Lateral Performance Factors')
dot_plot8 <- dot_plot(kpis, type = 'performance', title='Raw Performance Factors')
fig <- get_fig(playername,date)
accel_plot2 <- acceleration_bars(ad2 %>% arrange(desc(metric)),accel_subtitle_2)
cluster_scatter <- graph_page_2_2x2(playername,date)
radar_plot_athl <- radar_plot(cluster_athl) 
dot_plot1 <- dot_plot(percentiles_page3, type = 'low back', title='Low Back Mechanics')
dot_plot2 <- dot_plot(percentiles_page3, type = 'left knee', title='Left Knee Mechanics')
dot_plot3 <- dot_plot_right(percentiles_page3, type = 'right knee', title='Right Knee Mechanics')
dot_plot4 <- dot_plot(percentiles_page3, type = 'left foot', title='Left Foot Mechanics')
dot_plot5 <- dot_plot_right(percentiles_page3, type = 'right foot', title='Right Foot Mechanics')


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
print(drawtext(intro, 'P3 ATHLETICISM SUMMARY', header = TRUE), vp = vplayout(2, 2:16))
#print(get_eagle(), vp = vplayout(2:3, 17:21))
print(get_logo(), vp = vplayout(2:3, 22:25))

## ROW 3/4: summary text and scores
print(fig, vp = vplayout(4:17, 14:25))
print(drawtable(stats_df), vp = vplayout(3:6, 2:14))
#print(get_eagle(), vp = vplayout(3:6, 16:20))
#print(get_logo(), vp = vplayout(3:6, 21:23))


## ROW 5-6: lollipop plot and figure
print(dot_plot6, vp = vplayout(8:13, 2:12))
print(dot_plot7, vp = vplayout(15:20, 2:12))
print(dot_plot8, vp = vplayout(22:27, 2:12))


## ROW 7-8: history and acceleration plots
print(cluster_scatter, vp = vplayout(18:28, 14:25))



###########
## PAGE 2##
###########
newpage(grid)

## Page header: date
print(drawtext(paste0('Assessment Date: ', date)), vp = vplayout(1, 2:25))
## ROW 2: page title
print(drawtext(intro, 'P3 ATHLETICISM SUMMARY', header = TRUE), vp = vplayout(2, 2:16))
print(get_eagle(), vp = vplayout(2, 17:21))
print(get_logo(), vp = vplayout(2, 22:24))

## ROW 3-6: Table
drop_jump <- percentiles_page2 %>% filter(test_type=="SL Drop Stance") %>% select(metric, Percentile)
print(drawtable(drop_jump %>% dplyr::rename("SL Drop Stance"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(3:7, 2:11), newpage=FALSE)
drop_jump <- percentiles_page2 %>% filter(test_type=="SL Drop Kick") %>% select(metric, Percentile)
print(drawtable(drop_jump %>% dplyr::rename("SL Drop Kick"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(8:12, 2:11), newpage=FALSE)
st_vert <- percentiles_page2 %>% filter(test_type=="Standing Vertical") %>% select(metric, Percentile)
print(drawtable(st_vert %>% dplyr::rename("Standing Vert"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(13:18, 2:11), newpage=FALSE)
skater <- percentiles_page2 %>% filter(test_type== "Skater") %>% select(metric, Percentile)
print(drawtable(skater %>% dplyr::rename("Skater"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(19:26, 2:11), newpage=FALSE)


## ROW 3-5: Accel/Decel plot
print(accel_plot2, vp = vplayout(3:11, 12:25))
print(drawtext(page_2_detail, header = FALSE), vp = vplayout(12:15, 12:25))

## ROW 7-8: radar and cluster plots
print(radar_plot_athl, vp = vplayout(16:27, 12:26)) 


###########
## PAGE 3##
###########
newpage(grid)

## Page header: date
print(drawtext(paste0('Assessment Date: ', date)), vp = vplayout(1, 2:25))

## ROW 2: page title
print(drawtext(intro, 'P3 MECHANICS SUMMARY', header = TRUE), vp = vplayout(2, 2:16))
print(get_eagle(), vp = vplayout(2, 17:21))
print(get_logo(), vp = vplayout(2, 22:24))

## ROW 3-6 LEFT

## Man Figure
print(fig, vp = vplayout(11:20, 8:18))
## Low Back
print(dot_plot1, vp = vplayout(3:9, 7:16))
## Left Knee
print(dot_plot2, vp = vplayout(10:16, 1:9))
## Left Foot
print(dot_plot4, vp = vplayout(18:24, 1:9))
## Right Knee
print(dot_plot3, vp = vplayout(10:16, 17:26))
## Right Foot
print(dot_plot5, vp = vplayout(18:24, 17:26))

## ROW 7-8 LEFT: training targets and cluster plots
print(drawtext(training_recs, 'Training Targets', header = FALSE), vp = vplayout(26:28, 2:11))

dev.off()

filename = paste(playername,": ",date,".pdf",sep="")
file_id <- write_civis_file(output,name = filename)
job_id <- Sys.getenv("CIVIS_JOB_ID")
run_id <- Sys.getenv("CIVIS_RUN_ID")
civis::scripts_post_containers_runs_outputs(job_id, run_id,"File",file_id)
