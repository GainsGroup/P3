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
source('baseball-report/v2_themes.R')
source('baseball-report/v2_draw_objects.R')
source('baseball-report/v2_plotting.R')


extrafont::font_import(prompt=FALSE,pattern='Abel')

loadfonts() 
#switch the dkred to hex code for whatever accent we want
# #5A2D81 is kings purp.
# #B3A369 is ga tech
# #b11a21 is P3 red (standard)
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


intro_page_one <- paste(playername, "was taken through an assessment by P3 on",date, "to capture his biomechanical profile.  The report contains metrics that have been derived from P3's dataset of over 600 NBA players.  For additional information please reach out to P3 at any time.",sep =" ")
                      

################
## LOAD DATA  ##
################
bio_stats_df <- get_table_stats(playername, date)
percentiles_page2 <- get_percentiles_page_2(playername,date)
spider_data <- get_athl_cluster_data(playername,date)


print("Load DATA complete")
################
## GET PLOTS ##
################
radar_plot_athl <- radar_plot(spider_data) 

print("load plots complete")

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

print("now printing report")
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

# 1 
## Page header: date and title
print(drawtext(paste0('Assessment Date: ', date)), vp = vplayout(1, 3:25))
## page title
print(drawtext(intro_page_one, pagetitle, header = TRUE), vp = vplayout(2, 3:20))
print(get_logo(), vp = vplayout(2, 21:23))

# 2 Bio info table 
## ROW 3/4: summary text and scores
print(drawtable_pageone(bio_stats_df), vp = vplayout(3:6, 6:20))

# 4 Spider Plot 
print(radar_plot_athl, vp = vplayout(16:27, 12:26))  ## Make bottom left 

# 5 Table 
st_vert <- percentiles_page2 %>% filter(test_type=="Standing Vertical") %>% select(metric, Percentile)
print(drawtable(st_vert %>% dplyr::rename("Vertical: Standing"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(7:13, 2:11), newpage=FALSE)
skater <- percentiles_page2 %>% filter(test_type=="1 Off Skater") %>% select(metric, Percentile)
print(drawtable(skater %>% dplyr::rename("Lateral: Skater"=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(14:21, 2:11), newpage=FALSE)
upper <- percentiles_page2 %>% filter(test_type=="Upper Extremity") %>% select(metric, Percentile)
print(drawtable(upper %>% dplyr::rename("Upper Extrem."=metric), fill_col = 'Percentile', fill = dkgrey, width='fill'), vp = vplayout(22:25, 2:11), newpage=FALSE)


# 6 Training Targets 
print(drawtext(paste0(rec_one), 'Training Targets', header = FALSE), vp = vplayout(23:25, 2:11))
print(drawtext(paste0(rec_two), title = NULL, header = FALSE), vp = vplayout(26:27, 2:11))
#print(drawtext(paste0(rec_three), title = NULL, header = FALSE), vp = vplayout(27:28, 2:11))


dev.off()

filename = paste(playername,": ",date,".pdf",sep="")
file_id <- write_civis_file(output,name = filename)
job_id <- Sys.getenv("CIVIS_JOB_ID")
run_id <- Sys.getenv("CIVIS_RUN_ID")
civis::scripts_post_containers_runs_outputs(job_id, run_id,"File",file_id)

