library(civis)
library(lubridate)
library(scales)
library(stringr)
library(caret)
library(tibble)
library(dplyr)
set.seed(14343)
Sys.setenv(CIVIS_API_KEY = 'cdd57e940062ebda689520c4882a3024de788987e77709ab94f88c2f0883919c')   

###### Data Prep #################

### Load in the data 
vertical_jump<- read_civis("public.athlete_vertical_template_master", database = "P3")
drop_jump<- read_civis("public.athlete_drop_jump_template_master", database = "P3")
master<- read_civis("public.athlete_information_master", database = "P3")
skater_left<- read_civis("public.athlete_right_skater_template_master", database = "P3")   #### this will need to be updated (naming convention)
skater_right<- read_civis("public.athlete_left_skater_template_master", database = "P3")   #### this will need to be updated (naming convention)

### Clean - Remove rows where name is blank  ####
master <-  master[!(is.na(master$name) | master$name==""), ]
drop_jump <-  drop_jump[!(is.na(drop_jump$name) | drop_jump$name==""), ]
vertical_jump <-  vertical_jump[!(is.na(vertical_jump$name) | vertical_jump$name==""), ]
skater_left <-  skater_left[!(is.na(skater_left$name) | skater_left$name==""), ]
skater_right <-  skater_right[!(is.na(skater_right$name) | skater_right$name==""), ]


#Transform Assessment Date to lubridate, not character / factor
master$assessmentdate <- mdy(master$assessmentdate)
drop_jump$assessmentdate <- mdy(drop_jump$assessmentdate)
vertical_jump$assessmentdate <- mdy(vertical_jump$assessmentdate)
skater_left$assessmentdate <- mdy(skater_left$assessmentdate)
skater_right$assessmentdate <- mdy(skater_right$assessmentdate)

################# DUPLICATE CHECKING LOGIC ################################

### Saving the DFs while removing duplicates 
master <- master[!duplicated(master),]
drop_jump <- drop_jump[!duplicated(drop_jump),]
vertical_jump <- vertical_jump[!duplicated(vertical_jump),]
skater_right <- skater_right[!duplicated(skater_right),]
skater_left <- skater_left[!duplicated(skater_left),]

################# DUPLICATE CHECKING LOGIC ################################

#### Convert columns to numeric 

master1<- master[,c(1:6)]
master2<- master[,c(7:ncol(master))]
master2 <- as.data.frame(lapply(master2, function(x) as.numeric(as.character(x))))
master <- cbind(master1,master2)


### Baseline Calculations   #####

### Delta for Vert, Drop
master$Delta_Vert <- master$standingvertical1 - master$reach
master$Delta_Drop <- master$doublelegdrop1 - master$reach
master$Delta_TwoStep <- master$fullapproach1 - master$reach
master <- master[,c(1:6,60,61,62,7:59)]

### Force for Skater left, right (force/BW)
skater_left_join <- skater_left[,c(1:2,10)]
skater_right_join <- skater_right[,c(1:2,10)]


### Joining up the "Y" of skater data      
### Rename columns so we can join
names(master)[1] <- "name"
names(master)[2] <- "assessmentdate"
## Perform join for left
master<-left_join(master, skater_left_join, by= c("name", "assessmentdate"))
## Rename column and move up
master <- master[,c(1:8,ncol(master),9:62)]
colnames(master)[9] <- "maxlateralforce_left" 
## Perform join for right
master<-left_join(master, skater_right_join, by= c("name", "assessmentdate"))
## Rename column and move up
master <- master[,c(1:9,ncol(master), 10:63)]
colnames(master)[10] <- "maxlateralforce_right"

### Create new variables
master$LateralForceLeftBW <- master$maxlateralforce_left/master$bodyweightkg
master$LateralForceRightBW <- master$maxlateralforce_right/master$bodyweightkg
master$Conc_Raw_Avg <- ((master$peakconcentricforceleft + master$peakconcentricforceright) / 2)
master$Ecc_Raw_Avg <- ((master$peakloadingforceleft + master$peakloadingforceright)/2)
master$Imp_1_Avg <- ((master$imp1lraw + master$imp1rraw)/master$bodyweightkg)
master$Ecc_Rel_FF <- ((master$peakloadingforceleft + master$peakloadingforceright) - (master$freefallforceleft + master$freefallforceright))/ master$bodyweightkg
master$Conc_Rel_FF <- ((master$peakconcentricforceleft + master$peakconcentricforceright) - (master$freefallforceleft + master$freefallforceright)) / master$bodyweightkg
master$Net_Rel_Conc_Force <- ((master$peakconcentricforceleft + master$peakconcentricforceright)/master$bodyweightkg) ### If standing vert, its conc_rel_ff, if drop jump we dont do freefall
master$Load_Rel_FF <- ((master$peakloadingforceleft + master$peakloadingforceright)-(master$freefallforceleft + master$freefallforceright))/ master$bodyweightkg  #### LOAD REL FF = ECC REL FF 
drop_jump$ankleactivedecelerationleft <- -1 * drop_jump$ankleactivedecelerationleft #### 10/2/18 this needs to inverse, so just changed sign
drop_jump$ankleactivedecelerationright <- -1 * drop_jump$ankleactivedecelerationright #### 10/2/18 this needs to inverse, so just changed sign
master$Average_LateralForceBW <-  (master$LateralForceLeftBW + master$LateralForceRightBW)/2
master$position = ifelse(master$height <= 75, "Guard",
                         ifelse(master$height > 75 & master$height <= 81 , "Wing",
                                ifelse(master$height > 81, "Big", "Error")))

### Aggregation  #####

#### Prefix column names
colnames(drop_jump)[3:ncol(drop_jump)] <- paste("drop", colnames(drop_jump)[3:ncol(drop_jump)], sep = ".")
colnames(vertical_jump)[3:ncol(vertical_jump)] <- paste("vert", colnames(vertical_jump)[3:ncol(vertical_jump)], sep = ".")
colnames(skater_left)[3:ncol(skater_left)] <- paste("sl", colnames(skater_left)[3:ncol(skater_left)], sep = ".")
colnames(skater_right)[3:ncol(skater_right)] <- paste("sr", colnames(skater_right)[3:ncol(skater_right)], sep = ".")

### Join by name and date

master<-left_join(master, drop_jump, by= c("name", "assessmentdate"))
master<-left_join(master, vertical_jump, by= c("name", "assessmentdate"))
master<-left_join(master, skater_left, by= c("name", "assessmentdate"))
master<-left_join(master, skater_right, by= c("name", "assessmentdate"))

### Get rid of stuff to clean
rm(master1,master2,skater_left_join,skater_right_join, drop_jump, skater_left, skater_right, vertical_jump)

## FUNCTIONS ########

height_conversion_function <- function(data){
  ft <- (data/12)
  feet <- floor(data/12)
  inches <- round(((ft - feet)*12),1)
  value <- paste(feet,"ft",inches,"in")
  return(value)
}
bodyweight_conversion_function <- function(data){
  pounds <- (data *2.20462262185)
  pounds <- round(pounds,1)
  
  value <- paste(pounds,"lbs")
  return(value)
  
}
name_shortening_function <- function(data){
  
  first_initial <- paste(substr(word(data),1,1),".",sep="")
  last_name <- paste(substr(word(data,2),1,20))
  combined_name <- paste(first_initial, last_name, sep = " ")
  final_vector<- combined_name
  return(final_vector)
}
percentile_function <- function(data){
  percentiles <- data %>%
    as.numeric() %>%
    ntile(.,100)
  
  return(percentiles)
}
flag_function <- function(data) {
  ifelse(data<=30 , "red",
         ifelse(data>30 & data <61, "yellow",
                ifelse(data>=61, "green", "error")))
}

#############################################################################################################################################################################
#################################################  
### PAGE 1 DEVELOPMENT ####

### Player Bio Info (i.e., height, reach, etc...)
display_page1_bio_info <- master %>%
  select(c(name, assessmentdate, height, bodyweightkg, reach, Delta_Vert, Delta_Drop, Average_LateralForceBW)) %>%
  mutate(position = ifelse(height <= 75, "Guard",
                           ifelse(height > 75 & master$height <= 81 , "Wing",
                                  ifelse(height > 81, "Big", "Error")))) %>%
  mutate(average_vert = mean(Delta_Vert, na.rm = TRUE),
         average_drop = mean(Delta_Drop, na.rm = TRUE),
         average_latforce = mean(Average_LateralForceBW, na.rm = TRUE),
         perc_vert = round(((Delta_Vert-average_vert)/average_vert)*100,1),
         perc_drop = round(((Delta_Drop-average_drop)/average_drop)*100,1),
         perc_lat = round(((Average_LateralForceBW-average_latforce)/average_latforce)*100,1),
         #### Averages within positional groupings for the Orlando Magic - Vertical Jump
         average_vert_wing = mean(Delta_Vert[position == "Wing"], na.rm = TRUE),
         wing_vert_perc = ifelse(position == "Wing", round(((Delta_Vert-average_vert_wing)/average_vert_wing)*100,1), NA),
         average_vert_guard = mean(Delta_Vert[position == "Guard"], na.rm = TRUE),
         guard_vert_perc = ifelse(position == "Guard", round(((Delta_Vert-average_vert_guard)/average_vert_guard)*100,1), NA),
         average_vert_big = mean(Delta_Vert[position == "Big"], na.rm = TRUE),
         big_vert_perc = ifelse(position == "Big", round(((Delta_Vert-average_vert_big)/average_vert_big)*100,1), NA),
         #### Averages within positional groupings for the Orlando Magic - Drop Jump
         average_drop_wing = mean(Delta_Drop[position == "Wing"], na.rm = TRUE),
         wing_drop_perc = ifelse(position == "Wing", round(((Delta_Drop-average_drop_wing)/average_drop_wing)*100,1), NA),
         average_drop_guard = mean(Delta_Drop[position == "Guard"], na.rm = TRUE),
         guard_drop_perc = ifelse(position == "Guard", round(((Delta_Drop-average_drop_guard)/average_drop_guard)*100,1), NA),
         average_drop_big = mean(Delta_Drop[position == "Big"], na.rm = TRUE),
         big_drop_perc = ifelse(position == "Big", round(((Delta_Drop-average_drop_big)/average_drop_big)*100,1), NA),
         #### Averages within positional groupings for the Orlando Magic -Skater
         average_lat_wing = mean(Average_LateralForceBW[position == "Wing"], na.rm = TRUE),
         wing_lat_perc = ifelse(position == "Wing", round(((Average_LateralForceBW-average_lat_wing)/average_lat_wing)*100,1), NA),
         average_lat_guard = mean(Average_LateralForceBW[position == "Guard"], na.rm = TRUE),
         guard_lat_perc = ifelse(position == "Guard", round(((Average_LateralForceBW-average_lat_guard)/average_lat_guard)*100,1), NA),
         average_lat_big = mean(Average_LateralForceBW[position == "Big"], na.rm = TRUE),
         big_lat_perc = ifelse(position == "Big", round(((Average_LateralForceBW-average_lat_big)/average_lat_big)*100,1), NA)) %>%
  mutate(display_name = name_shortening_function(name),
         display_vert = paste(round(Delta_Vert,1), "in."),
         display_drop = paste(round(Delta_Drop,1), "in."),
         display_height = height_conversion_function(height),
         display_reach = height_conversion_function(reach),
         display_bw = bodyweight_conversion_function(bodyweightkg),
         display_latforce = paste0(round(Average_LateralForceBW,1), "N/kg."),
         average_vert = paste(round((mean(Delta_Vert, na.rm = TRUE)), 1), "in."),
         average_drop = paste(round((mean(Delta_Drop, na.rm = TRUE)), 1), "in."),
         average_latforce = paste(round((mean(Average_LateralForceBW, na.rm = TRUE)), 1), "N/kg.")) %>%
  select(-c( height, bodyweightkg, reach, Delta_Vert, Delta_Drop, Average_LateralForceBW))


#### Percentiles for lollipop plots 
page_1_summary_percentiles <- master %>% 
  select(name, assessmentdate, vert.maxkneeextensionvelocityavg, Conc_Rel_FF, drop.maxkneeextensionvelocityavg,
         Imp_1_Avg, LateralForceLeftBW, LateralForceRightBW,sl.maxhipabduction,sr.maxhipabduction,
         height, bodyweightkg, reach) %>% ######################################################################################  need to add wingspan when P3 adds it
  mutate_if(is.numeric, percentile_function) %>%
  mutate(Imp_1_Avg = abs(100- Imp_1_Avg))


###### Scatterplot Visualization -- THIS INCLUDES MODEL DEVELOPMENT ##### 
scatter_viz <- master %>%
  select(name, assessmentdate, height, Delta_Vert, Conc_Rel_FF, vert.maxkneeextensionvelocityavg,
         vert.maxankleplantarflexionaccelerationavg,vert.maxkneeextensionaccelerationavg,
         bodyweightkg,Average_LateralForceBW) %>%
  mutate(position = ifelse(height <= 75, "Guard",
                           ifelse(height > 75 & master$height <= 81 , "Wing",
                                  ifelse(height > 81, "Big", "Error")))) %>%
  na.omit() %>%
  select(-height) %>% 
  filter(assessmentdate != "2018-10-04") %>%  # Filter out guys from 10/4/18 - that was the USA testing date in Colorado    
  filter(assessmentdate != "2018-07-05") %>% # Filter out guys from 7/5/18  -- random european teen kids
  filter(name != "Curt Haywood") %>%
  filter(name != "TJ Howard") %>%
  filter(name != "Filip Novotny") %>%
  filter(name != "Devonte Bandoo") %>%
  filter(name != "Anonymous Athlete") %>%
  filter(name != "Jarred Vanderbilt") %>%
  filter(name != "Imani Boyette") %>%           # Filter out a bunch of random names .. has to be done one by one, no pattern to wipe them in a group 
  filter(name != "Sue Bird") %>%
  filter(name != "Chris Lutz") %>%
  filter(name != "Jordan Dumars") %>%
  filter(name != "Morgan Tuck") %>%
  filter(name != "Joshua Collins") %>%
  filter(name != "Aaron Valdes") %>%
  filter(name != "Roderick Bobbitt") %>%
  filter(name != "Mamadou N'Diaye") %>%
  filter(assessmentdate > "2013-09-30") %>%    # Filter out super early tests ... all before 9/20/2013
  filter(name != "Solomon Hill") %>%
  filter(name != "JaQuori McLaughlin") %>%
  filter(name != "Peashon Howard") %>%  #### Clean Up Outliers
  filter(name != "Damien Jones") %>%
  filter(name != "Nikola Jokic") 

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=10,
                        savePredictions = TRUE)

set.seed(100)
model <- train(Delta_Vert~., 
               data= scatter_viz[,3:8], 
               method='lm', 
               metric='RMSE',
               trControl=control)

print(model$results[3])

results <- model$pred %>%
  group_by(rowIndex) %>%
  summarise(predicted = mean(pred)) %>%
  mutate(rowIndex = as.character(rowIndex))

full_scatter_viz <- left_join((rownames_to_column(scatter_viz)), results, by = c("rowname" = "rowIndex")) %>%
  select(-c(Delta_Vert, Conc_Rel_FF, vert.maxkneeextensionvelocityavg,
            vert.maxankleplantarflexionaccelerationavg,vert.maxkneeextensionaccelerationavg,
            bodyweightkg, rowname)) %>%
  mutate_if(is.numeric, percentile_function)

rm(model, scatter_viz, results,control)


################################################
#### PAGE 2 DEVELOPMENT  #######
#### Page 2 Percentiles Table ####

page_2_percentiles <- master %>%
  select(name, assessmentdate,drop.maxkneeextensionvelocityavg, Imp_1_Avg,
         imp2lraw, imp2rraw,drop.maxkneeextensionaccelerationavg, Net_Rel_Conc_Force,
         Conc_Rel_FF,vert.maxkneeextensionvelocityavg,vert.maxkneeextensionaccelerationavg, 
         vert.maxankleplantarflexionaccelerationavg, Load_Rel_FF,
         LateralForceLeftBW, sl.maxhipextensionvelocity,    
         sl.maxhipabduction,LateralForceRightBW, sr.maxhipextensionvelocity,sr.maxhipabduction) %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(Imp_1_Avg = abs(100- Imp_1_Avg),
         imp2lraw = abs(100 - imp2lraw),
         imp2rraw = abs(100 - imp2rraw),
         vert.maxankleplantarflexionaccelerationavg = abs(100 - vert.maxankleplantarflexionaccelerationavg),
         sl.maxhipextensionvelocity = abs(100 - sl.maxhipextensionvelocity),
         sr.maxhipextensionvelocity = abs(100 - sr.maxhipextensionvelocity)
  ) 
### Update column names and make sure no percentiles are 0
colnames(page_2_percentiles)[3:ncol(page_2_percentiles)] <- paste("perc", colnames(page_2_percentiles)[3:ncol(page_2_percentiles)], sep = ".")
page_2_percentiles[,3:ncol(page_2_percentiles)][page_2_percentiles[,3:ncol(page_2_percentiles)] == 0] <- 1

#### FOR THE MAGIC - PAGE 2 PERCENTILES BY POSITION ---- WING
page_2_percentiles_wing <- master %>%
  select(name, assessmentdate,height,drop.maxkneeextensionvelocityavg, Imp_1_Avg,
         imp2lraw, imp2rraw,drop.maxkneeextensionaccelerationavg, Net_Rel_Conc_Force,
         Conc_Rel_FF,vert.maxkneeextensionvelocityavg,vert.maxkneeextensionaccelerationavg, 
         vert.maxankleplantarflexionaccelerationavg, Load_Rel_FF,
         LateralForceLeftBW, sl.maxhipextensionvelocity,    
         sl.maxhipabduction,LateralForceRightBW, sr.maxhipextensionvelocity,sr.maxhipabduction) %>%
  mutate(position = ifelse(height <= 75, "Guard",
                           ifelse(height > 75 & master$height <= 81 , "Wing",
                                  ifelse(height > 81, "Big", "Error")))) %>%
  filter(position == "Wing") %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(Imp_1_Avg = abs(100- Imp_1_Avg),
         imp2lraw = abs(100 - imp2lraw),
         imp2rraw = abs(100 - imp2rraw),
         vert.maxankleplantarflexionaccelerationavg = abs(100 - vert.maxankleplantarflexionaccelerationavg),
         sl.maxhipextensionvelocity = abs(100 - sl.maxhipextensionvelocity),
         sr.maxhipextensionvelocity = abs(100 - sr.maxhipextensionvelocity)) 
### Update column names and make sure no percentiles are 0
colnames(page_2_percentiles_wing)[3:ncol(page_2_percentiles_wing)] <- paste("perc", colnames(page_2_percentiles_wing)[3:ncol(page_2_percentiles_wing)], sep = ".")
page_2_percentiles_wing[,3:ncol(page_2_percentiles_wing)][page_2_percentiles_wing[,3:ncol(page_2_percentiles_wing)] == 0] <- 1

#### FOR THE MAGIC - PAGE 2 PERCENTILES BY POSITION ---- BIG
page_2_percentiles_big <- master %>%
  select(name, assessmentdate,height,drop.maxkneeextensionvelocityavg, Imp_1_Avg,
         imp2lraw, imp2rraw,drop.maxkneeextensionaccelerationavg, Net_Rel_Conc_Force,
         Conc_Rel_FF,vert.maxkneeextensionvelocityavg,vert.maxkneeextensionaccelerationavg, 
         vert.maxankleplantarflexionaccelerationavg, Load_Rel_FF,
         LateralForceLeftBW, sl.maxhipextensionvelocity,    
         sl.maxhipabduction,LateralForceRightBW, sr.maxhipextensionvelocity,sr.maxhipabduction) %>%
  mutate(position = ifelse(height <= 75, "Guard",
                           ifelse(height > 75 & master$height <= 81 , "Wing",
                                  ifelse(height > 81, "Big", "Error")))) %>%
  filter(position == "Big") %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(Imp_1_Avg = abs(100- Imp_1_Avg),
         imp2lraw = abs(100 - imp2lraw),
         imp2rraw = abs(100 - imp2rraw),
         vert.maxankleplantarflexionaccelerationavg = abs(100 - vert.maxankleplantarflexionaccelerationavg),
         sl.maxhipextensionvelocity = abs(100 - sl.maxhipextensionvelocity),
         sr.maxhipextensionvelocity = abs(100 - sr.maxhipextensionvelocity)
  ) 
### Update column names and make sure no percentiles are 0
colnames(page_2_percentiles_big)[3:ncol(page_2_percentiles_big)] <- paste("perc", colnames(page_2_percentiles_big)[3:ncol(page_2_percentiles_big)], sep = ".")
page_2_percentiles_big[,3:ncol(page_2_percentiles_big)][page_2_percentiles_big[,3:ncol(page_2_percentiles_big)] == 0] <- 1  

#### FOR THE MAGIC - PAGE 2 PERCENTILES BY POSITION ---- BIG
page_2_percentiles_guard <- master %>%
  select(name, assessmentdate,height,drop.maxkneeextensionvelocityavg, Imp_1_Avg,
         imp2lraw, imp2rraw,drop.maxkneeextensionaccelerationavg, Net_Rel_Conc_Force,
         Conc_Rel_FF,vert.maxkneeextensionvelocityavg,vert.maxkneeextensionaccelerationavg, 
         vert.maxankleplantarflexionaccelerationavg, Load_Rel_FF,
         LateralForceLeftBW, sl.maxhipextensionvelocity,    
         sl.maxhipabduction,LateralForceRightBW, sr.maxhipextensionvelocity,sr.maxhipabduction) %>%
  mutate(position = ifelse(height <= 75, "Guard",
                           ifelse(height > 75 & master$height <= 81 , "Wing",
                                  ifelse(height > 81, "Big", "Error")))) %>%
  filter(position == "Guard") %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(Imp_1_Avg = abs(100- Imp_1_Avg),
         imp2lraw = abs(100 - imp2lraw),
         imp2rraw = abs(100 - imp2rraw),
         vert.maxankleplantarflexionaccelerationavg = abs(100 - vert.maxankleplantarflexionaccelerationavg),
         sl.maxhipextensionvelocity = abs(100 - sl.maxhipextensionvelocity),
         sr.maxhipextensionvelocity = abs(100 - sr.maxhipextensionvelocity)
  ) 
### Update column names and make sure no percentiles are 0
colnames(page_2_percentiles_guard)[3:ncol(page_2_percentiles_guard)] <- paste("perc", colnames(page_2_percentiles_guard)[3:ncol(page_2_percentiles_guard)], sep = ".")
page_2_percentiles_guard[,3:ncol(page_2_percentiles_guard)][page_2_percentiles_guard[,3:ncol(page_2_percentiles_guard)] == 0] <- 1    




############## 3 NEW TABLES - PAGE2PERCENTILES GUARD, PAGE2PERCENTILESWING, PAGE2PERCENTILESBIG ############################################  



#### Page 2 Accel / Deccel Table #### 
page_2_accel_decel <- master %>%
  select(name, assessmentdate, Conc_Rel_FF,LateralForceLeftBW,LateralForceRightBW,vert.maxkneeextensionaccelerationavg,
         Ecc_Rel_FF,Load_Rel_FF,drop.ankleactivedecelerationright, Average_LateralForceBW, drop.ankleactivedecelerationleft) %>%
  mutate(ankle_act_dec_avg = (drop.ankleactivedecelerationleft + drop.ankleactivedecelerationright)/2) %>%
  select(name, assessmentdate, vert.maxkneeextensionaccelerationavg, Ecc_Rel_FF, Load_Rel_FF, ankle_act_dec_avg, Average_LateralForceBW) %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(accel_total = percentile_function(vert.maxkneeextensionaccelerationavg + Load_Rel_FF + Average_LateralForceBW),
         decel_total = percentile_function(ankle_act_dec_avg + Ecc_Rel_FF)) %>%
  mutate(ankle_act_dec_avg = -1 *(ankle_act_dec_avg),
         Ecc_Rel_FF = -1 * (Ecc_Rel_FF))


#### Anthro Score ######
ht_weight <- .2
BMI_weight <- .2
Conc_weight <- .6

anthro_score <- master %>%
  select(name, assessmentdate, height, bodyweightkg, reach, peakconcentricforceleft, peakconcentricforceright) %>%
  mutate(peak_conc = peakconcentricforceleft + peakconcentricforceright,
         BMI = bodyweightkg / ((height*.0254)^2),
         freak_ratio = reach/height) %>%
  mutate_if(is.numeric, percentile_function) %>% 
  na.omit() %>% 
  mutate(BMI_weighted = BMI_weight * BMI,
         height_weighted = ht_weight*height,
         Conc_weighted = Conc_weight * peak_conc,
         raw_anthro_score = height_weighted + BMI_weighted + Conc_weighted,
         scaled_anthro_score = percentile_function(raw_anthro_score))


### Athleticism Score  #########
vertical_weight <- .6
lateral_weight <- 1-vertical_weight

athleticism_score <- master %>%
  select(name, assessmentdate, drop.maxkneeextensionvelocityavg,
         Conc_Rel_FF, Ecc_Rel_FF,vert.maxkneeextensionvelocityavg,
         LateralForceLeftBW, sl.maxhipabduction,Delta_Vert, Delta_Drop,
         LateralForceRightBW, sr.maxhipabduction) %>%
  mutate_if(is.numeric, percentile_function) %>% 
  left_join(anthro_score, by = c("name", "assessmentdate")) %>%
  na.omit() %>%
  mutate(vertical_not_weighted = .7*(drop.maxkneeextensionvelocityavg+Conc_Rel_FF+Ecc_Rel_FF+vert.maxkneeextensionvelocityavg) + .15*((Delta_Vert+Delta_Drop)/2) + .15*(scaled_anthro_score) ,
         lateral__not_weighted = .8*(LateralForceLeftBW+sl.maxhipabduction+LateralForceRightBW+sr.maxhipabduction) + .2*(height + freak_ratio),
         composite = (vertical_weight*(vertical_not_weighted))+ (lateral_weight*(lateral__not_weighted)),
         scaled_athl_score = rescale(composite, to = c(50,100)))

#### NEW Page 2 Clustering #####

new_cluster <- master %>%
  select(name, assessmentdate, Ecc_Rel_FF, Conc_Rel_FF, Net_Rel_Conc_Force, height, reach,
         Average_LateralForceBW, sr.maxhipabduction, sl.maxhipabduction, sl.maxhipextensionvelocity, sr.maxhipextensionvelocity,
         Delta_Vert, Delta_Drop, vert.maxkneeextensionvelocityavg, drop.maxkneeextensionaccelerationavg) %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(skater_hip_abduction_average = (sr.maxhipabduction + sl.maxhipabduction)/2,
         skater_hip_velocity_average = (sr.maxhipextensionvelocity + sl.maxhipextensionvelocity)/2,
         delta_average = (Delta_Vert + Delta_Drop)/2,
         kneevelo_avg = (vert.maxkneeextensionvelocityavg + drop.maxkneeextensionaccelerationavg)/2) %>%
  mutate(skater_hip_velocity_average = abs(100 -skater_hip_velocity_average )) %>%
  select(-c(sr.maxhipabduction,sl.maxhipabduction,sl.maxhipextensionvelocity,sr.maxhipextensionvelocity,
            Delta_Vert, Delta_Drop,vert.maxkneeextensionvelocityavg,drop.maxkneeextensionaccelerationavg)) %>%
  na.omit()

### OLD Page 2 Clustering  ######
cluster_data <- master %>%
  select(name,assessmentdate, bodyweightkg, Delta_Vert, Delta_Drop,LateralForceRightBW,LateralForceLeftBW,
         Imp_1_Avg, drop.maxkneeextensionvelocityavg, Conc_Raw_Avg,
         Conc_Rel_FF, Ecc_Rel_FF,vert.maxkneeextensionvelocityavg,
         sl.averagelateralrfd, sl.maxhipextensionvelocity,sl.maxhipabduction,
         sr.averagelateralrfd, sr.maxhipextensionvelocity,sr.maxhipabduction) %>%
  mutate_if(is.numeric, percentile_function) %>% 
  mutate(Imp_1_Avg = abs(100 - Imp_1_Avg),
         sl.maxhipextensionvelocity = abs(100 - sl.maxhipextensionvelocity),
         sr.maxhipextensionvelocity = abs(100 - sr.maxhipextensionvelocity)) %>%
  na.omit()

### Find optimal number of clusters 
set.seed(2713)  ## ensure repeatability
optimal_clustering <- FitKMeans(cluster_data[,-c(1,2)], max.clusters = 8, nstart = 25,
                                seed = 2713)  ## Find the best amount of clusters

completed_cluster <- kmeans(x = cluster_data[,-c(1:2)], centers = 5)


### Get cluster averages for athletes 
cluster_centers <- as.data.frame(completed_cluster$centers)
cluster_centers$name <- paste("cluster",row.names(cluster_centers),"average",sep="") 
cluster_centers$assesmentdate <- "NA"
cluster_centers <- cluster_centers[,c(18, 19,1:17)]


### Get data frame for spider plot 
cluster_data$assessmentdate <- as.character(cluster_data$assessmentdate)
names(cluster_centers) <- names(cluster_data)
clusters_binding <- c(completed_cluster$cluster, rep("NA", each = max(completed_cluster$cluster)))
spider_plot_cluster_avg <- as.data.frame(rbind(cluster_data,cluster_centers))
spider_plot_cluster_avg$cluster <- clusters_binding

##### PAGE 3 DEVELOPMENT ###
page_3 <- master %>%
  select(name, assessmentdate,
         imp2lraw,imp2rraw, drop.deltahipflexionright,drop.deltahipflexionleft, drop.totalmovementimpulsepercentdifference,   ### back
         drop.maxrelativerotationright,translationr, drop.deltafemoralrotationright, drop.maxhipactivedecelerationright, ### right knee
         drop.maxrelativerotationleft,translationl ,drop.deltafemoralrotationleft, drop.maxhipactivedecelerationleft, #### left knee
         drop.ankleactivedecelerationright,inversionr, drop.ankleflexionatt0right,translationr, ### right ankle
         drop.ankleactivedecelerationleft,inversionl,drop.ankleflexionatt0left,translationl) %>% #### left ankle
  mutate(drop.totalmovementimpulseasymmetry = abs(drop.totalmovementimpulsepercentdifference),
         drop.deltahip_average = (drop.deltahipflexionleft + drop.deltahipflexionright)/2) %>%
  mutate_if(is.numeric, percentile_function) %>%
  mutate(drop.totalmovementimpulseasymmetry = abs(100 - drop.totalmovementimpulseasymmetry),
         drop.deltahip_average = abs(100 - drop.deltahip_average),
         translationl = abs(100 - translationl),
         translationr = abs(100 - translationr),
         inversionl = abs(100 - inversionl),
         inversionr = abs(100 - inversionr),
         imp2lraw = abs(100 - imp2lraw),
         imp2rraw = abs(100 - imp2rraw)
  ) %>%
  na.omit() %>%
  mutate(leftback = (imp2lraw + drop.totalmovementimpulseasymmetry + drop.deltahipflexionleft)/3,
         rightback= (imp2rraw + drop.totalmovementimpulseasymmetry + drop.deltahipflexionright)/3,
         lowback = (leftback + rightback)/2,
         leftknee = (translationl + drop.maxrelativerotationleft + drop.deltafemoralrotationleft + drop.maxhipactivedecelerationleft)/4,
         rightknee = (translationr + drop.maxrelativerotationright + drop.deltafemoralrotationright + drop.maxhipactivedecelerationright)/4,
         leftankle = (inversionl + drop.ankleflexionatt0left + translationl + drop.ankleactivedecelerationleft)/4,
         rightankle = (inversionr + drop.ankleflexionatt0right + translationr + drop.ankleactivedecelerationright)/4) %>%
  select(-c(leftback,rightback)) %>%
  mutate(flag_back = flag_function(lowback),
         flag_leftknee = flag_function(leftknee),
         flag_rightknee = flag_function(rightknee),
         flag_leftankle = flag_function(leftankle),
         flag_rightankle = flag_function(rightankle))

##### Mechanical Score  #########
back_weight <- .2
knee_weight <- .4
ankle_weight <- .4

mechanical_score <- page_3 %>%
  select(name, assessmentdate, lowback, leftknee, rightknee, leftankle, rightankle) %>%
  mutate(knee = (leftknee + rightknee) / 2,
         ankle = (leftankle + rightankle) /2 ,
         rawscore = (back_weight*lowback) + (knee_weight*knee) + (ankle_weight*ankle),
         scaled_mech_score = rescale(rawscore, to = c(50,100))) %>%
  select(-c(leftknee, leftankle, rightknee, rightankle))


#### Writing to Civis ####

##### Page 1 
write_civis(page_1_summary_percentiles, tablename="public.v2_page_1_percentiles", database="P3", if_exists="drop")
write_civis(display_page1_bio_info, tablename = "public.v2_page_1_bio_info", database = "P3", if_exists = "drop")
write_civis(full_scatter_viz, tablename="public.v2_scatter_model_data", database="P3", if_exists="drop")



#### Page 2 
write_civis(page_2_percentiles, tablename="public.v2_page_2_percentiles", database="P3", if_exists="drop")
write_civis(page_2_accel_decel, tablename="public.v2_page_2_accel_decel", database="P3", if_exists="drop")
write_civis(spider_plot_cluster_avg, tablename="public.v2_spider_plot_data", database="P3", if_exists="drop")
write_civis(new_cluster, tablename = "public.v2_plotting_cluster_data", database = "P3", if_exists = "drop") 

#### Page 2 -- MAGIC
write_civis(page_2_percentiles_guard, tablename="public.magic_page_2_percentiles_Guard", database="P3", if_exists="drop")
write_civis(page_2_percentiles_wing, tablename="public.magic_page_2_percentiles_Wing", database="P3", if_exists="drop")
write_civis(page_2_percentiles_big, tablename="public.magic_page_2_percentiles_Big", database="P3", if_exists="drop")



### Page 3 
write_civis(page_3, tablename="public.v2_page_3_percentiles", database="P3", if_exists="drop")

### Scores 
write_civis(athleticism_score, tablename="public.v2_athl_score", database="P3", if_exists="drop")
write_civis(mechanical_score, tablename="public.v2_mech_score", database="P3", if_exists="drop")
#write_civis(composite_score, tablename="public.v2_composite_score", database="P3", if_exists="drop")
write_civis(anthro_score, tablename="public.v2_anthro_score", database="P3", if_exists="drop")

