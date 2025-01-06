####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### TEST WORKFLOW OF NESTTOOL PACKAGE INCLUDING DATA DOWNLOAD FROM MOVEBANK ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########


####### LIBRARIES REQUIRED
#install.packages("devtools", dependencies = TRUE) 
#library(devtools)
library(tidyverse)
library(sf)
library(tmap)
library(move2)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(data.table); setDTthreads(percent = 65)
library(units)
library(tmaptools)
library(readxl)
#devtools::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(NestTool)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SET UP DOWNLOAD OF TRACKING DATA FROM MOVEBANK --------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### https://cran.r-project.org/web/packages/move2/vignettes/movebank.html
####### SPECIFY THE MOVEBANK ID OF THE STUDY FOR WHICH DATA ARE SHARED
MYSTUDY<-15869951
MYUSERNAME<-"Steffen"
movebank_store_credentials(username=MYUSERNAME, key_name = getOption("move2_movebank_key_name"), force = TRUE)
movebank_download_study_info(study_id=MYSTUDY)$sensor_type_ids




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD MOVEBANK DATA AND ANIMAL INFO ----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
deployments<-movebank_download_deployment(study_id=MYSTUDY) %>% # only needed for troubleshooting because move2 objects have no individual ID
  group_by(individual_id) %>%
  summarise(deploy_on_timestamp=min(deploy_on_timestamp))  ## avoid multiple dates for birs that had their transmitter replaced

birds<-movebank_retrieve(study_id=MYSTUDY, entity_type="individual") %>%
  dplyr::rename(individual_id=id,bird_id=local_identifier) %>%
  left_join(deployments, by="individual_id") %>%
  #filter(birth_hatch_latitude>25) %>%
  dplyr::select(individual_id,bird_id,sex,latest_date_born, group_id,sex,deploy_on_timestamp) %>%
  mutate(latest_date_born=if_else(is.na(latest_date_born),if_else(group_id=="wild",
                                  deploy_on_timestamp-months(2),
                                  deploy_on_timestamp-year(1)),latest_date_born)) %>%
  mutate(start_year=year(deploy_on_timestamp)+1) %>%
  arrange(start_year,bird_id)
birds
sort(birds$bird_id)

## download gps data for multiple individuals
locs<-movebank_retrieve(study_id=MYSTUDY, entity_type="event",
                        sensor_type_id = c(653,82798), #"gps",
                        individual_id = birds$individual_id)

### filter locations to the Balkans and summer months
sample_tracks<-locs %>%
  filter(location_lat>38.5) %>%
  filter(location_long<28.5) %>%
  filter(month(timestamp)>2) %>%
  filter(month(timestamp)<9) %>%
  left_join(birds, by="individual_id") %>%
  filter(year(timestamp)>=start_year) %>%
  mutate(age_cy=as.integer((year(timestamp)-year(latest_date_born))+1)) %>% ### calculate age in calendar years starting with 1
  st_as_sf(coords=c('location_long','location_lat'), crs=4326) %>%
  mutate(bird_id=droplevels(bird_id))  

dim(sample_tracks)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SHOW TRACKS ON MAP -------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
tmap_mode("view")
tm_basemap(server="OpenStreetMap") +
  tm_shape(sample_tracks)+
  tm_symbols(col = 'bird_id', size = 0.1)




## COMBINE ALL data-------

trackingdata<-sample_tracks %>%
  mutate(year_id=paste(year(timestamp), bird_id, sep="_")) %>%
  st_as_sf(coords = c("long", "lat"), crs=4326) %>%
  mutate(long_wgs=st_coordinates(.)[,1], lat_wgs=st_coordinates(.)[,2]) %>%
  st_transform(3035) %>%
  mutate(long_eea=st_coordinates(.)[,1], lat_eea=st_coordinates(.)[,2]) %>%
  st_drop_geometry()
indseasondata<-trackingdata %>%
  group_by(bird_id,year_id, sex) %>%
  summarise(age_cy=max(age_cy, na.rm=T),n_locs=length(unique(timestamp))) %>%
  mutate(sex=ifelse(is.na(sex),"m",
                    ifelse(sex=="m","m","f")))

unique(indseasondata$bird_id)


## IF MODELS ARE TO BE FITTED THEN A SEPERATE FILE IS NECESSARY THAT INCLUDES THE HR, nest and success information for year_ids
indseasondata<-read_excel("C:/Users/sop/OneDrive - Vogelwarte/EGVU/EGVU_indseasondata_VA_EK.xlsx", sheet="Data_Balkans") %>%
  mutate(year_id=paste(year, bird_id, sep="_")) %>%
  mutate(sex=ifelse(sex=="male","m","f")) %>%
  full_join(indseasondata, by=c("year_id","bird_id","sex")) %>%
  mutate(territory=ifelse(is.na(territory),"no",territory)) %>%
  rename(HR=territory) %>%
  mutate(nest=ifelse(is.na(nest),"no nest",nest)) %>%
  mutate(success=ifelse(is.na(success),"no",success)) %>%
  filter(!is.na(n_locs)) %>% ## remove birds without tracking data
  select(year_id,age_cy,sex,HR,nest,success)
indseasondata

trackingdata<-trackingdata %>%
  select(year_id,bird_id,timestamp,lat_wgs,long_wgs,lat_eea,long_eea)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START NEST TOOL ANALYSIS -------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(trackingdata=trackingdata,
                      indseasondata=indseasondata,
                      latboundary=38.5,
                      longboundary=19,
			                crs_epsg=3035,
                      broodstart= yday(ymd("2023-05-31")),
                      broodend<- yday(ymd("2023-06-25")),
                      minlocs=800,   ### need to play with that to include more birds? especially those that are nesting!
                      nestradius=250,
                      homeradius=5000,
			                startseason=yday(ymd("2023-03-20")),
			                endseason=yday(ymd("2023-08-01")),
			                settleEnd = yday(ymd("2023-04-20")),  # end of the settlement period in yday
			                Incu1End = yday(ymd("2023-05-10")),   # end of the first incubation phase in yday
			                Incu2End = yday(ymd("2023-05-31")),  # end of the second incubation phase in yday
			                Chick1End = yday(ymd("2023-06-25")), # end of the first chick phase in yday
			                age =10)         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)


#### STEP 2: identify home ranges
trackingsummary<-nest_data_input$summary
trackingsummary$HR
trackingsummary$nest
trackingsummary$success
hr_model<-train_home_range_detection(trackingsummary=trackingsummary,plot=T)
hr_model$eval_train
hr_model$eval_test
pred_hr<-predict_ranging(model=hr_model$model,trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 3: identify nests
nest_model<-train_nest_detection(trackingsummary=trackingsummary,plot=T)
nest_model$eval_train
nest_model$eval_test
pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=pred_hr) # uses the model trained with our data (automatically loaded in the function)


#### STEP 4: determine outcome
succ_model<-train_nest_success(nestingsummary=pred_nest,plot=T)
succ_model$eval_train
succ_model$eval_test
pred_succ<-predict_success(model=succ_model$model,nestingsummary=pred_nest, nest_cutoff=0.5) # uses the model trained with our data (automatically loaded in the function)



#### STEP 5: extract weekly movement metrics for manual classification
move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                     nest_locs=nest_data_input$pot_nests, 
                                     inddata=pred_succ,
						                         crs_epsg=3035,
                                     uncertainty=0.25,
                                     nestradius=50,
                                     startseason=70,endseason=175)

#### STEP 6: use ShinyApp to inspect all questionable individuals
?movement_visualisation
movement_visualisation(trackingdata=nest_data_input$movementtrack,
				crs_epsg=3035,
                       nest_locs=nest_data_input$pot_nests, 
                       inddata=pred_succ,			
                       move_metrics = move_metrics,
                       uncertainty = 0.25,
                       output_path="NestTool_example_nest_success_output.csv")




#### STEP 7: visualise the predictions for the EGVU

plotdat<-pred_succ %>%
  select(year_id,hr_prob,nest_prob,succ_prob) %>%
  gather(key='metric', value='prob',-year_id) %>%
  mutate(metric=ifelse(metric=="hr_prob", "HR",ifelse(metric=="nest_prob","nest","success"))) %>%
  left_join((indseasondata %>%
  select(year_id,HR,nest,success) %>%
  gather(key='metric', value='outcome',-year_id) %>%
  mutate(outcome=ifelse(outcome=="nest","yes", ifelse(outcome=="no nest","no",outcome)))),
  by=c("year_id","metric")) %>%
  
  ggplot() +
  geom_point(aes(x=outcome,y=prob, colour=year_id),size=2, position=position_dodge(0.2)) +
  facet_wrap(~metric, ncol=1)+
  theme(legend.position="none")
  
  

