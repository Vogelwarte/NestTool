####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### TUTORIAL WORKFLOW OF NESTTOOL PACKAGE INCLUDING DATA DOWNLOAD FROM MOVEBANK ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########


## THIS TUTORIAL WAS INTENDED TO USE A FREE DATA SET FROM MOVEBANK - but I failed to find one that covered a breeding season in southern Africa!!




####### LIBRARIES REQUIRED
#install.packages("devtools", dependencies = TRUE) 
#library(devtools)
library(tidyverse)
library(sf)
library(move2)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(data.table); setDTthreads(percent = 65)
library(units)
library(readxl)
#devtools::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(NestTool)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SET UP DOWNLOAD OF TRACKING DATA FROM MOVEBANK --------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### https://cran.r-project.org/web/packages/move2/vignettes/movebank.html
####### SPECIFY THE MOVEBANK ID OF THE STUDY FOR WHICH DATA ARE SHARED
MYSTUDY<-	2919708  ## this study of hornbills in South Africa has freely available data, but you need to accept the license terms on Movebank
MYUSERNAME<-"Steffen"
movebank_store_credentials(username=MYUSERNAME, key_name = getOption("move2_movebank_key_name"), force = TRUE)
movebank_download_study_info(study_id=MYSTUDY)$sensor_type_ids




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD MOVEBANK DATA AND ANIMAL INFO ----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## download bird metadata
birds<-movebank_retrieve(study_id=MYSTUDY, entity_type="individual",'license-md5'='02649fb55e22792dd9a8de5651264705') %>%
  dplyr::rename(individual_id=id,bird_id=local_identifier) %>%
  mutate(age_cy=10) %>% ## make up an age as unknown
  dplyr::select(individual_id,bird_id,sex,age_cy)

## download gps data for multiple individuals
locs<-movebank_retrieve(study_id=MYSTUDY, entity_type="event",
                        sensor_type_id = c(653,82798), #"gps",
                        'license-md5'='02649fb55e22792dd9a8de5651264705')



## COMBINE ALL data for use in data_prep function of NestTool-------

trackingdata<-locs %>%
  dplyr::filter(month(timestamp)>9 | month(timestamp)<4) %>%
  left_join(birds, by="individual_id") %>%
  dplyr::filter(!is.na(location_lat)) %>%
  st_as_sf(coords=c('location_long','location_lat'), crs=4326) %>%
  mutate(year_id=paste(year(timestamp), bird_id, sep="_")) %>%
  mutate(long_wgs=st_coordinates(.)[,1], lat_wgs=st_coordinates(.)[,2]) %>%
  st_transform(29333) %>%
  mutate(long_eea=st_coordinates(.)[,1], lat_eea=st_coordinates(.)[,2]) %>%
  st_drop_geometry()

indseasondata<-trackingdata %>%
  group_by(bird_id,year_id, sex) %>%
  summarise(age_cy=max(age_cy, na.rm=T),n_locs=length(unique(timestamp))) %>%
  mutate(sex=ifelse(is.na(sex),"m",
                    ifelse(sex=="m","m","f"))) %>%
  dplyr::filter(n_locs>300)




## add nighttime locations for individuals that don't have any

nightlocs<-trackingdata %>% group_by(year_id,bird_id, yday(timestamp)) %>%
  summarise(lasttime=max(timestamp)) %>%
  rename(timestamp=lasttime) %>%
  left_join(trackingdata, by=c('year_id','bird_id','timestamp')) %>%
  mutate(timestamp=timestamp+hours(6)) %>%
  dplyr::select(-`yday(timestamp)`)


## remove individuals with very few locations and add nightlocations
trackingdata<-trackingdata %>%
  dplyr::filter(bird_id %in% unique(indseasondata$bird_id)) %>%
  bind_rows(nightlocs) %>%
  arrange(year_id,timestamp) %>%
  select(year_id,bird_id,timestamp,lat_wgs,long_wgs,lat_eea,long_eea)

hist(yday(trackingdata$timestamp))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START NEST TOOL ANALYSIS -------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(trackingdata=trackingdata,
                      indseasondata=indseasondata,
                      latboundary=-31,
                      longboundary=29,
                      minlocs=500,   ### need to play with that to include more birds? especially those that are nesting!
                      nestradius=150,
                      homeradius=3000,
			                crs_epsg=29333,
			                age =10,

			                startseason=yday(ymd("2023-09-10")),
			                endseason=yday(ymd("2024-03-20")),
			                settleEnd = yday(ymd("2023-10-10")),  # end of the settlement period in yday
			                Incu1End = yday(ymd("2023-11-10")),   # end of the first incubation phase in yday
			                Incu2End = yday(ymd("2023-12-10")),  # end of the second incubation phase in yday
			                Chick1End = yday(ymd("2024-02-10")), # end of the first chick phase in yday
			                broodstart= yday(ymd("2023-12-01")),
			                broodend<- yday(ymd("2024-02-20"))
			                )         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)


#### STEP 2: identify nests
nest_model<-NestTool::nest_model
names(nest_model$summary)[which(names(nest_model$summary) %in% names(nest_data_input$summary)==FALSE)]
pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=nest_data_input) # uses the model trained with our data (automatically loaded in the function)


#### STEP 3: determine outcome
succ_model<-NestTool::succ_model
pred_succ<-predict_success(model=succ_model$model,nestingsummary=pred_nest, nest_cutoff=0.5) # uses the model trained with our data (automatically loaded in the function)



#### STEP 5: extract weekly movement metrics for manual classification
move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                     nest_locs=nest_data_input$pot_nests, 
                                     inddata=pred_succ,
						                         crs_epsg=3035,
                                     uncertainty=0.25,
                                     nestradius=150,
                                     startseason=yday(ymd("2023-09-10")),endseason=yday(ymd("2024-03-20")))

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

pred_succ %>%
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
  
  
data<-tibble(Age=runif(150,1,10)) %>% 
  mutate(succ_prob=rbeta(150,5,12)) %>%
  bind_rows(tibble(Age=runif(100,5,20)) %>% 
              mutate(succ_prob=rbeta(100,20,5)))

ggplot(data, aes(x=Age,y=succ_prob)) +
  geom_point(size=2, col="firebrick") +
  scale_x_continuous(name="Age of bird",limits=c(0,20), breaks=seq(0,20,4), labels=seq(0,20,4))+
  labs(y="predicted nest success") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2), labels=seq(0,1,0.2)) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"),
        panel.grid.major.y = element_line(linewidth=0.5, colour="grey59", linetype="dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=16))