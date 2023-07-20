####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### VALIDATION OF NESTTOOL PACKAGE USING DATA FROM GERMANY ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### VALIDATION DATA PROVIDED BY MARTIN KOLBE AT ROTMILANZENTRUM (GER)


#install.packages("devtools", dependencies = TRUE) 
#library(devtools)
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
#devtools::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(NestTool)

?data_prep
?movement_visualisation

# LOAD AND FORMAT EXAMPLE TRACKING DATA
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis")

indseasondata <- read_excel("NestTool2/data/REKI_NestTool_ValidationDataTemplate_RMZ.xlsx") %>%
  rename(bird_id=Bird_ID) %>%
  mutate(sex=ifelse(Sex=="?","m",Sex)) %>% ### randomly assign all unknowns to male
  mutate(age_cy=ifelse(Age_when_tagged==">3",Year-Tag_year+6,Year-Tag_year+as.numeric(Age_when_tagged))) %>% ### assume that adult birds were 5 years old when tagged
  mutate(year_id=paste(Year,bird_id,sep="_")) %>%
  select(year_id,bird_id,sex,age_cy,Homerange,Nest,Nest_lat,Nest_long,n_fledglings)

trackingdata<-readRDS("NestTool2/data/REKI_validation_tracks.rds") %>%
  mutate(lat_wgs=x_, long_wgs=y_) %>%
  mutate(timestamp=as.POSIXct(t_)) %>%
  rename(year_id=id) %>%
  mutate(bird_id=str_split_i(year_id,pattern="_",i=2)) %>%
  filter(!is.na(x_)) %>%
  st_as_sf(coords = c("x_", "y_"), crs=4326) %>%
  st_transform(3035) %>%
  dplyr::mutate(long_eea = sf::st_coordinates(.)[,1],
                lat_eea = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(year_id,bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
  arrange(year_id,timestamp)
head(trackingdata)



#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(trackingdata=as.data.frame(trackingdata),
                      indseasondata=indseasondata,
                      latboundary=40,
                      longboundary=4,
                      broodstart= yday(ymd("2023-05-01")),
                      broodend<- yday(ymd("2023-06-01")),
                      minlocs=500,
                      nestradius=50,
                      homeradius=2000,
                      startseason=20,
                      endseason=200,
                      settleEnd = 97,  # end of the settlement period in yday
                      Incu1End = 113,   # end of the first incubation phase in yday
                      Incu2End = 129,  # end of the second incubation phase in yday
                      Chick1End = 152, # end of the first chick phase in yday
                      age =10)         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)


#### STEP 2: identify home ranges
hr_model<-NestTool::hr_model
pred_hr<-predict_ranging(model=hr_model$model,trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 3: identify nests
nest_model<-NestTool::nest_model
pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=pred_hr) # uses the model trained with our data (automatically loaded in the function)


#### STEP 4: determine outcome
succ_model<-NestTool::succ_model
pred_succ<-predict_success(model=succ_model$model,nestingsummary=pred_nest, nest_cutoff=succ_model$nest_cutoff) # uses the model trained with our data (automatically loaded in the function)

#### STEP 5: extract weekly movement metrics for manual classification
move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                     nest_locs=nest_data_input$pot_nests, 
                                     inddata=pred_succ,
                                     uncertainty=0.25,
                                     nestradius=50,
                                     startseason=70,endseason=175)

#### STEP 6: use ShinyApp to inspect all questionable individuals
?movement_visualisation
movement_visualisation(trackingdata=nest_data_input$movementtrack,
                       nest_locs=nest_data_input$pot_nests, 
                       inddata=pred_succ,
                       move_metrics = move_metrics,
                       uncertainty = 0.25,
                       output_path="NestTool_example_nest_success_output.csv")


#### STEP 7: summarise the demographic parameters for the population

## READ IN AND COMBINE DATA OF MANUALLY CLASSIFIED AND AUTOMATICALLY CLASSIFIED DATA
MANUAL<-fread("NestTool_example_nest_success_output.csv") %>%
  rename(ManNest=Nest,ManSuccess=Success) %>%
  select(year_id,ManNest,ManSuccess)
ALL<-pred_succ %>% select(year_id,hr_prob,nest_prob,succ_prob) %>%
  left_join(MANUAL, by='year_id') %>%
  mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="Yes"),1,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="Yes"),1,succ_prob)) %>%
  mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="No"),0,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="No"),0,succ_prob)) %>%
  mutate(HR=ifelse(hr_prob>0.5,1,0),Nest=ifelse(nest_prob>0.5,1,0),Success=ifelse(succ_prob>0.5,1,0))

## breeding propensity - what proportion of birds with a homerange have a nesting attempt?
ALL %>% filter(HR==1) %>% ungroup() %>%
  summarise(Propensity=mean(Nest))

## breeding success - what proportion of birds with a nesting attempt are successful?
ALL %>% filter(Nest==1) %>% ungroup() %>%
  summarise(Success=mean(Success))


