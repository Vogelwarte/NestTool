####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### SCRATCH PAD TO RUN THROUGH ALL EXTERNALLY SAVED FUNCTIONS IN NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

library(data.table)
library(tidyverse)
library(devtools)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(amt)
library(sf)
library(mapview)
library(recurse)
library(ggplot2)
library(ggspatial)
library(ggpubr)
library(lubridate)
select<-dplyr::select
filter<-dplyr::filter
library(randomForest)
library(ranger)
library(caret)
library(shiny)
library(leaflet)
library(htmltools)
library(NestTool)

## set root folder for project
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2")
# library(here)
# source("R//data_prep.r")
source("R/train_nest_detection.r")
source("R/train_home_range_detection.r")
# source("R//predict_ranging.r")
# source("R//predict_nesting.r")
source("R/train_nest_success.r")
# source("R//predict_success.r")
# source("R//move_metric_extraction.r")
# source("R//plot_move_metrics.r")
# source("R//movement_visualisation.r")
# 
# ## check that the help files load - this does not work in RStudio!!
# devtools::load_all()
# #?data_prep


### LOAD THE TRACKING DATA AND INDIVIDUAL SEASON SUMMARIES 
trackingdata<-fread("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/02_preprocessing/03_milvus_combined.csv")
indseasondata<-fread("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/01_validation/03_validation_combined.csv") %>%
  mutate(nest = case_when(nest_id > 0 ~ "nest",
                        nest_id == 0 ~ "no nest")) %>%
  mutate(HR = case_when(home_range_id > 0 ~ "yes",
                        home_range_id == 0 ~ "no"))

# THURINGIAN BIRDS
# trackingdata<-fread(here("REKI/Analysis/NestTool/REKI/output/02_preprocessing/04_milvus_thuringia.csv"))
# ##make up indseasondata as input is now mandatory
# indseasondata <- trackingdata %>% group_by(year_id) %>%
#  summarise(sex=first(sex), bird_id=first(bird_id))

#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(trackingdata=trackingdata,
                      indseasondata=indseasondata,
                      latboundary=45,
                      longboundary=4,
                      broodstart= yday(ymd("2023-05-01")),
                      broodend<- yday(ymd("2023-06-01")),
                      minlocs=800,
                      nestradius=50,
                      homeradius=2000,
                      startseason=70,
                      endseason=175,
                      settleEnd = 97,  # end of the settlement period in yday
                      Incu1End = 113,   # end of the first incubation phase in yday
                      Incu2End = 129,  # end of the second incubation phase in yday
                      Chick1End = 152, # end of the first chick phase in yday
                      age =10)         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)
#fwrite(nest_data_input$summary,here("output/05_full_run_CH/trackingsummary.csv"))
#fwrite(nest_data_input$summary,here("output/05_full_run_THU/trackingsummary.csv"))
#saveRDS(nest_data_input, here("output/05_full_run_CH/nest_data_input_CH.rds"))
#nest_data_input <- readRDS(here("output/05_full_run_CH/nest_data_input_CH.rds"))



#### STEP 2: train home range model (if training data available)
trackingsummary<-nest_data_input$summary
hr_model<-train_home_range_detection(trackingsummary=trackingsummary,plot=T)
saveRDS(hr_model, "C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/dataR/hr_model.rds")
usethis::use_data(hr_model, overwrite=TRUE)


#### STEP 3: identify home ranges
pred_hr<-predict_ranging(model=hr_model$model,trackingsummary=trackingsummary) # if a model has been trained
#pred_hr<-predict_ranging(trackingsummary=trackingsummary) # uses the model trained with our data (automatically loaded in the function)
#fwrite(pred_hr,here("output/05_full_run_CH/rangingsummary.csv"))


#### STEP 4: train nest model (if training data available)
trackingsummary<-nest_data_input$summary
#trackingsummary <- fread(here("output/05_full_run_CH/trackingsummary.csv"))
#trackingsummary <- fread(here("output/05_full_run_THU/trackingsummary.csv"))
unique(trackingsummary$nest) # now includes NA which must be removed for training model
nest_model<-train_nest_detection(trackingsummary=trackingsummary[!is.na(trackingsummary$nest),],plot=T)
usethis::use_data(nest_model, overwrite=TRUE)
#saveRDS(nest_model, "C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/nest_model.rds"))


#### STEP 5: identify nests

## if prediction includes both training and test data, then remove column "nest_prob" first
pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=pred_hr) # if a model has been trained
#pred_nest<-predict_nesting(trackingsummary=trackingsummary) # uses the model trained with our data (automatically loaded in the function)
#fwrite(pred_nest,here("output/05_full_run_CH/nestingsummary.csv"))
#fwrite(pred_nest,here("output/05_full_run_THU/nestingsummary.csv"))
names(pred_nest)



#### STEP 6: train outcome model (if training data available)
# known_broods<-fread("output/01_validation/02_validation_brood.csv") %>%
#   filter(fledged %in% c("yes","no"))
#nestingsummary<-fread(here("output/05_full_run_CH/nestingsummary.csv"))
#nestingsummary<-fread(here("output/05_full_run_THU/nestingsummary.csv"))
succ_model<-train_nest_success(nestingsummary=pred_nest[pred_nest$success %in% c("yes","no"),],plot=T)
usethis::use_data(succ_model, overwrite=TRUE)
#saveRDS(succ_model, here("R/succ_model.rds"))



#### STEP 7: determine outcome
# troubleshoot error for prediction - age_cy is missing from data frame
# names(succ_model$summary)[which(!(names(succ_model$summary) %in% names(nestingsummary)))]

pred_succ<-predict_success(model=succ_model$model,nestingsummary=pred_nest,nest_cutoff =succ_model$nest_cutoff) # if a model has been trained
#pred_succ<-predict_success(nestingsummary=pred_nest) # uses the model trained with our data (automatically loaded in the function)
#fwrite(pred_succ,here("output/05_full_run_CH/successsummary.csv"))
#fwrite(pred_succ,here("output/05_full_run_THU/successsummary.csv"))




#### SELECT EXAMPLE INDIVIDUALS
# CANDLIST<-pred_succ %>% group_by(sex,nest_observed,success_observed) %>%
#   summarise(nest = sample(year_id[which(nest_prob == max(nest_prob))],1),
#             #ID2 = sample(year_id[which(nest_prob == median(nest_prob))],1),
#             nonest = sample(year_id[which(nest_prob == min(nest_prob))],1),
#             succ = sample(year_id[which(succ_prob == max(succ_prob))],1),
#             #ID5 = sample(year_id[which(succ_prob == median(succ_prob))],1),
#             fail = sample(year_id[which(succ_prob == min(succ_prob))],1)) %>%
#   gather(key="What",value="year_id",-sex,-nest_observed,-success_observed) %>%
#   filter(success_observed %in% c("yes","no")) %>%
#   filter(nest_observed!=What) %>%
#   filter(!(nest_observed=="no nest" & success_observed=="yes"))
# picks<-c(unique(CANDLIST$year_id),
# sample((pred_succ %>% filter(succ_prob>0.49 & succ_prob<0.51) %>% select(year_id))$year_id,3))
#   
# ### extract data for package
# kite.tracks<-trackingdata %>% filter(year_id %in% picks)
# kite.nesting<-indseasondata %>% filter(year_id %in% picks)
# usethis::use_data(kite.tracks)
# usethis::use_data(kite.nesting)

#### STEP 8: extract weekly movement metrics for manual classification
#trackingdata<-fread(here("output/02_preprocessing/03_milvus_combined.csv"))
#trackingdata<-fread(here("output/02_preprocessing/04_milvus_thuringia.csv"))
#pred_succ<-fread(here("output/05_full_run_CH/successsummary.csv"))
#pred_succ<-fread(here("output/05_full_run_THU/successsummary.csv"))

# this function takes over an hour for swiss data (3 mins for german data)

milvus_5d_move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                               nest_locs=nest_data_input$pot_nests, 
                                     inddata=pred_succ,
                                     uncertainty=0.45,
                                     nestradius=50,
                                     startseason=70,endseason=175)
#fwrite(milvus_5d_move_metrics,here("output/05_full_run_CH/movemetrics.csv"))
#fwrite(milvus_5d_move_metrics,here("output/05_full_run_THU/movemetrics.csv"))


#### STEP 9: use ShinyApp to inspect all questionable individuals
# nest_data_input <- readRDS(here("output/05_full_run_CH/nest_data_input_CH.rds"))
# pred_succ <- fread(here("output/05_full_run_CH/successsummary.csv"))
# milvus_5d_move_metrics<-fread(here("output/05_full_run_CH/movemetrics.csv"))
# milvus_5d_move_metrics<-fread(here("output/05_full_run_THU/movemetrics.csv"))

movement_visualisation(trackingdata = nest_data_input$movementtrack,
                       inddata = pred_succ,
                       nest_locs = nest_data_input$pot_nests,
                       move_metrics = milvus_5d_move_metrics,
                       uncertainty = 0.45,
                       output_path="C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/05_full_run_CH/nest_success_output.csv")


#### OPTIONAL STEP: show movement metrics 
plot_move_metrics(movemetrics=milvus_5d_move_metrics, individual="2022_526")

names(pred_succ_)[!(names(pred_succ_) %in% names(pred_succ))]
names(pred_succ)[!(names(pred_succ) %in% names(pred_succ_))]
# ### examples of unsuccessful birds:
# i="2017_186"
# i="2017_187"
# i="2017_27"
# i="2018_187"
# i="2018_26"
# i="2019_186"
# 
# 
# ### examples of successful birds:
# i="2020_80"
# i="2021_28"
# i="2021_454"
# i="2021_481"
# i="2021_61"
# milvus_5d_move_metrics$week




#### SUMMARISE BREEDING PROPENSITY AND SUCCESS

## READ IN AND COMBINE DATA OF MANUALLY CLASSIFIED AND AUTOMATICALLY CLASSIFIED DATA
MANUAL<-fread("output/05_full_run_CH/nest_success_output.csv") %>%
  rename(ManNest=Nest,ManSuccess=Success) %>%
  select(year_id,ManNest,ManSuccess)
ALL<-pred_succ %>% select(year_id,nest_observed,success_observed,hr_observed,hr_prob,nest_prob,succ_prob) %>%
  left_join(MANUAL, by='year_id') %>%
  mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="Yes"),1,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="Yes"),1,succ_prob)) %>%
  mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="No"),0,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="No"),0,succ_prob)) %>%
  mutate(HR=ifelse(hr_prob>0.5,1,0),Nest=ifelse(nest_prob>0.5,1,0),Success=ifelse(succ_prob>0.5,1,0))

## breeding propensity - what proportion of birds with a homerange have a nesting attempt?
ALL %>% filter(HR==1) %>% ungroup() %>%
  summarise(Propensity=mean(Nest))

ALL %>% filter(hr_observed=="yes") %>% ungroup() %>% 
  mutate(Nest=ifelse(nest_observed=="nest",1,0)) %>%
  summarise(Propensity=mean(Nest))

## breeding success - what proportion of birds with a nesting attempt are successful?
ALL %>% filter(Nest==1) %>% ungroup() %>%
  summarise(Success=mean(Success))

ALL %>% filter(nest_observed=="nest") %>% ungroup() %>% 
  mutate(Success=ifelse(success_observed=="yes",1,0)) %>%
  summarise(Success=mean(Success))
