####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### VALIDATION OF NESTTOOL PACKAGE USING DATA FROM GERMANY ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### VALIDATION DATA PROVIDED BY THOMAS PFEIFFER FROM THüRINGEN

### analysis was done blind - the outcome of seasons was unknown to the analyst


# install.packages("devtools", dependencies = TRUE)
# library(devtools)
# devtools::install_github("steffenoppel/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(NestTool)
library(pROC)

# LOAD AND COMPILE TRACKING DATA
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU")
alldat<-list.files(pattern=".txt")

trackingdata<-data.frame()
for(f in 1:length(alldat)){
  dat<-fread(alldat[f])
  names(dat)<-c("bird_id","timestamp","long_wgs","lat_wgs","alt")
  trackingdata<-dat %>% mutate(year=year(timestamp)) %>%
    mutate(bird_id=as.character(bird_id)) %>%
    mutate(year_id=paste(year,bird_id,sep="_")) %>%
    mutate(x=long_wgs,y=lat_wgs) %>%
    filter(!is.na(x)) %>%
    st_as_sf(coords = c("x", "y"), crs=4326) %>%
    st_transform(3035) %>%
    dplyr::mutate(long_eea = sf::st_coordinates(.)[,1],
                  lat_eea = sf::st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    select(year_id,bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
    arrange(year_id,timestamp) %>%
    bind_rows(trackingdata)
}

head(trackingdata)
dim(trackingdata)

##make up indseasondata as input is now mandatory
indseasondata <- trackingdata %>% group_by(year_id) %>%
  summarise(bird_id=first(bird_id)) %>%
  mutate(sex=if_else(bird_id %in% c("3803", "6249a", "6249", "4532", "6674", "8966", "8968", "94754"),"f","m"), age_cy=6) %>%  ### all birds were adult
  select(year_id,bird_id,sex,age_cy)

### ensure correct factor levels of sex to facilitate prediction
indseasondata$sex<-factor(indseasondata$sex, levels=c('m','f'))



#### PRELIMINARY ASSESSMENT - WHICH BIRDS HAVE ENOUGH DATA
### those are the birds that don't have enough data

trackingdata %>% group_by(year_id) %>%
  summarise(n=length(timestamp), start=min(timestamp), end=max(timestamp)) %>%
  filter(year_id %in% indseasondata$year_id) %>%
  arrange(n)


#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(trackingdata=trackingdata,
                      indseasondata=indseasondata,
                      latboundary=50,
                      longboundary=9,
                      broodstart= yday(ymd("2023-05-15")),
                      broodend<- yday(ymd("2023-06-15")),
                      minlocs=800,
                      nestradius=250,
                      homeradius=5000,
                      startseason=yday(ymd("2023-03-15")),
                      endseason=yday(ymd("2023-07-10")),
                      settleEnd = yday(ymd("2023-04-05")),  # end of the settlement period in yday
                      Incu1End = yday(ymd("2023-04-25")),   # end of the first incubation phase in yday
                      Incu2End = yday(ymd("2023-05-15")),  # end of the second incubation phase in yday
                      Chick1End = yday(ymd("2023-06-15")), # end of the first chick phase in yday
                      age =10)         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)
# missinddat<-nest_data_input$summary %>% dplyr::filter(is.na(age_cy)) %>% select(year_id,bird_id,sex,age_cy) %>%
#   filter(year_id %in% indseasondata$year_id)
# indseasondata  %>% dplyr::filter(year_id %in% missinddat$year_id) %>% select(year_id,bird_id,sex,age_cy)
# trackingdata  %>% dplyr::filter(year_id %in% missinddat$year_id) %>% group_by(year_id,bird_id) %>% summarise(N = length(timestamp))

#### STEP 2: identify home ranges
## cannot train model because only 1 year without home range
#nest_data_input$summary$sex<-factor(ifelse(nest_data_input$summary$sex==1,'m','f'), levels=c('m','f'))  ### recreate factor levels for sex to facilitate prediction
hr_model<-NestTool::hr_model
# hr_model_GER<-train_home_range_detection(trackingsummary=nest_data_input$summary,plot=T)
pred_hr<-predict_ranging(model=hr_model$model,trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 3: identify nests
nest_model<-NestTool::nest_model
#nest_model_GER<-train_nest_detection(trackingsummary=nest_data_input$summary,plot=T)
pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=pred_hr) # uses the model trained with our data (automatically loaded in the function)


#### STEP 4: determine outcome
succ_model<-NestTool::succ_model
pred_succ<-predict_success(model=succ_model$model,nestingsummary=pred_nest, nest_cutoff=succ_model$nest_cutoff) # uses the model trained with our data (automatically loaded in the function)

#### STEP 5: extract weekly movement metrics for manual classification
move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                     nest_locs=nest_data_input$pot_nests,
                                     inddata=pred_succ,
                                     uncertainty=0.25,
                                     nestradius=150,
                                     startseason=yday(ymd("2023-03-15")),
                                     endseason=yday(ymd("2023-07-10")))

#### STEP 6: use ShinyApp to inspect all questionable individuals
# ?movement_visualisation
movement_visualisation(trackingdata=nest_data_input$movementtrack,
                       nest_locs=nest_data_input$pot_nests,
                       inddata=pred_succ,
                       move_metrics = move_metrics,
                       uncertainty = 0.25,
                       output_path="NestTool_VALIDATION_Saxony_nest_success_output.csv")
# 
# ### to find out where the file is stored:
# here::here()

#### STEP 7: summarise the demographic parameters for the population

## READ IN AND COMBINE DATA OF MANUALLY CLASSIFIED AND AUTOMATICALLY CLASSIFIED DATA
# MANUAL<-fread(here::here("NestTool_VALIDATION_Saxony_nest_success_output.csv")) %>%
#   rename(ManNest=Nest,ManSuccess=Success) %>%
#   select(year_id,ManNest,ManSuccess)
# ALL<-pred_succ %>% select(year_id,hr_prob,nest_prob,succ_prob) %>%
#   left_join(MANUAL, by='year_id') %>%
#   mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="Yes"),1,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="Yes"),1,succ_prob)) %>%
#   mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="No"),0,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="No"),0,succ_prob)) %>%
#   mutate(HR=ifelse(hr_prob>0.5,1,0),Nest=ifelse(nest_prob>0.5,1,0),Success=ifelse(succ_prob>0.5,1,0))



##### COMBINE PREDICTIONS FOR NEST SUCCESS FROM SUISSE AND GERMAN MODEL
ALL<-pred_succ_SUI %>% select(year_id,hr_prob,nest_prob,succ_prob) %>%
  mutate(HR=ifelse(hr_prob>0.5,1,0),Nest=ifelse(nest_prob>0.5,1,0),Success=ifelse(succ_prob>0.5,1,0))
ALL<-pred_succ %>% select(year_id,succ_prob) %>%
  mutate(Success_GER=ifelse(succ_prob>0.5,1,0)) %>%
  rename(succ_prob_GER=succ_prob) %>%
  left_join(ALL,by="year_id")

end_time <- Sys.time()
runtimeSAX<-as.numeric(end_time - start_time,units="secs")




### check outlier 
pred_succ %>% filter(median_day_dist_to_max_night>10000) %>% select(year_id,hr_prob,nest_prob,succ_prob) %>% left_join(indseasondata, by="year_id")




save.image("NestTool2/NestToolValidation_GER.RData")  
load("NestTool2/NestToolValidation_GER.RData")



