####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### VALIDATION OF NESTTOOL PACKAGE USING DATA FROM GERMANY ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### VALIDATION DATA PROVIDED BY MARTIN KOLBE AT ROTMILANZENTRUM (GER)

# install.packages("devtools", dependencies = TRUE)
# library(devtools)
# devtools::install_github("steffenoppel/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(NestTool)
library(pROC)

?data_prep
?movement_visualisation

# LOAD AND FORMAT EXAMPLE TRACKING DATA
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis")

indseasondata <- read_excel("NestTool2/data/REKI_NestTool_ValidationDataTemplate_RMZ.xlsx") %>%
  rename(bird_id=Bird_ID) %>%
  mutate(sex=ifelse(Sex=="?","m",Sex)) %>% ### randomly assign all unknowns to male
  #mutate(sex=fct_recode(sex, 'm'='m','f'='f')) %>% ### convert sex to factor
  mutate(age_cy=as.integer(ifelse(Age_when_tagged==">3",Year-Tag_year+6,Year-Tag_year+as.numeric(Age_when_tagged)))) %>% ### assume that adult birds were 5 years old when tagged
  mutate(year_id=paste(Year,bird_id,sep="_")) %>%
  select(year_id,bird_id,sex,age_cy,Homerange,Nest,Nest_lat,Nest_long,n_fledglings)

### ensure correct factor levels of sex to facilitate prediction
indseasondata$sex<-factor(indseasondata$sex, levels=c('m','f'))
str(indseasondata$sex)
indseasondata %>% filter(is.na(sex))


trackingdata<-readRDS("NestTool2/data/REKI_validation_tracks.rds") %>%
  bind_rows(readRDS("NestTool2/data/ANITRA_validation_tracks.rds") %>% select(x_,y_,t_,year_id,burst_) %>% rename(id=year_id)) %>%
  mutate(lat_wgs=y_, long_wgs=x_) %>%
  mutate(timestamp=as.POSIXct(t_)) %>%
  rename(year_id=id) %>%
  mutate(bird_id=str_split_i(year_id,pattern="_",i=2)) %>%
  #mutate(event_id=seq_along(t_)) %>%
  filter(!is.na(x_)) %>%
  st_as_sf(coords = c("x_", "y_"), crs=4326) %>%
  st_transform(3035) %>%
  dplyr::mutate(long_eea = sf::st_coordinates(.)[,1],
                lat_eea = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(year_id,bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
  arrange(year_id,timestamp) %>%
  ungroup()
head(trackingdata)
unique(trackingdata$year_id)

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
                      nestradius=50,
                      homeradius=2000,
                      startseason=yday(ymd("2023-03-15")),
                      endseason=yday(ymd("2023-07-10")),
                      settleEnd = yday(ymd("2023-04-05")),  # end of the settlement period in yday
                      Incu1End = yday(ymd("2023-04-25")),   # end of the first incubation phase in yday
                      Incu2End = yday(ymd("2023-05-15")),  # end of the second incubation phase in yday
                      Chick1End = yday(ymd("2023-06-15")), # end of the first chick phase in yday
                      age =10)         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)
nest_data_input$summary %>% dplyr::filter(is.na(age_cy)) %>% select(year_id,bird_id,sex,age_cy)
indseasondata  %>% select(year_id,bird_id,sex,age_cy)

#### STEP 2: identify home ranges
hr_model<-NestTool::hr_model
nest_data_input$summary$sex<-factor(ifelse(nest_data_input$summary$sex==1,'m','f'), levels=c('m','f'))  ### recreate factor levels for sex to facilitate prediction
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
                                     startseason=yday(ymd("2023-03-15")),
                                     endseason=yday(ymd("2023-07-10")))

#### STEP 6: use ShinyApp to inspect all questionable individuals
?movement_visualisation
movement_visualisation(trackingdata=nest_data_input$movementtrack,
                       nest_locs=nest_data_input$pot_nests, 
                       inddata=pred_succ,
                       move_metrics = move_metrics,
                       uncertainty = 0.25,
                       output_path="NestTool_VALIDATION_Saxony_nest_success_output.csv")

### to find out where the file is stored:
here::here()

#### STEP 7: summarise the demographic parameters for the population

## READ IN AND COMBINE DATA OF MANUALLY CLASSIFIED AND AUTOMATICALLY CLASSIFIED DATA
MANUAL<-fread(here::here("NestTool_VALIDATION_Saxony_nest_success_output.csv")) %>%
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



##############---------------------------------------------------------------------#################
### STEP 8: EVALUATE PREDICTIVE SUCCESS FROM PROVIDED DATA
##############---------------------------------------------------------------------#################

VALIDAT<-indseasondata %>% rename(HR_true=Homerange, Nest_true=Nest) %>%
  mutate(Success_true=ifelse(n_fledglings==0,0,1)) %>%
  select(year_id,HR_true,Nest_true,Success_true) %>%
  right_join(ALL, by="year_id")

#### IDENTIFY BEST THRESHOLDS FOR PRED SUCCESS
ROC_val<-roc(data=VALIDAT,response=HR_true,predictor=hr_prob)
HRthresh<-coords(ROC_val, "best", "threshold")$threshold

ROC_val<-roc(data=VALIDAT,response=Nest_true,predictor=nest_prob)
nestthresh<-coords(ROC_val, "best", "threshold")$threshold

ROC_val<-roc(data=VALIDAT,response=Success_true,predictor=succ_prob)
succthresh<-coords(ROC_val, "best", "threshold")$threshold



ALL<-ALL %>%
  mutate(HR=ifelse(hr_prob>HRthresh,1,0),Nest=ifelse(nest_prob>nestthresh,1,0),Success=ifelse(succ_prob>succthresh,1,0))

## breeding propensity - what proportion of birds with a homerange have a nesting attempt?
ALL %>% filter(HR==1) %>% ungroup() %>%
  summarise(Propensity=mean(Nest))

## breeding success - what proportion of birds with a nesting attempt are successful?
ALL %>% filter(Nest==1) %>% ungroup() %>%
  summarise(Success=mean(Success))




### 8.1. evaluate home range identification
ggplot(VALIDAT, aes(x=hr_prob,y=HR_true)) +
  geom_point(size=2,position=position_dodge(0.05))
VALIDAT <- VALIDAT %>%
  dplyr::mutate(HR_true = as.factor(dplyr::case_when(HR_true==1 ~ "YES",
                                                     HR_true==0 ~ "NO"))) %>%
  dplyr::mutate(HR = as.factor(dplyr::case_when(HR==1 ~ "YES",
                                                     HR==0 ~ "NO")))
caret::confusionMatrix(data = VALIDAT$HR_true, reference = VALIDAT$HR, positive="YES")
caret::confusionMatrix(data = VALIDAT$HR_true[VALIDAT$hr_prob>0.75 | VALIDAT$hr_prob<0.25], reference = VALIDAT$HR[VALIDAT$hr_prob>0.75 | VALIDAT$hr_prob<0.25], positive="YES")


### create plot of variables that differ
HRmissid<-VALIDAT %>% filter(HR_true!=HR)

nest_data_input$summary %>%
  mutate(Prediction=ifelse(year_id %in% HRmissid$year_id,"wrong","correct")) %>%
  select(year_id,Prediction,median_day_dist_to_max_night,
         tottime100m,
         revisits_day,
         residence_time_night,
         residence_time_day,
         meandayrevisitsBrood,
         revisits_night,
         revisitsSettle,
         timeSettle) %>%
  gather(key="Variable",value="value",-year_id,-Prediction) %>%
  left_join(VALIDAT, by="year_id") %>%
  
  ggplot(aes(x=Prediction,y=value,colour=HR)) +
  geom_point(position=position_jitterdodge(0.1)) +
  facet_wrap(~Variable, ncol=3, scales="free_y") +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"), 
                 strip.text=ggplot2::element_text(size=14, color="black"),
                 axis.text=ggplot2::element_text(size=16, color="black"), 
                 axis.title=ggplot2::element_text(size=20), 
                 panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.border = ggplot2::element_blank())

ggsave("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/plots/HR_validation_variable_plot.jpg", width=12, height=12)  



### 8.2. evaluate nesting identification
ggplot(VALIDAT, aes(x=nest_prob,y=Nest_true)) +
  geom_point(size=2,position=position_dodge(0.05))
VALIDAT <- VALIDAT %>%
  dplyr::mutate(Nest_true = as.factor(dplyr::case_when(Nest_true==1 ~ "YES",
                                                     Nest_true==0 ~ "NO"))) %>%
  dplyr::mutate(Nest = as.factor(dplyr::case_when(Nest==1 ~ "YES",
                                                Nest==0 ~ "NO")))
caret::confusionMatrix(data = VALIDAT$Nest_true, reference = VALIDAT$Nest, positive="YES")
caret::confusionMatrix(data = VALIDAT$Nest_true[VALIDAT$nest_prob>0.75 | VALIDAT$nest_prob<0.25], reference = VALIDAT$Nest[VALIDAT$nest_prob>0.75 | VALIDAT$nest_prob<0.25], positive="YES")


### create plot of variables that differ
Nestmissid<-VALIDAT %>% filter(Nest_true!=Nest)

nest_data_input$summary %>%
  mutate(Prediction=ifelse(year_id %in% Nestmissid$year_id,"wrong","correct")) %>%
  select(year_id,Prediction,median_day_dist_to_max_night,
         tottime100m,
         revisits_day,
         residence_time_night,
         residence_time_day,
         revisitsIncu1,
         maxtimeawayBrood2km,
         revisitsSettle,
         timeSettle) %>%
  gather(key="Variable",value="value",-year_id,-Prediction) %>%
  left_join(VALIDAT, by="year_id") %>%
  
  ggplot(aes(x=Prediction,y=value,colour=Nest)) +
  geom_point(position=position_jitterdodge(0.1)) +
  facet_wrap(~Variable, ncol=3, scales="free_y") +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"), 
                 strip.text=ggplot2::element_text(size=14, color="black"),
                 axis.text=ggplot2::element_text(size=16, color="black"), 
                 axis.title=ggplot2::element_text(size=20), 
                 panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.border = ggplot2::element_blank())

ggsave("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/plots/Nest_validation_variable_plot.jpg", width=12, height=12)  





### 8.3. evaluate nest success prediction
ggplot(VALIDAT, aes(x=succ_prob,y=Success_true)) +
  geom_point(size=2,position=position_dodge(0.05))

VALIDAT <- VALIDAT %>%
  dplyr::mutate(Success_true = as.factor(dplyr::case_when(Success_true==1 ~ "YES",
                                                       Success_true==0 ~ "NO"))) %>%
  dplyr::mutate(Success = as.factor(dplyr::case_when(Success==1 ~ "YES",
                                                  Success==0 ~ "NO")))
caret::confusionMatrix(data = VALIDAT$Success_true, reference = VALIDAT$Success, positive="YES")
caret::confusionMatrix(data = VALIDAT$Success_true[VALIDAT$succ_prob>0.75 | VALIDAT$succ_prob<0.25], reference = VALIDAT$Success[VALIDAT$succ_prob>0.75 | VALIDAT$succ_prob<0.25], positive="YES")


### create plot of variables that differ
Successmissid<-VALIDAT %>% filter(Success_true!=Success)
Successmissid<-VALIDAT %>% filter(Success_true!=ManSuccess)

nest_data_input$summary %>%
  mutate(Prediction=ifelse(year_id %in% Successmissid$year_id,"wrong","correct")) %>%
  select(year_id,Prediction,
         timeChick2,
         revisitsChick2,
         timeChick1,
         lastvisitDay,
         MCP95Chick1,
         dist_max_day_to_max_night,
         median_day_dist_to_max_night,
         residence_time_night) %>%
  gather(key="Variable",value="value",-year_id,-Prediction) %>%
  left_join(VALIDAT, by="year_id") %>%
  
  ggplot(aes(x=Prediction,y=value,colour=Success)) +
  geom_point(position=position_jitterdodge(0.1)) +
  facet_wrap(~Variable, ncol=3, scales="free_y") +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"), 
                 strip.text=ggplot2::element_text(size=14, color="black"),
                 axis.text=ggplot2::element_text(size=16, color="black"), 
                 axis.title=ggplot2::element_text(size=20), 
                 panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.border = ggplot2::element_blank())

ggsave("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/plots/Success_validation_variable_plot.jpg", width=12, height=12)  

