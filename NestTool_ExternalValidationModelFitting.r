####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### VALIDATION OF NESTTOOL PACKAGE USING DATA FROM GERMANY ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### VALIDATION DATA PROVIDED BY MARTIN KOLBE AT ROTMILANZENTRUM (GER)
### COMBINED WITH DATA FROM THOMAS PFEIFFER FROM THüRINGEN

### tried to re-build models - does not work with hourly data because too few cases
### but even with 15 min resolution there are too few cases of no HR and no nest -> CANNOT BUILD MODELS
### validation based on extrapolated models from SUI
### only nest success model will be fit for GER

### revised on 5 Oct to include additional data from THU



# install.packages("devtools", dependencies = TRUE)
# library(devtools)
# devtools::install_github("steffenoppel/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(NestTool)
library(pROC)

start_time <- Sys.time()

# LOAD AND FORMAT EXAMPLE TRACKING DATA
try(setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis"),silent=T)
try(setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis"),silent=T)

indseasondata <- read_excel("NestTool2/data/REKI_NestTool_ValidationDataTemplate_RMZ.xlsx") %>%
  rename(bird_id=Bird_ID, HR=Homerange,nest=Nest) %>%
  mutate(sex=ifelse(Sex=="?","m",Sex)) %>% ### randomly assign all unknowns to male
  #mutate(sex=fct_recode(sex, 'm'='m','f'='f')) %>% ### convert sex to factor
  mutate(age_cy=as.integer(ifelse(Age_when_tagged==">3",Year-Tag_year+6,Year-Tag_year+as.numeric(Age_when_tagged)))) %>% ### assume that adult birds were 5 years old when tagged
  mutate(year_id=paste(Year,bird_id,sep="_")) %>%
  mutate(success=ifelse(n_fledglings>0,1,0)) %>%
  select(year_id,bird_id,sex,age_cy,HR,nest,success)

trackingdata<-readRDS("NestTool2/data/REKI_validation_tracks.rds") %>%
  bind_rows(readRDS("NestTool2/data/ANITRA_validation_tracks.rds") %>% select(x_,y_,t_,year_id,burst_) %>% rename(id=year_id)) %>%
  mutate(lat_wgs=y_, long_wgs=x_) %>%
  mutate(timestamp=as.POSIXct(t_)) %>%
  rename(year_id=id) %>%
  mutate(year_id=str_replace(string=year_id,pattern="\xf6",replacement="ö")) %>%   ## takes all special characters and converts them to ü
  mutate(year_id=str_replace(string=year_id,pattern="Mostrichmöhle",replacement="Mostrichmühle")) %>% ## manually replace the ü for ö
  #mutate(bird_id=str_split_i(year_id,pattern="_",i=2)) %>%  ### this does not work because some birds have _03 in their name
  mutate(bird_id=indseasondata$bird_id[match(year_id,indseasondata$year_id)]) %>%
  #mutate(event_id=seq_along(t_)) %>%
  filter(year_id %in% unique(indseasondata$year_id)) %>%
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
dim(trackingdata)
# unique(trackingdata$bird_id)
# unique(trackingdata$year_id)



# ADD BIRDS FROM THURINGEN
try(THUdata<-fread("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/02_preprocessing/04_milvus_thuringia.csv"),silent=T)
try(THUdata<-fread("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/02_preprocessing/04_milvus_thuringia.csv"),silent=T)
##make up indseasondata as input is now mandatory
indseasondata <- THUdata %>% group_by(year_id) %>%
  summarise(sex=first(sex), bird_id=first(bird_id)) %>%
  mutate(HR=1,nest=1,success=1, age_cy=6) %>%  ### all birds bred successfully and were adult
  select(year_id,bird_id,sex,age_cy,HR,nest,success) %>%
  bind_rows(indseasondata)

trackingdata<- THUdata %>% select(year_id,bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
  arrange(year_id,timestamp) %>%
  ungroup() %>%
  bind_rows(trackingdata)
dim(trackingdata)

### ensure correct factor levels of sex to facilitate prediction
indseasondata$sex<-factor(indseasondata$sex, levels=c('m','f'))

### for these birds there are no tracking data
indseasondata[-(which(indseasondata$year_id %in% unique(trackingdata$year_id))),]



#### ADD THE UNSUCCESSFUL DATA FROM THURINGEN SUPPLIED BY THOMAS PFEIFFER IN OCTOBER 2023 ################


# LOAD AND COMPILE TRACKING DATA
try(setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU"),silent=T)
try(setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU"),silent=T)
alldat<-list.files(pattern=".txt")

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

## load nesting information
try(setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU"),silent=T)
try(setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU"),silent=T)
indseasondata<- read_excel("THU_nest_coordinates.xlsx", sheet="ALL") %>%
  rename(nest=breeding) %>%
  dplyr::mutate(year_id=paste(year,bird_id,sep="_")) %>%
  mutate(sex=if_else(bird_id %in% c("3803", "6249a", "6249", "4532", "6674", "8966", "8968", "94754"),"f","m"),
         age_cy=6, ### all birds were adult
         HR=1) %>%  
  select(year_id,bird_id,sex,age_cy,HR,nest,success) %>%
  bind_rows(indseasondata)


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
                      minlocs=500,
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
succ_model_GER<-train_nest_success(nestingsummary=pred_nest,plot=T)
pred_succ<-predict_success(model=succ_model_GER$model,nestingsummary=pred_nest, nest_cutoff=succ_model$nest_cutoff) # uses the model trained with our data (automatically loaded in the function)
pred_succ_SUI<-predict_success(model=succ_model$model,nestingsummary=pred_nest, nest_cutoff=succ_model$nest_cutoff) # uses the model trained with our data (automatically loaded in the function)

#### STEP 5: extract weekly movement metrics for manual classification
# move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
#                                      nest_locs=nest_data_input$pot_nests, 
#                                      inddata=pred_succ,
#                                      uncertainty=0.25,
#                                      nestradius=150,
#                                      startseason=yday(ymd("2023-03-15")),
#                                      endseason=yday(ymd("2023-07-10")))

#### STEP 6: use ShinyApp to inspect all questionable individuals
# ?movement_visualisation
# movement_visualisation(trackingdata=nest_data_input$movementtrack,
#                        nest_locs=nest_data_input$pot_nests,
#                        inddata=pred_succ,
#                        move_metrics = move_metrics,
#                        uncertainty = 0.25,
#                        output_path="NestTool_VALIDATION_Saxony_nest_success_output.csv")
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




##############---------------------------------------------------------------------#################
### STEP 8: EVALUATE PREDICTIVE SUCCESS FROM PROVIDED DATA
##############---------------------------------------------------------------------#################

VALIDAT<-indseasondata %>% rename(HR_true=HR, Nest_true=nest) %>%
  mutate(Success_true=ifelse(success==0,0,1)) %>%
  select(year_id,HR_true,Nest_true,Success_true) %>%
  right_join(ALL, by="year_id")

#### IDENTIFY BEST THRESHOLDS FOR PRED SUCCESS
ROC_val_hr<-roc(data=VALIDAT,response=HR_true,predictor=hr_prob)
HRthresh<-pROC::coords(ROC_val_hr, "best", "threshold")$threshold

ROC_val_nest<-roc(data=VALIDAT,response=Nest_true,predictor=nest_prob)
nestthresh<-pROC::coords(ROC_val_nest, "best", "threshold")$threshold

ROC_val_succ<-roc(data=VALIDAT,response=Success_true,predictor=succ_prob)
succthresh<-pROC::coords(ROC_val_succ, "best", "threshold")$threshold

ROC_val_succ_GER<-roc(data=VALIDAT,response=Success_true,predictor=succ_prob_GER)
succthresh_GER<-pROC::coords(ROC_val_succ_GER, "best", "threshold")$threshold

# VALIDAT %>% filter(Nest=="YES") %>% filter(succ_prob>0.25 & succ_prob<0.75)

VALIDAT<-VALIDAT %>%
  mutate(Nest=ifelse(nest_prob>nestthresh,1,0),Success=ifelse(succ_prob>succthresh,1,0),Success_GER=ifelse(succ_prob_GER>succthresh_GER,1,0)) #HR=ifelse(hr_prob>HRthresh,1,0),

## breeding propensity - what proportion of birds with a homerange have a nesting attempt?
VALIDAT %>% filter(HR==1) %>% ungroup() %>%
  summarise(Propensity=mean(Nest))

## breeding success - what proportion of birds with a nesting attempt are successful?
VALIDAT %>% filter(Nest==1) %>% ungroup() %>%
  summarise(Success=mean(Success), Success_GER=mean(Success_GER))




### 8.1. evaluate home range identification
ggplot(VALIDAT, aes(x=hr_prob,y=HR_true)) +
  geom_point(size=2,position=position_dodge(0.05))
VX <- VALIDAT %>%
  dplyr::mutate(HR_true = as.factor(dplyr::case_when(HR_true==1 ~ "YES",
                                                     HR_true==0 ~ "NO"))) %>%
  dplyr::mutate(HR = as.factor(dplyr::case_when(HR==1 ~ "YES",
                                                     HR==0 ~ "NO")))
caret::confusionMatrix(data = VX$HR_true, reference = VX$HR, positive="YES")
caret::confusionMatrix(data = VX$HR_true[VALIDAT$hr_prob>0.75 | VX$hr_prob<0.25], reference = VX$HR[VX$hr_prob>0.75 | VX$hr_prob<0.25], positive="YES")


### create plot of variables that differ
HRmissid<-VX %>% filter(HR_true!=HR)
table(VX$HR_true)
nest_data_input$summary %>%
  mutate(Prediction=ifelse(year_id %in% HRmissid$year_id,"false","true")) %>%
  select(year_id,Prediction,median_day_dist_to_max_night,
         tottime_nest,
         revisits_day,
         residence_time_night,
         residence_time_day,
         meandayrevisitsBrood,
         revisits_night,
         revisitsSettle,
         timeSettle) %>%
  gather(key="Variable",value="value",-year_id,-Prediction) %>%
  left_join(VX, by="year_id") %>%
  
  ggplot(aes(x=HR_true,y=value,colour=Prediction)) +
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

HRmissid<-VX %>% filter(HR_true!=HR)
table(VX$HR_true)
round(as.numeric(nest_data_input$summary %>%
  filter(year_id %in% HRmissid$year_id)  %>%
  filter(HR==0) %>%
  ungroup() %>%
  select(median_day_dist_to_max_night))/1000,1)


### 8.2. evaluate nesting identification
ggplot(VALIDAT, aes(x=nest_prob,y=Nest_true)) +
  geom_point(size=2,position=position_dodge(0.05))
VX <- VX %>%
  dplyr::mutate(Nest_true = as.factor(dplyr::case_when(Nest_true==1 ~ "YES",
                                                     Nest_true==0 ~ "NO"))) %>%
  dplyr::mutate(Nest = as.factor(dplyr::case_when(Nest==1 ~ "YES",
                                                Nest==0 ~ "NO")))
SAXval_nest<-caret::confusionMatrix(data = VX$Nest_true, reference = VX$Nest, positive="YES")
caret::confusionMatrix(data = VX$Nest_true[VX$nest_prob>0.75 | VX$nest_prob<0.25], reference = VX$Nest[VX$nest_prob>0.75 | VX$nest_prob<0.25], positive="YES")


### create plot of variables that differ
Nestmissid<-VX %>% filter(Nest_true!=Nest)

nest_data_input$summary %>%
  mutate(Prediction=ifelse(year_id %in% Nestmissid$year_id,"false","true")) %>%
  select(year_id,Prediction,median_day_dist_to_max_night,
         tottime_nest,
         revisits_day,
         residence_time_night,
         residence_time_day,
         revisitsIncu1,
         maxtimeawayBrood2km,
         revisitsSettle,
         timeSettle) %>%
  gather(key="Variable",value="value",-year_id,-Prediction) %>%
  left_join(VX, by="year_id") %>%
  
  ggplot(aes(x=Nest_true,y=value,colour=Prediction)) +
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

VX <- VX %>%
  dplyr::mutate(Success_true = as.factor(dplyr::case_when(Success_true==1 ~ "YES",
                                                       Success_true==0 ~ "NO"))) %>%
  dplyr::mutate(Success = as.factor(dplyr::case_when(Success==1 ~ "YES",
                                                  Success==0 ~ "NO"))) %>%
  dplyr::mutate(Success_GER = as.factor(dplyr::case_when(Success_GER==1 ~ "YES",
                                                     Success_GER==0 ~ "NO")))
caret::confusionMatrix(data = VX$Success_true, reference = VX$Success, positive="YES")
caret::confusionMatrix(data = VX$Success_true, reference = VX$Success_GER, positive="YES")
caret::confusionMatrix(data = VX$Success_true[VX$succ_prob>0.75 | VX$succ_prob<0.25], reference = VX$Success[VX$succ_prob>0.75 | VX$succ_prob<0.25], positive="YES")


### create plot of variables that differ
Successmissid<-VX %>% filter(Success_true!=Success)
#Successmissid<-VX %>% filter(Success_true!=ManSuccess)

nest_data_input$summary %>%
  mutate(Prediction=ifelse(year_id %in% Successmissid$year_id,"false","true")) %>%
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
  left_join(VX, by="year_id") %>%
  filter(!is.na(Success_true)) %>%
  
  ggplot(aes(x=Success_true,y=value,colour=Prediction)) +
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





##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## COMPILE THE OUTPUT REPORT   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
library(knitr)
library(markdown)
library(rmarkdown)

## create HTML report for overall summary report
# Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/resources/app/bin/quarto/share/pandoc")
# pandoc_version()
try(setwd("C:\\STEFFEN\\OneDrive - Vogelwarte\\General\\MANUSCRIPTS\\NestTool"),silent=T)
try(setwd("C:\\Users\\sop\\OneDrive - Vogelwarte\\General\\MANUSCRIPTS\\NestTool"),silent=T)
# rmarkdown::render('C:\\Users\\sop\\OneDrive - Vogelwarte\\General\\MANUSCRIPTS\\NestTool\\NestTool_ResultsValidation.Rmd',
#                   output_file = "NestTool_ResultsValidation_GER.html",
#                   output_dir = 'C:\\Users\\sop\\OneDrive - Vogelwarte\\General\\MANUSCRIPTS\\NestTool')


save.image("NestToolValidation_GER.RData")  
load("NestToolValidation_GER.RData")


### WRITE MANUSCRIPT RESULTS SECTION ###

rmarkdown::render('C:\\STEFFEN\\OneDrive - Vogelwarte\\General\\MANUSCRIPTS\\NestTool\\NestTool_ResultsSection_GER.Rmd',
                  output_file = "NestTool_ResultsSection_GER.docx",
                  output_dir = 'C:\\STEFFEN\\OneDrive - Vogelwarte\\General\\MANUSCRIPTS\\NestTool')



