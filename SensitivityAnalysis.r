####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### SENSITIVITY ANALYSIS TO TEST EFFECT OF HR AND NEST RADIUS ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

## requested by reviewer
## abandoned parallel approach because it failed - set up sequential loop over weekend instead

library(data.table)
library(tidyverse)
library(devtools)
library(lubridate)
select<-dplyr::select
filter<-dplyr::filter
library(randomForest)
library(ranger)
# library(parallel)
# library(doParallel)
library(NestTool)

## set root folder for project
try(setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2"),silent=T)
try(setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2"),silent=T)


### LOAD THE TRACKING DATA AND INDIVIDUAL SEASON SUMMARIES 
trackingdata<-fread("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/02_preprocessing/03_milvus_combined.csv")
indseasondata<-fread("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI/output/01_validation/03_validation_combined.csv") %>%
  mutate(nest = case_when(nest_id > 0 ~ "nest",
                        nest_id == 0 ~ "no nest")) %>%
  mutate(HR = case_when(home_range_id > 0 ~ "yes",
                        home_range_id == 0 ~ "no"))


### set up sensitivity analysis
nsamp<-c(600,800,1000)
nrad<-c(30,40,50,60,70)
rhr<-c(1000,1500,2000,2500,3000)

sens.ana<-expand.grid(minlocs=nsamp,nestradius=nrad,homeradius=rhr) %>%
  mutate(n=0,acc.HR=NA,acc.nest=NA,acc.succ=NA) %>%
  mutate(SIM=seq_along(minlocs))



### SET UP PARALLEL LOOP 
# n.cores <- 2  ## this is to be run on the server
# cl <- makeCluster(n.cores)
# registerDoParallel(cl)

sens.ana.out <- data.frame()
#   foreach(s = 1:10,
#           .combine='rbind',
#           .packages=c('NestTool','caret','tidyverse','dplyr'),
#           .inorder=FALSE,
#           .verbose=TRUE) %dopar% {
for(s in sens.ana$SIM) {
  output<-sens.ana[sens.ana$SIM==s,]
  #### STEP 1: prepare data - this takes approximately 15 minutes
  nest_data_input<-data_prep(trackingdata=trackingdata,
                             indseasondata=indseasondata,
                             latboundary=45,
                             longboundary=4,
                             crs_epsg=3035,
                             broodstart= yday(ymd("2023-05-01")),
                             broodend<- yday(ymd("2023-06-01")),
                             minlocs=output$minlocs,
                             nestradius=output$nestradius,
                             homeradius=output$homeradius,
                             startseason=70,
                             endseason=175,
                             settleEnd = 97,  # end of the settlement period in yday
                             Incu1End = 113,   # end of the first incubation phase in yday
                             Incu2End = 129,  # end of the second incubation phase in yday
                             Chick1End = 152, # end of the first chick phase in yday
                             age =10)         # age of individuals for which no age is provided with data 
  
  
  output$n<-length(unique(nest_data_input$summary$year_id))
  
  #### STEP 2: train home range model (if training data available)
  trackingsummary<-nest_data_input$summary
  hr_model<-train_home_range_detection(trackingsummary=trackingsummary,plot=T)
  output$acc.HR<-paste(round(hr_model$eval_test$overall[1],3)," (",round(hr_model$eval_test$overall[3],3)," - ",round(hr_model$eval_test$overall[4],3),")", sep="")
  
  #### STEP 3: identify home ranges
  pred_hr<-predict_ranging(model=hr_model$model,trackingsummary=trackingsummary) # if a model has been trained
  
  #### STEP 4: train nest model (if training data available)
  trackingsummary<-nest_data_input$summary
  nest_model<-train_nest_detection(trackingsummary=trackingsummary[!is.na(trackingsummary$nest),],plot=T)
  output$acc.nest<-paste(round(nest_model$eval_test$overall[1],3)," (",round(nest_model$eval_test$overall[3],3)," - ",round(nest_model$eval_test$overall[4],3),")", sep="")
  
  #### STEP 5: identify nests
  pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=pred_hr) # if a model has been trained

  #### STEP 6: train outcome model (if training data available)
  succ_model<-train_nest_success(nestingsummary=pred_nest[pred_nest$success %in% c("yes","no"),],plot=T)
  output$acc.succ<-paste(round(succ_model$eval_test$overall[1],3)," (",round(succ_model$eval_test$overall[3],3)," - ",round(succ_model$eval_test$overall[4],3),")", sep="")
  fwrite(output,"C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/output/SensitivityAnalysisOutput.csv",append=T)
  #return(output)
  sens.ana.out <- bind_rows(sens.ana.out,output)
  rm(nest_data_input,succ_model,nest_model,hr_model,pred_hr,pred_nest,trackingsummary,output)
  gc()
}
# stopCluster(cl)


