####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### MINIMUM TUTORIAL OF WORKFLOW OF NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########


## FOR FIRST INSTALLATION - if you have not installed NestTool before then you need to do this first:
install.packages("devtools", dependencies = TRUE) 
library(devtools)
devtools::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process


## LOAD PACKAGES
library(data.table)
library(tidyverse)
library(NestTool)


# LOAD EXAMPLE TRACKING DATA FROM THE PACKAGE
trackingdata<-NestTool::kite.tracks %>%
  dplyr::select(year_id,bird_id,timestamp,lat_wgs,long_wgs,lat_eea, long_eea)
indseasondata <- NestTool::kite.nesting %>%
  dplyr::select(year_id,age_cy,sex,HR,nest,success)

head(trackingdata)
head(indseasondata)




#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(
                      # Specify the data
                      trackingdata=trackingdata,
                      indseasondata=indseasondata,
                      age = 10,
                      
                      # Resolution filter
                      minlocs=800,
                      
                      # Spatial domain
                      latboundary=45,
                      longboundary=4,
			                crs_epsg=3035,
                      nestradius=50,
                      homeradius=2000,
                      
                      # Phenology
			                startseason=70,
			                endseason=175,
			                settleEnd = 97,  # end of the settlement period in yday
			                Incu1End = 113,   # end of the first incubation phase in yday
			                Incu2End = 129,  # end of the second incubation phase in yday
                      broodstart= yday(ymd("2023-05-01")),
                      broodend<- yday(ymd("2023-06-01")),
                      Chick1End = 152 # end of the first chick phase in yday
                      )         # age of individuals for which no age is provided with data 

names(nest_data_input$summary)


#### STEP 2: identify nests
nest_model<-NestTool::nest_model
pred_nest<-predict_nesting(model=nest_model$model,
                           trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 3: determine outcome
succ_model<-NestTool::succ_model
pred_succ<-predict_success(model=succ_model$model,nestingsummary=pred_nest, nest_cutoff=0.5) # uses the model trained with our data (automatically loaded in the function)


#### STEP 4: extract weekly movement metrics for manual classification
move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                     nest_locs=nest_data_input$pot_nests, 
                                     inddata=pred_succ,
						                         crs_epsg=3035,
                                     uncertainty=0.25,
                                     nestradius=50,
                                     startseason=70,endseason=175)

#### STEP 5: use ShinyApp to inspect all questionable individuals
movement_visualisation(trackingdata=nest_data_input$movementtrack,
				crs_epsg=3035,
                       nest_locs=nest_data_input$pot_nests, 
                       inddata=pred_succ,			
                       move_metrics = move_metrics,
                       uncertainty = 0.25,
                       output_path="NestTool_example_nest_success_output.csv")



#### STEP 6: summarise the demographic parameters for the population

## READ IN AND COMBINE DATA OF MANUALLY CLASSIFIED AND AUTOMATICALLY CLASSIFIED DATA
MANUAL<-fread("NestTool_example_nest_success_output.csv") %>%
  rename(ManNest=Nest,ManSuccess=Success) %>%
  dplyr::select(year_id,ManNest,ManSuccess)
ALL<-pred_succ %>% select(year_id,nest_prob,succ_prob) %>%
  left_join(MANUAL, by='year_id') %>%
  mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="Yes"),1,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="Yes"),1,succ_prob)) %>%
  mutate(nest_prob=ifelse((!is.na(ManNest) & ManNest=="No"),0,nest_prob), succ_prob=ifelse((!is.na(ManSuccess) & ManSuccess=="No"),0,succ_prob)) %>%
  mutate(Nest=ifelse(nest_prob>0.5,1,0),Success=ifelse(succ_prob>0.5,1,0))

## breeding propensity - what proportion of birds with a homerange have a nesting attempt?
ALL %>% ungroup() %>%
  summarise(Propensity=mean(Nest))

## breeding success - what proportion of birds with a nesting attempt are successful?
ALL %>% dplyr::filter(Nest==1) %>% ungroup() %>%
  summarise(Success=mean(Success))




