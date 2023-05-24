####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### TEST WORKFLOW OF NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
install.packages("devtools", dependencies = TRUE) 
library(devtools)
devtools::install_github("steffenoppel/REKI", dependencies=TRUE) # development version - add argument 'build_vignettes = FALSE' to 

## set root folder for project
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI")

## check that the help files load - this does not work in RStudio!!
?data_prep


### LOAD THE TRACKING DATA AND INDIVIDUAL SEASON SUMMARIES 
# trackingdata<-fread(here("output/02_preprocessing/03_milvus_combined.csv"))
# indseasondata<-fread(here("output/01_validation/03_validation_combined.csv")) %>%
#   mutate(nest = case_when(nest_id > 0 ~ "nest",
#                         nest_id == 0 ~ "no nest")) %>%
#   mutate(HR = case_when(home_range_id > 0 ~ "yes",
#                         home_range_id == 0 ~ "no"))

# THURINGIAN BIRDS
trackingdata<-fread(here("output/02_preprocessing/04_milvus_thuringia.csv"))
# make up indseasondata as input is now mandatory
indseasondata <- trackingdata %>% group_by(year_id) %>%
 summarise(sex=first(sex))

#### STEP 1: prepare data - this takes approximately 15 minutes
nest_data_input<-data_prep(trackingdata=trackingdata,
                      indseasondata=indseasondata,
                      latboundary=47,
                      longboundary=6,
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


#### STEP 3: identify home ranges
pred_hr<-predict_ranging(trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 5: identify nests
pred_nest<-predict_nesting(trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 7: determine outcome
pred_succ<-predict_success(nestingsummary=pred_nest) # uses the model trained with our data (automatically loaded in the function)

#### STEP 8: extract weekly movement metrics for manual classification
move_metrics<-move_metric_extraction(trackingdata=nest_data_input$movementtrack,
                                     nest_locs=nest_data_input$pot_nests, 
                                     inddata=pred_succ,
                                     uncertainty=0.25,
                                     nestradius=50,
                                     startseason=70,endseason=175)

#### STEP 9: use ShinyApp to inspect all questionable individuals
movement_visualisation(pred_succ = pred_succ,
                       movementtrack = nest_data_input$movementtrack,
                       pot_nests = nest_data_input$pot_nests,
                       milvus_5d_move_metrics = move_metrics,
                       uncertainty_success = 0.25,
                       uncertainty_nest = 0.3,
                       output_path="output/05_full_run_THU/nest_success_output.csv")
