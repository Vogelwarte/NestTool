####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### TEST WORKFLOW OF NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
install.packages("devtools", dependencies = TRUE) 
library(devtools)
devtools::install_github("steffenoppel/NestTool", dependencies=TRUE) # development version - add argument 'build_vignettes = FALSE' to 

library(NestTool)


## check that the help files load - this does not work in RStudio!!
?data_prep


# LOAD EXAMPLE TRACKING DATA
trackingdata<-NestTool::kite.tracks
indseasondata <- NestTool::kite.nesting

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


#### STEP 2: identify home ranges
hr_model<-NestTool::hr_model
pred_hr<-predict_ranging(model=hr_model$model,trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


#### STEP 3: identify nests
nest_model<-NestTool::nest_model
pred_nest<-predict_nesting(model=nest_model$model,trackingsummary=nest_data_input$summary) # uses the model trained with our data (automatically loaded in the function)


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

