####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### VALIDATION OF NESTTOOL PACKAGE USING DATA FROM GERMANY ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### VALIDATION DATA PROVIDED BY THOMAS PFEIFFER FROM THüRINGEN

### analysis was done blind - the outcome of seasons was unknown to the analyst


# install.packages("devtools", dependencies = TRUE)
# library(devtools)
# devtools::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
# library(remotes)
# options(download.file.method="wininet")
# remotes::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process

library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(NestTool)
library(pROC)
library(RANN)

# LOAD AND COMPILE TRACKING DATA
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU")
#setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU")
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
                      minlocs=500,
                      nestradius=250,
                      homeradius=5000,
                      startseason=yday(ymd("2023-03-15")),
                      endseason=yday(ymd("2023-07-20")),
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
                       output_path="C:/Users/sop/OneDrive - Vogelwarte/NestTool_VALIDATION_THU2_nest_success_output.csv")
# 
# ### to find out where the file is stored:
# here::here()

#### STEP 7: summarise the demographic parameters for the population

## READ IN AND COMBINE DATA OF MANUALLY CLASSIFIED AND AUTOMATICALLY CLASSIFIED DATA
MANUAL<-fread(here::here("C:/Users/sop/OneDrive - Vogelwarte/NestTool_VALIDATION_THU2_nest_success_output.csv")) %>%
  rename(ManualNest=Nest,ManualSuccess=Success) %>%
  select(year_id,ManualNest,ManualSuccess)
ALL<-pred_succ %>% select(year_id,hr_prob,nest_prob,succ_prob) %>%
  left_join(MANUAL, by='year_id') %>%
  mutate(HR=ifelse(hr_prob>0.5,1,0),Nest=ifelse(nest_prob>0.5,1,0),Success=ifelse(succ_prob>0.5,1,0)) %>%
  mutate(Nest=ifelse((!is.na(ManualNest) & ManualNest=="Yes"),1,Nest), Success=ifelse((!is.na(ManualSuccess) & ManualSuccess=="Yes"),1,Success)) %>%
  mutate(Nest=ifelse((!is.na(ManualNest) & ManualNest=="No"),0,Nest), Success=ifelse((!is.na(ManualSuccess) & ManualSuccess=="No"),0,Success)) %>%
  separate(year_id, c("year", "bird_id"), sep = "_", remove=T)

fwrite(ALL,"C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU/THU_nest_validation_predictions.csv")




##### EXTRACT DATA FROM IND WITH INSUFFICIENT DATA
indseasondata %>% filter(!(bird_id %in% ALL$bird_id)) %>%
  left_join(trackingdata, by=c("year_id","bird_id")) %>%
  group_by(year_id,bird_id,sex,age_cy) %>%
  summarise(first=min(timestamp),last=max(timestamp),n_locs=length(timestamp))



save.image("NestTool2/NestToolValidation_GER.RData")  
load("NestTool2/NestToolValidation_GER.RData")


#### troubleshoot data prep to understand why nest location is off
milvus=trackingdata %>% #filter(year_id %in% c("2021_6674","2023_4532")) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                date = as.Date(timestamp, tz = "UTC"),
                week = as.integer(format(date, format = "%W")),
                year_week = format(date, format = "%Y-%W"),
                year_week_id = paste0(format(date, format = "%Y_%W"), "_", bird_id),
                date_id = paste0(date, "_", bird_id),
                event_id=seq_along(timestamp),
                year_day = lubridate::yday(timestamp)) %>%
  dplyr::filter(long_wgs>longboundary &  lat_wgs>latboundary)   ## remove all locations from Spain and southern France
"2018_5081"
"2020_5081"
"2021_5081"
"2021_6369"
"2021_8165"
"2022_52172"
"2022_6369"




##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## CALCULATE DISTANCE BETWEEN PREDICTED AND ACTUAL NEST LOCATION #################################
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
### read in nest locations provided by Thomas Pfeiffer on 4 Oct 2023
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU")
setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2/data/valdat/BlindValidationTHU")
nests<- read_excel("THU_nest_coordinates.xlsx", sheet="ALL") %>%
  dplyr::mutate(year_id=paste(year,bird_id,sep="_")) %>%
  filter(!is.na(lat)) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(3035) %>%
  arrange(year_id)

pred_nests<-nest_data_input$pot_nests %>%
  dplyr::rename(year_id=id) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 3035)


# ##### CALCULATE THE DISTANCE BETWEEN ACTUAL AND PREDICTED NESTS

PRED_NESTS <-  pred_nests %>% 
  filter(year_id %in% nests$year_id) %>%
  arrange(year_id) %>%
  mutate(dist_real_nest = st_distance(.,nests, by_element=T)) %>%
  arrange(desc(as.numeric(dist_real_nest))) %>%
  st_transform(4326)

PRED_NESTS


##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## PLOT PREDICTED AND ACTUAL NEST LOCATION   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################

library(leaflet)

out <- leaflet(options = leafletOptions(zoomControl = F)) %>% #changes position of zoom symbol
  setView(lng = mean(st_coordinates(nests %>%  st_transform(4326))[,1]),
          lat = mean(st_coordinates(nests %>%  st_transform(4326))[,2]),
          zoom = 8) %>%
  htmlwidgets::onRender("function(el, x) {L.control.zoom({
                         position: 'bottomright' }).addTo(this)}"
  ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
  addProviderTiles("Esri.WorldImagery", group = "Satellite",
                   options = providerTileOptions(opacity = 0.6, attribution = F,minZoom = 5, maxZoom = 20)) %>%
  addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F,minZoom = 5, maxZoom = 15)) %>%
  addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%

  addCircleMarkers(
    data = trackingdata  %>% sf::st_as_sf(coords = c("long_wgs", "lat_wgs"), crs = 4326),
    radius = 4,
    color = "lightgreen",
    weight = 0.5,
    opacity = 0.7,
    fillColor = "lightgreen",
    fillOpacity = 0.7
  ) %>%

  addCircleMarkers(
    data = PRED_NESTS,
    radius = 6,
    color = "firebrick",
    weight = 0.5,
    opacity = 0.7,
    fillColor = "firebrick",
    fillOpacity = 0.7,
    popup = ~paste0("year_id: ", year_id, " - ", dist_real_nest)
  ) %>%


  addCircleMarkers(
    data = nests %>% st_transform(4326),
    radius = 6,
    color = "orange",
    weight = 0.5,
    opacity = 0.7,
    fillColor = "orange",
    fillOpacity = 0.7,
    popup = ~paste0("year_id: ", year_id)
  ) %>%

  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F))

out
