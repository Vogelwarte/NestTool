##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## ANIMATE SEASONAL RED KITE TRACKING DATA           ######################################
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
### this script aims to create a simple animation of seasonal movements
### hope is that this could be incorporated in the Shiny App of NestTool

library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(NestTool)
#library(moveVis)
#library(move)
require(maps)
library(plotly)
library(magrittr)
library(tibble)
library(leaflet)
library(leaflet.extras2)
library(sf)


## set root folder for project
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis")



# LOAD EXAMPLE TRACKING DATA
trackingdata<-NestTool::kite.tracks
indseasondata <- NestTool::kite.nesting

# # add color variable for males and females
# tracks <- trackingdata %>% mutate(sex=indseasondata$sex[match(year_id,indseasondata$year_id)]) %>%
#   mutate(colour= if_else(sex=="f",'#e41a1c','#377eb8')) %>%
#   arrange(year_id,timestamp) %>%
#   filter(year_id=="2017_65")
# 
# # use df2move to convert the data.frame into a moveStack
# dm = df2move(tracks, proj = "+proj=longlat +datum=WGS84",
#              x = 'long_wgs', y = 'lat_wgs', time = 'timestamp', 
#              track_id = 'year_id',removeDuplicatedTimestamps=T)
# 
# # align move_data to a uniform time scale
# move_data <- align_move(dm, res = 1, digit = 0, unit = "hours")
# 
# 
# # add information for males and females
# move_data@data$Name<-gsub("X", "", move_data@trackId)
# move_data@data$sex<-indseasondata$sex[match(move_data@data$Name,indseasondata$year_id)]
# move_data@data$colour<-if_else(move_data@data$sex=="f",'#e41a1c','#377eb8')
# move_data@data$Month<-month.abb[month(move_data@data$time)]
# 
# 
# 
# # create spatial frames 
# get_maptypes()
# #move_data@bbox
# #extent = extent(-3,9,38,48)
# #To use mapbox maps, you need to register for a free mapbox account and get a token key, which can be inserted below
# frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
#                          #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
#                          #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1Ijoic3RlZmZlbjEwODAiLCJhIjoiY2swbThrOHN4MTE0cTNkbXozNTN4Mmt0MyJ9.g88XgM96l8oEse7rhUFrjg",
#                          #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
#                          map_service = "osm", map_type = "terrain_bg",
#                          equidistant = F,
#                          path_size = 2, path_end = "round", path_join = "round", #path_fade = T, 
#                          #path_colours = c('red', 'green', '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120'),
#                          path_colours = NA,
#                          tail_length = 75, tail_size = .3, #tail_colour = '#ff7f00', trace_show = T, trace_colour = '#ff7f00', 
#                          path_legend = FALSE)
# 
# 
# 
# # add axis labels and scalebar
# # frames <- add_labels(frames, title = "Egyptian Vulture autumn migration",
# #             # caption = "Egyptian Vulture New LIFE (LIFE16 NAT/BG/000152, www.LifeNeophron.eu) \n
# #             #             funded by the European Union and co-funded by the A. G. Leventis Foundation") 
# frames <- add_scalebar(frames, height = 0.01, distance = 1.5, x = 5, y = 5, label_margin = 2, colour = "white") # add a scale bar
# frames <- add_labels(frames, x = "Longitude", y = "Latitude")
# 
# 
# ### create monthly labels
# ## the default function 'add_labels' is not temporally dynamic!
# length(unique(move_data@data$time))
# monthlabel<-month.name[month(unique(move_data@data$time))]
# 
# for (f in 1: length(frames)){
#   frames[[f]]$labels$title <- sprintf("Red Kite in %s",monthlabel[f])
# }
# 
# frames[[1620]]
# 
# # animate frame
# 
# ### THIS TAKES ABOUT 45 MIN PER SEASON!!
# 
# suggest_formats()
# animate_frames(frames, out_file = "C:\\Users\\sop\\OneDrive - Vogelwarte\\REKI\\Analysis\\NestTool2\\output\\REKI_brood_animation.mov", overwrite = TRUE,
#                fps = 160, res = 500, width = 4000, height = 3000)
# 




# ### TRY ALTERNATIVES:
# https://stackoverflow.com/questions/30370840/animate-map-in-r-with-leaflet-and-xts
# https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
# https://stackoverflow.com/questions/35512306/r-maps-with-time-slider



# ### THIS EXAMPLE WORKS PERFECTLY BUT ONLY OVER 1 HOUR
# ## FALLS APART WHEN TRIED TO DO IT OVER 1 MONTH
# # how many test data points to create
# num_points <- 100
# 
# # set up an sf object with a datetime column matching each point to a date/time
# # make the GPS tracks interesting
# df <- tibble::tibble(temp = (1:num_points),
#                      lat = seq(from = 45, to = 46, length.out = num_points) + .1*sin(temp),
#                      lon = seq(from = -75, to = -75.5, length.out = num_points) + .1*cos(temp),
#                      datetime = seq(from = lubridate::ymd_hms("2021-09-01 8:00:00"),
#                                     to = lubridate::ymd_hms("2021-09-01 9:00:00"),
#                                     length.out = num_points)) %>%
#   sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE)
# 
# # create a leaflet map and add an animated marker
# leaflet() %>%
#   addTiles() %>%
#   leaflet.extras2::addPlayback(data = df,
#                                time = "datetime",
#                                options = leaflet.extras2::playbackOptions(speed = 100000,
#                                                                           tickLen=1000*60*60*5,  ## hourly tick lengths stated in milliseconds
#                                                                           maxInterpolationTime=1000*60*60*5, ## 5 hrs interpolation time
#                                                                           staleTime=1000*60*60*96))  ### 3 days before being faded out
# 

# EXTRACT DATA PREPARATION FROM movement_visualisation
unique(trackingdata$year_id)
milvus_track <- trackingdata %>%
  dplyr::filter(year_id=="2019_461") %>%
  sf::st_as_sf(coords = c("long_wgs", "lat_wgs"), crs = 4326) %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])
#milvus_track <- milvus_track[1:1000,] ## trying whether it works for few locations
milvus_track$timestamp


### very basic animation ###
leaflet() %>%
  addTiles() %>%
  leaflet.extras2::addPlayback(data = milvus_track,
                               time = "timestamp",
                               options = leaflet.extras2::playbackOptions(color = "blue",
                                                                          radius = 2,
                                                                          speed = 20000000,
                                                                          tickLen=1000*60*60,  ## hourly tick lengths stated in milliseconds
                                                                          maxInterpolationTime=1000*60*60*48, ## 5 hrs interpolation time
                                                                          staleTime=1000*60*60*240))  ### 3 days before being faded out


### trying a more nuanced animation ###
leaflet() %>%
  addTiles() %>%
  leaflet.extras2::addPlayback(data = milvus_track,
                               time = "timestamp",
                               icon=makeIcon(iconUrl="https://images.phylopic.org/images/aec14bd0-7666-45e2-8a30-17fdd0c79578/vector.svg",
                                             iconWidth=30,
                                             iconHeight=18,
                                             iconAnchorX=-1,
                                             iconAnchorY=-1),
                               options = leaflet.extras2::playbackOptions(color = "firebrick",
                                                                          fill = "firebrick",
                                                                          radius = 1,
                                                                          speed = 10000000,
                                                                          tickLen=1000*60*60,  ## hourly tick lengths stated in milliseconds
                                                                          maxInterpolationTime=1000*60*60*5 ## 5 hrs interpolation time
                                                                          ))  ### 3 days before being faded out



### trying the best possible animation ###
leaflet(options = leafletOptions(zoomControl = F)) %>% #changes position of zoom symbol
  setView(lng = mean(st_coordinates(milvus_track)[,1]), lat = mean(st_coordinates(milvus_track)[,2]), zoom = 12) %>%
  htmlwidgets::onRender("function(el, x) {L.control.zoom({ 
                           position: 'bottomright' }).addTo(this)}"
  ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
  addProviderTiles("Esri.WorldImagery", group = "Satellite",
                   options = providerTileOptions(opacity = 0.6, attribution = F,minZoom = 5, maxZoom = 20)) %>%
  addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F,minZoom = 5, maxZoom = 15)) %>%  
  addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>% 
  
  addPolylines(
    data = milvus_track,
    lat = milvus_track$lat,
    lng = milvus_track$long,
    weight = 1,
    color = "#444444",
    opacity = 0.7,
    group = "Trajectory"
  ) %>%
  
  addCircleMarkers(
    data = milvus_track,
    radius = 2,
    weight = 0.5,
    opacity = 0.4,
    fillOpacity = 0.4,
    group = "Locations"
  ) %>%
  
  leaflet.extras2::addPlayback(data = milvus_track,
                               time = "timestamp",
                               icon=makeIcon(iconUrl="https://images.phylopic.org/images/aec14bd0-7666-45e2-8a30-17fdd0c79578/vector.svg",
                                             iconWidth=30,
                                             iconHeight=18,
                                             iconAnchorX=15,
                                             iconAnchorY=9),
                               options = leaflet.extras2::playbackOptions(tracksLayer = FALSE,
                                                                          speed = 10000000,
                                                                          tickLen=1000*60*60,  ## hourly tick lengths stated in milliseconds
                                                                          maxInterpolationTime=1000*60*60*5 ## 5 hrs interpolation time
                               )) 



##########################  try visualisation in shiny app ################################
## copied from workflow demo
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
source("NestTool2//R//movement_visualisation.r")
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(DT)
movement_visualisation(trackingdata=nest_data_input$movementtrack,
                       nest_locs=nest_data_input$pot_nests, 
                       inddata=pred_succ,
                       move_metrics = move_metrics,
                       uncertainty = 0.25,
                       output_path="NestTool_example_nest_success_output.csv")




