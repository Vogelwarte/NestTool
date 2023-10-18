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
library(moveVis)
library(move)
require(maps)
library(plotly)

## set root folder for project
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis")



# LOAD EXAMPLE TRACKING DATA
trackingdata<-NestTool::kite.tracks
indseasondata <- NestTool::kite.nesting

# add color variable for males and females
tracks <- trackingdata %>% mutate(sex=indseasondata$sex[match(year_id,indseasondata$year_id)]) %>%
  mutate(colour= if_else(sex=="f",'#e41a1c','#377eb8')) %>%
  arrange(year_id,timestamp) %>%
  filter(year_id=="2017_65")

# use df2move to convert the data.frame into a moveStack
dm = df2move(tracks, proj = "+proj=longlat +datum=WGS84",
             x = 'long_wgs', y = 'lat_wgs', time = 'timestamp', 
             track_id = 'year_id',removeDuplicatedTimestamps=T)

# align move_data to a uniform time scale
move_data <- align_move(dm, res = 1, digit = 0, unit = "hours")


# add information for males and females
move_data@data$Name<-gsub("X", "", move_data@trackId)
move_data@data$sex<-indseasondata$sex[match(move_data@data$Name,indseasondata$year_id)]
move_data@data$colour<-if_else(move_data@data$sex=="f",'#e41a1c','#377eb8')
move_data@data$Month<-month.abb[month(move_data@data$time)]



# create spatial frames 
get_maptypes()
#move_data@bbox
#extent = extent(-3,9,38,48)
#To use mapbox maps, you need to register for a free mapbox account and get a token key, which can be inserted below
frames <- frames_spatial(move_data, alpha = 1, map_res = 1, margin_factor = 1.2,
                         #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1Ijoic3RlZmZlbjEwODAiLCJhIjoiY2swbThrOHN4MTE0cTNkbXozNTN4Mmt0MyJ9.g88XgM96l8oEse7rhUFrjg",
                         #map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoiZWJ1ZWNobGV5IiwiYSI6ImNqc2xiZXYxejBxanA0NHBpOWhndnRzbDMifQ.JKpJkhVzqWqJbgjNZzLKnA",
                         map_service = "osm", map_type = "terrain_bg",
                         equidistant = F,
                         path_size = 2, path_end = "round", path_join = "round", #path_fade = T, 
                         #path_colours = c('red', 'green', '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#000120'),
                         path_colours = NA,
                         tail_length = 75, tail_size = .3, #tail_colour = '#ff7f00', trace_show = T, trace_colour = '#ff7f00', 
                         path_legend = FALSE)



# add axis labels and scalebar
# frames <- add_labels(frames, title = "Egyptian Vulture autumn migration",
#             # caption = "Egyptian Vulture New LIFE (LIFE16 NAT/BG/000152, www.LifeNeophron.eu) \n
#             #             funded by the European Union and co-funded by the A. G. Leventis Foundation") 
frames <- add_scalebar(frames, height = 0.01, distance = 1.5, x = 5, y = 5, label_margin = 2, colour = "white") # add a scale bar
frames <- add_labels(frames, x = "Longitude", y = "Latitude")


### create monthly labels
## the default function 'add_labels' is not temporally dynamic!
length(unique(move_data@data$time))
monthlabel<-month.name[month(unique(move_data@data$time))]

for (f in 1: length(frames)){
  frames[[f]]$labels$title <- sprintf("Red Kite in %s",monthlabel[f])
}

frames[[1620]]

# animate frame
suggest_formats()
animate_frames(frames, out_file = "C:\\Users\\sop\\OneDrive - Vogelwarte\\REKI\\Analysis\\NestTool2\\output\\REKI_brood_animation.mov", overwrite = TRUE,
               fps = 120, res = 500, width = 4000, height = 3000)










# CREATE A GRID TO SHOW DATA INTENSITY -------------------------------------------------------------------------

## create hexagon grid at 0.1 degree resolution - convert to epsg::3035 if you want to specify km
grid <- swisstracks %>% 
  st_make_grid(cellsize = 0.1, what = "polygons",
               square = FALSE) # This statements leads to hexagons

tab <- st_intersects(grid, swisstracks)
lengths(tab)
countgrid <- st_sf(n = lengths(tab), geometry = st_cast(grid, "MULTIPOLYGON")) %>%
  filter(n>0)
summary(log(countgrid$n+1))





# CREATE A SIMPLE LEAFLET MAP TO SHOW DATA INTENSITY -------------------------------------------------------------------------


pal <- colorNumeric(c("cornflowerblue","firebrick"), seq(0,10))

map <- leaflet(options = leafletOptions(zoomControl = F)) %>% #changes position of zoom symbol
  setView(lng = mean(st_coordinates(swisstracks)[,1]), lat = mean(st_coordinates(swisstracks)[,2]), zoom = 10) %>%
  htmlwidgets::onRender("function(el, x) {L.control.zoom({ 
                           position: 'bottomright' }).addTo(this)}"
  ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
  addProviderTiles("Esri.WorldImagery", group = "Satellite",
                   options = providerTileOptions(opacity = 0.6, attribution = F,minZoom = 5, maxZoom = 20)) %>%
  addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F,minZoom = 5, maxZoom = 15)) %>%  
  addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%  
  
  addPolygons(
    data=countgrid,
    stroke = TRUE, color = ~pal(log(n+1)), weight = 1,
    fillColor = ~pal(log(n+1)), fillOpacity = 0.5,
    popup = ~ paste0(countgrid$n, " GPS locations")
  ) %>%
  
  addCircleMarkers(
    data=swisstracks,
    radius = 2,
    stroke = TRUE, color = "black", weight = 0.5,
    fillColor = "grey75", fillOpacity = 0.5,
    popup = ~ paste0("year ID: ", swisstracks$id, "<br>", swisstracks$t_)
  ) %>%
  
  addLegend(
    position = "topleft",
    pal = pal,
    values = log(countgrid$n+1),
    opacity = 1,
    title = "Intensity of tracking <br>locations (log scale)"
  )

map




# SAVE MAP AS SHAPEFILE, HTML OR KML -------------------------------------------------------------------------

## save as shp or kml
st_write(countgrid, dsn = "REKI_GPS_locs.shp", layer = "REKI_GPS_locs.shp", driver = "ESRI Shapefile")
st_write(countgrid,"REKI_GPS_locs.kml",append=FALSE)

## save leaflet map as html
htmltools::save_html(html = map, file = "REKI_GPS_locs.html")
mapview::mapshot(map, url = "REKI_GPS_locs.html")


