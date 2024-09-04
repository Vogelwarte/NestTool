##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## DOWNLOAD TRACKING DATA AND PREDICT PLAUSIBLE NEST LOCATIONS  #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
## nest_field_detection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Extract recent tracking data to indicate nest location of individually tracked birds
#'
#' \code{nest_field_detection} extracts the latest tracking data from Movebank for individuals
#' to assist with nest searching in the field. This function is designed to work in a ShinyApp through a browser interface where users
#' specify the credentials for their study (Movebank ID, Movebank login name, Movebank password), and a time horizon (in weeks) to retrieve the latest data.
#' The function determines the most plausible location of a potential nest similar to the function \code{\link{data_prep}} by calculating distances and times spent within or
#' outwith the user-specified \code{nestradius}.
#'
#'
#' @param MovebankID numeric. The numeric ID of the Movebank study.
#' @param MovebankUser character. The username to log in to a Movebank account. This user must have full access to the data in \code{MovebankID}.
#' @param MovebankPW character. The username to log in to a Movebank account. This user must have full access to the data in \code{MovebankID}.
#' @param n.weeks numeric. Number of (past) weeks over which tracking data will be retrieved and analysed, i.e. the beginning time point of the data period.
#' The end point of the tracking data period is always the current time as returned by \code{\link{base::Sys.time}}.
#' @param nestradius numeric. Radius around potential nest site in metres. 
#' This radius will determine in which radius times will be calculated, i.e. how long an animal remained within that radius or how long it spent outside this radius.
#' Please consider device accuracy and general movement behaviour of the species when specifying the radius.
#' @return Returns an interactive map that shows a potential nest location for the selected individual.
#'
#' @export
#' @importFrom dplyr filter mutate select arrange case_when
#' @importFrom lubridate yday ymd week
#' @importFrom dplyr arrange mutate filter intersect left_join group_by summarise n select first rename bind_rows ungroup if_else slice_min slice_max ungroup
#' @importFrom purrr reduce pluck
#' @importFrom amt mk_track time_of_day arrange hr_mcp hr_area
#' @importFrom recurse getRecursions getRecursionsAtLocations
#' @importFrom RANN nn2
#' @importFrom sf st_as_sf st_distance st_drop_geometry st_transform st_buffer st_within st_centroid
#' @importFrom stats quantile median
#' @importFrom tidyr replace_na spread gather
#' @importFrom leaflet.extras2 addPlayback playbackOptions
#' @importFrom lubridate yday
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom shiny actionButton column fluidPage fluidRow htmlOutput mainPanel observe observeEvent reactive reactiveValues renderText selectInput shinyApp sidebarLayout sidebarPanel sliderInput titlePanel updateSelectInput
#' @importFrom shinyWidgets chooseSliderSkin materialSwitch prettyRadioButtons
#' @importFrom shinythemes shinytheme
#' @importFrom leaflet addCircleMarkers addLayersControl addLegend addMeasure addPolylines addProviderTiles addScaleBar clearMarkers clearShapes clearTiles colorFactor colorNumeric fitBounds hideGroup labelFormat layersControlOptions leaflet leafletOutput leafletProxy providerTileOptions providers removeControl renderLeaflet scaleBarOptions makeIcon
#' @importFrom htmltools HTML div h3 tags
#' @importFrom DT dataTableOutput renderDataTable datatable dataTableProxy formatStyle selectPage styleEqual
#' @importFrom plotly plotlyOutput renderPlotly style


####### LIBRARIES REQUIRED------------------
library(tidyverse)
library(sf)
library(move2)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(dtplyr)
library(leaflet)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER INPUT (needs to be added to the shiny app) --------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MovebankID<-230545451  #1356790386
MovebankUser<-"Steffen"
n.weeks<-3
nestradius<-50


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOG IN TO MOVEBANK --------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

movebank_store_credentials(username=MovebankUser, key_name = getOption("move2_movebank_key_name"), force = TRUE)
#movebank_download_study_info(study_id=MovebankID)$sensor_type_ids

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD MOVEBANK DATA AND ANIMAL INFO ----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

birds<-move2::movebank_retrieve(study_id=MovebankID, entity_type="individual") %>%
  dplyr::rename(individual_id=id,bird_id=local_identifier)  %>%
  dplyr::mutate(bird_id=as.numeric(as.character(bird_id))) %>%
  dplyr::select(individual_id,comments, bird_id,ring_id,sex,latest_date_born) 



locs<-move2::movebank_retrieve(study_id=MovebankID,
                         entity_type="event",
                         #sensor_type_id=if_else("GPS" %in% movebank_download_study_info(study_id=MovebankID)$sensor_type_ids,"gps",),   ## may need to define sensor if there are multiple?
                         timestamp_start=Sys.time()-weeks(n.weeks),
                         timestamp_end=Sys.time(),
                         progress=T) %>%
  dplyr::left_join(birds, by="individual_id") %>%
  dplyr::mutate(tag_year=as.numeric(comments)) %>%
  dplyr::mutate(age_cy=as.integer((timestamp-latest_date_born)/365)) %>%
  dplyr::select(bird_id,ring_id,sex,age_cy,timestamp,location_lat,location_long) %>% mutate(year=year(timestamp)) %>%
  mutate(bird_id=as.character(bird_id)) %>%
  mutate(year_id=paste(year,bird_id,sep="_")) %>%
  rename(long_wgs=location_long,lat_wgs=location_lat) %>%
  mutate(x=long_wgs,y=lat_wgs) %>%
  filter(!is.na(x)) 
head(locs)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AUTO-DETECT best EPSG CODE derived from average lat and long -------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## taken from: https://stackoverflow.com/questions/58828828/auto-detect-coordinate-reference-system-based-on-coordinates-in-gpx-file

tf.avg.lat <- mean(locs$lat_wgs)
tf.avg.lon <- mean(locs$long_wgs)

EPSG <- 32700-round((45+tf.avg.lat)/90,0)*100+round((183+tf.avg.lon)/6,0)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREPARE DATA FOR NEST DETECTION WITH PLANAR COORDINATES -------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CREATE TRACKING DATA WITH REQUIRED FIELDS ----------
trackingdata<- locs %>%
  st_as_sf(coords = c("x", "y"), crs=4326) %>%
  st_transform(EPSG) %>%
  dplyr::mutate(long_eea = sf::st_coordinates(.)[,1],
                lat_eea = sf::st_coordinates(.)[,2]) %>%
  # st_drop_geometry() %>%
  select(year_id,bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
  arrange(year_id,timestamp) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                date = as.Date(timestamp, tz = "UTC"),
                week = as.integer(format(date, format = "%W")),
                year_week = format(date, format = "%Y-%W"),
                year_week_id = paste0(format(date, format = "%Y_%W"), "_", bird_id),
                date_id = paste0(date, "_", bird_id),
                event_id=seq_along(timestamp),
                year_day = lubridate::yday(timestamp))




################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################
# DETECT NESTS  ---------------------------------------------------
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################


# LOADING DATA -----------------------------------------------------------------
 
# Creating a track
# 3 mins
milvus_track_amt <- trackingdata %>% sf::st_drop_geometry() %>%
  amt::mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = year_id,
    date_id,
    event_id,
    crs = EPSG
  ) %>%
  amt::time_of_day(include.crepuscule = T) %>% # if F, crepuscule is considered as night
  amt::arrange(id, t_)

# creating a night and a day data frame, include crepuscule in day
milvus_track_night <- milvus_track_amt %>%
  dplyr::filter(tod_ %in% c("night","dusk","dawn")) %>%
  dplyr::select(-tod_)

milvus_track_day <- milvus_track_amt %>%
  dplyr::filter(tod_ %in% c("day")) %>%
  dplyr::select(-tod_)


### identify individuals for which there are no night locations and break the data prep
if(length(unique(milvus_track_day$id[which(!(milvus_track_day$id %in% milvus_track_night$id))]))>0){
  mismatches<-unique(milvus_track_day$id[which(!(milvus_track_day$id %in% milvus_track_night$id))])
  milvus_track_night <- milvus_track_night %>% dplyr::filter(!(id %in% mismatches))
  milvus_track_day <- milvus_track_day %>% dplyr::filter(!(id %in% mismatches))
  milvus_track_amt <- milvus_track_amt %>% dplyr::filter(!(id %in% mismatches))
  milvus <- trackingdata %>% dplyr::filter(!(year_id %in% mismatches))
  print(sprintf("Individual(s) %s were removed because they do not have day and night locations.", mismatches))
}


# RECURSIONS DURING NIGHTTIME --------------------------------------------------
# splitting track into a list with each single id grouped to an element
milvus_track_night <- as.data.frame(milvus_track_night)
milvus_track_night$id <- factor(milvus_track_night$id, levels=unique(milvus_track_night$id))  ## required to prevent re-ordering in split
milvus_track_night_list <- split(milvus_track_night, milvus_track_night$id)

# calculating recursions
print(sprintf("Calculating night recursions for %i nocturnal locations",dim(milvus_track_night)[1]))
milvus_night_recurse <- lapply(milvus_track_night_list, function(x)
  recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))

# allocating recurse information to track data frame (1.5 mins)
milvus_track_night$revisits <- NA
milvus_track_night$residence_time <- NA
for (i in 1:length(milvus_night_recurse)) {
  milvus_track_night[milvus_track_night$id == unique(milvus_track_night$id)[i] ,]$revisits <-
    milvus_night_recurse[[i]]$revisits
  milvus_track_night[milvus_track_night$id == unique(milvus_track_night$id)[i] ,]$residence_time <-
    milvus_night_recurse[[i]]$residenceTime
}


# RECURSIONS DURING DAYTIME ----------------------------------------------------
# splitting track into a list with each single id grouped to an element
milvus_track_day <- as.data.frame(milvus_track_day)
milvus_track_day$id <- factor(milvus_track_day$id, levels=unique(milvus_track_day$id))  ## required to prevent re-ordering in split
milvus_track_day_list <- split(milvus_track_day, milvus_track_day$id)

# calculating recursions (1.5 mins)
print(sprintf("Calculating day recursions for %i diurnal locations",dim(milvus_track_day)[1]))
milvus_day_recurse <- lapply(milvus_track_day_list, function(x)
  recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))

# allocating recurse information to track data frame (4 mins)
milvus_track_day$revisits <- NA
milvus_track_day$residence_time <- NA
for (i in 1:length(milvus_day_recurse)) {
  milvus_track_day[milvus_track_day$id == unique(milvus_track_day$id)[i] ,]$revisits <-
    milvus_day_recurse[[i]]$revisits
  milvus_track_day[milvus_track_day$id == unique(milvus_track_day$id)[i] ,]$residence_time <-
    milvus_day_recurse[[i]]$residenceTime
}

# creating a milvus data set for night locations only
milvus_night <- milvus %>%
  dplyr::filter(event_id %in% milvus_track_night$event_id) %>%
  dplyr::left_join(milvus_track_night[,6:8], by = "event_id")

# creating a milvus data set for night day locations only
milvus_day <- milvus %>%
  dplyr::filter(event_id %in% milvus_track_day$event_id) %>%
  dplyr::left_join(milvus_track_day[,6:8], by = "event_id")

# filtering the location with longest residence time during nighttime
suppressWarnings({
  milvus_night_max <- milvus_night %>% #filter(year_id=="2024_228") %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(residence_time_night = max(residence_time),
                     revisits_night = first(revisits[which(residence_time == max(residence_time))]),
                     date_night = first(date[which(residence_time == max(residence_time))]),
                     long_night = first(long_eea[which(residence_time == max(residence_time))]),
                     lat_night = first(lat_eea[which(residence_time == max(residence_time))])
    ) %>%
    dplyr::group_by(year_id) %>%   ## because there are sometimes duplicates, we need to group again and take the one with max revisits for those where time is equal
    dplyr::summarise(revisits_night = max(revisits_night),
                     residence_time_night = first(residence_time_night[which(revisits_night == max(revisits_night))]),
                     date_night = first(date_night[which(revisits_night == max(revisits_night))]),
                     long_night = first(long_night[which(revisits_night == max(revisits_night))]),
                     lat_night = first(lat_night[which(revisits_night == max(revisits_night))])
    )
})

# filtering the location with longest residence time during daytime
suppressWarnings({
  milvus_day_max <- milvus_day %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(residence_time_day = max(residence_time),
                     revisits_day = first(revisits[which(residence_time == max(residence_time))]),
                     date_day = first(date[which(residence_time == max(residence_time))]),
                     long_day = first(long_eea[which(residence_time == max(residence_time))]),
                     lat_day = first(lat_eea[which(residence_time == max(residence_time))])
    ) %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(revisits_day = max(revisits_day),
                     residence_time_day = first(residence_time_day[which(revisits_day == max(revisits_day))]),
                     date_day = first(date_day[which(revisits_day == max(revisits_day))]),
                     long_day = first(long_day[which(revisits_day == max(revisits_day))]),
                     lat_day = first(lat_day[which(revisits_day == max(revisits_day))])
    )
})

# creating sf objects for distance calculation
milvus_night_max_sf <- milvus_night_max %>%
  sf::st_as_sf(coords = c("long_night", "lat_night"), crs = EPSG)
milvus_day_max_sf <- milvus_day_max %>%
  sf::st_as_sf(coords = c("long_day", "lat_day"), crs = EPSG)

# calculating the distance from the day location to the night location
milvus_max_res_time <- milvus_day_max_sf %>%
  dplyr::mutate(dist_day_to_night = as.numeric(sf::st_distance(milvus_day_max_sf, milvus_night_max_sf, by_element = T))) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(milvus_night_max_sf %>% dplyr::select(year_id,revisits_night,residence_time_night,date_night), by = "year_id") %>%
  sf::st_drop_geometry()

rm(milvus, milvus_night,milvus_day_recurse,milvus_night_recurse, milvus_track_night_list, milvus_track_day_list)

##### CALCULATING REVISITS TO POTENTIAL NEST SITE
milvus_track <- as.data.frame(milvus_track_amt %>% dplyr::select(-tod_))
milvus_track$id <- factor(milvus_track$id, levels=unique(milvus_track$id))  ## required to prevent re-ordering in split
milvus_track_list <- split(milvus_track, milvus_track$id)
milvus_track_amt$id <- factor(milvus_track_amt$id, levels=unique(milvus_track_amt$id))  ## required to prevent re-ordering in split
milvus_track_amt_list <- split(milvus_track_amt, milvus_track_amt$id)

# calculating recursions (1.5 mins)
milvus_recurse <- lapply(milvus_track_list, function(x)
  recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))

# # calculating MCPs for total season for each year_id
# milvus_MCP <- lapply(milvus_track_amt_list, function(x)
#   amt::hr_mcp(x = x[1:4], levels = c(0.95,0.99))$mcp)

# allocating recurse information to track data frame (4 mins)
milvus_track$revisits <- NA
milvus_track$residence_time <- NA
for (i in 1:length(milvus_recurse)) {
  milvus_track[milvus_track$id == unique(milvus_track$id)[i] ,]$revisits <-
    milvus_recurse[[i]]$revisits
  milvus_track[milvus_track$id == unique(milvus_track$id)[i] ,]$residence_time <-
    milvus_recurse[[i]]$residenceTime
}


##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####################################################################
########## IDENTIFY PLAUSIBLE NESTS BY SEQUENTIAL FILTERING AND COUNTING LOCS AROUND EACH POT NEST #################################
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####################################################################

## NEW APPROACH OF SEQUENTIAL HIERARCHY INTRODUCED ON 4 OCT 2023
# 1. select top 1% of locs with highest residence time
# 2. select top 25% of remaining locs with highest revisits
# 3. select top 75% of remaining locs with smallest median nearest neighbour distance to minlocs/10 locations
# 4. Of those select the location with the greatest number of neighbours in nestradius

## LOOP OVER EACH INDIVIDUAL YEAR
milvus_pot_nests<-data.frame()
for (i in unique(milvus_track$id)) {
  
  ### subset the data ####
  workdat<- milvus_track %>% 
    dplyr::rename(year_id=id) %>%
    dplyr::filter(year_id==i)
  dim(workdat)
  
  ### calculate nearest neighbour distances
  # identify nearest neighbours and calculate the mean distance to fixed number of nearest neighbours
  nearest <- RANN::nn2(workdat[,1:2],workdat[,1:2],k=min(dim(workdat)[1],25))$nn.dists
  workdat$NN50dist<-apply(nearest,1,median)
  
  ### apply sequential filters of residence time and revisits and nearest neighbour distance
  potnests <- workdat %>% dplyr::arrange(desc(residence_time)) %>% 
    dplyr::filter(residence_time > quantile(residence_time, 0.99)) %>%
    ## need a second filter step here to eliminate locations if they differ too much in time
    dplyr::arrange(desc(revisits)) %>% 
    dplyr::filter(revisits >= quantile(revisits, 0.75)) %>%
    dplyr::arrange(NN50dist) %>% 
    dplyr::filter(NN50dist <= quantile(NN50dist, 0.25))
  dim(potnests)
  
  ### build in failsafe for when number is very low
  if(dim(potnests)[1]==0){
    potnests <- workdat %>% dplyr::arrange(desc(residence_time)) %>%
      dplyr::slice_max(n=1, order_by=residence_time)
  }
  
  ### compare point counts for 3 alternatives
  radnests2 <- RANN::nn2(workdat[,1:2],
                         potnests[,1:2],
                         k=dim(workdat)[1],
                         searchtype="radius", radius = nestradius)$nn.idx
  ## count the number of points
  idmat<-apply(radnests2,1,unique)
  if(is.list(idmat)){
    potnests$npoint<-lengths(apply(radnests2,1,unique))
  }else{
    for(l in 1:dim(idmat)[2]){
      potnests$npoint[l]<-length(idmat[,l]>0)
    }
  }
  
  ### retain only the nest with the highest point count
  ## in case of ties use most time, visits, distance and then earliest point
  milvus_pot_nests<-potnests %>% 
    dplyr::slice_max(order_by=npoint, n=1) %>%
    dplyr::slice_max(order_by=residence_time, n=1) %>%
    dplyr::slice_max(order_by=revisits, n=1) %>%
    dplyr::slice_min(order_by=NN50dist, n=1) %>%
    dplyr::slice_min(order_by=t_, n=1) %>%
    dplyr::select(year_id,event_id,x_,y_,npoint,revisits,residence_time,NN50dist) %>%
    dplyr::rename(id=year_id,x=x_,y=y_) %>%
    #mutate(Type="Revised") %>%
    dplyr::bind_rows(milvus_pot_nests)
}

# Predicted nest locations
milvus_nest <- milvus_pot_nests %>%
  dplyr::rename(year_id=id) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = EPSG) %>%
  sf::st_transform(crs = 4326) %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])



############ PLOT NEST AND TRACKING LOCATION ON LEAFLET MAP LEAFLET MAP------------------------

m2 <- leaflet(options = leafletOptions(zoomControl = F)) %>% #changes position of zoom symbol
  setView(lng = median(st_coordinates(milvus_nest)[,1]), lat = median(st_coordinates(milvus_nest)[,2]), zoom = 11) %>%
  htmlwidgets::onRender("function(el, x) {L.control.zoom({ 
                           position: 'bottomright' }).addTo(this)}"
  ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
  addProviderTiles("Esri.WorldImagery", group = "Satellite",
                   options = providerTileOptions(opacity = 0.6, attribution = F,minZoom = 5, maxZoom = 20)) %>%
  addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F,minZoom = 5, maxZoom = 15)) %>%  
  addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%  
  
  addCircleMarkers(
    data=milvus_nest,
    radius = 10,
    stroke = TRUE, color = "white", weight = 0.8,
    fillColor = "red",fillOpacity = 0.9,
    popup = ~ paste0("bird ID: ", milvus_nest$year_id)
  ) %>%

  addCircleMarkers(
    data=trackingdata %>% st_transform(4326),
    radius = 2,
    stroke = TRUE, color = "white", weight = 0.3,
    fillColor = "firebrick", fillOpacity = 0.5,
    popup = ~ paste0("bird ID: ", trackingdata$year_id)
  ) %>%
  
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F))

m2








# SHINY APP ------------------------------------------------------------------



# USER INTERFACE -------------------------------------------------------------
ui <- shiny::fluidPage(theme = shinythemes::shinytheme("flatly"),
                       # layout of the action buttons
                       htmltools::tags$style(
                         HTML('#save_decision, #zoom{
                       height:65px; width:110px; margin-top:50px;}')
                       ),
                       # layout of the shiny app
                       titlePanel(title = "Individual Bird Movement Trajectory"),
                       sidebarLayout(
                         sidebarPanel(width = 4, style = "height: 675px; position:relative;",
                                      h3("Data selection"),
                                      fluidRow(
                                        # Build selection tool for changing the individual to be displayed
                                        column(width = 12,
                                               selectInput(inputId = "ID", label = "Select Individual",
                                                           choices = sort(as.character(unique(milvus_metrics$ID))), multiple = F)
                                        )
                                      ),
                                      # Change amount of data displayed
                                      fluidRow(
                                        column(width = 6,
                                               prettyRadioButtons(inputId = "data_period",
                                                                  label = "Data Period",
                                                                  choices = list("Whole Period" = 1,
                                                                                 "February" = 2,
                                                                                 "March" = 3,
                                                                                 "April" = 4,
                                                                                 "May" = 5,
                                                                                 "June" = 6,
                                                                                 "July" = 7),
                                                                  selected = 1,
                                                                  shape = "curve",
                                                                  outline = F,
                                                                  fill = T,
                                                                  plain = T
                                               )
                                        ),
                                        # Action button to save the nest decision input
                                        column(width = 6, align = "center",
                                               actionButton(inputId = "zoom",
                                                            label = HTML("Zoom to<br>data extent"))
                                        )
                                      ),
                                      # Show warning if no data is available in the selected period
                                      fluidRow(
                                        column(width = 12,
                                               htmlOutput("warning")
                                        )
                                      ),
                                      # Switch to allow visualisation of day and night points
                                      fluidRow(
                                        column(width = 12,
                                               materialSwitch(
                                                 inputId = "day_night",
                                                 label = "Day & Night Locations", 
                                                 value = F)
                                        )
                                      ),
                                      # Slider to select opacity of background map
                                      fluidRow(
                                        column(width = 12,
                                               chooseSliderSkin("Flat", "#5489C5"),
                                               sliderInput(inputId = "map_slider",
                                                           label = "Background Map Transparency",
                                                           min = 0, max = 1, value = 0.7, step = 0.1)
                                        )
                                      ),
                                      
                                      # Radio button to decide whether bird has a nest or not and if brood was successful or not
                                      fluidRow(
                                        column(width = 6,
                                               prettyRadioButtons(inputId = "nest_decision",
                                                                  label = "Did this bird nest?",
                                                                  choices = list("Yes" = 1,
                                                                                 "No" = 0,
                                                                                 "Uncertain" = 0.5),
                                                                  selected = 1,
                                                                  status = "danger",
                                                                  shape = "curve",
                                                                  outline = F,
                                                                  fill = T,
                                                                  plain = T,inline=T)
                                        ),
                                        # Action button to save the nest decision input
                                        column(width = 6, align = "center",
                                               actionButton(inputId = "save_decision",
                                                            label = HTML("Save<br>decisions"), class = "btn-danger")
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 6, style = "margin-top:-20px",
                                               prettyRadioButtons(inputId = "brood_decision",
                                                                  label = HTML("Was the brood<br>successful?"),
                                                                  choices = list("Yes" = 1,
                                                                                 "No" = 0,
                                                                                 "Uncertain" = 0.5),
                                                                  selected = 1,
                                                                  status = "danger",
                                                                  shape = "curve",
                                                                  outline = F,
                                                                  fill = T,
                                                                  plain = T,inline=T)
                                        )
                                      ),
                         ),
                         mainPanel(width = 8,
                                   # For selected individual: Plotting movement track on a map
                                   fluidRow(style = "padding-right:15px;",
                                            column(width = 12, class = "well", style = "height: 675px; position:relative;",
                                                   h3("Map of selected movement trajectory"),
                                                   leafletOutput("map", height = "575px")
                                            )
                                   )
                         )
                       ),
                       sidebarLayout(
                         mainPanel(width = 7, 
                                   # Table with brood specific metrics of the inspected bird
                                   fluidRow(style = "padding-left:15px;",
                                            column(width = 12, class = "well", style = "height: 750px;",
                                                   h3("List of individuals for manual nest and success classification"),
                                                   div(DT::dataTableOutput("table"),
                                                       # style = "font-size:clamp(10px, 0.6vw, 0.7vw);") # this would allow dynamic adaptation of font size based on window size
                                                       style = "font-size:12px;")
                                            )
                                   )
                         ),
                         sidebarPanel(width = 5,
                                      # Interactive plot with brood relevant metrics
                                      fluidRow(h3("Visualisation of seasonal movement metrics", style = "padding-left:15px;"),
                                               column(width = 12, class = "well", style = "height: 630px;",
                                                      plotlyOutput(outputId = "metrics_plot",
                                                                   height = "600px")
                                               )
                                      )
                         )
                       )
)



# SERVER ---------------------------------------------------------------------
server <- function(input, output, session){
  # Number of the input for the period to inspect
  input_period <- shiny::reactive({input$data_period})
  # ID of the bird/year
  input_id <- shiny::reactive({input$ID})
  # Should data be displayed in day-night mode
  input_day_night <- shiny::reactive({if(input$day_night == T){1} else{0}})
  # Defines map opacity
  input_map_opacity <- shiny::reactive({input$map_slider})
  # Defines data size
  input_size <- shiny::reactive({input$size_slider})
  
  # Creates reactiveValues to store the metrics data frame (empty)
  values <- shiny::reactiveValues(milvus_metrics = NULL)
  # loads the latest metrics data frame
  values$milvus_metrics <- data.table::fread(here::here(output_path))
  # Observes the action button (nest & brood decisions)
  shiny::observeEvent(input$save_decision, {
    # loads the latest metrics data frame
    values$milvus_metrics <- data.table::fread(here::here(output_path))
    # if "yes" is chosen, "yes" is assigned to the selected bird for Nest
    if (input$nest_decision == 1) {
      values$milvus_metrics <- values$milvus_metrics %>%
        dplyr::mutate(Nest = dplyr::case_when(ID == input_id() ~ "Yes",
                                              Nest == "Yes" ~ "Yes",
                                              Nest == "Uncertain" ~ "Uncertain",
                                              Nest == "No" ~ "No"))
      # if "no" is chosen, "no" is assigned to the selected bird for Nest
    } else if (input$nest_decision == 0) {
      values$milvus_metrics <- values$milvus_metrics %>%
        dplyr::mutate(Nest = dplyr::case_when(ID == input_id() ~ "No",
                                              Nest == "Yes" ~ "Yes",
                                              Nest == "Uncertain" ~ "Uncertain",
                                              Nest == "No" ~ "No"))
    } else if (input$nest_decision == 0.5) {
      values$milvus_metrics <- values$milvus_metrics %>%
        dplyr::mutate(Nest = dplyr::case_when(ID == input_id() ~ "Uncertain",
                                              Nest == "Yes" ~ "Yes",
                                              Nest == "Uncertain" ~ "Uncertain",
                                              Nest == "No" ~ "No"))
    }
    # if "yes" is chosen, "yes" is assigned to the selected bird for Success
    if (input$brood_decision == 1) {
      values$milvus_metrics <- values$milvus_metrics %>%
        dplyr::mutate(Success = dplyr::case_when(ID == input_id() ~ "Yes",
                                                 Success == "Yes" ~ "Yes",
                                                 Success == "Uncertain" ~ "Uncertain",
                                                 Success == "No" ~ "No"))
      # if "no" is chosen, "no" is assigned to the selected bird for Success
    } else if (input$brood_decision == 0) {
      values$milvus_metrics <- values$milvus_metrics %>%
        dplyr::mutate(Success = dplyr::case_when(ID == input_id() ~ "No",
                                                 Success == "Yes" ~ "Yes",
                                                 Success == "Uncertain" ~ "Uncertain",
                                                 Success == "No" ~ "No"))
    } else if (input$brood_decision == 0.5) {
      values$milvus_metrics <- values$milvus_metrics %>%
        dplyr::mutate(Success = dplyr::case_when(ID == input_id() ~ "Uncertain",
                                                 Success == "Yes" ~ "Yes",
                                                 Success == "Uncertain" ~ "Uncertain",
                                                 Success == "No" ~ "No"))
    }
    # saves the data frame with the information if bird has a nest
    data.table::fwrite(values$milvus_metrics, here::here(output_path), row.names = F)
  })
  
  # Updates input selection (ID): Completed birds are grouped at the bottom
  shiny::observe({
    # The if else is only necessary to avoid the following case:
    # Two groups are shown in the selection. When a group consists of only one
    # item, the group name is shown instead of the item.
    if (length(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                            !is.na(values$milvus_metrics$Success),]$ID)) == 1) {
      # covers the case when there is only one item in the "Complete" group
      shiny::updateSelectInput(inputId = "ID", label = "Select Individual",
                               choices = list(
                                 Incomplete = sort(as.character(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                                                               is.na(values$milvus_metrics$Success),]$ID))),
                                 Complete = list(sort(as.character(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                                                                                  !is.na(values$milvus_metrics$Success),]$ID))))
                               )
      )
    } else if (length(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                   is.na(values$milvus_metrics$Success),]$ID)) == 1) {
      # covers the case when there is only one item in the "Incomplete" group
      shiny::updateSelectInput(inputId = "ID", label = "Select Individual",
                               choices = list(
                                 Incomplete = list(sort(as.character(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                                                                    is.na(values$milvus_metrics$Success),]$ID)))),
                                 Complete = sort(as.character(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                                                                             !is.na(values$milvus_metrics$Success),]$ID)))
                               )
      )
    } else {
      shiny::updateSelectInput(inputId = "ID", label = "Select Individual",
                               choices = list(
                                 Incomplete = sort(as.character(unique(values$milvus_metrics[is.na(values$milvus_metrics$Nest) |
                                                                                               is.na(values$milvus_metrics$Success),]$ID))),
                                 Complete = sort(as.character(unique(values$milvus_metrics[!is.na(values$milvus_metrics$Nest) &
                                                                                             !is.na(values$milvus_metrics$Success),]$ID)))
                               )
      )
    }
  })
  
  # Creates subset of locations by chosen ID and period
  milvus_track_subset <- shiny::reactive({
    if(input_period() == 1){ # whole period
      milvus_track %>%
        dplyr::filter(id == input_id())}
    else { # number of month
      milvus_track %>%
        dplyr::filter(id == input_id() & month == input_period())}
  })
  
  # Creates subset of nest by chosen ID and period
  milvus_nest_subset <- shiny::reactive({
    milvus_nest %>%
      dplyr::filter(id == input_id())})
  
  # Creates map of locations and trajectories
  output$map <- renderLeaflet({
    leaflet() %>%
      addLayersControl(
        baseGroups = c("Topo", "Satellite"),
        overlayGroups = c("Locations", "Nest", "Trajectory"),
        options = layersControlOptions(collapsed = T)) %>%
      hideGroup("Trajectory") %>%
      # Adds a scale bar
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = F)) %>%
      # Adds a tool to measure distances and areas
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = F,
        primaryAreaUnit = "hectares",
        activeColor = "#00CCCC",
        completedColor = "#006666"
      )
  })
  
  # Changes zoom extent to chosen bird
  # (only when new bird is chosen, but not when period selection etc. changes)
  shiny::observe({
    leafletProxy("map") %>%
      fitBounds(min(milvus_track[milvus_track$id == input_id(),]$long), min(milvus_track[milvus_track$id == input_id(),]$lat),
                max(milvus_track[milvus_track$id == input_id(),]$long), max(milvus_track[milvus_track$id == input_id(),]$lat))
  })
  
  # Changes zoom extent when zoom button is clicked
  shiny::observeEvent(input$zoom, {
    leafletProxy("map") %>%
      fitBounds(min(milvus_track_subset()$long), min(milvus_track_subset()$lat),
                max(milvus_track_subset()$long), max(milvus_track_subset()$lat))
  })
  
  # Changes base map according to selection
  shiny::observe({
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(leaflet::providers$OpenTopoMap, group = "Topo",
                       options = providerTileOptions(opacity = input_map_opacity())
      ) %>%
      addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                       options = providerTileOptions(opacity = input_map_opacity())
      )
  })
  
  # Changes Palette, Labels, Points, Lines & Legend according to input
  shiny::observe({
    # Creates a palette that adapts to the range of dates
    pal <- colorNumeric(palette = "BrBG", domain = milvus_track_subset()$year_day)
    # Creates a palette for time of day (tod)
    pal_tod <- colorFactor(palette = c("#FFFFFF", "#000000", "#777777"), levels = c("day", "night", "twilight"))
    
    # Custom legend (Function)
    # Inputs for custom legend function
    if (input$day_night == T) {
      colors <- c("#740000", "#FFFFFF","#000000", "#777777") # fill colour inside borders of circles
      labels <- c("Nest", "Day", "Night", "Twilight")
      sizes <- c(rep(15, 3))
      shapes <- "circle"
      borders <- c("#740000", "#444444","#777777", "#444444") # border colour of circles
      margin_right <- -2.25 # 7.5 without twilight
      margin_left <- -2.25 # 0 without twilight
    }
    else {
      colors <- "#740000" # fill colour inside borders of circles
      labels <- "Nest"
      sizes <- 15
      shapes <- "circle"
      borders <- "#740000" # border colour of circles
      margin_right <- 11.5
      margin_left <- 0
    }
    # Function that creates a custom legend
    addLegendCustom <- function(map, layerId = NULL, group = NULL, labels, sizes, shapes, borders, opacity = 1, 
                                position = c("topright", "bottomright", "bottomleft", "topleft")){
      position <- match.arg(position)
      make_shapes <- function(sizes, borders, shapes) {
        shapes <- gsub("circle", "100%", shapes)
        paste0(colors, "; width:", sizes, "px; margin-top:5px; margin-left:",
               margin_left, "px; height:", sizes, "px; border:1.5px solid ",
               borders, "; border-radius:", shapes)
      }
      make_labels <- function(sizes, labels) {
        paste0("<div style='display: inline-block; font-size:14px ;height: ", 
               sizes, "px; margin-top:5px; margin-right:", margin_right,
               "px; margin-left:", margin_left, "px; line-height: ",
               sizes, "px;'>", labels, "</div>")
      }
      legend_colors <- make_shapes(sizes, borders, shapes)
      legend_labels <- make_labels(sizes, labels)
      
      return(addLegend(map, layerId = layerId, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
    }
    
    # Function that creates labels for date legend (from year_day back to desired format)
    myLabelFormat = function(...,date=FALSE){ 
      if(date){ 
        function(type = "numeric", cuts){
          as <- as.Date(cuts, origin="2020-01-01")
          format(as,"%d.%m.")
        } 
      }else{
        labelFormat(...)
      }
    }
    # Adds Data
    leafletProxy("map") %>%
      # Trajectories
      clearShapes() %>%
      addPolylines(
        data = milvus_track_subset(),
        lat = milvus_track_subset()$lat,
        lng = milvus_track_subset()$long,
        weight = 1,
        color = "#444444",
        opacity = 0.7,
        group = "Trajectory"
      ) %>%
      # Locations
      clearMarkers() %>%
      addCircleMarkers(
        data = milvus_track_subset(),
        radius = 4,
        color = ~pal(year_day),
        stroke = input$day_night,
        weight = 0.5,
        opacity = 0.7,
        fillColor = if(input$day_night == T){~pal_tod(tod_)}else{~pal(year_day)},
        fillOpacity = 0.7,
        group = "Locations",
        popup = ~paste0(t2)
      ) %>%
      # Animation
      #clearMarkers() %>%
      leaflet.extras2::addPlayback(data = milvus_track_subset(),
                                   time = "t_",
                                   icon=leaflet::makeIcon(iconUrl="https://images.phylopic.org/images/aec14bd0-7666-45e2-8a30-17fdd0c79578/vector.svg",
                                                          iconWidth=30,
                                                          iconHeight=18,
                                                          iconAnchorX=15,
                                                          iconAnchorY=9),
                                   options = leaflet.extras2::playbackOptions(tracksLayer = FALSE,
                                                                              speed = 10000000,
                                                                              tickLen=1000*60*60,  ## hourly tick lengths stated in milliseconds
                                                                              maxInterpolationTime=1000*60*60*5 ## 5 hrs interpolation time
                                   )) %>%
      # Adds legend
      removeControl(layerId = 1) %>% # removes legend
      removeControl(layerId = 2) # removes legend
    # Adds nest location and legends if data is available
    if (nrow(milvus_track_subset()) != 0) {
      leafletProxy("map", data = milvus_track_subset()) %>%
        # Nest location
        addCircleMarkers(
          data = milvus_nest_subset(),
          radius = 7,
          stroke = F,
          opacity = 0.7,
          fillColor = "#740000",
          fillOpacity = 0.7,
          group = "Nest",
          popup = ~paste0("Predicted nest location:<br>", round(lat, 2), "°N", round(long, 2), "°E ")
        ) %>%
        # Legend with date coloring
        addLegend(layerId = 1,
                  position = "bottomright",
                  pal = pal,
                  values = ~year_day,
                  opacity = 1,
                  #bins = 5,
                  labFormat = myLabelFormat(date=T),
                  title = NULL
        ) %>%
        # Legend with Nest, Day & Night
        addLegendCustom(layerId = 2,
                        labels,
                        sizes,
                        shapes,
                        borders,
                        position = "bottomright",
                        group = "Nest")
    }
  })
  
  # Creates a warning when no data is available in the selected period
  output$warning <- renderText({
    if (nrow(milvus_track_subset()) == 0) {
      return(paste("<span style=\"color:red\">No data in this period &#128559<br>&nbsp</span>"))
    }})
  
  # Creates a table with brood specific metrics of the inspected bird
  observe({
    # Shows the data frame with input data
    output$table <- DT::renderDataTable(
      datatable(values$milvus_metrics, options = list(dom = "ftip", pageLength = 20), rownames = F,
                selection = "none", class = "compact hover row-border") %>%
        # Color codes the decisions made on nest and brood
        DT::formatStyle(c("Nest", "Success"), target = "cell",
                        backgroundColor = styleEqual(c("No", "Yes", "Uncertain"), c("#996622", "#66AAAA", "#999988"))) %>%
        # Highlights the row of the selected bird
        DT::formatStyle("ID", target = "row",
                        fontWeight = styleEqual(c(input_id()), c("bold")),
                        backgroundColor = styleEqual(c(input_id()), c("#999999")))
    )
    # Ensures that the page of the table is shown that contains the observed bird
    proxy <- DT::dataTableProxy("table")
    if (input_id() %in% sort(unique(milvus_metrics$ID)[1:20])) {
      table_page <- 1
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[21:40])) {
      table_page <- 2
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[41:60])) {
      table_page <- 3
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[61:80])) {
      table_page <- 4
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[81:100])) {
      table_page <- 5
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[101:120])) {
      table_page <- 6
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[121:140])) {
      table_page <- 7
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[141:160])) {
      table_page <- 8
    } else if (input_id() %in% sort(unique(milvus_metrics$ID)[161:180])) {
      table_page <- 9
    } else {table_page <- 10}
    DT::selectPage(proxy = proxy, page = table_page)
  })
  
  # Creates an interactive plot with all brood relevant metrics
  output$metrics_plot <-renderPlotly({
    suppressWarnings({
      NestTool::plot_move_metrics(movemetrics = move_metrics, individual = input_id())
    })
  })
}



# START SHINY APP ------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
}


