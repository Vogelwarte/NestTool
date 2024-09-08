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
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(htmltools)
library(plotly)
library(viridis)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER INPUT (needs to be added to the shiny app) --------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MovebankID<-15869951 #230545451  #1356790386
MovebankUser<-"Steffen"
n.weeks<-3
nestradius<-50


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOG IN TO MOVEBANK --------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

movebank_store_credentials(username=MovebankUser, key_name = getOption("move2_movebank_key_name"), force = TRUE)
#movebank_download_study_info(study_id=MovebankID)$sensor_type_ids

#### https://shiny.posit.co/r/reference/shiny/1.6.0/passwordinput
#### https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html was too complicated and required a list of passwords

library(shiny)
library(move)
# retrieve 1 month worth of data
timestamp_end <- paste0(format(Sys.time(), format="%Y%m%d%H%M%S"), "000")
timestamp_start <- paste0(format(as.Date(Sys.time())  %m+%  days(-as.numeric(31)) , format="%Y%m%d%H%M%S"), "000")

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    textInput("username", "Your Movebank username:"),
    passwordInput("password", "Your Movebank password:"),
    textInput("studyID", "Your Movebank study name:"),
    actionButton("go", "Sign in to retrieve data from Movebank"),
    verbatimTextOutput("value"),
    tableOutput("five.points.e")
  )
  server <- function(input, output) {
    
    
    
    
    
    
    ########### 0 - Download data Egyptian Vulture ############
    # output$myDF <- reactive({
    #   req(input$go)
    #   
    # 
    #   
    #   })
    
    output$value <- renderText({
      req(input$go)
      curl <- movebankLogin(username=isolate(input$username),
                            password=isolate(input$password))
      
      data<-getMovebankLocationData(study=as.character(isolate(input$studyID)), sensorID="GPS", login=curl,
                                    timestamp_start=timestamp_start, timestamp_end=timestamp_end)
      #isolate(input$password)
      sprintf("In the last month there were %i locations from %s animals",dim(data)[1], length(unique(data$tag.local.identifier)))
    })
    
    # Display information of the last 5 points
    output$five.points.e <- renderTable({
      req(input$go)
      myDF %>%
        utils::tail(n = 5) %>%
        dplyr::select(timestamp, tag.local.identifier, individual.local.identifier, tag.voltage, location.long, location.lat)
    }, spacing = "xs", align = "lrrrrr")
    
    
    
  }
  shinyApp(ui, server)
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD MOVEBANK DATA AND ANIMAL INFO ----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

birds<-move2::movebank_retrieve(study_id=MovebankID, entity_type="individual") %>%
  dplyr::rename(individual_id=id,bird_id=local_identifier)  %>%
  #dplyr::mutate(bird_id=as.numeric(as.character(bird_id))) %>%
  dplyr::select(individual_id, bird_id,ring_id,sex) 



locs<-move2::movebank_retrieve(study_id=MovebankID,
                         entity_type="event",
                         #sensor_type_id=if_else("GPS" %in% movebank_download_study_info(study_id=MovebankID)$sensor_type_ids,"gps",),   ## may need to define sensor if there are multiple?
                         timestamp_start=Sys.time()-weeks(n.weeks),
                         timestamp_end=Sys.time(),
                         progress=T) %>%
  dplyr::filter(!is.na(individual_id)) %>%
  dplyr::left_join(birds, by="individual_id") %>%
  dplyr::select(bird_id,ring_id,sex,timestamp,location_lat,location_long) %>%
  mutate(bird_id=as.character(bird_id)) %>%
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
  select(bird_id,timestamp,long_wgs,lat_wgs,long_eea,lat_eea) %>%
  arrange(bird_id,timestamp) %>%
  dplyr::mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                date = as.Date(timestamp, tz = "UTC"),
                date_id = paste0(date, "_", bird_id),
                event_id=seq_along(timestamp),
                year_day = lubridate::yday(timestamp))



################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################
# DETECT NESTS  ---------------------------------------------------
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##########################


# LOADING DATA -----------------------------------------------------------------
 
# Creating a track
# 3 mins
avidat_track_amt <- trackingdata %>% sf::st_drop_geometry() %>%
  amt::mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = bird_id,
    date_id,
    event_id,
    crs = EPSG
  ) %>%
  amt::time_of_day(include.crepuscule = T) %>% # if F, crepuscule is considered as night
  amt::arrange(id, t_)

# creating a night and a day data frame, include crepuscule in day
avidat_track_night <- avidat_track_amt %>%
  dplyr::filter(tod_ %in% c("night","dusk","dawn")) %>%
  dplyr::select(-tod_)

avidat_track_day <- avidat_track_amt %>%
  dplyr::filter(tod_ %in% c("day")) %>%
  dplyr::select(-tod_)


### identify individuals for which there are no night locations and break the data prep
if(length(unique(avidat_track_day$id[which(!(avidat_track_day$id %in% avidat_track_night$id))]))>0){
  mismatches<-unique(avidat_track_day$id[which(!(avidat_track_day$id %in% avidat_track_night$id))])
  avidat_track_night <- avidat_track_night %>% dplyr::filter(!(id %in% mismatches))
  avidat_track_day <- avidat_track_day %>% dplyr::filter(!(id %in% mismatches))
  avidat_track_amt <- avidat_track_amt %>% dplyr::filter(!(id %in% mismatches))
  avidat <- trackingdata %>% dplyr::filter(!(bird_id %in% mismatches))
  print(sprintf("Individual(s) %s were removed because they do not have day and night locations.", mismatches))
}


# RECURSIONS DURING NIGHTTIME --------------------------------------------------
# splitting track into a list with each single id grouped to an element
avidat_track_night <- as.data.frame(avidat_track_night)
avidat_track_night$id <- factor(avidat_track_night$id, levels=unique(avidat_track_night$id))  ## required to prevent re-ordering in split
avidat_track_night_list <- split(avidat_track_night, avidat_track_night$id)

# calculating recursions
print(sprintf("Calculating night recursions for %i nocturnal locations",dim(avidat_track_night)[1]))
avidat_night_recurse <- lapply(avidat_track_night_list, function(x)
  recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))

# allocating recurse information to track data frame (1.5 mins)
avidat_track_night$revisits <- NA
avidat_track_night$residence_time <- NA
for (i in 1:length(avidat_night_recurse)) {
  avidat_track_night[avidat_track_night$id == unique(avidat_track_night$id)[i] ,]$revisits <-
    avidat_night_recurse[[i]]$revisits
  avidat_track_night[avidat_track_night$id == unique(avidat_track_night$id)[i] ,]$residence_time <-
    avidat_night_recurse[[i]]$residenceTime
}


# RECURSIONS DURING DAYTIME ----------------------------------------------------
# splitting track into a list with each single id grouped to an element
avidat_track_day <- as.data.frame(avidat_track_day)
avidat_track_day$id <- factor(avidat_track_day$id, levels=unique(avidat_track_day$id))  ## required to prevent re-ordering in split
avidat_track_day_list <- split(avidat_track_day, avidat_track_day$id)

# calculating recursions (1.5 mins)
print(sprintf("Calculating day recursions for %i diurnal locations",dim(avidat_track_day)[1]))
avidat_day_recurse <- lapply(avidat_track_day_list, function(x)
  recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))

# allocating recurse information to track data frame (4 mins)
avidat_track_day$revisits <- NA
avidat_track_day$residence_time <- NA
for (i in 1:length(avidat_day_recurse)) {
  avidat_track_day[avidat_track_day$id == unique(avidat_track_day$id)[i] ,]$revisits <-
    avidat_day_recurse[[i]]$revisits
  avidat_track_day[avidat_track_day$id == unique(avidat_track_day$id)[i] ,]$residence_time <-
    avidat_day_recurse[[i]]$residenceTime
}

# creating avidat data set for night locations only
avidat_night <- trackingdata %>%
  dplyr::filter(event_id %in% avidat_track_night$event_id) %>%
  dplyr::left_join(avidat_track_night[,6:8], by = "event_id")

# creating avidat data set for night day locations only
avidat_day <- trackingdata %>%
  dplyr::filter(event_id %in% avidat_track_day$event_id) %>%
  dplyr::left_join(avidat_track_day[,6:8], by = "event_id")

# filtering the location with longest residence time during nighttime
suppressWarnings({
  avidat_night_max <- avidat_night %>%
    dplyr::group_by(bird_id) %>%
    dplyr::summarise(residence_time_night = max(residence_time),
                     revisits_night = first(revisits[which(residence_time == max(residence_time))]),
                     date_night = first(date[which(residence_time == max(residence_time))]),
                     long_night = first(long_eea[which(residence_time == max(residence_time))]),
                     lat_night = first(lat_eea[which(residence_time == max(residence_time))])
    ) %>%
    dplyr::group_by(bird_id) %>%   ## because there are sometimes duplicates, we need to group again and take the one with max revisits for those where time is equal
    dplyr::summarise(revisits_night = max(revisits_night),
                     residence_time_night = first(residence_time_night[which(revisits_night == max(revisits_night))]),
                     date_night = first(date_night[which(revisits_night == max(revisits_night))]),
                     long_night = first(long_night[which(revisits_night == max(revisits_night))]),
                     lat_night = first(lat_night[which(revisits_night == max(revisits_night))])
    )
})

# filtering the location with longest residence time during daytime
suppressWarnings({
  avidat_day_max <- avidat_day %>%
    dplyr::group_by(bird_id) %>%
    dplyr::summarise(residence_time_day = max(residence_time),
                     revisits_day = first(revisits[which(residence_time == max(residence_time))]),
                     date_day = first(date[which(residence_time == max(residence_time))]),
                     long_day = first(long_eea[which(residence_time == max(residence_time))]),
                     lat_day = first(lat_eea[which(residence_time == max(residence_time))])
    ) %>%
    dplyr::group_by(bird_id) %>%
    dplyr::summarise(revisits_day = max(revisits_day),
                     residence_time_day = first(residence_time_day[which(revisits_day == max(revisits_day))]),
                     date_day = first(date_day[which(revisits_day == max(revisits_day))]),
                     long_day = first(long_day[which(revisits_day == max(revisits_day))]),
                     lat_day = first(lat_day[which(revisits_day == max(revisits_day))])
    )
})

# creating sf objects for distance calculation
avidat_night_max_sf <- avidat_night_max %>%
  sf::st_as_sf(coords = c("long_night", "lat_night"), crs = EPSG)
avidat_day_max_sf <- avidat_day_max %>%
  sf::st_as_sf(coords = c("long_day", "lat_day"), crs = EPSG)

# calculating the distance from the day location to the night location
avidat_max_res_time <- avidat_day_max_sf %>%
  dplyr::mutate(dist_day_to_night = as.numeric(sf::st_distance(avidat_day_max_sf, avidat_night_max_sf, by_element = T))) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(avidat_night_max_sf %>% dplyr::select(bird_id,revisits_night,residence_time_night,date_night), by = "bird_id") %>%
  sf::st_drop_geometry()

rm(avidat_night,avidat_day_recurse,avidat_night_recurse, avidat_track_night_list, avidat_track_day_list)

##### CALCULATING REVISITS TO POTENTIAL NEST SITE
avidat_track <- as.data.frame(avidat_track_amt %>% dplyr::select(-tod_))
avidat_track$id <- factor(avidat_track$id, levels=unique(avidat_track$id))  ## required to prevent re-ordering in split
avidat_track_list <- split(avidat_track, avidat_track$id)
avidat_track_amt$id <- factor(avidat_track_amt$id, levels=unique(avidat_track_amt$id))  ## required to prevent re-ordering in split
avidat_track_amt_list <- split(avidat_track_amt, avidat_track_amt$id)

# calculating recursions (1.5 mins)
avidat_recurse <- lapply(avidat_track_list, function(x)
  recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))

# # calculating MCPs for total season for each bird_id
# avidat_MCP <- lapply(avidat_track_amt_list, function(x)
#   amt::hr_mcp(x = x[1:4], levels = c(0.95,0.99))$mcp)

# allocating recurse information to track data frame (4 mins)
avidat_track$revisits <- NA
avidat_track$residence_time <- NA
for (i in 1:length(avidat_recurse)) {
  avidat_track[avidat_track$id == unique(avidat_track$id)[i] ,]$revisits <-
    avidat_recurse[[i]]$revisits
  avidat_track[avidat_track$id == unique(avidat_track$id)[i] ,]$residence_time <-
    avidat_recurse[[i]]$residenceTime
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
avidat_pot_nests<-data.frame()
for (i in unique(avidat_track$id)) {
  
  ### subset the data ####
  workdat<- avidat_track %>% 
    dplyr::rename(bird_id=id) %>%
    dplyr::filter(bird_id==i)
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
  avidat_pot_nests<-potnests %>% 
    dplyr::slice_max(order_by=npoint, n=1) %>%
    dplyr::slice_max(order_by=residence_time, n=1) %>%
    dplyr::slice_max(order_by=revisits, n=1) %>%
    dplyr::slice_min(order_by=NN50dist, n=1) %>%
    dplyr::slice_min(order_by=t_, n=1) %>%
    dplyr::select(bird_id,event_id,x_,y_,npoint,revisits,residence_time,NN50dist) %>%
    dplyr::rename(id=bird_id,x=x_,y=y_) %>%
    #mutate(Type="Revised") %>%
    dplyr::bind_rows(avidat_pot_nests)
}

# RETAIN Predicted nest locations
avidat_nest <- avidat_pot_nests %>%
  dplyr::rename(bird_id=id) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = EPSG) %>%
  sf::st_transform(crs = 4326) %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])

# RETAIN tracking data 
trackingdata <- trackingdata %>%
        sf::st_transform(4326) %>%
  mutate(num_time = as.numeric(timestamp, origin=as.POSIXct("2015-01-01", tz="GMT"))) #as workaround for color legend

# REMOVE the rest
rm(avidat_night,avidat_day_recurse,avidat_night_recurse, avidat_track_night_list, avidat_track_day_list)


############ PLOT NEST AND TRACKING LOCATION ON LEAFLET MAP LEAFLET MAP------------------------

m2 <- leaflet(options = leafletOptions(zoomControl = F)) %>% #changes position of zoom symbol
  setView(lng = median(st_coordinates(avidat_nest)[,1]), lat = median(st_coordinates(avidat_nest)[,2]), zoom = 11) %>%
  htmlwidgets::onRender("function(el, x) {L.control.zoom({ 
                           position: 'bottomright' }).addTo(this)}"
  ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
  addProviderTiles("Esri.WorldImagery", group = "Satellite",
                   options = providerTileOptions(opacity = 0.6, attribution = F,minZoom = 5, maxZoom = 20)) %>%
  addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F,minZoom = 5, maxZoom = 15)) %>%  
  addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%  
  
  addCircleMarkers(
    data=avidat_nest,
    radius = 10,
    stroke = TRUE, color = "white", weight = 0.8,
    fillColor = "red",fillOpacity = 0.9,
    popup = ~ paste0("bird ID: ", avidat_nest$bird_id)
  ) %>%

  addCircleMarkers(
    data=trackingdata,
    radius = 2,
    stroke = TRUE, color = "white", weight = 0.3,
    fillColor = "firebrick", fillOpacity = 0.5,
    popup = ~ paste0("bird ID: ", trackingdata$bird_id)
  ) %>%
  
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F))

m2








# SHINY APP ------------------------------------------------------------------
# List of Milsar / EGVU choices, sorted alphabetically
BirdList <- as.factor(sort(as.character(unique(avidat_nest$bird_id))))


########### 2 - user interface ###########

ui <- fluidPage(    
  #tags$head(
  #  tags$style(HTML('.dygraph-legend {color: black; background-color: transparent !important;} .highlight {display: inline;background-color: #B0B0B0;font-size: 15px;}'))), #left: 50px !important; 
  navbarPage("Potential location of nest",
             #theme = shinytheme("slate"), # or darkly
             
             ### EGVU ###
             tabPanel("Individual bird",
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          
                          selectInput(inputId = "ID.e", label = "Select the Individual", 
                                      choices = BirdList, multiple = F),
                          column(6, actionButton("prevBtn.e", "<<"), align = "right"),
                          column(6, actionButton("nextBtn.e", ">>"), align = "left"),#style='padding:4px; font-size:80%')
                          
                          br(), br(), br(), hr(),
                          
                          radioButtons(inputId = "PointsToDisplay.e",
                                       label = "Data",
                                       choices = c("last 5 points" = 1,
                                                   "last 10 points" = 2,
                                                   "last 2 days" = 3,
                                                   "last 5 days" = 4,
                                                   "last 10 days" = 5,
                                                   "all data" = 6),
                                       selected = 1),
                          
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  #some empty rows to align sidebarPanel with mainPanel
                          br(), br(), br(), br(), br(),
                        ),
                        
                        mainPanel(
                          
                          # Display last 5 points
                          #tableOutput("five.points.e"),
                          
                          hr(),
                          
                          # Plot points on map
                          leafletOutput("zoomplot.e", height = 1250),
                          
                          br()

                        )
                      )
             )
  )
)


############### 3 - server ###############

server <- function(input, output, session){
  
  nestPerID.e <- reactive({avidat_nest[avidat_nest$bird_id == input$ID.e,] })
  dataPerID.e <- reactive({trackingdata[trackingdata$bird_id == input$ID.e,] })
  
  # site updates when clicking on Previous / Next Red Kite
  observeEvent(input$prevBtn.e, {
    listPlacement.e <- which(BirdList == input$ID.e)
    if (listPlacement.e > 1) { 
      newSelection <- BirdList[listPlacement.e-1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  observeEvent(input$nextBtn.e, {
    listPlacement.e <- which(BirdList == input$ID.e)
    if (listPlacement.e < length(BirdList)) { 
      newSelection <- BirdList[listPlacement.e+1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  
  # determining subset based on Data to Display 
  dataInd.e <- reactive({
    if(input$PointsToDisplay.e == 1) {utils::tail(dataPerID.e(), n=5)} #last 5 points
    else if(input$PointsToDisplay.e == 2) {utils::tail(dataPerID.e(), n=10)} #last 10 points
    else if(input$PointsToDisplay.e == 3) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-1))} #last 2 days
    else if(input$PointsToDisplay.e == 4) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-4))} #last 5 days
    else if(input$PointsToDisplay.e == 5) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-9))} #last 10 days
    else if(input$PointsToDisplay.e == 6) {dataPerID.e()} #all data
  })
  
  # # Display information of the last 5 points
  # output$five.points.e <- renderTable({
  #   utils::tail(dataPerID.e(), n = 6) %>% 
  #     mutate(dist_m = round(distHaversine(cbind(long_wgs, lat_wgs),
  #                                         cbind(lag(long_wgs), lag(lat_wgs))),0)) %>% 
  #     dplyr::slice_tail(n = 5) %>% 
  #     dplyr::mutate(time=as.character(timestamp)) %>% 
  #     dplyr::select(time, bird_id, dist_m)
  # }, spacing = "xs", align = "lrr")
  
  # Plot GPS points on map
  output$zoomplot.e <- renderLeaflet({
    
    ### make colour palette for Date
    pal.date <- colorNumeric(palette = viridis::viridis(200), domain = NULL, reverse=T)
    
    ### legend for Date coloration
    myLabelFormat = function(...,dates=FALSE){ 
      if(dates){ 
        function(type = "numeric", cuts){
          as <- as.POSIXct(cuts, origin="1970-01-01", tz="GMT")
          format(as,"%y-%m-%d %H:%M")
        } 
      }else{
        labelFormat(...)
      }
    }
    
    l1.e <- leaflet(options = leafletOptions(zoomControl = FALSE) #zoom Snap controls padding of points to map border, but then
                    #zoom symbols (+,-) don't work
    ) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this)}"
      ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      addProviderTiles("Esri.WorldImagery", group = "Satellite",
                       options = providerTileOptions(opacity = 0.6, attribution = F)) %>%
      addProviderTiles("CartoDB.Voyager", group = "Roadmap", options = providerTileOptions(attribution = F)) %>%  
      addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%  
      addCircleMarkers(
        data=dataInd.e(), lng=dataInd.e()$long_wgs, lat=dataInd.e()$lat_wgs,
        radius = 5,
        stroke = TRUE, color = "black", weight = 0.5,
        fillColor = ~pal.date(num_time), fillOpacity = 0.5,
        popup = ~ paste0("bird ID: ", bird_id, "<br>", timestamp)
      ) %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>%
      
      addCircleMarkers(
        data=nestPerID.e(), lng=nestPerID.e()$long, lat=nestPerID.e()$lat,
        radius = 10,
        stroke = TRUE, color = "red", weight = 0.5,
        fillColor = "red", fillOpacity = 0.7,
        popup = ~ paste0("bird ID: ", bird_id, "<br>", "N visits: ", revisits, "<br>", "Duration (hrs): ", residence_time)
      ) %>% 

      addLegend(     # legend for date (viridis scale)
        data = dataInd.e(),
        position = "topleft", 
        pal = pal.date,
        values = ~num_time,
        opacity = 1,
        bins = 4,
        labFormat = myLabelFormat(dates=T),
        title = NULL
      )
  })
  
}

############### 4 - start shinyApp ##############

shinyApp(ui = ui, server = server)




# 
# # DISCARDED USER INTERFACE -------------------------------------------------------------
# ui <- shiny::fluidPage(theme = shinythemes::shinytheme("flatly"),
#                        # layout of the action buttons
#                        htmltools::tags$style(
#                          HTML('#save_decision, #zoom{
#                        height:65px; width:110px; margin-top:50px;}')
#                        ),
#                        # layout of the shiny app
#                        titlePanel(title = "Individual Bird Potential Nest Location"),
#                        sidebarLayout(
#                          sidebarPanel(width = 4, style = "height: 675px; position:relative;",
#                                       h3("Data selection"),
#                                       fluidRow(
#                                         # Build selection tool for changing the individual to be displayed
#                                         column(width = 12,
#                                                selectInput(inputId = "ID", label = "Select Individual",
#                                                            choices = sort(as.character(unique(avidat_nest$bird_id))), multiple = F)
#                                         )
#                                       ),
# 
#                                       # Slider to select opacity of background map
#                                       fluidRow(
#                                         column(width = 12,
#                                                chooseSliderSkin("Flat", "#5489C5"),
#                                                sliderInput(inputId = "map_slider",
#                                                            label = "Background Map Transparency",
#                                                            min = 0, max = 1, value = 0.7, step = 0.1)
#                                         )
#                                       ),
#                          ),
#                          mainPanel(width = 8,
#                                    # For selected individual: Plotting movement track on a map
#                                    fluidRow(style = "padding-right:15px;",
#                                             column(width = 12, class = "well", style = "height: 675px; position:relative;",
#                                                    h3("Map of recent locations on potential nest site"),
#                                                    leafletOutput("map", height = "575px")
#                                             )
#                                    )
#                          )
#                        ),
# 
# )
# 
# 
# 
# # SERVER ---------------------------------------------------------------------
# server <- function(input, output, session){
#   # ID of the bird/year
#   input_id <- shiny::reactive({input$ID})
#   # Defines map opacity
#   input_map_opacity <- shiny::reactive({input$map_slider})
# 
#   
#   
#   # Updates input selection (ID): SELECT BIRDS THAT ARE AVAILABLE FOR DISPLAY
#   shiny::observe({
#       # covers the case when there is only one item in the "Complete" group
#       shiny::updateSelectInput(inputId = "ID", label = "Select Individual",
#                                choices = list(Available = sort(as.character(unique(avidat_nest$bird_id))))
#       )
#   })
#   
#   # Creates subset of locations by chosen ID and period
#   avidat_track_subset <- shiny::reactive({
#     trackingdata %>%
#       sf::st_transform(4326) %>%
#       #dplyr::filter(bird_id == input$ID)
#       dplyr::filter(bird_id == input_id())
#   })
#   
#   # Creates subset of nest by chosen ID
#   avidat_nest_subset <- shiny::reactive({
#     avidat_nest %>%
#       #dplyr::filter(id == input$ID)})
#       dplyr::filter(id == input_id())})
#   
#   # Creates map of locations and trajectories
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addLayersControl(
#         baseGroups = c("Topo", "Satellite"),
#         overlayGroups = c("Locations", "Nest", "Trajectory"),
#         options = layersControlOptions(collapsed = T)) %>%
#       hideGroup("Trajectory") %>%
#       # Adds a scale bar
#       addScaleBar(position = "bottomright",
#                   options = scaleBarOptions(imperial = F)) %>%
#       # Adds a tool to measure distances and areas
#       addMeasure(
#         position = "bottomleft",
#         primaryLengthUnit = "kilometers",
#         secondaryLengthUnit = F,
#         primaryAreaUnit = "hectares",
#         activeColor = "#00CCCC",
#         completedColor = "#006666"
#       )
#   })
#   
#   # Changes zoom extent to chosen bird
#   shiny::observe({
#     leafletProxy("map") %>%
#       fitBounds(min(avidat_track_subset()$long_wgs), min(avidat_track_subset()$lat_wgs),
#                 max(avidat_track_subset()$long_wgs), max(avidat_track_subset()$lat_wgs))
#   })
#   
#   # Changes zoom extent when zoom button is clicked
#   shiny::observeEvent(input$zoom, {
#     leafletProxy("map") %>%
#       fitBounds(min(avidat_track_subset()$long_wgs), min(avidat_track_subset()$lat_wgs),
#                 max(avidat_track_subset()$long_wgs), max(avidat_track_subset()$lat_wgs))
#   })
#   
#   # Changes base map according to selection
#   shiny::observe({
#     leafletProxy("map") %>%
#       clearTiles() %>%
#       addProviderTiles(leaflet::providers$OpenTopoMap, group = "Topo",
#                        options = providerTileOptions(opacity = input_map_opacity())
#       ) %>%
#       addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
#                        options = providerTileOptions(opacity = input_map_opacity())
#       )
#   })
#   
#   # Changes Palette, Labels, Points, Lines & Legend according to input
#   shiny::observe({
#     # Creates a palette that adapts to the range of dates
#     pal <- colorNumeric(palette = "BrBG", domain = avidat_track_subset()$year_day)
#     # Function that creates a custom legend
#     # addLegendCustom <- function(map, layerId = NULL, group = NULL, labels, sizes, shapes, borders, opacity = 1, 
#     #                             position = c("topright", "bottomright", "bottomleft", "topleft")){
#     #   position <- match.arg(position)
#     #   make_shapes <- function(sizes, borders, shapes) {
#     #     shapes <- gsub("circle", "100%", shapes)
#     #     paste0(colors, "; width:", sizes, "px; margin-top:5px; margin-left:",
#     #            margin_left, "px; height:", sizes, "px; border:1.5px solid ",
#     #            borders, "; border-radius:", shapes)
#     #   }
#     #   make_labels <- function(sizes, labels) {
#     #     paste0("<div style='display: inline-block; font-size:14px ;height: ", 
#     #            sizes, "px; margin-top:5px; margin-right:", margin_right,
#     #            "px; margin-left:", margin_left, "px; line-height: ",
#     #            sizes, "px;'>", labels, "</div>")
#     #   }
#     #   legend_colors <- make_shapes(sizes, borders, shapes)
#     #   legend_labels <- make_labels(sizes, labels)
#     #   
#     #   return(addLegend(map, layerId = layerId, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
#     # }
#     
#     # Function that creates labels for date legend (from year_day back to desired format)
#     myLabelFormat = function(...,date=FALSE){ 
#       if(date){ 
#         function(type = "numeric", cuts){
#           as <- as.Date(cuts, origin="2020-01-01")
#           format(as,"%d.%m.")
#         } 
#       }else{
#         labelFormat(...)
#       }
#     }
#     # Adds Data
#     leafletProxy("map") %>%
#       # Trajectories
#       clearShapes() %>%
#       addPolylines(
#         data = avidat_track_subset(),
#         lat = avidat_track_subset()$lat_wgs,
#         lng = avidat_track_subset()$long_wgs,
#         weight = 1,
#         color = "#444444",
#         opacity = 0.7,
#         group = "Trajectory"
#       ) %>%
#       # Locations
#       clearMarkers() %>%
#       addCircleMarkers(
#         data = avidat_track_subset(),
#         radius = 4,
#         color = ~pal(year_day),
#         stroke = input$day_night,
#         weight = 0.5,
#         opacity = 0.7,
#         fillColor = ~pal(year_day),
#         fillOpacity = 0.7,
#         group = "Locations",
#         popup = ~paste0(timestamp)
#       ) %>%
#       # Animation
#       #clearMarkers() %>%
#       leaflet.extras2::addPlayback(data = avidat_track_subset(),
#                                    time = "timestamp",
#                                    icon=leaflet::makeIcon(iconUrl="https://images.phylopic.org/images/aec14bd0-7666-45e2-8a30-17fdd0c79578/vector.svg",
#                                                           iconWidth=30,
#                                                           iconHeight=18,
#                                                           iconAnchorX=15,
#                                                           iconAnchorY=9),
#                                    options = leaflet.extras2::playbackOptions(tracksLayer = FALSE,
#                                                                               speed = 10000000,
#                                                                               tickLen=1000*60*60,  ## hourly tick lengths stated in milliseconds
#                                                                               maxInterpolationTime=1000*60*60*5 ## 5 hrs interpolation time
#                                    )) %>%
#       # Adds legend
#       removeControl(layerId = 1) %>% # removes legend
#       removeControl(layerId = 2) # removes legend
#     # Adds nest location and legends if data is available
#     if (nrow(avidat_track_subset()) != 0) {
#       leafletProxy("map", data = avidat_track_subset()) %>%
#         # Nest location
#         addCircleMarkers(
#           data = avidat_nest_subset(),
#           radius = 7,
#           stroke = F,
#           opacity = 0.7,
#           fillColor = "#740000",
#           fillOpacity = 0.7,
#           group = "Nest",
#           popup = ~paste0("Predicted nest location:<br>", round(lat, 2), "°N", round(long, 2), "°E ")
#         ) %>%
#         # Legend with date coloring
#         addLegend(layerId = 1,
#                   position = "bottomright",
#                   pal = pal,
#                   values = ~year_day,
#                   opacity = 1,
#                   #bins = 5,
#                   labFormat = myLabelFormat(date=T),
#                   title = NULL
#         ) #%>%
#         # # Legend with Nest, Day & Night
#         # addLegendCustom(layerId = 2,
#         #                 labels,
#         #                 sizes,
#         #                 shapes,
#         #                 borders,
#         #                 position = "bottomright",
#         #                 group = "Nest")
#     }
#   })
#   
#   # Creates a warning when no data is available in the selected period
#   output$warning <- renderText({
#     if (nrow(avidat_track_subset()) == 0) {
#       return(paste("<span style=\"color:red\">No data in this period &#128559<br>&nbsp</span>"))
#     }})
#   
# }
# 
# 
# 
# # START SHINY APP ------------------------------------------------------------
# shiny::shinyApp(ui = ui, server = server)
# }
# 
# 
