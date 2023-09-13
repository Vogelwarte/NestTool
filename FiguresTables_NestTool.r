####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### LOAD PREDICTION OUTPUT FROM NEDSTTOOL AND CREATE FIGURES FOR MANUSCRIPT ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

library(data.table)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
library(amt)
library(sf)
library(mapview)
library(ggplot2)
library(ggspatial)
library(ggpubr)
library(lubridate)
select<-dplyr::select
filter<-dplyr::filter
library(shiny)
library(leaflet)
library(htmltools)
library(NestTool)

## set root folder for project
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2")


### LOAD THE TRACKING DATA AND INDIVIDUAL SEASON SUMMARIES 
load("NestTool_SUI_all.RData")
ls()



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### select good candidate individuals (3 of each group)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(pred_succ)
NONESTEX<-pred_succ %>% filter(nest_observed=="no nest") %>%
  filter(nest_prob<0.1) %>%   ungroup() %>%
  slice_min(nest_prob,n = 3)

UNSUCCEX<-pred_succ %>% filter(success_observed=="no") %>%
  filter(succ_prob<0.1) %>% ungroup() %>%
  slice_min(succ_prob,n = 3)

SUCCEX<-pred_succ %>% filter(success_observed=="yes") %>%
  filter(succ_prob>0.9) %>% ungroup() %>%
  slice_max(succ_prob,n = 3)

UNCERTSUCCEX<-pred_succ %>% filter(succ_prob>0.4) %>%
  filter(succ_prob<0.6) %>% ungroup() %>%
  slice_min(abs(succ_prob-0.5),n = 3)

UNCERTNESTEX<-pred_succ %>% filter(nest_prob>0.4) %>%
  filter(nest_prob<0.6) %>% ungroup() %>%
  slice_min(abs(nest_prob-0.5),n = 3)


# ### examples of unsuccessful birds:
# i="2017_186"
# i="2017_187"
# i="2017_27"
# i="2018_187"
# i="2018_26"
# i="2019_186"
# 
# 
# ### examples of successful birds:
# i="2020_80"
# i="2021_28"
# i="2021_454"
# i="2021_481"
# i="2021_61"
# milvus_5d_move_metrics$week




# # DATA PREPARATION -------------------------------------------------------------

# Creating a track and calculating step lengths
# 3 mins
milvus_track_amt <- nest_data_input$movementtrack %>%
  amt::mk_track(
    .x = x_,
    .y = y_,
    .t = t_,
    id = id,
    date_id,
    event_id,
    tod_,
    crs = 3035
  ) %>%
  amt::arrange(id, t_)
milvus_track_amt$step_dist<-amt::step_lengths(milvus_track_amt)

### ADD THE MOST VISITED LOCATION TO THE TRACK TO GET RECURSIONS TO THIS LOCATION

milvus_track <- as.data.frame(milvus_track_amt) %>% dplyr::left_join(nest_data_input$pot_nests, by = "id") %>%
  dplyr::rename(nest_long=x,nest_lat=y)





####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### FIGURE 1: 4 panel plot with example of non-nesting, unsuccessful nesting, successful nesting and uncertain bird
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### plot only home range size and median daily travel distance
fig1inds<-c(NONESTEX$year_id,SUCCEX$year_id,UNCERTNESTEX$year_id)



##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## LOOP OVER EACH INDIVIDUAL AND EXTRACT DATA FOR 5-day MOVING WINDOW   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################

## define the central points of moving window (with 2 days on either side of centre day)
wincentres<-seq(72,173,3)

### create blank output summaries
fig1_move_metrics<-data.frame()



##### start loop across all individuals and time windows ##############################################################################################

for (i in fig1inds) {
  
  ind_track<-milvus_track %>% dplyr::filter(id==i)
  ind_track_amt<-milvus_track_amt %>% dplyr::filter(id==i)
  
  for (w in wincentres) {
    window<-seq(w-2,w+2,1)  ## create a 5 day moving window
    win_track<-ind_track %>% dplyr::mutate(yday=lubridate::yday(t_)) %>% dplyr::filter(yday %in% window)
    
    if(dim(win_track)[1]>1){
      
      win_track_amt<-ind_track_amt %>% dplyr::mutate(yday=lubridate::yday(t_)) %>% dplyr::filter(yday %in% window)
      
      ### calculate mean daily travel distance and distance from nest
      day_sum<-win_track %>% dplyr::group_by(id,yday) %>%
        dplyr::summarise(daydist=sum(step_dist,na.rm=T)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(median_daydist=median(daydist,na.rm=T))
      
      ### calculate MCP95
      day_sum$MCP<-as.numeric(amt::hr_mcp(win_track_amt, levels=0.95)$mcp$area)/10000  ## convert home range area to hectares
      
      ## summarise the output and write into data.frame
      fig1_move_metrics<- day_sum %>% dplyr::mutate(week=format(parse_date_time(x = w, orders = "j"), format="%d %b")) %>%
        dplyr::select(id,week,MCP,median_daydist) %>%
        dplyr::bind_rows(fig1_move_metrics)
      
    }else{print(sprintf("no data for %s in week %s",i,format(lubridate::parse_date_time(x = w, orders = "j"), format="%d %b")))}
    
  }
}

##### end of loop across all individuals and weeks ##############################################################################################

milvus_5d_move_metrics <- milvus_5d_move_metrics %>% dplyr::left_join(
  (inddata %>% dplyr::select(year_id,sex,age_cy) %>%
     dplyr::rename(id=year_id)),by ="id")











####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### FIGURE 2: 3 panel plot with example of unsuccessful nesting, successful nesting and uncertain bird
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## CALCULATE DISTANCES FROM AND TIMES AT POTENTIAL NEST   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################


# calculating recursions to potential nest site
milvus_track_list <- split(milvus_track, milvus_track$id)
nest_revisits <- lapply(milvus_track_list, function(x)
  recurse::getRecursionsAtLocations(x = as.data.frame(x[1:4]), locations = as.data.frame(x %>% dplyr::select(nest_long,nest_lat))[1,],
                                    radius = nestradius, timeunits = "hours"))

# writing the distances into the track data frame
for (i in 1:length(nest_revisits)) {
  milvus_track$nest_dist[milvus_track$id == unique(milvus_track$id)[i]] <-
    nest_revisits[[i]]$dists
}

#### max absence time during key brood phase 15 May to 15 June
absence_times <- lapply(nest_revisits, function(x)
  purrr::pluck(x,5)) %>% bind_rows()




