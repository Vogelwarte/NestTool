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
fig1inds<-bind_rows(NONESTEX,SUCCEX,UNCERTNESTEX) %>%
  select(year_id,age_cy,sex,nest_observed,nest_prob) %>%
  mutate(label=if_else(nest_prob==0,"not breeding",if_else(nest_prob>0.9,"breeding","uncertain")))



##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## LOOP OVER EACH INDIVIDUAL AND EXTRACT DATA FOR 5-day MOVING WINDOW   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################

## define the central points of moving window (with 2 days on either side of centre day)
wincentres<-seq(72,173,3)

### create blank output summaries
fig1_move_metrics<-data.frame()



##### start loop across all individuals and time windows ##############################################################################################

for (i in fig1inds$year_id) {
  
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

fig1_move_metrics <- fig1_move_metrics %>% rename(year_id=id) %>%
  dplyr::left_join(fig1inds,by ="year_id")



# plotting

plot_df<-fig1_move_metrics %>%
  select(-nest_observed,-nest_prob) %>%
  dplyr::filter(year_id %in% c("2019_426","2019_28","2022_747")) %>%  ## select three animals at a time
  dplyr::rename(`Home range size (95% MCP, ha)`= MCP,
                `Median daily travel distance (m)`= median_daydist,
  ) %>%
  tidyr::gather(key="MoveMetric",value="Value",-year_id,-week,-age_cy,-sex,-label) %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::mutate(Date=lubridate::dmy(paste0(week, "2020"))) %>%
  dplyr::ungroup()


ggplot2::ggplot(plot_df) +
  ggplot2::geom_point(ggplot2::aes(x = Date, y=Value, color=MoveMetric), size = 2) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y=Value, color=MoveMetric, group=MoveMetric), linewidth = 1) +
  ggplot2::facet_grid(MoveMetric~label, scales="free_y") + 
  
  ggplot2::labs(y = "", x = "") +
  ggplot2::scale_x_date(date_breaks="2 weeks",date_labels=format("%d %b")) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"),
                 plot.background=ggplot2::element_rect(fill="white"),
                 legend.position="none",
                 panel.grid.major = ggplot2::element_line(colour = "gray70", size = .05),
                 panel.grid.minor = ggplot2::element_line(colour = "gray70"),
                 axis.text=ggplot2::element_text(size=10, color="black"),
                 axis.title=ggplot2::element_text(size=12), 
                 strip.text=ggplot2::element_text(size=12, color="black"), 
                 strip.background=ggplot2::element_rect(fill="white", colour="black"))

ggsave("C:/Users/sop/OneDrive - Vogelwarte/General/MANUSCRIPTS/NestTool/Fig_1.jpg", width=13, height=7)







####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### FIGURE 2: 3 panel plot with example of unsuccessful nesting, successful nesting and uncertain bird
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# calculating recursions to potential nest site
milvus_track_list <- split(milvus_track, milvus_track$id)
nest_revisits <- lapply(milvus_track_list, function(x)
  recurse::getRecursionsAtLocations(x = as.data.frame(x[1:4]), locations = as.data.frame(x %>% dplyr::select(nest_long,nest_lat))[1,],
                                    radius = 50, timeunits = "hours"))

# writing the distances into the track data frame
for (i in 1:length(nest_revisits)) {
  milvus_track$nest_dist[milvus_track$id == unique(milvus_track$id)[i]] <-
    nest_revisits[[i]]$dists
}

#### max absence time during key brood phase 15 May to 15 June
absence_times <- lapply(nest_revisits, function(x)
  purrr::pluck(x,5)) %>% bind_rows()



### plot only home range size and median daily travel distance
fig2inds<-bind_rows(UNSUCCEX,SUCCEX,UNCERTEX) %>%
  select(year_id,age_cy,sex,success_observed,succ_prob) %>%
  mutate(label=if_else(succ_prob<0.1,"failed",if_else(succ_prob>0.9,"successful","uncertain")))



##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## LOOP OVER EACH INDIVIDUAL AND EXTRACT DATA FOR 5-day MOVING WINDOW   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################

## define the central points of moving window (with 2 days on either side of centre day)
wincentres<-seq(72,173,3)

### create blank output summaries
fig2_move_metrics<-data.frame()



##### start loop across all individuals and time windows ##############################################################################################

for (i in fig2inds$year_id) {
  
  ind_track<-milvus_track %>% dplyr::filter(id==i)
  ind_times<-absence_times %>% dplyr::filter(id==i)
  ind_track_amt<-milvus_track_amt %>% dplyr::filter(id==i)
  
  for (w in wincentres) {
    window<-seq(w-2,w+2,1)  ## create a 5 day moving window
    win_track<-ind_track %>% dplyr::mutate(yday=lubridate::yday(t_)) %>% dplyr::filter(yday %in% window)
    
    if(dim(win_track)[1]>1){
      
      win_times<-ind_times %>% dplyr::mutate(yday=lubridate::yday(entranceTime)) %>% dplyr::filter(yday %in% window)
      win_track_amt<-ind_track_amt %>% dplyr::mutate(yday=lubridate::yday(t_)) %>% dplyr::filter(yday %in% window)
      
      ### calculate mean daily distance from nest
      day_sum<-win_track %>% dplyr::group_by(id,yday) %>%
        dplyr::summarise(nestdist=max(nest_dist,na.rm=T)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(median_nestdist=median(nestdist,na.rm=T))
      
      ### calculate MCP95
      day_sum$MCP<-as.numeric(amt::hr_mcp(win_track_amt, levels=0.95)$mcp$area)/10000  ## convert home range area to hectares
      
      ### calculate time at nest and max time away from nest
      if(dim(win_times)[1]>0){
        day_sum<-win_times %>% dplyr::group_by(id,yday) %>%
          dplyr::summarise(time=sum(timeInside)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(median_time_at_nest=median(time,na.rm=T)) %>%
          dplyr::mutate(median_time_at_nest=ifelse(median_time_at_nest>24,24,median_time_at_nest)) %>% ## time at nest cannot exceed hrs in the day, but can occur mathematically because recursions are calculated not daily
          dplyr::left_join(day_sum, by="id")        ## calculate median daily time at nest
        suppressWarnings({day_sum<-win_times %>% dplyr::summarise(max_time_away_from_nest=max(timeSinceLastVisit,na.rm=T)) %>% bind_cols(day_sum)}) ## calculate max time away from nest
      }else{
        day_sum$median_time_at_nest<-ifelse(day_sum$median_nestdist>150,0,24)         ## if there are no locations near the nest the bird spent all time away
        day_sum$max_time_away_from_nest<-ifelse(day_sum$median_nestdist>150,5*24,0)   ## if there are no locations near the nest then the bird was away all 5 days
      }
      
      ## summarise the output and write into data.frame
      fig2_move_metrics<- day_sum %>% dplyr::mutate(week=format(parse_date_time(x = w, orders = "j"), format="%d %b")) %>%
        dplyr::select(id,week,median_nestdist,median_time_at_nest,max_time_away_from_nest) %>%
        dplyr::bind_rows(fig2_move_metrics)
      
    }else{print(sprintf("no data for %s in week %s",i,format(lubridate::parse_date_time(x = w, orders = "j"), format="%d %b")))}
    
  }
}

##### end of loop across all individuals and weeks ##############################################################################################

fig2_move_metrics <- fig2_move_metrics %>% rename(year_id=id) %>%
  dplyr::left_join(fig2inds,by ="year_id")



# plotting

plot_df<-fig2_move_metrics %>%
  select(-success_observed, -succ_prob) %>%
  dplyr::filter(year_id %in% c("2019_337","2020_270","2021_342")) %>%  ## select three animals at a time
  dplyr::rename(`Median max distance from nest (m)`= median_nestdist,
       `Median daily time at nest (hrs)`= median_time_at_nest,
       `Max time away from nest (hrs)`= max_time_away_from_nest,
  ) %>%
  tidyr::gather(key="MoveMetric",value="Value",-year_id,-week,-age_cy,-sex,-label) %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::mutate(Date=lubridate::dmy(paste0(week, "2020"))) %>%
  dplyr::ungroup()


ggplot2::ggplot(plot_df) +
  ggplot2::geom_point(ggplot2::aes(x = Date, y=Value, color=MoveMetric), size = 2) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y=Value, color=MoveMetric, group=MoveMetric), linewidth = 1) +
  ggplot2::facet_grid(MoveMetric~label, scales="free_y") + 
  
  ggplot2::labs(y = "", x = "") +
  ggplot2::scale_x_date(date_breaks="2 weeks",date_labels=format("%d %b")) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"),
                 plot.background=ggplot2::element_rect(fill="white"),
                 legend.position="none",
                 panel.grid.major = ggplot2::element_line(colour = "gray70", size = .05),
                 panel.grid.minor = ggplot2::element_line(colour = "gray70"),
                 axis.text=ggplot2::element_text(size=10, color="black"),
                 axis.title=ggplot2::element_text(size=12), 
                 strip.text=ggplot2::element_text(size=12, color="black"), 
                 strip.background=ggplot2::element_rect(fill="white", colour="black"))

ggsave("C:/Users/sop/OneDrive - Vogelwarte/General/MANUSCRIPTS/NestTool/Fig_2.jpg", width=13, height=7)




