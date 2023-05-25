## move_metric_extraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Extract seasonal movement metrics of tracked individuals during breeding season
#'
#' \code{move_metric_extraction} extracts temporally varying summary information from tracking data of individuals
#' to help diagnose whether a breeding attempt was successful or not. The following metrics are calculate: 95% MCP home range area,
#' maximum time away from nest radius, median daily time spent at nest, median daily travel distance, and median daily maximum distance from nest.
#' The function first identifies the most plausible location of a potential nest by calculating time spent and revisits to all locations within a buffer of 
#' \code{nestradius}, and then uses this location of a nest to calculate distances and times spent within or outwith that radius.
#'
#'
#' @param trackingdata tibble with tracking data set with information on individual season and id.
#' Created by \code{\link{data_prep}} and returned as \code{movementtrack}. Must contain id, timestamp, and coordinates in projected coordinate system.
#' @param nest_locs data.frame with potential nest coordinates for each individual season and id.
#' Created by \code{\link{data_prep}} and returned as \code{pot_nests}. Must contain id, and nest coordinates in projected coordinate system.
#' @param ind.data data.frame with unique identifier for individual seasons.
#' Must contain columns for the age and sex of individuals in that season. Format of id must match that of \code{trackingdata}
#' @param uncertainty numeric value between 0 and 0.5. Individuals for which the nest success classification resulted in a probability <\code{uncertainty}
#' are retained for data extraction. A value close to 0 will result in many individuals falling below the uncertainty threshold, resulting in longer runtime of the function as more individuals will be processed.
#' A value close to 0.5 will result in very few individuals that will be processed and for which movement metrics will be returned.
#' @param nestradius numeric. Radius around potential nest site in metres. 
#' This radius will determine in which radius times will be calculated, i.e. how long an animal remained within that radius or how long it spent outside this radius.
#' Please consider device accuracy and general movement behaviour of the species when specifying the radius.
#' @param startseason integer. Day of the year (obtained by \code{yday(SomeDate)}) that marks the start of the breeding season. Tracking data before this date will be removed.
#' This day should be the earliest recorded egg laying date for the species. Defaults to 70 (11 Mar).
#' @param endseason integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the breeding season. Tracking data after this date will be removed.
#' This day should be the earliest recorded fledging date for the species. Defaults to 175 (24 Jun).
#'
#' @return Returns a data.frame with movement metrics for each individual season and 5-day window, including age and sex information.
#'
#'
#' @export 
#' @importFrom dplyr filter select bind_rows arrange ungroup rename group_by mutate summarise bind_cols left_join
#' @importFrom amt hr_mcp mk_track step_lengths
#' @importFrom lubridate yday parse_date_time
#' @importFrom purrr pluck
#' @importFrom recurse getRecursionsAtLocations
#' @importFrom stats median
#' @importFrom utils head
#'

move_metric_extraction <- function(trackingdata,
                                   nest_locs,
                                   inddata,
                                   nestradius=50,
                                   startseason=70,
                                   endseason=175,
                                   uncertainty=0.45) {



# LOADING DATA -----------------------------------------------------------------
# nest success classification data to select which individuals plots are needed for
unsure<-inddata %>%
  filter(succ_prob>uncertainty & succ_prob <(1-uncertainty))
check_inds<-unique(unsure$year_id)  ## the individuals that need to be checked and for which manual classification will be necessary
 
# # DATA PREPARATION -------------------------------------------------------------

# Creating a track and calculating step lengths
# 3 mins
milvus_track_amt <- trackingdata %>%
  mk_track(
    .x = x_,
    .y = y_,
    .t = t_,
    id = id,
    date_id,
    event_id,
    tod_,
    crs = 3035
  ) %>%
  arrange(id, t_)
milvus_track_amt$step_dist<-step_lengths(milvus_track_amt)


### ADD THE MOST VISITED LOCATION TO THE TRACK TO GET RECURSIONS TO THIS LOCATION

milvus_track <- as.data.frame(milvus_track_amt) %>% left_join(nest_locs, by = "id") %>%
  rename(nest_long=x,nest_lat=y)
head(milvus_track)


##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## CALCULATE DISTANCES FROM AND TIMES AT POTENTIAL NEST   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################


# calculating recursions to potential nest site
milvus_track_list <- split(milvus_track, milvus_track$id)
nest_revisits <- lapply(milvus_track_list, function(x)
  getRecursionsAtLocations(x = as.data.frame(x[1:4]), locations = as.data.frame(x %>% dplyr::select(nest_long,nest_lat))[1,],
                           radius = nestradius, timeunits = "hours"))

# writing the distances into the track data frame
for (i in 1:length(nest_revisits)) {
  milvus_track$nest_dist[milvus_track$id == unique(milvus_track$id)[i]] <-
    nest_revisits[[i]]$dists
}
head(milvus_track)

#### max absence time during key brood phase 15 May to 15 June
absence_times <- lapply(nest_revisits, function(x)
  purrr::pluck(x,5)) %>% bind_rows()





##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
########## LOOP OVER EACH INDIVIDUAL AND EXTRACT DATA FOR 5-day MOVING WINDOW   #############
##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################

## define the central points of moving window (with 2 days on either side of centre day)
wincentres<-seq(startseason+2,endseason-2,3)

### create blank output summaries
milvus_5d_move_metrics<-data.frame()


 
 ##### start loop across all individuals and time windows ##############################################################################################
  
 for (i in check_inds) {

   ind_track<-milvus_track %>% filter(id==i)
   ind_times<-absence_times %>% filter(id==i)
   ind_track_amt<-milvus_track_amt %>% filter(id==i)
   
   for (w in wincentres) {
     window<-seq(w-2,w+2,1)  ## create a 5 day moving window
     win_track<-ind_track %>% mutate(yday=yday(t_)) %>% filter(yday %in% window)
     
     if(dim(win_track)[1]>1){
       
       win_times<-ind_times %>% mutate(yday=yday(entranceTime)) %>% filter(yday %in% window)
       win_track_amt<-ind_track_amt %>% mutate(yday=yday(t_)) %>% filter(yday %in% window)
       
       ### calculate mean daily travel distance and distance from nest
       day_sum<-win_track %>% group_by(id,yday) %>%
         summarise(daydist=sum(step_dist,na.rm=T), nestdist=max(nest_dist,na.rm=T)) %>%
         ungroup() %>%
         group_by(id) %>%
         summarise(median_daydist=median(daydist,na.rm=T), median_nestdist=median(nestdist,na.rm=T))
       
       ### calculate MCP95
        day_sum$MCP<-as.numeric(hr_mcp(win_track_amt, levels=0.95)$mcp$area)/10000  ## convert home range area to hectares
       
       ### calculate time at nest and max time away from nest
        if(dim(win_times)[1]>0){
          day_sum<-win_times %>% group_by(id,yday) %>%
            summarise(time=sum(timeInside)) %>%
            ungroup() %>%
            group_by(id) %>%
            summarise(median_time_at_nest=median(time,na.rm=T)) %>%
            mutate(median_time_at_nest=ifelse(median_time_at_nest>24,24,median_time_at_nest)) %>% ## time at nest cannot exceed hrs in the day, but can occur mathematically because recursions are calculated not daily
            left_join(day_sum, by="id")        ## calculate median daily time at nest
          suppressWarnings({day_sum<-win_times %>% summarise(max_time_away_from_nest=max(timeSinceLastVisit,na.rm=T)) %>% bind_cols(day_sum)}) ## calculate max time away from nest
        }else{
          day_sum$median_time_at_nest<-ifelse(day_sum$median_nestdist>150,0,24)         ## if there are no locations near the nest the bird spent all time away
          day_sum$max_time_away_from_nest<-ifelse(day_sum$median_nestdist>150,5*24,0)   ## if there are no locations near the nest then the bird was away all 5 days
        }
      
      ## summarise the output and write into data.frame
      milvus_5d_move_metrics<- day_sum %>% mutate(week=format(parse_date_time(x = w, orders = "j"), format="%d %b")) %>%
                                  select(id,week,MCP,median_daydist,median_nestdist,median_time_at_nest,max_time_away_from_nest) %>%
                                  bind_rows(milvus_5d_move_metrics)

     }else{print(sprintf("no data for %s in week %s",i,format(parse_date_time(x = w, orders = "j"), format="%d %b")))}

   }
 }

 ##### end of loop across all individuals and weeks ##############################################################################################
 
milvus_5d_move_metrics <- milvus_5d_move_metrics %>% left_join(
  (inddata %>% select(year_id,sex,age_cy) %>%
    rename(id=year_id)),by ="id")

return(milvus_5d_move_metrics)
}

