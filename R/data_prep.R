## data_prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Prepare and extract seasonal summaries of tracked individuals during breeding season to predict nesting behaviour.
#'
#' \code{data_prep} extracts temporally varying summary information from tracking data of individuals
#' to help diagnose whether a breeding attempt was initiated, and whether it was successful or not. The following metrics are calculated: 95% MCP home range area,
#' maximum time away from nest radius, median daily time spent at nest, median daily travel distance, and median daily maximum distance from nest.
#' The function first identifies the most plausible location of a potential nest by calculating time spent and revisits to all locations within a buffer of 
#' \code{nestradius}, and then uses this location of a nest to calculate distances and times spent within or outwith that radius.
#'
#'
#' @param trackingdata data.frame with tracking data set with information on individual season and id as unique identifier ('year_id').
#' Must contain a timestamp, an individual animal id ('bird_id'), and coordinates in both geographic ('lat_wgs', 'long_wgs') and projected coordinate systems ('lat_eea', 'long_eea'). 
#' @param indseasondata data.frame with nesting information per individual and season, which links to \code{trackingdata} by the unique identifier ('year_id').
#' Must contain columns for the age ('age_cy' as integer in calender years) and sex ('sex': either 'm' or 'f') of individuals in that season.
#' Will optionally contain three columns of binary training data, namely 'HR' (whether a home range existed or not), 'nest' (whether a nesting attempt took place) and 'success' (whether the nest was successful)
#' If no nesting information is available, \code{indseasondata} can be created from \code{trackingdata} by grouping and summarising \code{trackingdata} to yield one line per individual and season.
#' @param crs_epsg numeric. EPSG code for the Coordinate Reference System of the projected coordinates in \code{trackingdata$lat_eea} and \code{trackingdata$long_eea}. Find EPSG code at https://epsg.io/. Default is 3035 (Lamberth Azimuthal Equal Area for Europe)
#' @param latboundary numeric. Value for the southern latitudinal boundary, i.e. locations will only be retained if \code{trackingdata$lat_wgs} is >\code{latboundary}
#' @param longboundary  numeric. Value for the western longitudinal boundary, i.e. locations will only be retained if \code{trackingdata$long_wgs}  is >\code{longboundary}
#' @param minlocs integer. Minimum number of GPS locations that an individual must have recorded between \code{startseason} and \code{endseason} to be considered for analysis. The remaining individuals will be removed.
#' @param nestradius numeric. Radius around potential nest site in metres. 
#' This radius will determine in which radius the residency times will be calculated, i.e. how long an animal remained within that radius or how long it spent outside this radius.
#' Please consider device accuracy and general movement behaviour of the species when specifying the radius. Default is 50 m.
#' @param homeradius numeric. Radius of home ranging movements around potential nest site in metres. 
#' This radius will determine the core home range and will be used to quantify the amount of time individuals spent outside their core home range to inform the distinction of nest failure.
#' Please consider general movement behaviour of the species when specifying the radius. Default is 2000 m.
#' @param startseason integer. Day of the year (obtained by \code{yday(SomeDate)}) that marks the start of the breeding season. Tracking data before this date will be removed.
#' This day should be the earliest recorded egg laying date for the species. Defaults to 70 (11 Mar).
#' @param endseason integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the breeding season. Tracking data after this date will be removed.
#' This day should be the earliest recorded fledging date for the species. Defaults to 175 (24 Jun).
#' @param settleEnd integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the settlement phase at the beginning of the breeding season.
#' This day will demarcate the time period when birds exhibit a different movement behaviour than once eggs have been laid. Defaults to 97 (7 Apr).
#' @param Incu1End integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the first half of the incubation phase during the breeding season.
#' This day will facilitate that different movement behaviours are quantified during incubation. Defaults to 113 (23 Apr).
#' @param Incu2End integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the incubation phase during the breeding season.
#' This day will demarcate the time period when birds exhibit a different movement behaviour once chicks hatch. Defaults to 129 (9 May).
#' @param Chick1End integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the first half of the chick rearing phase during the breeding season.
#' This day will facilitate that different movement behaviours are quantified during chick rearing. Defaults to 152 (1 Jun).
#' @param broodstart integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the start of the small chick phase. Defaults to 121 (1 May).
#' @param broodend integer. Day of the year (obtained by \code{yday(SomeOtherDate)}) that marks the end of the small chick phase. Defaults to 152 (1 Jun).
#' @param age integer. For birds that have no age information, or for datasets where no 'age_cy' (age in calender years) exists, a value will be filled in to facilitate the prediction of nest success.
#' Defaults to 10, but should be informed by a sensible value for the species in question, so that \code{age} reflects the age in calendar years of birds for which no age information exists.
#'
#' @return Returns a list with 4 data.frames. \code{summary} contains one row per individual season and 45 variables, which each characterise movement metrics for a different period of the breeding season. Output data.frame includes age and sex information.
#' \code{pot_nests} contains one row per individual season and two variables (x and y), representing the longitude and latitude of potential nest sites (in projected coordinate system).
#' \code{nightlocs} contains all nocturnal locations for all individuals, including the number of revisits to within \code{nestradius} and the residence time within \code{nestradius} at that location.
#' \code{daylocs} contains all diurnal locations for all individuals, including the number of revisits to within \code{nestradius} and the residence time within \code{nestradius} at that location.
#'
#'
#' @export 
#' @importFrom lubridate yday ymd
#' @importFrom dplyr arrange mutate filter intersect left_join group_by summarise n select first rename bind_rows ungroup if_else slice_min slice_max ungroup
#' @importFrom purrr reduce pluck
#' @importFrom amt mk_track time_of_day arrange hr_mcp hr_area
#' @importFrom recurse getRecursions getRecursionsAtLocations
#' @importFrom RANN nn2
#' @importFrom sf st_as_sf st_distance st_drop_geometry st_transform st_buffer st_within st_centroid
#' @importFrom stats quantile median
#' @importFrom tidyr replace_na spread gather

data_prep <- function(trackingdata,
                      indseasondata,
                      crs_epsg=3035,
                      latboundary=45,
                      longboundary=4,
                      minlocs=800,
                      nestradius=50,
                      homeradius=2000,
                      startseason=70,
                      endseason=175,
                      broodstart= lubridate::yday(lubridate::ymd("2023-05-01")),
                      broodend= lubridate::yday(lubridate::ymd("2023-06-01")),
                      settleEnd = 97,  # end of the settlement period in yday
                      Incu1End = 113,   # end of the first incubation phase in yday
                      Incu2End = 129,  # end of the second incubation phase in yday
                      Chick1End = 152, # end of the first chick phase in yday
                      age =10           # age in calendar years for birds for which no data exist
                      ) {
  

  
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### DATA PREPARATION TO SUMMARISE MOVEMENT DATA OF RED KITES FOR NEST CLASSIFICATION ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

# created on 16 May 2023 to provide diagnostic outputs over time

  
  # LOADING DATA -----------------------------------------------------------------
  # movement data
  milvus <- trackingdata %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
           date = as.Date(timestamp, tz = "UTC"),
           week = as.integer(format(date, format = "%W")),
           year_week = format(date, format = "%Y-%W"),
           year_week_id = paste0(format(date, format = "%Y_%W"), "_", bird_id),
           date_id = paste0(date, "_", bird_id),
           event_id=seq_along(timestamp),
           year_day = lubridate::yday(timestamp)) %>%
    dplyr::filter(long_wgs>longboundary &  lat_wgs>latboundary)   ## remove all locations from Spain and southern France
 
   
  # DATA PREPARATION -------------------------------------------------------------
  # keeping only the information of relevant dates (yday 70-175)
  milvus <- milvus %>%
    dplyr::filter(year_day %in% c(startseason:endseason))
  
  # IF VALIDATION DATA ARE PROVIDED READ THEM IN AND JOIN WITH OTHER DATA ##
  # the conditional formulation does not work, so indseasondata must always be provided, but it may not have all columns
  
    if('age_cy' %in% names(indseasondata)){
      indseasondata<- indseasondata %>% dplyr::ungroup() %>% dplyr::mutate(age_cy=dplyr::if_else(is.na(indseasondata$age_cy),age,indseasondata$age_cy)) ### assign user-specified value to missing values in data
    }else{
      indseasondata<- indseasondata %>% dplyr::ungroup() %>% dplyr::mutate(age_cy=age) ### assign user-specified value to non-existing column
    }
    
    if('sex' %in% names(indseasondata)){
      indseasondata<- indseasondata %>% dplyr::ungroup() %>% dplyr::mutate(sex=dplyr::if_else(is.na(indseasondata$sex),"m",indseasondata$sex)) ### assign random value (males) to missing values in data
      indseasondata$sex<-factor(indseasondata$sex, levels=c('m','f'))
    }else{
      indseasondata<- indseasondata %>% dplyr::ungroup() %>% dplyr::mutate(sex="m") ### assign random value (males) to non-existing column
      indseasondata$sex<-factor(indseasondata$sex, levels=c('m','f'))
    }

    if('sex' %in% names(milvus)){
      milvus<- milvus %>% dplyr::mutate(sex=dplyr::if_else(is.na(milvus$sex),"m",milvus$sex)) ### assign random value (males) to missing values in data
      milvus$sex<-factor(milvus$sex, levels=c('m','f'))
    }
    
  ### joining of data must consider whether column names are already present in tracking data
  
  join.cols <- purrr::reduce(list(names(indseasondata), names(milvus), c("year_id","sex","age_cy","nest","fledged","HR","year","bird_id")), dplyr::intersect)
  
  ## insert error message when people try to join numeric and factors
  if(length(join.cols)>1){
  checkTypes<- data.frame(Column = join.cols,
               df1 = sapply(as.data.frame(milvus)[,join.cols], class),
               df2 = sapply(as.data.frame(indseasondata)[,join.cols], class)) %>%
    dplyr::mutate(Diff=dplyr::if_else(df1== df2, "Same", "Different")) %>%
    dplyr::filter(Diff=="Different")}else{
      checkTypes<- data.frame(Column = join.cols,
                              df1 = sapply(as.data.frame(milvus)[,join.cols], class)[1],
                              df2 = sapply(as.data.frame(indseasondata)[,join.cols], class)[1]) %>%
        dplyr::mutate(Diff=dplyr::if_else(df1== df2, "Same", "Different")) %>%
        dplyr::filter(Diff=="Different")
    }
                    
  if(dim(checkTypes)[1]>0){
    print(sprintf("Stopped because columns %s are not of the same type in trackingdata and indseasondata. This will cause a problem when data are joined.", paste(checkTypes$Column,sep=", ")))
    break
  }else{
    milvus <- dplyr::left_join(milvus, indseasondata, by = join.cols)
  }
  
  #### ELIMINATING INDIVIDUALS WITH INSUFFICIENT DATA

  ## counting the gps fixes per individual
  sufficient_data <- milvus %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(number_of_fixes = dplyr::n()) %>%
    dplyr::filter(number_of_fixes > minlocs)
    
  if(dim(sufficient_data)[1]>0){
    milvus <- milvus %>%
      dplyr::filter(year_id %in% sufficient_data$year_id)
    print(sprintf("%i individuals had more than %i locations and have been retained for further analysis",dim(sufficient_data)[1],minlocs))
  }else{
    print("Stopped because not enough data remain. Check the minlocs, startseason and endseason parameters as they may be too restrictive.")
    break
  }  
  
  
  # Creating a track
  # 3 mins
  milvus_track_amt <- milvus %>%
    amt::mk_track(
      .x = long_eea,
      .y = lat_eea,
      .t = timestamp,
      id = year_id,
      date_id,
      event_id,
      crs = crs_epsg
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
    print(sprintf("Stopped because individual(s) %s do not have night locations.", mismatches))
    break
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
  milvus_night_max <- milvus_night %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(sex = dplyr::first(sex),
              age_cy = {if("age_cy" %in% names(.)) dplyr::first(age_cy) else age},   # insert user-specified value 'age'
              residence_time_night = max(residence_time),
              revisits_night = revisits[which(residence_time == max(residence_time))],
              date_night = date[which(residence_time == max(residence_time))],
              long_night = long_eea[which(residence_time == max(residence_time))],
              lat_night = lat_eea[which(residence_time == max(residence_time))],
              nest = {if("nest" %in% names(.)) dplyr::first(nest) else NA}
              ) %>%
    dplyr::group_by(year_id) %>%   ## because there are sometimes duplicates, we need to group again and take the one with max revisits for those where time is equal
    dplyr::summarise(sex = dplyr::first(sex),
              age_cy = {if("age_cy" %in% names(.)) dplyr::first(age_cy) else age},
              revisits_night = max(revisits_night),
              residence_time_night = residence_time_night[which(revisits_night == max(revisits_night))],
              date_night = date_night[which(revisits_night == max(revisits_night))],
              long_night = long_night[which(revisits_night == max(revisits_night))],
              lat_night = lat_night[which(revisits_night == max(revisits_night))],
              nest = {if("nest" %in% names(.)) dplyr::first(nest) else NA}
    )
  })
  
  # filtering the location with longest residence time during daytime
  suppressWarnings({
  milvus_day_max <- milvus_day %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(sex = dplyr::first(sex),
              age_cy = {if("age_cy" %in% names(.)) dplyr::first(age_cy) else age},
              residence_time_day = max(residence_time),
              revisits_day = revisits[which(residence_time == max(residence_time))],
              date_day = date[which(residence_time == max(residence_time))],
              long_day = long_eea[which(residence_time == max(residence_time))],
              lat_day = lat_eea[which(residence_time == max(residence_time))],
              nest = {if("nest" %in% names(.)) dplyr::first(nest) else NA}
    ) %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(sex = dplyr::first(sex),
              age_cy = {if("age_cy" %in% names(.)) dplyr::first(age_cy) else age},
              revisits_day = max(revisits_day),
              residence_time_day = residence_time_day[which(revisits_day == max(revisits_day))],
              date_day = date_day[which(revisits_day == max(revisits_day))],
              long_day = long_day[which(revisits_day == max(revisits_day))],
              lat_day = lat_day[which(revisits_day == max(revisits_day))],
              nest = {if("nest" %in% names(.)) dplyr::first(nest) else NA}
    )
  })
  
  # creating sf objects for distance calculation
  milvus_night_max_sf <- milvus_night_max %>%
    sf::st_as_sf(coords = c("long_night", "lat_night"), crs = crs_epsg)
  milvus_day_max_sf <- milvus_day_max %>%
    sf::st_as_sf(coords = c("long_day", "lat_day"), crs = crs_epsg)
  
  # calculating the distance from the day location to the night location
  milvus_max_res_time <- milvus_day_max_sf %>%
    dplyr::mutate(dist_day_to_night = as.numeric(sf::st_distance(milvus_day_max_sf, milvus_night_max_sf, by_element = T))) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(milvus_night_max_sf %>% dplyr::select(year_id,revisits_night,residence_time_night,date_night), by = "year_id") %>%
    sf::st_drop_geometry()
  
  rm(milvus, milvus_night,milvus_day_recurse,milvus_night_recurse, milvus_track_night_list, milvus_track_day_list)
  
  ##### CALCULATING REVISITS TO POTENTIAL NEST SITE
  # using list apply over all individuals
  # to benefit from getRecursionsAtLocations we add the pseudo-nest coordinates to each 
  # add nest locations to tracking data
  # first calculate the most visited location for both day and night (as above, but data combined)
  milvus_track <- as.data.frame(milvus_track_amt %>% dplyr::select(-tod_))
  milvus_track$id <- factor(milvus_track$id, levels=unique(milvus_track$id))  ## required to prevent re-ordering in split
  milvus_track_list <- split(milvus_track, milvus_track$id)
  milvus_track_amt$id <- factor(milvus_track_amt$id, levels=unique(milvus_track_amt$id))  ## required to prevent re-ordering in split
  milvus_track_amt_list <- split(milvus_track_amt, milvus_track_amt$id)
  
  # calculating recursions (1.5 mins)
  milvus_recurse <- lapply(milvus_track_list, function(x)
    recurse::getRecursions(x = x[1:4], radius = nestradius, timeunits = "hours"))
  
  # calculating MCPs for total season for each year_id
  milvus_MCP <- lapply(milvus_track_amt_list, function(x)
    amt::hr_mcp(x = x[1:4], levels = c(0.95,0.99))$mcp)
  #names(milvus_MCP)
  
  # allocating recurse information to track data frame (4 mins)
  milvus_track$revisits <- NA
  milvus_track$residence_time <- NA
  for (i in 1:length(milvus_recurse)) {
    milvus_track[milvus_track$id == unique(milvus_track$id)[i] ,]$revisits <-
      milvus_recurse[[i]]$revisits
    milvus_track[milvus_track$id == unique(milvus_track$id)[i] ,]$residence_time <-
      milvus_recurse[[i]]$residenceTime
  }
  
  
  ########### REMOVED ON 4 OCT 2023 as too clumsy and imperfect ##########################################
  # # ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  # # ########## CALCULATE DISTANCES BETWEEN NEAREST POINTS AND SELECT NEST ###############################
  # # ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  # ### COMBINED METRIC OF MEAN DISTANCE TO NEAREST NEIGHBOURS AND TIME AND REVISITS
  # 
  # # # identify nearest neighbours and calculate the mean distance to fixed number of nearest neighbours
  # # nearest <- RANN::nn2(milvus_track[,1:2],milvus_track[,1:2],k=150)$nn.dists
  # # milvus_track$NN50dist<-apply(nearest,1,mean)
  # 
  # # identify potential nest by averaging over coordinates with joint greatest residence time and nearest neighbour distance
  # suppressWarnings({milvus_pot_nests <- milvus_track %>% mutate(MOST=residence_time+revisits) %>% #slice_max(order_by=MOST, n=50)
  #   dplyr::group_by(id) %>%
  #   # summarise(revisits=revisits[which(residence_time == max(residence_time))],
  #   #           x = x_[which(residence_time == max(residence_time))],
  #   #           y = y_[which(residence_time == max(residence_time))]) %>%
  #   summarise(revisits=revisits[which(MOST == max(MOST))],
  #               x = x_[which(MOST == max(MOST))],
  #               y = y_[which(MOST == max(MOST))]) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(id) %>%
  #   dplyr::summarise(x = mean(x),
  #             y = mean(y))
  # })
  # 
  # milvus_pot_nest_sf <- milvus_pot_nests %>%
  #   dplyr::rename(year_id=id) %>%
  #   mutate(Type="MaxTime") %>%
  #   sf::st_as_sf(coords = c("x", "y"), crs = crs_epsg)
  # 
  # 
  # 
  # ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  # ########## ALTERNATIVE APPROACH TO POTENTIAL NEST ID USING CENTROID #################################
  # ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  # ### discarded on 3 Oct 2023 because it can create artificial locations in places where the bird never went
  # ### reinstated by providing 3 alternative options
  # 
  # # milvus_pot_nest_sf_alt2 <- milvus_track %>% 
  # #   dplyr::rename(year_id=id) %>%
  # #   slice_min(order_by=NN50dist, n=50, by=year_id) %>%
  # #   sf::st_as_sf(coords = c("x_", "y_"), crs = crs_epsg) %>%
  # #   group_by(year_id) %>%
  # #   summarize(geometry = st_union(geometry)) %>%
  # #   st_centroid()
  # 
  # 
  # # ##### visualise the problem animals where nests are >100 m from actual nest
  # #
  # # PROBLEM_NEST_LOCS <- milvus_pot_nest_sf %>%
  # #   filter(year_id %in% known_nests_sf$year_id) %>%
  # #   arrange(year_id)
  # # PROBLEM_NEST_LOCS <-  PROBLEM_NEST_LOCS %>% mutate(dist_real_nest = st_distance(PROBLEM_NEST_LOCS,known_nests_sf, by_element=T)) %>%
  # #   filter(as.numeric(dist_real_nest) > 100) %>%
  # #   st_transform(4326)

  
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
    nearest <- RANN::nn2(workdat[,1:2],workdat[,1:2],k=max(1,(minlocs/10)))$nn.dists
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
    
  milvus_pot_nest_sf <- milvus_pot_nests %>%
    dplyr::rename(year_id=id) %>%
    sf::st_as_sf(coords = c("x", "y"), crs = crs_epsg)
  print(sprintf("Identified potential nest locations for %i individuals",dim(milvus_pot_nest_sf)[1]))
  
  
  ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  ########## FOR CHECKING ONLY, PLOT NEST AND TRACKING LOCATION ON LEAFLET MAP   #############
  ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  # ### need to make sure that 2 ways of calculating "milvus_pot_nests_sf" are labelled differently
  # library(leaflet)
# 
#   m4 <- leaflet(options = leafletOptions(zoomControl = F)) %>% #changes position of zoom symbol
#     setView(lng = mean(st_coordinates(milvus_pot_nest_sf %>%  st_transform(4326))[,1]),
#             lat = mean(st_coordinates(milvus_pot_nest_sf %>%  st_transform(4326))[,2]),
#             zoom = 8) %>%
#     htmlwidgets::onRender("function(el, x) {L.control.zoom({
#                            position: 'bottomright' }).addTo(this)}"
#     ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
#     addProviderTiles("Esri.WorldImagery", group = "Satellite",
#                      options = providerTileOptions(opacity = 0.6, attribution = F,minZoom = 5, maxZoom = 20)) %>%
#     addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F,minZoom = 5, maxZoom = 15)) %>%
#     addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%
# 
#     addCircleMarkers(
#       data = milvus_night %>% filter(year_id =="2023_8164") %>%sf::st_as_sf(coords = c("long_wgs", "lat_wgs"), crs = 4326),
#       radius = 4,
#       color = "grey1",
#       weight = 0.5,
#       opacity = 0.7,
#       fillColor = "grey1",
#       fillOpacity = 0.7
#     ) %>%
# 
#     addCircleMarkers(
#       data = milvus_day  %>% filter(year_id =="2023_8164") %>% sf::st_as_sf(coords = c("long_wgs", "lat_wgs"), crs = 4326),
#       radius = 4,
#       color = "lightgreen",
#       weight = 0.5,
#       opacity = 0.7,
#       fillColor = "lightgreen",
#       fillOpacity = 0.7,
#       popup = ~paste(residence_time,revisits, sep=" / ")
#     ) %>%
# 
#     addCircleMarkers(
#       data = milvus_pot_nest_sf %>% filter(year_id =="2023_8164") %>% st_transform(4326),   ##%>% filter(Type=="Revised") 
#       radius = 6,
#       color = "firebrick",
#       weight = 0.5,
#       opacity = 0.7,
#       fillColor = "firebrick",
#       fillOpacity = 0.7,
#       popup = ~paste0("year_id: ", year_id)
#     ) %>%
# 
# 
#     addCircleMarkers(
#       data = nests %>% filter(year_id =="2022_8164") %>% filter(year_id %in% milvus_pot_nest_sf$year_id) %>% st_transform(4326),
#       radius = 6,
#       color = "orange",
#       weight = 0.5,
#       opacity = 0.7,
#       fillColor = "orange",
#       fillOpacity = 0.7,
#       popup = ~paste0("year_id: ", year_id)
#     ) %>%
# 
#     addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F))
# 
#   m4
# 
# 
# 
#   
  
  
  ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  ########## ONCE POTENTIAL NEST IS IN APPROPRIATE LOCATION, PROCEED WITH SCRIPT   #############
  ##########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################################
  
  ### ADD THE MOST VISITED LOCATION TO THE TRACK TO GET RECURSIONS TO THIS LOCATION
  #milvus_track <- milvus_track %>% left_join(milvus_day_max %>% dplyr::select(year_id,long_day,lat_day) %>% rename(id=year_id,x=long_day,y=lat_day), by = "id")
  milvus_track <- milvus_track %>% dplyr::left_join(milvus_pot_nests, by = "id") %>%
    dplyr::rename(nest_long=x,nest_lat=y)
  milvus_track_list <- split(milvus_track, milvus_track$id)
  
  # calculating recursions at nest (much quicker because only one location is evaluated)
  nest_revisits <- lapply(milvus_track_list, function(x)
    recurse::getRecursionsAtLocations(x = as.data.frame(x[1:4]), locations = as.data.frame(x %>% dplyr::select(nest_long,nest_lat))[1,],
                             radius = nestradius, timeunits = "hours"))
  
  # calculating recursions to much larger radius to quantify absence time for several days
  nest_absences <- lapply(milvus_track_list, function(x)
    recurse::getRecursionsAtLocations(x = as.data.frame(x[1:4]), locations = as.data.frame(x %>% dplyr::select(nest_long,nest_lat))[1,],
                             radius = homeradius, timeunits = "hours"))
  
  #### calculating the OVERALL 95 and 99 quantiles of nest distances for each year_id
  ## to put broodphase specific distances into perspective
  
  nest_distances <- lapply(nest_revisits, function(x)
    stats::quantile(x = x$dists, c(0.95,0.99)))
  
  absence_times <- lapply(nest_absences, function(x)
    purrr::pluck(x,5)) %>% dplyr::bind_rows()
  
  #### max absence time during key brood phase 15 May to 15 June
  
  suppressWarnings({max_absences<- absence_times %>%
    dplyr::filter(lubridate::yday(exitTime) >= broodstart  & lubridate::yday(exitTime) <= broodend) %>% dplyr::group_by(id) %>%   #### the same days are used below
    dplyr::summarise(T=max(timeSinceLastVisit, na.rm=T))
  })
  
  ###### SUMMARISING REVISIT AND TIME INFORMATION TO MAX RES TIME SUMMARY (1 line per bird year) with summaries per brood phase
  
  milvus_max_res_time$revisitsSettle <- NA
  milvus_max_res_time$revisitsIncu1 <- NA
  milvus_max_res_time$revisitsIncu2 <- NA
  milvus_max_res_time$revisitsChick1 <- NA
  milvus_max_res_time$revisitsChick2 <- NA
  milvus_max_res_time$timeSettle <- NA
  milvus_max_res_time$timeIncu1 <- NA
  milvus_max_res_time$timeIncu2 <- NA
  milvus_max_res_time$timeChick1 <- NA
  milvus_max_res_time$timeChick2 <- NA
  milvus_max_res_time$meandayrevisitsBrood <- NA
  milvus_max_res_time$lastvisitDay <- NA
  milvus_max_res_time$maxtimeawayBrood <- NA
  milvus_max_res_time$tottime_nest <- NA
  milvus_max_res_time$maxtimeawayBrood2km <-NA
  
  ### create blank output summaries
  milvus_all_nest_revisits<-data.frame()
  milvus_broodphase_revisit_summaries<-data.frame()
  mcp_out<-data.frame()
  dim(milvus_max_res_time)
  length(nest_revisits) ## should be the same
  
  
  
  ##### start loop across all individuals ##############################################################################################
  print(sprintf("Now calculating home ranges and movement metrics for %i individuals",dim(milvus_max_res_time)[1]))
  for (i in 1:dim(milvus_max_res_time)[1]) {
    if(milvus_max_res_time$year_id[i] != names(nest_revisits)[i]){
      print(paste("Coming out from for loop Where i =  ", i))
      break
    }
    options(dplyr.summarise.inform = FALSE)
    out<-nest_revisits[[i]] $revisitStats %>% dplyr::mutate(yday=lubridate::yday(entranceTime)) %>%
      dplyr::mutate(broodphase=dplyr::if_else(yday<settleEnd,"Settle",
                               dplyr::if_else(yday<Incu1End,"Incu1",
                                      dplyr::if_else(yday<Incu2End,"Incu2",
                                             dplyr::if_else(yday<Chick1End,"Chick1","Chick2"))))) %>%
      dplyr::mutate(count=1)
    suppressWarnings({summary<- out %>% dplyr::group_by(id,broodphase) %>%
      dplyr::summarise(revisits=sum(count),time=sum(timeInside), last=max(yday),maxTimeAway=max(timeSinceLastVisit,na.rm=T)) %>%
      dplyr::mutate(maxTimeAway=dplyr::if_else(maxTimeAway<0,720,maxTimeAway))})
    
    
    milvus_max_res_time$revisitsSettle[i] <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Settle") %>% dplyr::select(revisits))
    milvus_max_res_time$revisitsIncu1[i]  <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Incu1") %>% dplyr::select(revisits))
    milvus_max_res_time$revisitsIncu2[i]  <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Incu2") %>% dplyr::select(revisits))
    milvus_max_res_time$revisitsChick1[i]  <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Chick1") %>% dplyr::select(revisits))
    milvus_max_res_time$revisitsChick2[i]  <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Chick2") %>% dplyr::select(revisits))
    milvus_max_res_time$timeSettle[i] <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Settle") %>% dplyr::select(time))
    milvus_max_res_time$timeIncu1[i] <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Incu1") %>% dplyr::select(time))
    milvus_max_res_time$timeIncu2[i] <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Incu2") %>% dplyr::select(time))
    milvus_max_res_time$timeChick1[i] <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Chick1") %>% dplyr::select(time))
    milvus_max_res_time$timeChick2[i] <- as.numeric(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase=="Chick2") %>% dplyr::select(time))
    milvus_max_res_time$meandayrevisitsBrood[i] <- sum(summary %>% dplyr::ungroup() %>% dplyr::filter(broodphase %in% c("Incu2","Chick1")) %>% dplyr::select(revisits), na.rm=T)/30
    milvus_max_res_time$lastvisitDay[i] <- max(out$yday,na.rm=T)
    milvus_max_res_time$maxtimeawayBrood[i] <- dplyr::if_else(max(out %>% dplyr::filter(broodphase %in% c("Incu2","Chick1")) %>% dplyr::select(timeSinceLastVisit), na.rm=T)<0,
                                                      720,
                                                      max(out %>% dplyr::filter(broodphase %in% c("Incu2","Chick1")) %>% dplyr::select(timeSinceLastVisit), na.rm=T))
    milvus_max_res_time$tottime_nest[i] <- sum(summary$time, na.rm=t)
    milvus_max_res_time$maxtimeawayBrood2km[i] <- ifelse(length(max_absences$T[max_absences$id==milvus_max_res_time$year_id[i]])==1,   ### this does not work with dplyr::if_else because it throws an error if max_absences==NULL
                                                         max_absences$T[max_absences$id==milvus_max_res_time$year_id[i]], -10)
    
    
    ### write out the tables created
    milvus_all_nest_revisits<-dplyr::bind_rows(milvus_all_nest_revisits,out)
    milvus_broodphase_revisit_summaries<-dplyr::bind_rows(milvus_broodphase_revisit_summaries,summary)
    
    ### calculate MCP for each brood phase
    mcpin<-milvus_track_amt %>% dplyr::filter(id==names(nest_revisits)[i]) %>% dplyr::mutate(yday=lubridate::yday(t_)) %>%
      dplyr::mutate(broodphase=dplyr::if_else(yday<settleEnd,"Settle",
                               dplyr::if_else(yday<Incu1End,"Incu1",
                                      dplyr::if_else(yday<Incu2End,"Incu2",
                                             dplyr::if_else(yday<Chick1End,"Chick1","Chick2")))))
    for(s in unique(mcpin$broodphase)){
      mcp_area <- amt::hr_mcp(mcpin %>% dplyr::filter(broodphase==s), levels = c(0.95,0.99)) %>% amt::hr_area()
      mcp_area$area <- mcp_area$area/milvus_MCP[[i]]$area   #### sets the MCP area in proportion to the individuals 
      mcp_out<-mcp_area %>% dplyr::select(level,area) %>% dplyr::mutate(id=names(nest_revisits)[i], broodphase=s) %>%
        dplyr::bind_rows(mcp_out)
    }
    
    ### if no locations exist inside nest 2km radius then set time away to 720, otherwise to 0
    if(milvus_max_res_time$maxtimeawayBrood2km[i]<0){
      focal_nest<-milvus_pot_nest_sf %>% dplyr::filter(year_id==milvus_max_res_time$year_id[i]) %>%
        sf::st_transform(crs = crs_epsg) %>%
        sf::st_buffer(dist=homeradius)
      locs_brood<-milvus_track_amt %>% dplyr::filter(id==names(nest_revisits)[i]) %>% dplyr::mutate(yday=lubridate::yday(t_)) %>%
        dplyr::filter(yday >= broodstart  & yday <= broodend) %>% 
        sf::st_as_sf(coords = c("x_", "y_"), crs = crs_epsg) %>%
        sf::st_within(focal_nest)
      
      milvus_max_res_time$maxtimeawayBrood2km[i] <- dplyr::if_else(dim(locs_brood)[2]==1,0,720) ### if there are no locations in the 2km circle around the nest then set max time away to 720, otherwise 0
      milvus_max_res_time$maxtimeawayBrood[i] <- dplyr::if_else(dim(locs_brood)[2]==1,0,720) ### if there are no locations in the 2km circle around the nest then set max time away to 720, otherwise 0
    }
    
    ### SAFETY LOOP to flag up any mismatches in list dimensions
    
    if(milvus_max_res_time$year_id[i] != names(milvus_MCP)[i]){
      print(paste("Coming out from for loop Where i =  ", i))
      break
    }
  }
  
  
  ##### end of loop across all individuals ##############################################################################################
  
  
  
  
  
  ## replace all NA with 0 because it means that the bird did not spend time in that brood phase
  names(milvus_max_res_time)
  milvus_max_res_time<-as.data.frame(milvus_max_res_time)
  for(c in seq(13,26,1)){
    milvus_max_res_time[,c]<-tidyr::replace_na(milvus_max_res_time[,c],0)
  }
  #milvus_max_res_time$maxtimeawayBrood<-tidyr::replace_na(milvus_max_res_time$maxtimeawayBrood,720)  ## if no data exist the bird was away for 30 days or 720 hrs
  milvus_max_res_time$maxtimeawayBrood2km<-dplyr::if_else(milvus_max_res_time$maxtimeawayBrood2km<0,720,milvus_max_res_time$maxtimeawayBrood2km)  ## if no data exist the bird was away for 30 days or 720 hrs
  
  ### TRANSCRIBING THE MCP_OUT INTO VARIABLES
  milvus_max_res_time<-mcp_out %>% dplyr::mutate(VarName=paste("MCP",as.integer(100*level),broodphase, sep="")) %>%
    dplyr::select(-broodphase,-level) %>%
    tidyr::spread(key=VarName, value=area, fill=1) %>%  ### fill with 1 as this is now a relative measure, 1 indicates that broodphase MCP is same size as total MCP
    dplyr::rename(year_id=id) %>%
    dplyr::left_join(milvus_max_res_time, by="year_id") 
  
  
  
  
  ############ CALCULATING BROODPHASE SPECIFIC DISTANCES ##############
  
  # creating sf objects for distance calculation
  milvus_track_sf <- milvus_track %>% dplyr::mutate(yday=lubridate::yday(t_)) %>%
    dplyr::mutate(broodphase=dplyr::if_else(yday<settleEnd,"Settle",
                             dplyr::if_else(yday<Incu1End,"Incu1",
                                    dplyr::if_else(yday<Incu2End,"Incu2",
                                           dplyr::if_else(yday<Chick1End,"Chick1","Chick2"))))) %>%
    dplyr::select(id,t_, broodphase,x_,y_) %>%
    sf::st_as_sf(coords = c("x_", "y_"), crs = crs_epsg)
  milvus_nest_sf <- milvus_pot_nests %>%
    sf::st_as_sf(coords = c("x", "y"), crs = crs_epsg)
  
  # calculating the distance from each location to the potential nest location
  # and taking the 95% quantile per broodphase
  print(sprintf("Now calculating distances of all locations to the nests of %i individuals",length(unique(milvus_track_sf$id))))
  dist_out<-data.frame()
  for (i in unique(milvus_track_sf$id)) {
    ### extract distances for individual
    refdist<-nest_distances[[which(names(nest_distances)==i)]]
    ### summarise broodphase distances
    dist_out<-milvus_track_sf[milvus_track_sf$id==i,] %>%
      dplyr::mutate(dist=sf::st_distance(milvus_track_sf[milvus_track_sf$id==i,], milvus_nest_sf[milvus_nest_sf$id==i,]))%>%
      dplyr::group_by(id,broodphase) %>%
      dplyr::summarise(`Dist95`=stats::quantile(dist,0.95)/refdist[1],Dist99=stats::quantile(dist,0.99)/refdist[2]) %>%   ### converted to relative distances
      sf::st_drop_geometry() %>%
      dplyr::bind_rows(dist_out)
  }
  
  ### TRANSCRIBING THE dist_OUT INTO VARIABLES
  milvus_max_res_time<-dist_out %>% tidyr::gather(key="Dist",value="max",-id,-broodphase) %>%
    dplyr::mutate(VarName=paste(Dist,broodphase, sep="")) %>%
    dplyr::select(-broodphase,-Dist) %>%
    tidyr::spread(key=VarName, value=max, fill=2) %>%   ## assuming that no data means the individual was twice as far away as most of the time
    dplyr::rename(year_id=id) %>%
    dplyr::left_join(milvus_max_res_time, by="year_id") 
  
  
  
  
  # NORMALISING DISTANCE FROM LONGEST VISITED DAY LOCATION TO LONGEST VISITED
  # NIGHT LOCATION BY MEDIAN DISTANCE OF ALL DAY LOCATIONS TO LONGEST VISITED
  # NIGHT LOCATION
  
  # creating two data frames of same dimension to calculate distances
  
  # sf object of most visited night location
  milvus_night_max_sf_dist <- milvus_day %>%
    dplyr::select(event_id, year_id) %>%
    dplyr::left_join(milvus_night_max_sf, by = "year_id") %>%
    sf::st_as_sf(crs = crs_epsg)
  
  # sf object of all day locations
  milvus_day_sf <- milvus_day %>%
    sf::st_as_sf(coords = c("long_eea", "lat_eea"), crs = crs_epsg)
  
  # calculating the distances
  milvus_day_sf$dist_to_max_night <- as.numeric(sf::st_distance(milvus_day_sf,
                                                            milvus_night_max_sf_dist,
                                                            by_element = T))
  
  # summarising median for each brood cycle
  milvus_day_median_dist <- milvus_day_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(year_id) %>%
    dplyr::summarise(median_dist_to_max_night = stats::median(dist_to_max_night))
  
  # creating a data frame with all relevant information
  milvus_dist_norm <- milvus_max_res_time %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(milvus_day_median_dist, by = "year_id") %>%
    dplyr::mutate(distance_normalised = dist_day_to_night/median_dist_to_max_night)
  
  
  ## CREATING A TABLE WITH ALL RELEVANT INFORMATION FOR A RANDOM FOREST RUN
  milvus_dist_summary <- milvus_dist_norm %>%
    dplyr::rename(dist_max_day_to_max_night = dist_day_to_night,
           median_day_dist_to_max_night = median_dist_to_max_night,
           relative_dist_max_day_to_max_night = distance_normalised) %>%
    dplyr::select(-geometry)
  
  
  ## ADDING VALIDATION DATA FOR NEST, SUCCESS AND HOME RANGE
  ### joining of data must consider whether column names are already present in tracking data
  
  join.cols <- purrr::reduce(list(names(indseasondata), names(milvus_dist_summary), c("year_id","sex","age_cy","nest","fledged","HR","year","bird_id","success")), dplyr::intersect)
  milvus_dist_summary <- dplyr::left_join(milvus_dist_summary, indseasondata, by = join.cols)
  
  
  ## saving files
  #data.table::fwrite(milvus_dist_summary, "output/04_nest/03_distance_RF.csv")
  return(list(summary=milvus_dist_summary, pot_nests=milvus_pot_nests, movementtrack=milvus_track_amt, nightlocs=milvus_track_night, daylocs=milvus_track_day))
  print(sprintf("Data preparation completed for %i individuals. Summary output in slot 'summary', nest locations in slot 'pot_nests'.",length(unique(milvus_track_sf$id))))
  
}
