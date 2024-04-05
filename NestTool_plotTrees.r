####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### PLOT RF TREES FOR MODELS IN NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### requested by a reviewer who does not understand random forests

# ### install reprtree package ###
# options(repos='http://cran.rstudio.org')
# have.packages <- installed.packages()
# cran.packages <- c('devtools','plotrix','randomForest','tree')
# to.install <- setdiff(cran.packages, have.packages[,1])
# if(length(to.install)>0) install.packages(to.install)
# 
# library(devtools)
# if(!('reprtree' %in% installed.packages())){
#   install_github('munoztd0/reprtree')
# }
# for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
library(randomForest)
library(reprtree)


## set root folder for project
try(setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2"),silent=T)
try(setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2"),silent=T)


### LOAD PREPARED TRACKING DATA AND FIT A RANDOM FOREST MODEL 
trackingsummary<-loadRDS("input_data_prepared.rds")$summary
trackingsummary$nest
trackingsummary$success

# SPLIT DATA FRAME RANDOMLY INTO TWO--------------------------------------------
milvus_id <- unique(trackingsummary$year_id)
milvus_id_train <- sample(milvus_id, round(length(milvus_id)*0.63))
milvus_train <- trackingsummary %>%
  dplyr::filter(year_id %in% milvus_id_train)

##### RUN MODEL ##########



RF1 <- randomForest::randomForest(as.factor(HR) ~ sex + revisits_day + residence_time_day + age_cy +
                                    revisits_night + residence_time_night + 
                                    dist_max_day_to_max_night + median_day_dist_to_max_night +
                                    relative_dist_max_day_to_max_night+
                                    revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                                    timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                                    Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                                    MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                                  data=trackingsummary, mtry=3, ntree=1500, replace=T, maxnodes=16)

reprtree::plot.getTree(RF1, k=50, main="Example decision tree for the classification of home ranging behaviour", depth=8)


RF2 <- randomForest::randomForest(as.factor(nest) ~ sex + revisits_day + residence_time_day + age_cy +
                        revisits_night + residence_time_night + 
                        dist_max_day_to_max_night + median_day_dist_to_max_night +
                        relative_dist_max_day_to_max_night+
                        revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                        timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                        Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                        MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                      data=trackingsummary, mtry=3, ntree=1500, replace=T, maxnodes=16)

reprtree::plot.getTree(RF2, k=50, main="Example decision tree for the classification of nest initiation", depth=8)


RF3 <- randomForest::randomForest(as.factor(success) ~ sex + revisits_day + residence_time_day + age_cy +
                                    revisits_night + residence_time_night + 
                                    dist_max_day_to_max_night + median_day_dist_to_max_night +
                                    relative_dist_max_day_to_max_night+
                                    revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                                    timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                                    Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                                    MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                                  data=trackingsummary[trackingsummary$success %in% c("yes","no"),], mtry=3, ntree=1500, replace=T, maxnodes=16)

reprtree::plot.getTree(RF3, k=50, main="Example decision tree for the classification of nest success", depth=8)

