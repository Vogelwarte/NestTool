####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### PLOT RF TREES FOR MODELS IN NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### requested by a Reviewer 1 to visualise and 'grasp' how each datapoint is classified

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
library(reprtree)  ## instead of loading that package manually define the functions so we can adjust the digits:

labelBG <- function(tr){
  require(plotrix)
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr,uniform=TRUE)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  rows <- tree:::labels.tree(tr)[left.child]
  rows <- gsub('NA,','',rows)
  ind <- !is.na(left.child)
  boxed.labels(xy$x[ind],xy$y[ind]+0.5*charht, rows[ind] , border=F, bg='white',
               cex=0.8, xpad=0.5, ypad=1)
}
labelYN <- function(tr){
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr, uniform=T)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  ind <- !is.na(left.child)
  text(xy$x[ind]-0.1, xy$y[ind]-0.2*charht, '<< Y',cex=0.6, adj=1)
  text(xy$x[ind]+0.1, xy$y[ind]-0.2*charht, 'N >>', cex=0.6, adj=0)
}

plot.tree <- function (x, y = NULL, type = c("proportional", "uniform"), ...) 
{
  if (inherits(x, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(x, "tree")) 
    stop("not legitimate tree")
  type <- match.arg(type)
  uniform <- type == "uniform"
  dev <- dev.cur()
  if (dev == 1L) 
    dev <- 2L
  #assign(paste0("device", dev), uniform, envir = tree_env)
  invisible(treepl(tree:::treeco(x, uniform), node = as.integer(row.names(x$frame)), 
                   ...))
}


treepl <- function (xy, node, erase = FALSE, ...) 
{
  # Modified from tree:::treepl
  x <- xy$x
  y <- xy$y
  parent <- match((node%/%2L), node)
  sibling <- match(ifelse(node%%2L, node - 1L, node + 1L), 
                   node)
  xx <- rbind(x, x, x[sibling], x[sibling], NA)
  yy <- rbind(y, y[parent], y[parent], y[sibling], NA)
  if (any(erase)) {
    lines(c(xx[, erase]), c(yy[, erase]), col = par("bg"))
    return(x = x[!erase], y = y[!erase])
  }
  plot(range(x), c(min(y)-0.5, max(y)+0.5), type = "n", axes = FALSE, xlab = "", 
       ylab = "")
  text(x[1L], y[1L], "|", ...)
  lines(c(xx[, -1L]), c(yy[, -1L]), ...)
  list(x = x, y = y)
}


plot.getTree <-function (rforest = NULL, k = 1, main = NULL) 
{
  gTree <- randomForest::getTree(rforest, k = k, labelVar = TRUE)
  x <- as.tree(gTree, rforest)
  x$frame$splits<-substr(x$frame$splits,1,sig.digits+3)
  plot(x, type = "uniform")
  text(x, split = FALSE, ...)
  labelBG(x)
  labelYN(x)
  title(main = main)
}



## set root folder for project
try(setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2"),silent=T)
try(setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool2"),silent=T)


### LOAD PREPARED TRACKING DATA AND FIT A RANDOM FOREST MODEL 
trackingsummary<-readRDS("input_data_prepared.rds")$summary
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
                                  data=trackingsummary, mtry=3, ntree=2500, replace=T, maxnodes=16)

### manually plot the tree
gTree <- getTree(RF1, k = 50, labelVar = TRUE)
x <- as.tree(gTree, RF1)
plot(x, type = "uniform")
str(x)
x$frame$splits<-substr(x$frame$splits,1,6)
text(x, split = FALSE)
labelBG(x)
labelYN(x)
title(main = "Example decision tree for the classification of home ranging behaviour")

RF2 <- randomForest::randomForest(as.factor(nest) ~ sex + revisits_day + residence_time_day + age_cy +
                        revisits_night + residence_time_night + 
                        dist_max_day_to_max_night + median_day_dist_to_max_night +
                        relative_dist_max_day_to_max_night+
                        revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                        timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                        Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                        MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                      data=trackingsummary, mtry=3, ntree=2500, replace=T, maxnodes=16)
### manually plot the tree
gTree <- getTree(RF2, k = 50, labelVar = TRUE)
x <- as.tree(gTree, RF2)
plot(x, type = "uniform")
str(x)
x$frame$splits<-substr(x$frame$splits,1,6)
text(x, split = FALSE)
labelBG(x)
labelYN(x)
title(main = "Example decision tree for the classification of nest initiation")
#reprtree::plot.getTree(RF2, k=50, main="Example decision tree for the classification of nest initiation", depth=8)


RF3 <- randomForest::randomForest(as.factor(success) ~ sex + revisits_day + residence_time_day + age_cy +
                       revisits_night + residence_time_night + 
                                    dist_max_day_to_max_night + median_day_dist_to_max_night +
                                 relative_dist_max_day_to_max_night+
                                    revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                                    timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                                    Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                                    MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                                  data=trackingsummary[trackingsummary$success %in% c("yes","no"),], mtry=3, ntree=2500, replace=T, maxnodes=16)
### manually plot the tree
gTree <- getTree(RF3, k = 50, labelVar = TRUE)
x <- as.tree(gTree, RF3)
plot(x, type = "uniform")
str(x)
x$frame$splits<-substr(x$frame$splits,1,6)
text(x, split = FALSE)
labelBG(x)
labelYN(x)
title(main = "Example decision tree for the classification of nest success")
#reprtree::plot.getTree(RF3, k=50, main="Example decision tree for the classification of nest success", depth=8)



# RANDOM FOREST TREE PLOTTING LOOP (ALL TREES) --------------------------------------------
pdf("HomeRange_model_trees.pdf")
for (t in 1:2500) {
  gTree <- getTree(RF1, k = t, labelVar = TRUE)
  x <- as.tree(gTree, RF1)
  plot(x, type = "uniform")
  x$frame$splits<-substr(x$frame$splits,1,6)
  text(x, split = FALSE)
  labelBG(x)
  labelYN(x)
  title(main = sprintf("Decision tree %i for the classification of home range behaviour",t))
}
dev.off()

pdf("NestInitiation_model_trees.pdf")
for (t in 1:2500) {
  gTree <- getTree(RF2, k = t, labelVar = TRUE)
  x <- as.tree(gTree, RF2)
  plot(x, type = "uniform")
  x$frame$splits<-substr(x$frame$splits,1,6)
  text(x, split = FALSE)
  labelBG(x)
  labelYN(x)
  title(main = sprintf("Decision tree %i for the classification of nest occurrence",t))
}
dev.off()



pdf("NestSucc_model_trees.pdf")
for (t in 1:2500) {
  gTree <- getTree(RF3, k = t, labelVar = TRUE)
  x <- as.tree(gTree, RF3)
  plot(x, type = "uniform")
  x$frame$splits<-substr(x$frame$splits,1,6)
  text(x, split = FALSE)
  labelBG(x)
  labelYN(x)
  title(main = sprintf("Decision tree %i for the classification of nest success",t))
}
dev.off()
