####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########
###### PLOT RF TREES FOR MODELS IN NESTTOOL PACKAGE ################
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###########

### install reprtree package ###
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('munoztd0/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))



### install NestTool
library(data.table)
library(tidyverse)
#devtools::install_github("Vogelwarte/NestTool", dependencies=TRUE, force=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed up the process
library(reprtree)
library(NestTool)



# THIS DOES NOT WORK FOR RANGER MODELS; SO NEED TO REFIT EVERYTHING AS RANDOM FOREST
hr_model<-NestTool::hr_model
reprtree::plot.getTree(hr_model$model)



