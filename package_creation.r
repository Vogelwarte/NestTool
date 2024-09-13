#### NEED TO DO THIS FOR NEW REPO ###
##  https://kbroman.org/AdvData/18_rpack_demo.html
## https://www.mjandrews.org/blog/how-to-make-an-R-package/
## https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
## https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
  
library(devtools)
library(usethis)
library(roxygen2)
#install.packages("sinew") 
library(sinew)
#remotes::install_github("dreamRs/prefixer")

### CREATING PACKAGE BACKBONE

setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2")
#setwd("C:/STEFFEN/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI")

#devtools::create("NestTool")
#cat("Package: NestTool\n", file = "DESCRIPTION")
#cat("Version: 1.0.0\n", file = "DESCRIPTION", append = TRUE)
# usethis::create_package(
#   path="C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI",
#   fields = list(),
#   rstudio = rstudioapi::isAvailable(),
#   roxygen = TRUE,
#   check_name = TRUE,
#   open = rlang::is_interactive()
# )



# Package: NestTool
# Title: Detect nesting and outcome from tracking data
# Version: 1.0
# Authors@R: 
#   person("Steffen", "Oppel", , "steffen.oppel@vogelwarte.ch", role = c("aut", "cre"),
#          comment = c(ORCID = "0000-0002-8220-3789"))
# person("Ursin", "Beeli", , "ursin@asdfg.ch", role = c("ctb"))
# Description: The NestTool package is an informal collection of R functions created by the Swiss Ornithological Institute
# to extract demographic information from the tracking data of birds. The functions prepare movement metrics,
# and predict whether a nesting attempt occurred and whether the attempt was successful.
# License: `use_mit_license()`, `use_gpl3_license()` or friends to pick a
# license
# Encoding: UTF-8

### creates a utility function for the pipe operator
#usethis::use_pipe()

### CREATING DOCUMENTATION
devtools::load_all()
#prefixer()

makeOxygen(data_prep)
makeOxygen(move_metric_extraction)
makeOxygen(train_nest_detection)
makeOxygen(movement_visualisation)
makeOxygen(train_nest_success)
makeOxygen(predict_success)
makeOxygen(plot_move_metrics)
makeOxygen(train_nest_detection)
makeOxygen(predict_nesting)
makeOxygen(predict_ranging)
makeOxygen(train_home_range_detection)

roxygen2::roxygenize(package.dir = "C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2")

### ADD DATA
# kite.tracks <- fread(here("output/06_example_individuals/example_individuals_trackingdata.csv"))
# kite.nesting  <- fread(here("output/06_example_individuals/example_individuals_validation.csv"))
# usethis::use_data(kite.tracks)
# usethis::use_data(kite.nesting)


saveRDS(nest_model$model,'data/nest_model.rds')
saveRDS(hr_model$model,'data/hr_model.rds')
saveRDS(succ_model$model,'data/succ_model.rds')


devtools::document()



### CREATING THE READ ME.md file for GitHub
library(knitr)
knit(input="README.rmd", output = "README.md") #see ?knit for more options

## use this manually if the knitting does not show the images
## ![Home range variable importance](plots/HR_varimp.png?raw=true "Permutation-derived variable importance of predictor variables to classify whether home range behaviour occurred")


### add all the libraries we need to import
#usethis::use_tidy_dependencies() 

# use_package('dplyr',type = "Imports")
# use_package('data.table',type = "Imports")
# use_package('geosphere',type = "Imports")
# use_package('amt',type = "Imports")
# use_package('mapview',type = "Imports")
# use_package('ggplot2',type = "Imports")
# use_package('ggpubr',type = "Imports")
# use_package('randomForest',type = "Imports")
# use_package('here',type = "Imports")
# use_package('tidyverse',type = "Depends")
# use_package('devtools',type = "Imports")
# use_package('sf',type = "Imports")
# use_package('recurse',type = "Imports")
# use_package('ggspatial',type = "Imports")
# use_package('lubridate',type = "Imports")
# use_package('ranger',type = "Imports")
# use_package('caret',type = "Imports")
              

## this fails in RStudio but works in base R
## https://github.com/rstudio/rstudio/issues/12945
?move_metric_extraction
?movement_visualisation


### CREATING A VIGNETTE
# usethis::use_vignette(name="NestTool")



### INSTALLING THE PACKAGE

### THIS WILL ONLY WORK IF YOU MANUALLY REMOVE THE importFrom(tidyr,ALL) lines in the NAMESPACE

devtools::load_all()
devtools::install()

