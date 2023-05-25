#### NEED TO DO THIS FOR NEW REPO ###
##  https://kbroman.org/AdvData/18_rpack_demo.html
## https://www.mjandrews.org/blog/how-to-make-an-R-package/
## https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
## https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
  
library(devtools)
library(usethis)
library(roxygen2)

### CREATING PACKAGE BACKBONE

setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool2")
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
# person("Ursin", "Beeli", , "ursin@asdfg.ch", role = c("aut", "cre"))
# Description: The NestTool package is an informal collection of R functions created by the Swiss Ornithological Institute
# to extract demographic information from the tracking data of birds. The functions prepare movement metrics,
# and predict whether a nesting attempt occurred and whether the attempt was successful.
# License: `use_mit_license()`, `use_gpl3_license()` or friends to pick a
# license
# Encoding: UTF-8

### CREATING DOCUMENTATION
roxygen2::roxygenize()

### ADD DATA
#kites <- fread(here("output/02_preprocessing/04_milvus_thuringia.csv"))
#usethis::use_data(kites)

devtools::document()
devtools::load_all()


### add all the libraries we need to import
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


### CREATING A VIGNETTE
# usethis::use_vignette(name="NestTool")



### INSTALLING THE PACKAGE
devtools::load_all()
devtools::install()
devtools::install_github('NestTool','steffenoppel')
