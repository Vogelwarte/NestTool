## predict_ranging ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Predict whether ranging occurred based on movement summary metrics.
#'
#' \code{predict_ranging} uses temporally varying summary information from tracking data of individuals
#' to predict whether ranging behaviour occurred or not. The input data will have been prepared by \code{\link{data_prep}}, and a model may have been created by \code{\link{train_home_range_detection}} if suitable training data exist from the
#' studied population. If no training data exist, and no model has been created, the function will draw on a model created by the Swiss Ornithological Institute for red kites tracked in Switzerland.
#' The Random Forest model either created by \code{\link{train_home_range_detection}} or contained in the package will be used to predict the probability that a given individual in the season showed ranging behaviour or not. 
#'

#' @param model RandomForest model object created with \code{\link{train_home_range_detection}}. If none is provided, a model trained for red kites in Switzerland is used.
#' @param trackingsummary data.frame created by \code{\link{data_prep}} with information on individual identity, age, sex, and seasonal movement metrics.
#' If this file results from the output of \code{\link{predict_ranging}}, then no further prediction will be performed as the prediction is already contained in the file.
#' @return Returns a data.frame with predicted probabilities of a home range, including all input data for further use in \code{\link{predict_success}}.
#'
#' @export 
#' @importFrom here here
#' @importFrom dplyr rename bind_cols
#' @importFrom stats predict
#'

predict_ranging <- function(model = readRDS(here::here("data/hr_model.rds"))$model,
                            trackingsummary) {
  
###### IDENTIFYING HOME RANGES OF RED KITES BASED ON GPS TRACKING DATA ################
# original script and data prepared by Ursin Beeli 6 May 2023
# data input includes extraction of most frequently used day and night locations, and distances between them
# modified by Steffen Oppel 8 May 2023 to identify misclassifications and match with those misclassified by nestR
# expanded on 8 May 2023 with more input variables

  
#### classification success of training data
  if("hr_prob" %in% names(trackingsummary)){
    OUT<-nestingsummary   ### there is no need to predict ranging again if it is already in the data frame
  } else{
    
    if("HR" %in% names(trackingsummary)){
      trackingsummary <- trackingsummary %>% dplyr::rename(hr_observed = HR)
    }
    PRED<-stats::predict(model,data=trackingsummary, type = "response")
    OUT<-trackingsummary %>%
      dplyr::bind_cols(PRED$predictions) %>%
      dplyr::rename(no_hr_prob = no, hr_prob = yes)}

return(OUT)

}

