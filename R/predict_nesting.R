## predict_nesting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Predict whether nesting occurred based on movement summary metrics.
#'
#' \code{predict_nesting} uses temporally varying summary information from tracking data of individuals
#' to predict whether a breeding attempt occurred or not. The input data will have been prepared by \code{\link{data_prep}}, and a model may have been created by \code{\link{train_nest_detection}} if suitable training data exist from the
#' studied population. If no training data exist, and no model has been created, the function will draw on a model created by the Swiss Ornithological Institute for Red Kites tracked in Switzerland.
#' The Random Forest model either created by \code{\link{train_nest_detection}} or contained in the package will be used to predict the probability that a given individual in the season initiated a nesting attempt or not. 
#'

#' @param model RandomForest model object created with \code{\link{train_nest_detection}}. If none is provided, a model trained for red kites in Switzerland can be used with \code{NestTool::nest_model}.
#' @param trackingsummary data.frame created by \code{\link{data_prep}} with information on individual identity, age, sex, and seasonal movement metrics.
#' If this file results from the output of \code{\link{predict_nesting}}, then no further prediction will be performed as the prediction is already contained in the file.
#' @return Returns a data.frame with predicted probabilities of a nest (called \code{nest_prob}), including all input data for further use in \code{\link{predict_success}}. The first five columns of this data.frame are the year_id, sex, age_cy, and nest probabilities, the remaining columns consist of input data.
#'
#' @export
#' @importFrom here here
#' @importFrom dplyr rename bind_cols relocate
#' @importFrom stats predict
#'

predict_nesting <- function(model,
                            trackingsummary) {
  
###### IDENTIFYING NESTS OF RED KITES BASED ON GPS TRACKING DATA ################
# original script and data prepared by Ursin Beeli 6 May 2023
# data input includes extraction of most frequently used day and night locations, and distances between them
# modified by Steffen Oppel 8 May 2023 to identify misclassifications and match with those misclassified by nestR
# expanded on 8 May 2023 with more input variables

  
#### classification success of training data
  if("nest_prob" %in% names(trackingsummary)){
    OUT<-trackingsummary   ### there is no need to predict nesting  again if it is already in the data frame
  } else{
    
    if("nest" %in% names(trackingsummary)){
      trackingsummary <- trackingsummary %>% dplyr::rename(nest_observed = nest)
    }
    PRED<-stats::predict(model,data=trackingsummary, type = "response")
    OUT<-trackingsummary %>%
      dplyr::bind_cols(PRED$predictions) %>%
      dplyr::rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
      dplyr::relocate(sex,age_cy,nest_prob,no_nest_prob,.after = year_id)}

return(OUT)

}

