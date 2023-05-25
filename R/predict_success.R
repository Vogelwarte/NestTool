## predict_success ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Predict whether nesting was successful based on movement summary metrics.
#'
#' \code{predict_success} uses temporally varying summary information from tracking data of individuals
#' to predict whether a breeding attempt was successful or not. The input data will have been prepared by \code{\link{predict_nesting}}, and a model may have been created by \code{\link{train_nest_success}} if suitable training data exist from the
#' studied population. If no training data exist, and no model has been created, the function will draw on a model created by the Swiss Ornithological Institute for Red Kites tracked in Switzerland.
#' The Random Forest model either created by \code{\link{train_nest_success}} or contained in the package will be used to predict the probability that a given individual in the season successfully bred or not. 
#'
#'
#' @param model RandomForest model object created with \code{\link{train_nest_success}}. If none is provided, a model trained for red kites in Switzerland is used.
#' @param nestingsummary data.frame created by \code{\link{predict_nesting}} with information on individual identity, age, sex, seasonal movement metrics, and probability of nesting.
#' If this file results from the output of \code{\link{train_nest_success}}, then no further prediction will be performed as the prediction is already contained in the file.
#' @param nest_cutoff numeric between 0 and 1. Created by \code{\link{train_nest_success}} to indicate the minimum probability of a nesting attempt occurring in the training data for \code{model}
#' All nesting attempts that have a 'nest_prob' below this threshold will automatically be classified as unsuccessful.
#' 
#' @return Returns a data.frame with predicted probabilities of nest success.
#'
#' @export 
#' @importFrom here here
#' @importFrom dplyr rename mutate rowwise bind_cols
#' @importFrom stats var predict
#' 

predict_success <- function(model,
                            nestingsummary,
                            nest_cutoff) {
  
###### PREDICTING NEST OUTCOME OF RED KITES BASED ON GPS TRACKING DATA ################
# original script and data prepared by Ursin Beeli 6 May 2023
# all data are first run through "nest_detection" algorithm to be classified into nesting/not nesting
# subset of predicted nesting attempts is then combined with observed breeding outcome
# modified by Steffen Oppel on 8 May using the same movement metrics as for nest detection

# updated 10 May 2023: included 20 more variables to specify distances and MCP for various broodphases
# also calculated differences in distances and MCP


#### classification success of training data
if("success" %in% names(nestingsummary)){
  nestingsummary <- nestingsummary %>% dplyr::rename(success_observed = success)
}

if("succ_prob" %in% names(nestingsummary)){
  OUT<-nestingsummary %>%   ### there is no need to predict nesting success again if it is already in the data frame
    dplyr::mutate(succ_prob=ifelse(nest_prob<nest_cutoff,0,succ_prob))
} else{
  nestingsummary<-nestingsummary %>%
    dplyr::rowwise() %>%
    dplyr::mutate(DistDiffChick2=Dist95Chick2-Dist95Incu2,
           DistDiffChick1=Dist95Chick2-Dist95Incu2,
           DistDiffIncu2=Dist95Incu2-Dist95Incu1,
           MCPDiffChick2=MCP95Chick2-MCP95Incu2,
           MCPDiffChick1=MCP95Chick2-MCP95Incu2,
           MCPDiffIncu2=MCP95Incu2-MCP95Incu1,
           VarMCP=stats::var(c(MCP95Incu1,MCP95Incu2,MCP95Chick1,MCP95Chick2)),
           VarDist=stats::var(c(Dist95Incu1,Dist95Incu2,Dist95Chick1,Dist95Chick2)))
  nestingsummary$sex <- factor(nestingsummary$sex, levels = c("m","f"))
  PRED<-stats::predict(model,data=nestingsummary, type = "response")
  OUT<-nestingsummary %>%
    dplyr::bind_cols(PRED$predictions) %>%
    dplyr::rename(no_succ_prob = no, succ_prob = yes) %>%
    dplyr::mutate(succ_prob=ifelse(nest_prob<nest_cutoff,0,succ_prob)) ### manual adjustment of success = 0 when there was below-threshold probability of a nesting attempt occurring
}
return(OUT)

}


