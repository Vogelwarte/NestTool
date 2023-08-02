## train_nest_success ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Train a model that predicts whether nesting was successful based on movement summary metrics.
#'
#' \code{train_nest_success} uses temporally varying summary information from tracking data of individuals
#' to help diagnose whether a breeding attempt was successful or not. The input data will have been prepared by \code{\link{data_prep}}, and are passed into a RandomForest model that uses the provided information on whether an individual
#' nested successfully or not in a given season. The model uses patterns in the seasonal change in movements (birds that lose a nest will spend more time away from the nest's location) to predict the outcome.
#' The model is evaluated using internal cross-validation and a plot of variable importance and accuracy tables are returned. 
#'
#'
#' @param nestingsummary data.frame created by \code{\link{predict_nesting}} with information on individual identity, age, sex, seasonal movement metrics, and the probability that nesting occurred in that season.
#' Must contain a column 'success' with two possible values, 'yes' or 'no' (or 1 or 0) to be able to train the model (at least 5 of each are required) - these data will be read in by \code{\link{data_prep}} and should be present in the input data.frame \code{indseasondata}.
#' If no such data are available, use \code{\link{predict_success}} instead.
#' @param plot logical. If TRUE, a variable importance plot is produced.
#' @return Returns a list with five elements: \code{model} is the random forest model to predict nesting;
#' \code{summary} is the output data.frame with predicted probabilities of nesting success;
#' \code{eval_train} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on training data.
#' \code{eval_test} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on internally cross-validated test data.
#' \code{nest_cutoff} numeric value indicating the minimum of 'nest_prob' that the training data contained. This can be used as cutoff in \code{\link{predict_success}} to set success predictions to "no" for those seasons where a nesting attempt was very unlikely.
#'
#' @export
#' @importFrom dplyr filter rowwise mutate arrange rename desc select bind_cols case_when bind_rows
#' @importFrom stats var predict
#' @importFrom ranger ranger
#' @importFrom caret confusionMatrix
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip ylab xlab scale_y_continuous annotate theme element_rect element_text element_blank
#' 


train_nest_success <- function(nestingsummary, plot=TRUE) {
  
###### PREDICTING NEST OUTCOME OF RED KITES BASED ON GPS TRACKING DATA ################
# original script and data prepared by Ursin Beeli 6 May 2023
# all data are first run through "nest_detection" algorithm to be classified into nesting/not nesting
# subset of predicted nesting attempts is then combined with observed breeding outcome
# modified by Steffen Oppel on 8 May using the same movement metrics as for nest detection


if(!("success" %in% names(nestingsummary))){
  print("Your nestingsummary data.frame does not have a column labelled 'success'. Without this you cannot train the success model.
        Either add the column, rename a column that contains relevant information, or use the function 'predict_success' if you want to predict from an existing model.")
  break
}  



# DATA PREPARATION -------------------------------------------------------------
  
# cast dependent variable and sex to factor
  if(is.numeric(nestingsummary$success)==TRUE){
    nestingsummary$success <- factor(ifelse(nestingsummary$success==1,"yes", "no"), levels = c("yes", "no"))
  } else{
    nestingsummary$success <- factor(nestingsummary$success, levels = c("yes", "no"))
  }
  nestingsummary$sex <- factor(nestingsummary$sex, levels = c("m","f"))  
  
  
  
  
  
  
## calculate differences between variables

nestingsummary <- nestingsummary %>%
  dplyr::filter(success %in% c("yes","no"))  %>%     ### filter out all data that are not useful for training purposes
  dplyr::rowwise() %>%
  dplyr::mutate(DistDiffChick2=Dist95Chick2-Dist95Chick1,
         DistDiffChick1=Dist95Chick1-Dist95Incu2,
         DistDiffIncu2=Dist95Incu2-Dist95Incu1,
         MCPDiffChick2=MCP95Chick2-MCP95Chick1,
         MCPDiffChick1=MCP95Chick1-MCP95Incu2,
         MCPDiffIncu2=MCP95Incu2-MCP95Incu1,
         VarMCP=stats::var(c(MCP95Incu1,MCP95Incu2,MCP95Chick1,MCP95Chick2)),
         VarDist=stats::var(c(Dist95Incu1,Dist95Incu2,Dist95Chick1,Dist95Chick2)))





# SPLIT DATA FRAME RANDOMLY INTO TWO--------------------------------------------

milvus_id <- unique(nestingsummary$year_id)
training_ids <- sample(milvus_id, round(length(milvus_id)*.7))

nestingsummary_train <- nestingsummary %>%
  dplyr::filter(year_id %in% training_ids)
nestingsummary_test <- nestingsummary %>%
  dplyr::filter(!year_id %in% training_ids)
names(nestingsummary_train)



# CREATE INFORMATIVE ERROR MESSAGE WHEN THERE ARE INSUFFICIENT DATA ---------------

if(min(table(nestingsummary$success))<5){
  print(sprintf("There are only %i cases of the minority class in your input datafile, which is insufficient for model training and testing. Either add more balanced data to train a model, or use the function 'predict_success' if you want to predict from an existing model.",
                min(table(nestingsummary$success))))
  break
} 






############### LOOP OVER TUNING SETTINGS TO IMPROVE PERFORMANCE ##################
tuning.out<-expand.grid(m=seq(1:30),t=c(500,750,100,1500,2000,2500,5000)) %>%
  dplyr::mutate(oob.error=0)
for (m in seq(1:30)) {
  for(t in c(500,750,100,1500,2000,2500,5000)){
    RFtest<-ranger::ranger(success ~ sex + revisits_day + residence_time_day + age_cy +
                     revisits_night + residence_time_night +
                     dist_max_day_to_max_night + median_day_dist_to_max_night +
                     relative_dist_max_day_to_max_night+nest_prob+
                     revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                     timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime100m+
                     Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                     MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Incu2+MCP95Settle+
                     DistDiffChick2+DistDiffChick1+DistDiffIncu2+MCPDiffChick2+MCPDiffChick1+MCPDiffIncu2+VarMCP+VarDist,
                   data = nestingsummary_train, mtry = m, num.trees = t, replace = T, importance = "permutation", oob.error=T, write.forest=F)
    tuning.out[tuning.out$t==t & tuning.out$m==m,3] <-RFtest$prediction.error
  }
}

tuning.out<-tuning.out %>% dplyr::arrange(oob.error)


##### RUN MODEL ##########

RF4 <- ranger::ranger(success ~ sex + revisits_day + residence_time_day + age_cy +
                revisits_night + residence_time_night + 
                dist_max_day_to_max_night + median_day_dist_to_max_night +
                relative_dist_max_day_to_max_night+nest_prob+
                revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime100m+
                  Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                  MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2+
                DistDiffChick2+DistDiffChick1+DistDiffIncu2+MCPDiffChick2+MCPDiffChick1+MCPDiffIncu2+VarMCP+VarDist,
              data = nestingsummary_train, mtry=tuning.out$m[1], num.trees=tuning.out$t[1], replace=T, importance="permutation", oob.error=T, write.forest=T, probability=T)

IMP<-as.data.frame(RF4$variable.importance) %>%
  dplyr::mutate(variable=names(RF4$variable.importance)) %>%
  dplyr::rename(red.accuracy=`RF4$variable.importance`) %>%
  dplyr::arrange(dplyr::desc(red.accuracy)) %>%
  dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  dplyr::select(variable,red.accuracy,rel.imp)


#### classification success of training data

PRED<-stats::predict(RF4,data=nestingsummary_train, type = "response")

nestingsummary_train <- nestingsummary_train %>%
  dplyr::rename(success_observed = success) %>%
  dplyr::bind_cols(PRED$predictions) %>%
  dplyr::rename(no_succ_prob = no, succ_prob = yes) %>%
  dplyr::mutate(success_predicted = as.factor(dplyr::case_when(succ_prob > no_succ_prob ~ "yes",
                                              succ_prob < no_succ_prob ~ "no")))

suppressWarnings({eval_train<-caret::confusionMatrix(data = nestingsummary_train$success_observed, reference = nestingsummary_train$success_predicted)})



#### classification success of test data

PRED<-stats::predict(RF4,data=nestingsummary_test, type = "response")

nestingsummary_test <- nestingsummary_test %>%
  dplyr::rename(success_observed = success) %>%
  dplyr::bind_cols(PRED$predictions) %>%
  dplyr::rename(no_succ_prob = no, succ_prob = yes) %>%
  dplyr::mutate(success_predicted = as.factor(dplyr::case_when(succ_prob > no_succ_prob ~ "yes",
                                                 succ_prob < no_succ_prob ~ "no")))

suppressWarnings({eval_test<-caret::confusionMatrix(data = nestingsummary_test$success_observed, reference = nestingsummary_test$success_predicted)})


## export data for further test and validations
OUT<-dplyr::bind_rows(nestingsummary_train, nestingsummary_test)

#### CREATE PLOT FOR VARIABLE IMPORTANCE
if(plot==T){
mylevels<-IMP$variable[10:1]
impplot<-IMP[10:1,] %>%
  dplyr::mutate(variable=forcats::fct_relevel(variable,mylevels)) %>%
  ggplot2::ggplot(ggplot2::aes(x=variable, y=rel.imp)) +
  ggplot2::geom_bar(stat='identity', fill='lightblue') +
  ggplot2::coord_flip()+
  ggplot2::ylab("Variable importance (%)") +
  ggplot2::xlab("Explanatory variable") +
  ggplot2::scale_y_continuous(limits=c(-5,105), breaks=seq(0,100,20), labels=seq(0,100,20))+
  ggplot2::annotate("text",x=2,y=80,label=paste("Accuracy = ",round(eval_test$overall[1],3)),size=8) +
  ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"), 
        axis.text.x=ggplot2::element_text(size=18, color="black"),
        axis.text.y=ggplot2::element_text(size=16, color="black"), 
        axis.title=ggplot2::element_text(size=20), 
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), 
        panel.border = ggplot2::element_blank())
print(impplot)
}

return(list(model=RF4, summary=OUT, eval_train=eval_train, eval_test=eval_test, nest_cutoff=min(nestingsummary_train$nest_prob, na.rm=T)))

}  # end function







