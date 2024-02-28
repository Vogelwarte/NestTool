## train_nest_detection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Train a model that predicts whether nesting occurred based on movement summary metrics.
#'
#' \code{train_nest_detection} uses temporally varying summary information from tracking data of individuals
#' to help diagnose whether a breeding attempt occurred or not. The input data will have been prepared by \code{\link{data_prep}}, and are passed into a RandomForest model that uses the provided information on whether an individual had a nest or not in a given season.
#' The model is evaluated using internal cross-validation and a plot of variable importance and accuracy tables are returned. 
#'
#'
#' @param trackingsummary data.frame created by \code{\link{data_prep}} with information on individual identity, age, sex, and seasonal movement metrics.
#' Must contain a column 'nest' with two possible values, 'no nest' or 'nest' (or 0 or 1) to be able to train the model (at least 5 of each are required). If no such data are available, use \code{\link{predict_nesting}} instead.
#' @param train_frac numeric. A fractional value (>0 and <1) that specifies what fraction of the data are used to train models. The remainder is used for model assessment. Default is 0.7.
#' @param plot logical. If TRUE, a variable importance plot is produced.
#' @return Returns a list with four elements: \code{model} is the random forest model to predict nesting;
#' \code{summary} is the output data.frame with predicted probabilities of a nest, including all input data for further use in \code{\link{predict_success}};
#' \code{eval_train} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on training data.
#' \code{eval_test} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on internally cross-validated test data.
#'
#'
#' @export 
#' @importFrom dplyr filter mutate arrange rename desc select bind_cols case_when bind_rows group_by
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom caret confusionMatrix
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip ylab xlab scale_y_continuous scale_x_discrete annotate theme element_rect element_text element_blank
#' 

train_nest_detection <- function(trackingsummary, train_frac=0.7, plot=TRUE) {
  
###### IDENTIFYING NESTS OF RED KITES BASED ON GPS TRACKING DATA ################
# original script and data prepared by Ursin Beeli 6 May 2023
# data input includes extraction of most frequently used day and night locations, and distances between them
# modified by Steffen Oppel 8 May 2023 to identify misclassifications and match with those misclassified by nestR
# expanded on 8 May 2023 with more input variables

### total script run time <5min

  
if(!("nest" %in% names(trackingsummary))){
  print("Your input datafile does not have a column labelled 'nest'. Without this you cannot train the model.
        Either add the column, rename a column that contains relevant information, or use the function 'predict_nesting' if you want to predict from an existing model.")
  break
}  
  
  if(train_frac>1){
    print("The training data fraction is poorly specified. Please specify a value <1 or omit 'train_frac' to accept the default value of 0.7")
    break
  }
  
  if(train_frac<0){
    print("The training data fraction is poorly specified. Please specify a value >0 or omit 'train_frac' to accept the default value of 0.7")
    break
  } 
  

# DATA PREPARATION -------------------------------------------------------------
# cast dependent variable and sex to factor
  if(is.numeric(trackingsummary$nest)==TRUE){
    trackingsummary$nest <- factor(ifelse(trackingsummary$nest==1,"nest","no nest"), levels = c("nest", "no nest"))
  } else{
    trackingsummary$nest <- factor(trackingsummary$nest, levels = c("nest", "no nest"))
  }

trackingsummary$sex <- factor(trackingsummary$sex, levels = c("m","f"))


# SPLIT DATA FRAME RANDOMLY INTO TWO WHILE KEEPING SEX RATIO CONSTANT --------------------------------------------
milvus_id_F <- unique(trackingsummary$year_id[trackingsummary$sex=="f"])
milvus_id_M <- unique(trackingsummary$year_id[trackingsummary$sex=="m"])
milvus_id_train_F <- sample(milvus_id_F, round(length(milvus_id_F)*train_frac))
milvus_id_train_M <- sample(milvus_id_M, round(length(milvus_id_M)*train_frac))

milvus_train_M <- trackingsummary %>%
  dplyr::filter(year_id %in% milvus_id_train_M)
milvus_test_M <- trackingsummary %>%
  dplyr::filter(!year_id %in% milvus_id_train_M)

milvus_train_F <- trackingsummary %>%
  dplyr::filter(year_id %in% milvus_id_train_F)
milvus_test_F <- trackingsummary %>%
  dplyr::filter(!year_id %in% milvus_id_train_F)
names(milvus_train)



# CREATE INFORMATIVE ERROR MESSAGE WHEN THERE ARE INSUFFICIENT DATA ---------------

if(min(table(trackingsummary$nest))<5){
  print(sprintf("There are only %i cases of the minority class in your input datafile, which is insufficient for model training and testing. Either add more balanced data to train a model, or use the function 'predict_nesting' if you want to predict from an existing model.",
                min(table(trackingsummary$nest))))
  break
} 


############### LOOP OVER TUNING SETTINGS TO IMPROVE PERFORMANCE ##################
dim.vars<-length(names(trackingsummary))-10
tuning.out<-expand.grid(m=seq(1:10),t=c(500,750,100,1500,2000,2500,5000)) %>%
  dplyr::mutate(oob.error.M=0,oob.error.F=0)
for (m in seq(1:10)) {
  for(t in c(500,750,100,1500,2000,2500,5000)){
    RFtest_M<-ranger::ranger(nest ~ revisits_day + residence_time_day + age_cy +
                             revisits_night + residence_time_night + 
                             dist_max_day_to_max_night + median_day_dist_to_max_night +
                             relative_dist_max_day_to_max_night+
                             revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                             timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                             Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                             MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                   data = milvus_train_M, mtry = m, num.trees = t, replace = T, importance = "permutation", oob.error=T, write.forest=F)
    tuning.out[tuning.out$t==t & tuning.out$m==m,3] <-RFtest_M$prediction.error
    RFtest_F<-ranger::ranger(nest ~ revisits_day + residence_time_day + age_cy +
                               revisits_night + residence_time_night + 
                               dist_max_day_to_max_night + median_day_dist_to_max_night +
                               relative_dist_max_day_to_max_night+
                               revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                               timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                               Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                               MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                             data = milvus_train_F, mtry = m, num.trees = t, replace = T, importance = "permutation", oob.error=T, write.forest=F)
    tuning.out[tuning.out$t==t & tuning.out$m==m,4] <-RFtest_F$prediction.error
  }
}




##### RUN MODEL ##########

tuning.out<-tuning.out %>% dplyr::arrange(oob.error.M)
RF2M <- ranger::ranger(nest ~ revisits_day + residence_time_day + age_cy +
                        revisits_night + residence_time_night + 
                        dist_max_day_to_max_night + median_day_dist_to_max_night +
                        relative_dist_max_day_to_max_night+
                        revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                        timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                        Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                        MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
              data=milvus_train_M, mtry=tuning.out$m[1], num.trees=tuning.out$t[1], replace=T, importance="permutation", oob.error=T, write.forest=T, probability=T)

tuning.out<-tuning.out %>% dplyr::arrange(oob.error.F)
RF2F <- ranger::ranger(nest ~ revisits_day + residence_time_day + age_cy +
                        revisits_night + residence_time_night + 
                        dist_max_day_to_max_night + median_day_dist_to_max_night +
                        relative_dist_max_day_to_max_night+
                        revisitsSettle+revisitsIncu1+revisitsIncu2+revisitsChick1+revisitsChick2+timeSettle+
                        timeIncu1+timeIncu2+timeChick1+ timeChick2+meandayrevisitsBrood+lastvisitDay+maxtimeawayBrood2km+maxtimeawayBrood+tottime_nest+
                        Dist99Chick2+Dist99Settle+Dist99Incu1+Dist99Incu2+Dist99Chick1+
                        MCP95Chick2+MCP95Chick1+MCP95Incu1+MCP95Settle+MCP95Incu2,
                      data=milvus_train_F, mtry=tuning.out$m[1], num.trees=tuning.out$t[1], replace=T, importance="permutation", oob.error=T, write.forest=T, probability=T)



IMP_M<-as.data.frame(RF2M$variable.importance) %>%
  dplyr::mutate(variable=names(RF2M$variable.importance)) %>%
  dplyr::rename(red.accuracy=`RF2M$variable.importance`) %>%
  dplyr::arrange(dplyr::desc(red.accuracy)) %>%
  dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  dplyr::select(variable,red.accuracy,rel.imp) %>%
  dplyr::mutate(sex="Male")

IMP_F<-as.data.frame(RF2F$variable.importance) %>%
  dplyr::mutate(variable=names(RF2F$variable.importance)) %>%
  dplyr::rename(red.accuracy=`RF2F$variable.importance`) %>%
  dplyr::arrange(dplyr::desc(red.accuracy)) %>%
  dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  dplyr::select(variable,red.accuracy,rel.imp) %>%
  dplyr::mutate(sex="Female")


#### classification success of training data

PRED_M<-stats::predict(RF2M,data=milvus_train_M, type = "response")
PRED_F<-stats::predict(RF2F,data=milvus_train_F, type = "response")

milvus_train_M <- milvus_train_M %>%
  dplyr::rename(nest_observed = nest) %>%
  dplyr::bind_cols(PRED_M$predictions) %>%
  dplyr::rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
  dplyr::mutate(nest_predicted = as.factor(dplyr::case_when(nest_prob > no_nest_prob ~ "nest",
                                              nest_prob < no_nest_prob ~ "no nest")))

suppressWarnings({trainmat_M<-caret::confusionMatrix(data = milvus_train_M$nest_observed, reference = milvus_train_M$nest_predicted)})

milvus_train_F <- milvus_train_F %>%
  dplyr::rename(nest_observed = nest) %>%
  dplyr::bind_cols(PRED_F$predictions) %>%
  dplyr::rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
  dplyr::mutate(nest_predicted = as.factor(dplyr::case_when(nest_prob > no_nest_prob ~ "nest",
                                                            nest_prob < no_nest_prob ~ "no nest")))

suppressWarnings({trainmat_F<-caret::confusionMatrix(data = milvus_train_F$nest_observed, reference = milvus_train_F$nest_predicted)})




#### classification success of test data

PRED_M<-stats::predict(RF2M,data=milvus_test_M, type = "response")
PRED_F<-stats::predict(RF2F,data=milvus_test_F, type = "response")

milvus_test_M <- milvus_test_M %>%
  rename(nest_observed = nest) %>%
  dplyr::bind_cols(PRED_M$predictions) %>%
  dplyr::rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
  dplyr::mutate(nest_predicted = as.factor(dplyr::case_when(nest_prob > no_nest_prob ~ "nest",
                                    nest_prob < no_nest_prob ~ "no nest")))

suppressWarnings({testmat_M<-caret::confusionMatrix(data = milvus_test_M$nest_observed, reference = milvus_test_M$nest_predicted)})

milvus_test_F <- milvus_test_F %>%
  rename(nest_observed = nest) %>%
  dplyr::bind_cols(PRED_F$predictions) %>%
  dplyr::rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
  dplyr::mutate(nest_predicted = as.factor(dplyr::case_when(nest_prob > no_nest_prob ~ "nest",
                                                            nest_prob < no_nest_prob ~ "no nest")))

suppressWarnings({testmat_F<-caret::confusionMatrix(data = milvus_test_F$nest_observed, reference = milvus_test_F$nest_predicted)})

## export data for further use in outcome prediction
OUT<-dplyr::bind_rows(milvus_train_M, milvus_train_F, milvus_test_M, milvus_test_F)


#### CREATE PLOT FOR VARIABLE IMPORTANCE
topvars<-dplyr::bind_rows(IMP_M,IMP_F) %>% group_by(sex) %>%
  dplyr::slice_max(.,order_by=rel.imp,n=10)
IMP<-dplyr::bind_rows(IMP_M,IMP_F) %>% 
  dplyr::filter(variable %in% unique(topvars$variable)) %>%
  dplyr::group_by(sex) %>%
  dplyr::arrange(sex,desc(rel.imp))

if(plot==T){
  ann_text <- data.frame(rel.imp=80,sex=c("Female","Male"),acc=c(paste("Accuracy = ",round(testmat_F$overall[1],3)),paste("Accuracy = ",round(testmat_M$overall[1],3))))
  impplot<-IMP %>%
    dplyr::mutate(variable=forcats::fct_relevel(variable,rev(unique(topvars$variable)))) %>%
    ggplot2::ggplot(ggplot2::aes(x=variable, y=rel.imp, order = -rel.imp, fill=sex)) +
    ggplot2::geom_bar(stat='identity', position =  position_dodge2(reverse=TRUE)) +
    ggplot2::coord_flip()+
    ggplot2::facet_wrap(~sex, ncol=2) +
    ggplot2::ylab("Variable importance (%)") +
    ggplot2::xlab("Explanatory variable") +
    ggplot2::scale_y_continuous(limits=c(-5,105), breaks=seq(0,100,20), labels=seq(0,100,20))+
    #ggplot2::annotate("text",x=2,y=80,label=paste("Accuracy = ",round(testmat_M$overall[1],3)),size=8) +
    ggplot2::geom_text(data=ann_text,mapping = aes(x = 0.6, y = 50, label = acc),hjust= -0.1,vjust = -1) +
    ggplot2::theme(panel.background=ggplot2::element_rect(fill="white", colour="black"), 
                   axis.text.x=ggplot2::element_text(size=18, color="black"),
                   axis.text.y=ggplot2::element_text(size=14, color="black"), 
                   axis.title=ggplot2::element_text(size=20),
                    legend.position="none",
                   strip.text=ggplot2::element_text(size=18, color="black"),
                   strip.background=ggplot2::element_rect(fill="white", colour="black"), 
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), 
                   panel.border = ggplot2::element_blank())
  print(impplot)
}


return(list(model=RF2,eval_train=trainmat,eval_test=testmat, summary=OUT))

}