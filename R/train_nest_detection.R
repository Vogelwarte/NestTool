## train_nest_detection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Train a model that predicts whether nesting occurred based on movement summary metrics.
#'
#' \code{train_nest_detection} uses temporally varying summary information from tracking data of individuals
#' to help diagnose whether a breeding attempt occurred or not. The input data will have been prepared by \code{\link{data_prep}}, and are passed into a RandomForest model that uses the provided information on whether an individual had a nest or not in a given season.
#' The model is evaluated using internal cross-validation and a plot of variable importance and accuracy tables are returned. 
#'
#'
#' @param trackingsummary data.frame created by \code{\link{data_prep}} with information on individual identity, age, sex, and seasonal movement metrics.
#' Must contain a column 'nest' with two possible values, 'no nest' or 'nest' to be able to train the model. If no such data are available, use \code{\link{predict_nesting}} instead.
#' @param plot logical. If TRUE, a variable importance plot is produced.
#' @return Returns a list with four elements: \code{model} is the random forest model to predict nesting;
#' \code{summary} is the output data.frame with predicted probabilities of a nest, including all input data for further use in \code{\link{predict_success}};
#' \code{eval_train} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on training data.
#' \code{eval_test} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on internally cross-validated test data.
#'
#'
#' @export
#' @importFrom base as.data.frame as.factor c expand.grid factor length list max min names paste print return round sample seq suppressWarnings unique
#' @importFrom dplyr filter select bind_rows case_when arrange rename mutate bind_cols desc
#' @importFrom ggplot2 ggplot aes element_rect element_text theme annotate coord_flip element_blank geom_bar scale_x_discrete scale_y_continuous xlab ylab
#' @importFrom caret confusionMatrix
#' @importFrom ranger ranger
#' @importFrom forcats fct_relevel
#' @importFrom stats predict var
#' 

train_nest_detection <- function(trackingsummary, plot=TRUE) {
  
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

# DATA PREPARATION -------------------------------------------------------------
# cast dependend variable and sex to factor
trackingsummary$nest <- factor(trackingsummary$nest, levels = c("nest", "no nest"))
trackingsummary$sex <- factor(trackingsummary$sex, levels = c("m","f"))


# SPLIT DATA FRAME RANDOMLY INTO TWO--------------------------------------------
milvus_id <- unique(trackingsummary$year_id)
milvus_id_train <- sample(milvus_id, round(length(milvus_id)*.7))

milvus_train <- trackingsummary %>%
  filter(year_id %in% milvus_id_train)
milvus_test <- trackingsummary %>%
  filter(!year_id %in% milvus_id_train)
names(milvus_train)

############### LOOP OVER TUNING SETTINGS TO IMPROVE PERFORMANCE ##################
dim.vars<-length(names(trackingsummary))-10
tuning.out<-expand.grid(m=seq(1:10),t=c(500,750,100,1500,2000,2500,5000)) %>%
  mutate(oob.error=0)
for (m in seq(1:10)) {
  for(t in c(500,750,100,1500,2000,2500,5000)){
    RFtest<-ranger(nest ~ sex + revisits_day + residence_time_day +
                     revisits_night + residence_time_night +
                     dist_max_day_to_max_night + median_day_dist_to_max_night +
                     relative_dist_max_day_to_max_night+
                     maxtimeawayBrood+tottime100m,
                   data = milvus_train, mtry = m, num.trees = t, replace = T, importance = "permutation", oob.error=T, write.forest=F)
    tuning.out[tuning.out$t==t & tuning.out$m==m,3] <-RFtest$prediction.error
  }
}

tuning.out<-tuning.out %>% arrange(oob.error)


##### RUN MODEL ##########


RF2 <- ranger(nest ~ sex + revisits_day + residence_time_day + 
                revisits_night + residence_time_night + 
                dist_max_day_to_max_night + median_day_dist_to_max_night +
                relative_dist_max_day_to_max_night+
                maxtimeawayBrood+tottime100m,
              data=milvus_train, mtry=tuning.out$m[1], num.trees=tuning.out$t[1], replace=T, importance="permutation", oob.error=T, write.forest=T, probability=T)

IMP<-as.data.frame(RF2$variable.importance) %>%
  mutate(variable=names(RF2$variable.importance)) %>%
  rename(red.accuracy=`RF2$variable.importance`) %>%
  arrange(desc(red.accuracy)) %>%
  mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  select(variable,red.accuracy,rel.imp)


#### classification success of training data

PRED<-predict(RF2,data=milvus_train, type = "response")

milvus_train <- milvus_train %>%
  rename(nest_observed = nest) %>%
  bind_cols(PRED$predictions) %>%
  rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
  mutate(nest_predicted = as.factor(case_when(nest_prob > no_nest_prob ~ "nest",
                                              nest_prob < no_nest_prob ~ "no nest")))

suppressWarnings({trainmat<-confusionMatrix(data = milvus_train$nest_observed, reference = milvus_train$nest_predicted)})

#### classification success of test data

PRED<-predict(RF2,data=milvus_test, type = "response")

milvus_test <- milvus_test %>%
  rename(nest_observed = nest) %>%
  bind_cols(PRED$predictions) %>%
  rename(no_nest_prob = `no nest`, nest_prob = nest) %>%
  mutate(nest_predicted = as.factor(case_when(nest_prob > no_nest_prob ~ "nest",
                                    nest_prob < no_nest_prob ~ "no nest")))

suppressWarnings({testmat<-confusionMatrix(data = milvus_test$nest_observed, reference = milvus_test$nest_predicted)})

## export data for further use in outcome prediction
OUT<-bind_rows(milvus_train, milvus_test)


#### CREATE PLOT FOR VARIABLE IMPORTANCE
if(plot==T){
  impplot<-ggplot(IMP, aes(x=variable, y=rel.imp)) +
  geom_bar(stat='identity', fill='lightblue') +
  coord_flip()+
  ylab("Variable importance (%)") +
  xlab("Explanatory variable") +
  scale_y_continuous(limits=c(-5,105), breaks=seq(0,100,20), labels=seq(0,100,20))+
  scale_x_discrete(name="",limit = c("maxtimeawayBrood","revisits_night","dist_max_day_to_max_night","revisits_day","residence_time_night","residence_time_day","median_day_dist_to_max_night","tottime100m"),
                   labels = c("max time away from nest",
                              "n nighttime visits",
                              "night time in nestradius",
                              "dist nest to night roost",
                              "n daytime visits",
                              "day time in nestradius", 
                              "median dist to night roost",
                              "total time in nestradius"))  +
  annotate("text",x=2,y=80,label=paste("Accuracy = ",round(testmat$overall[1],3)),size=8) +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
  print(impplot)
}

return(list(model=RF2,eval_train=trainmat,eval_test=testmat, summary=OUT))

}