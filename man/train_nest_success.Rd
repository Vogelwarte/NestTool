% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_nest_success.R
\name{train_nest_success}
\alias{train_nest_success}
\title{Train a model that predicts whether nesting was successful based on movement summary metrics.}
\usage{
train_nest_success(nestingsummary, train_frac = 0.7, plot = TRUE)
}
\arguments{
\item{nestingsummary}{data.frame created by \code{\link{predict_nesting}} with information on individual identity, age, sex, seasonal movement metrics, and the probability that nesting occurred in that season.
Must contain a column 'success' with two possible values, 'yes' or 'no' (or 1 or 0) to be able to train the model (at least 5 of each are required) - these data will be read in by \code{\link{data_prep}} and should be present in the input data.frame \code{indseasondata}.
If no such data are available, use \code{\link{predict_success}} instead.}

\item{train_frac}{numeric. A fractional value (>0 and <1) that specifies what fraction of the data are used to train models. The remainder is used for model assessment. Default is 0.7.}

\item{plot}{logical. If TRUE, a variable importance plot is produced.}
}
\value{
Returns a list with six elements: \code{model} is the random forest model to predict nesting;
\code{summary} is the output data.frame with predicted probabilities of nesting success;
\code{eval_train} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on training data.
\code{eval_test} is a confusion matrix and accuracy assessment of the predictive accuracy of the model on internally cross-validated test data.
\code{plot} if \code{plot==TRUE} then a variable importance plot is included showing the 10 most important variables for classification based on a permutation procedure.
\code{nest_cutoff} numeric value indicating the minimum of 'nest_prob' that the training data contained. This can be used as cutoff in \code{\link{predict_success}} to set success predictions to "no" for those seasons where a nesting attempt was very unlikely.
}
\description{
\code{train_nest_success} uses temporally varying summary information from tracking data of individuals
to help diagnose whether a breeding attempt was successful or not. The input data will have been prepared by \code{\link{data_prep}}, and are passed into a RandomForest model that uses the provided information on whether an individual
nested successfully or not in a given season. The model uses patterns in the seasonal change in movements (birds that lose a nest will spend more time away from the nest's location) to predict the outcome.
The model is evaluated using internal cross-validation and a plot of variable importance and accuracy tables are returned.
}
