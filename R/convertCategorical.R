#' @title Convert Categorical to Numerical Features.
#'
#' @description Perform One-Hot-Encoding for the categorical features to convert them to numerical ones.
#'
#' @param dataset List of training and validation dataframes containing the dataset to process.
#' @param trainDataset Dataframe of full training set
#' @param testDataset Dataframe of full testing set
#' @param B number of trees in the forest of trees of SMAC optimization algorithm (default = 10).
#'
#' @return List of data frames for the new dataset after encoding Ctegorical to numerical (TD = Training Dataset, VD = Validation Dataset, FD = Training Dataset after splitting it into \code{B} folds).
#'
#' @examples
#' convertCategorical(data.frame(salary = c(623, 515, 611, 729, 843), class = c (0, 0, 0, 1, 1)),  1).
#'
#' @import caret
#'
#' @noRd
#'
#' @keywords internal

convertCategorical <- function(dataset, trainDataset, testDataset, B = 10) {
  #Convert Factor/String Features into numeric features
  dmy <- caret::dummyVars(" ~ .", data = trainDataset[,names(trainDataset) != "class"])
  datasetTmp <- data.frame(predict(dmy, newdata = dataset$TD))
  dataset$FULLTD <- data.frame(predict(dmy, newdata = trainDataset))
  dataset$TED <- data.frame(predict(dmy, newdata = testDataset))

  datasetTmp$class <- dataset$TD$class
  dataset$TD <- datasetTmp
  dataset$FULLTD$class <- trainDataset$class
  dataset$TED$class <- testDataset$class

  if(nrow(dataset$VD) > 1){
    #dmy <- dummyVars(" ~ .", data = dataset$VD[,names(dataset$VD) != "class"])
    validationSet <- data.frame(predict(dmy, newdata = dataset$VD))
    validationSet$class <- dataset$VD$class
    dataset$VD <- validationSet
    dataset$FD <- createFolds(dataset$TD$class, k = B, list = TRUE, returnTrain = FALSE)
  }
  return(dataset)
}
