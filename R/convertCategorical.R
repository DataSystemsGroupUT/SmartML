#' @title One-Hot Encoding for Categorical Features
#' @description Covnvert each categorical feature into numrical ones using one hot encoding
#' @param dataset dataset for which we will convert the categorical features into numerical
#' @keywords AutoML, SMAC
#' @seealso \code{\link[utils]{head}}
#' @return dataset after performing one-hot encoding on categorical features
#' @examples \dontrun{ convertCategorical(dataSet)
#' }
convertCategorical <- function(dataset, B = 10) {
  library(caret)
  #Convert Factor/String Features into numeric features
  dmy <- dummyVars(" ~ .", data = dataset$TD[,names(dataset$TD) != "class"])
  trainingSet <- data.frame(predict(dmy, newdata = dataset$TD))
  trainingSet$class <- dataset$TD$class
  dataset$TD <- trainingSet

  dmy <- dummyVars(" ~ .", data = dataset$VD[,names(dataset$VD) != "class"])
  validationSet <- data.frame(predict(dmy, newdata = dataset$VD))
  validationSet$class <- dataset$VD$class
  dataset$VD <- validationSet

  dataset$FD <- createFolds(dataset$TD$class, k = B, list = TRUE, returnTrain = FALSE)
  return(dataset)
}
