#' @title Convert Categorical to Numerical Features.
#'
#' @description Perform One-Hot-Encoding for the categorical features to convert them to numerical ones.
#'
#' @param dataset List of training and validation dataframes containing the dataset to process.
#' @param B number of trees in the forest of trees of SMAC optimization algorithm (default = 10).
#'
#' @return List of data frames for the new dataset after encoding Ctegorical to numerical (TD = Training Dataset, VD = Validation Dataset, FD = Training Dataset after splitting it into \code{B} folds).
#'
#' @examples
#' convertCategorical(data.frame(salary = c(623, 515, 611, 729, 843), class = c (0, 0, 0, 1, 1)),  1).
#'
#' @noRd
#'
#' @keywords internal

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
