#' @title Perform Feature Preprocessing if specified by user.
#'
#' @description Perform a preprocessing algorithm on the dataset and return the preprocessed one.
#'
#' @param dataset Data frame containing the dataset to process.
#' @param preProcessF string containing the name of the preprocessing algorithm:
#' "boxcox": apply a Box–Cox transform and values must be non-zero and positive in all features,
#' "yeo-Johnson": apply a Yeo-Johnson transform, like a BoxCox, but values can be negative,
#' "zv": remove attributes with a zero variance (all the same value),
#' "center": subtract mean from values,
#' "scale": divide values by standard deviation,
#' "standardize": perform both centering and scaling,
#' "normalize": normalize values,
#' "pca": transform data to the principal components,
#' "ica": transform data to the independent components.
#' @param nComp Integer of Number of components needed if either "pca" or "ica" feature preprocessors are needed.
#'
#' @return Data frame of the preprocessed dataset.
#'
#' @examples featurePreProcessing(data.frame(salary = c(623, 515, 611, 729, 843), class = c (0, 0, 0, 1, 1)), "center", 0).
#'
#' @noRd
#'
#' @keywords internal

featurePreProcessing <- function(data, preProcessF, nComp) {
  library(caret)
  library(mlbench)
  library(fastICA)

  if(preProcessF == 'scale'){
    preprocessParams <- preProcess(data, method=c("scale"))
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'center'){
    preprocessParams <- preProcess(data, method=c("center"))
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'standardize'){
    preprocessParams <- preProcess(data, method=c("center", "scale"))
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'normalize'){
    preprocessParams <- preProcess(data, method=c("range"))
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'pca'){
    if (is.na(nComp))
      preprocessParams <- preProcess(data, method=c("center", "scale", "pca"))
    else
      preprocessParams <- preProcess(data, method=c("center", "scale", "pca"), pcaComp = nComp)

    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'ica'){
    preprocessParams <- preProcess(data, method=c("center", "scale", "ica"), n.comp=nComp)
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'yeo-Johnson'){
    preprocessParams <- preProcess(data, method=c("YeoJohnson"))
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'boxcox'){
    preprocessParams <- preProcess(data, method=c("BoxCox"))
    data <- predict(preprocessParams, data)
  }
  else if(preProcessF == 'zv'){
    preprocessParams <- preProcess(data, method=c("zv"))
    data <- predict(preprocessParams, data)
  }
  else{
    print('Error: No defined Preprocessing Algorithm...Skip feature preprocessing part!')
  }
  print('Summary After PreProcessing: ')
  print(summary(data))
  return (data)
}
