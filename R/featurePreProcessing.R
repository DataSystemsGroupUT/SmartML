#' @title featurePreProcessing
#' @description perform feature preprocessing
#' @param directory the path to the dataset
#' @keywords AutoML, SMAC
#' @seealso \code{\link[utils]{head}}
#' @return dataset after performing feature preprocessing
#' @examples \dontrun{ featurePreProcessing(data, preProcess, featuresToPreProcess)
#' }
featurePreProcessing <- function(data, preProcessF, nComp) {
  library(caret)
  library(mlbench)
  library(fastICA)
  #“BoxCox“: apply a Box–Cox transform, values must be non-zero and positive.
  #“YeoJohnson“: apply a Yeo-Johnson transform, like a BoxCox, but values can be negative.
  #“zv“: remove attributes with a zero variance (all the same value).
  #“center“: subtract mean from values.
  #“scale“: divide values by standard deviation.
  #“range“: normalize values.
  #“pca“: transform data to the principal components.
  #“ica“: transform data to the independent components.

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
    print('Error: Not defined Preprocessing Algorithm...Skip feature preprocessing part!')
  }
  print('PreProcessing Summary: ')
  print(summary(data))
  return (data)
}
