#' @title Perform Feature Preprocessing if specified by user.
#'
#' @description Perform a preprocessing algorithm on the dataset and return the preprocessed one.
#'
#' @param data Data frame containing the dataset to process.
#' @param dataTED Data frame containing the test dataset to process.
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
#' @return List of two Dataframes of the preprocessed training and testing datasets.
#'
#' @examples featurePreProcessing(\code{data}, \code{dataTED}, "center", 0).
#'
#' @noRd
#'
#' @keywords internal

featurePreProcessing <- function(data, dataTED, preProcessF, nComp) {

  if(preProcessF == 'scale'){
    preprocessParams <- preProcess(data, method=c("scale"))
  }
  else if(preProcessF == 'center'){
    preprocessParams <- preProcess(data, method=c("center"))
  }
  else if(preProcessF == 'standardize'){
    preprocessParams <- preProcess(data, method=c("center", "scale"))
  }
  else if(preProcessF == 'normalize'){
    preprocessParams <- preProcess(data, method=c("range"))
  }
  else if(preProcessF == 'pca'){
    if (is.na(nComp))
      preprocessParams <- preProcess(data, method=c("center", "scale", "pca"))
    else
      preprocessParams <- preProcess(data, method=c("center", "scale", "pca"), pcaComp = nComp)
  }
  else if(preProcessF == 'ica'){
    preprocessParams <- preProcess(data, method=c("center", "scale", "ica"), n.comp=nComp)
  }
  else if(preProcessF == 'yeo-Johnson'){
    preprocessParams <- preProcess(data, method=c("YeoJohnson"))
  }
  else if(preProcessF == 'boxcox'){
    preprocessParams <- preProcess(data, method=c("BoxCox"))
  }
  else if(preProcessF == 'zv'){
    preprocessParams <- preProcess(data, method=c("zv"))
  }
  else{
    print('Error: No defined Preprocessing Algorithm...Skip feature preprocessing part!')
    return(list(TD = data, TED = dataTED))
  }
  data <- predict(preprocessParams, data)
  dataTED <- predict(preprocessParams, dataTED)
  return(list(TD = data, TED = dataTED))
}
