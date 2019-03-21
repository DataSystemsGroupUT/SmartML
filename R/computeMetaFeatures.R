#' @title Compute Meta-Features.
#'
#' @description Compute Statistical Meta-Features for a dataset.
#'
#' @param dataset The dataframe containing the dataset to process.
#' @param maxTime The maximum time budget entered by user for the parameter optimization part (in minutes).
#' @param featureTypes Vector of Types of each feature in the dataset either ('numerical', 'categorical').
#'
#' @return dataframe with 25 statistical meta-feature of \code{dataset}.
#'
#' @examples
#' computeMetaFeatures(data.frame(salary = c(623, 515, 611, 729, 843), class = c (0, 0, 0, 1, 1)),  10, c('numerical', 'numerical')).
#'
#' @noRd
#'
#' @keywords internal

computeMetaFeatures <- function(dataset, maxTime, featureTypes) {
  #library(e1071)
  print('###################Preparation of Meta-Features of the Dataset###################')
  #1- number of instances
  nInstances <- nrow(dataset)
  cat(sprintf("1-Number of Instances: %d\n", nInstances))
  #2- log number of instances
  lognInstances <- log(nInstances)
  cat(sprintf("2-Log number of Instances: %f\n",lognInstances))
  #3- number of features
  nFeatures <- ncol(dataset) - 1
  cat(sprintf("3-Number of Features: %d\n", nFeatures))
  #4- log number of features
  lognFeatures <- log(nFeatures)
  cat(sprintf("4-Log number of Features: %f\n", lognFeatures))
  #5- number of classes
  classes <- unique(dataset$class)
  nClasses <- length(classes)
  cat(sprintf("5-Total number of Classes: %d\n", nClasses))
  #6- number of categorical features
  nCatFeatures <- 0
  nNumFeatures <- 0
  skewVector <- c()
  kurtosisVector <- c()
  symbolsVector <- c()
  featsType <- lapply(dataset, class)
  if(length(featureTypes) == 0){
    for(i in colnames(dataset)){
      if(i == 'class')next
      if(featsType[[i]] != 'factor' && featsType[[i]] != 'character' && length(unique(dataset[[i]])) > lognInstances){
        nNumFeatures <- nNumFeatures + 1
        skewVector <- c(skewVector, skewness(dataset[[i]]))
        kurtosisVector <- c(kurtosisVector, kurtosis(dataset[[i]]))
      }
      else{
        nCatFeatures <- nNumFeatures + 1
        symbolsVector <- c(symbolsVector, length(unique(dataset[[i]])))
      }
    }
  }
  else{
    counter <- 0
    for(i in colnames(dataset)){
      counter <- counter + 1
      if(i == 'class')next
      if(featureTypes[counter] == 'numerical'){
        nNumFeatures <- nNumFeatures + 1
        skewVector <- c(skewVector, skewness(dataset[[i]]))
        kurtosisVector <- c(kurtosisVector, kurtosis(dataset[[i]]))
      }
      else{
        nCatFeatures <- nNumFeatures + 1
        symbolsVector <- c(symbolsVector, length(unique(dataset[[i]])))
      }
    }
  }
  cat(sprintf("6-Number of Categorical Features: %d\n", nCatFeatures))
  #7- number of numerical features
  cat(sprintf("7-Number of Numerical Features: %d\n", nNumFeatures))
  #8- ratio of numerical to categorical features
  if(nNumFeatures > 0){
    ratioNumToCat <- nCatFeatures / nNumFeatures
  }
  else{
    ratioNumToCat <- 999999
  }
  cat(sprintf("8-Ratio of Categorical to Numerical Features %f\n", ratioNumToCat))
  #9- class entropy
  probClasses <- c()
  classEntropy <- 0
  for(i in classes){
    prob <- length(which(dataset$class==i))/nInstances
    probClasses <- c(probClasses, prob)
    classEntropy <- classEntropy - prob * log2(prob)
  }
  cat(sprintf("9-Class Entropy: %f\n", classEntropy))
  #10- class probability max
  classProbMax <- max(probClasses)
  cat(sprintf("10-Maximum Class Probability: %f\n", classProbMax))
  #11- class probability min
  classProbMin <- min(probClasses)
  cat(sprintf("11-Minimum Class Probability: %f\n", classProbMin))
  #12- class probability mean
  classProbMean <- mean(probClasses)
  cat(sprintf("12-Mean Class Probability: %f\n", classProbMean))
  #13- class probability std. dev
  classProbStdDev <- sqrt(var(probClasses))
  cat(sprintf("13-Standard Deviation of Class Probability: %f\n", classProbStdDev))
  #14- Symbols Mean
  if(length(symbolsVector) > 0) symbolsMean <- mean(symbolsVector)
  else symbolsMean <- 'NULL'
  cat(sprintf("14-Mean of Number of Symbols: %s\n", symbolsMean))
  #15- Symbols sum
  if(length(symbolsVector) > 0) symbolsSum <- sum(symbolsVector)
  else symbolsSum <- 'NULL'
  cat(sprintf("15-Sum of Number of Symbols: %s\n", symbolsSum))
  #16- Symbols Std. Deviation
  if(length(symbolsVector) > 0) symbolsStdDev <- sqrt(var(symbolsVector))
  else symbolsStdDev <- 'NULL'
  cat(sprintf("16-Std. Deviation of Number of Symbols: %s\n", symbolsStdDev))
  #17- skewness min
  if(length(skewVector) > 0) featuresSkewMin <- try(min(skewVector))
  else featuresSkewMin <- 0
  cat(sprintf("17-Features Skewness Minimum: %s\n", featuresSkewMin))
  #18- skewness mean
  if(length(skewVector) > 0) featuresSkewMean <- try(mean(skewVector))
  else featuresSkewMean <- 0
  cat(sprintf("18-Features Skewness Mean: %s\n", featuresSkewMean))
  #19- skewness max
  if(length(skewVector) > 0) featuresSkewMax <- try(max(skewVector))
  else featuresSkewMax <- 0
  cat(sprintf("19-Features Skewness Maximum: %s\n", featuresSkewMax))
  #20- skewness std. dev.
  if(length(skewVector) > 0) featuresSkewStdDev <- try(sqrt(var(skewVector)))
  else featuresSkewStdDev <- 0
  cat(sprintf("20-Features Skewness Std. Deviation: %s\n", featuresSkewStdDev))
  #21- Kurtosis min
  if(length(kurtosisVector) > 0) featuresKurtMin <- try(min(kurtosisVector))
  else featuresKurtMin <- 0
  cat(sprintf("21-Features Kurtosis Min: %s\n", featuresKurtMin))
  #22- Kurtosis max
  if(length(kurtosisVector) > 0) featuresKurtMax <- try(max(kurtosisVector))
  else featuresKurtMax <- 0
  cat(sprintf("22-Features Kurtosis Max: %s\n", featuresKurtMax))
  #23- Kurtosis mean
  if(length(kurtosisVector) > 0) featuresKurtMean <- try(mean(kurtosisVector))
  else featuresKurtMean <- 0
  cat(sprintf("23-Features Kurtosis Mean: %s\n", featuresKurtMean))
  #24- Kurtosis std. dev.
  if(length(kurtosisVector) > 0) featuresKurtStdDev <- try(sqrt(var(kurtosisVector)))
  else featuresKurtStdDev <- 0
  cat(sprintf("24-Features Kurtosis Std. Deviation: %s\n", featuresKurtStdDev))
  #25- Dataset Ratio (ratio of number features: number of instances)
  datasetRatio <- nFeatures / nInstances
  cat(sprintf("25-Dataset Ratio: %f\n", datasetRatio))

  #Collecting Meta-Features in a dataFrame
  df <- data.frame(datasetRatio = datasetRatio, featuresKurtStdDev = featuresKurtStdDev,
                   featuresKurtMean = featuresKurtMean, featuresKurtMax = featuresKurtMax,
                   featuresKurtMin = featuresKurtMin, featuresSkewStdDev = featuresSkewStdDev,
                   featuresSkewMean = featuresSkewMean, featuresSkewMax = featuresSkewMax,
                   featuresSkewMin = featuresSkewMin, symbolsStdDev = symbolsStdDev, symbolsSum = symbolsSum,
                   symbolsMean = symbolsMean, classProbStdDev = classProbStdDev, classProbMean = classProbMean,
                   classProbMax = classProbMax, classProbMin = classProbMin, classEntropy = classEntropy,
                   ratioNumToCat = ratioNumToCat, nCatFeatures = nCatFeatures, nNumFeatures = nNumFeatures,
                   nInstances = nInstances, nFeatures = nFeatures, nClasses = nClasses,
                   lognFeatures = lognFeatures, lognInstances = lognInstances, maxTime = maxTime)
  return(df)
}
