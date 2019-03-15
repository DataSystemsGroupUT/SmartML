#' @title
#'
#' @description
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @noRd
#'
#' @keywords internal

getCandidateClassifiers <- function(maxTime=10, metaFeatures, nModels = 3) {
  library(RMySQL)
  library(BBmisc)
  classifiers <- c('svm', 'naiveBayes','knn', 'bagging', 'part', 'j48', 'randomForest', 'fda', 'c50', 'rpart', 'lda', 'lmt', 'rda', 'neuralnet', 'deepboost', 'plsda')
  classifiersWt <- c(21, 10, 5, 25, 11, 11, 10, 13, 20, 6, 5, 10, 5, 21, 6)
  qOut <- dbSendQuery(mydb, "select * from metafeatures")
  metaData <- fetch(qOut, n=-1)
  dbDisconnect(mydb)
  metaDataFeatures <- metaData
  #Remove useless columns for now
  metaDataFeatures$performance <- NULL
  metaDataFeatures$metric <- NULL
  metaDataFeatures$ipInserted <- NULL
  metaDataFeatures$maxTime <- NULL
  metaDataFeatures$dateInserted <- NULL
  metaDataFeatures$ID <- NULL
  metaFeatures$maxTime <- NULL

  #Separate Best Classifier Algorithms and Their Parameters
  bestClf <- metaDataFeatures$classifierAlgorithm
  bestClfParams <- metaDataFeatures$parameters
  metaDataFeatures$classifierAlgorithm <- NULL
  metaDataFeatures$parameters <- NULL

  #Append new dataset meta features to the metaDataFeatures
  metaDataFeatures <- rbind(metaDataFeatures, metaFeatures)

  #Normalize the distance matrix
  metaDataFeatures[] <- lapply(metaDataFeatures, function(x) as.numeric(as.character(x)))
  metaDataFeatures <- normalize(metaDataFeatures, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")

  #Construct the distance list to extract the nearest neighbors
  cntMeta <- nrow(metaDataFeatures)
  distMat <- data.frame()
  distMat[['dist']] <- as.numeric()
  distMat[['index']] <- as.numeric()

  for(i in 1:(nrow(metaDataFeatures)-1)){
    dist <- 0
    for(j in 1:ncol(metaDataFeatures)){
      if(is.na(metaDataFeatures[i,j]) == TRUE && is.na(metaDataFeatures[cntMeta,j]) == TRUE)
        dist <- dist + 0

      else if ( (is.na(metaDataFeatures[i,j]) == TRUE && is.na(metaDataFeatures[cntMeta,j]) == FALSE)  || (is.na(metaDataFeatures[i,j]) == FALSE && is.na(metaDataFeatures[cntMeta,j]) == TRUE) )
        dist <- dist + 0.5

      else
        dist <- dist + (as.numeric(metaDataFeatures[i,j]) - as.numeric(metaDataFeatures[cntMeta, j]))^2

    }
    tmpDist <- list("dist" = dist, "index" = i)
    distMat <- rbind(distMat, tmpDist)
  }
  #print(distMat)
  #Sort Dataframe
  orderInd <- order(distMat$dist)
  distMat <- distMat[orderInd, ]
  classifiers <- c()
  params <- c()
  #print(distMat)
  #Get best classifiers with their parameters
  for(i in 1:nrow(distMat)){
    ind <- distMat[i,]$index
    clf <- bestClf[ind]
    if(is.element(clf, classifiers) == FALSE){
      classifiers <- c(classifiers, clf)
      params <- c(params, bestClfParams[ind])
    }
    if(length(classifiers) == nModels)
      break
  }
  #Assign time ratio for each classifier
  sum <- 0
  ratio <- c()
  for (c in classifiers){
    ind = which(classifiers == c)
    sum <- sum + classifiersWt[ind]
    ratio <- c(ratio, classifiersWt[ind])
  }
  ratio <- ratio / sum * maxTime

  #ratio <- c(0.1135135135, 0.05405405405, 0.02702702703, 0.1351351351, 0.05882352941, 0.05945945946, 0.05945945946, 0.05405405405, 0.03243243243, 0.07027027027, 0.1081081081,
  #           0.03243243243, 0.02702702703, 0.05405405405, 0.02702702703, 0.1135135135, 0.03243243243)

  #for(i in 1:length(ratio)){
  #  ratio[i] <- ratio[i] * maxTime
  #}
  return (list(c = classifiers, r = ratio, p = params))
}
