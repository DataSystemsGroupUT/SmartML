#' @title Get candidate Good Classifier Algorithms.
#'
#' @description Compare Dataset Meta-Features with the Knowledge base to recommend good Classifier Algorithms based on nearest neighbor datasets with outperformaing pipelines.
#'
#' @param maxTime Float of the maximum time budget allowed.
#' @param metaFeatures List of the meta-features collected from the dataset.
#' @param nModels Integer of number of required number of recommendations of classifier algorithms to get.
#'
#' @return List of recommended classifier algorithms, their initial parameter configurations, and time ratio to be spent in tuning each classifier.
#'
#' @examples getCandidateClassifiers(10, \code{metaFeatures}, 3)
#'
#' @importFrom BBmisc normalize
#' @importFrom RMySQL MySQL fetch dbDisconnect dbSendQuery dbConnect
#'
#' @noRd
#'
#' @keywords internal

getCandidateClassifiers <- function(maxTime, metaFeatures, nModels) {
  classifiers <- c('randomForest', 'c50', 'j48', 'deepboost', 'svm', 'naiveBayes','knn', 'bagging', 'neuralnet', 'plsda', 'part', 'fda', 'rpart', 'lda', 'lmt', 'rda')
  classifiersWt <- c(10, 13, 11, 21, 21, 10, 5, 25, 5, 6, 11, 20, 6, 5, 10)

  readKnowledgeBase <- try(
  {
    mydb <- ""
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
    nClasses <- metaDataFeatures$nClasses
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
    #Sort Dataframe
    orderInd <- order(distMat$dist)
    distMat <- distMat[orderInd, ]
    classifiers <- c()
    params <- c()
    #Get best classifiers with their parameters
    for(i in 1:nrow(distMat)){
      ind <- distMat[i,]$index
      clf <- bestClf[ind]
      if(is.element(clf, classifiers) == FALSE){
        #Exception for deep Boost requires binary classes dataset
        if(clf == 'deepboost' && nClasses > 2)
          next
        classifiers <- c(classifiers, clf)
        params <- c(params, bestClfParams[ind])
      }
      if(length(classifiers) == nModels)
        break
    }
  })
  if(inherits(readKnowledgeBase, "try-error")){
    print('Failed Downloading KnowledgeBase Data!...Check your internet connectivity. \n
            Assuming All Classifiers will be used....Consider Using Large Time Budget')
  }

  #Assign time ratio for each classifier
  sum <- 0
  ratio <- c()
  for (c in classifiers){
    ind = which(classifiers == c)
    sum <- sum + classifiersWt[ind]
    ratio <- c(ratio, classifiersWt[ind])
  }
  ratio <- ratio / sum * (maxTime * 0.9) #Only using 90% of the allowed time budget

  return (list(c = classifiers, r = ratio, p = params))
}
