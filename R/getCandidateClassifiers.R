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
#' @importFrom httr POST content
#' @importFrom stats setNames
#'
#' @noRd
#'
#' @keywords internal

getCandidateClassifiers <- function(maxTime, metaFeatures, nModels) {
  classifiers <- c('randomForest', 'c50', 'j48', 'svm', 'naiveBayes','knn', 'bagging', 'rda', 'neuralnet', 'plsda', 'part', 'deepboost', 'rpart', 'lda', 'fda', 'lmt')
  classifiersWt <- c(10, 20, 11, 21, 10, 5, 25, 5, 5, 6, 11, 21, 6, 5, 13, 10) #weight of each classifier to tune based on number and types of parameters

  #Choosen Classifiers parameters initialization
  params <- c()
  cclassifiers <- c() #chosen classifiers
  ratio <- c() #time ratios for each classifier
  for(trial in 1:3){ #TRY to connect to knowledge base
    readKnowledgeBase <- try(
    {
      metaData <- content(POST("https://jncvt2k156.execute-api.eu-west-1.amazonaws.com/default/callKnowledgeBase"))
      KBFlag <- TRUE
      metaDataFeatures <- data.frame(matrix(unlist(metaData, recursive = FALSE), nrow = length(metaData), byrow = T))
      colnames(metaDataFeatures) <- c('datasetRatio', 'featuresKurtStdDev', 'featuresKurtMean', 'featuresKurtMax', 'featuresKurtMin', 'featuresSkewStdDev', 'featuresSkewMean', 'featuresSkewMax', 'featuresSkewMin', 'symbolsStdDev', 'symbolsSum', 'symbolsMean', 'classProbStdDev', 'classProbMean', 'classProbMax', 'classProbMin', 'classEntropy', 'ratioNumToCat', 'nCatFeatures', 'nNumFeatures', 'nInstances', 'nFeatures', 'nClasses', 'lognFeatures', 'lognInstances', 'classifierAlgorithm', 'parameters', 'maxTime', 'metric', 'performance')

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
      metaDataFeatures[] <- suppressWarnings(lapply(metaDataFeatures, function(x) as.numeric(as.character(x))))
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
            dist <- dist + (suppressWarnings(as.numeric(metaDataFeatures[i,j])) - suppressWarnings(as.numeric(metaDataFeatures[cntMeta, j])) )^2

        }
        tmpDist <- list(dist = dist, index = i)
        distMat <- rbind(distMat, tmpDist)
      }
      #Sort Dataframe
      orderInd <- order(distMat$dist)
      distMat <- distMat[orderInd, ]

      #Get best classifiers with their parameters
      for(i in 1:nrow(distMat)){
        ind <- distMat[i,]$index
        clf <- bestClf[ind]
        if(is.element(clf, cclassifiers) == FALSE){
          #Exception for deep Boost requires binary classes dataset
          if(clf == 'deepboost'  && nClasses > 2)
            next
          cclassifiers <- c(cclassifiers, clf)
          params <- c(params, bestClfParams[ind])

          clfInd = which(classifiers == clf)
          ratio <- c(ratio, classifiersWt[clfInd])
        }
        if(length(cclassifiers) == nModels)
          break
      }
    })
    if(inherits(readKnowledgeBase, "try-error")){
      KBFlag <- FALSE
      print('Warning: Can not connect to KnowledgeBase Data! Check your internet connectivity.
              Assuming Random Classifiers will be used. You should use Large Time Budgets and nModels for better results.')
    }

    if(KBFlag == TRUE) #managed to get information from knowledge base
      break
  }
  #Assign time ratio for each classifier
  if (length(cclassifiers) < nModels){ #failed to make use of meta-learning --> tune over all classifiers
    #cclassifiers <- classifiers
    for (clf in classifiers){
      if(is.element(clf, cclassifiers) == TRUE) #already inserted this classifier
        next
      ind = which(classifiers == clf)
      ratio <- c(ratio, classifiersWt[ind])
      cclassifiers <- c(cclassifiers, clf)
      params <- c(params, '')
      if(length(cclassifiers) == nModels) #completed number of required models
        break
    }
  }
  ratio <- ratio / sum(ratio) * (maxTime * 0.9) #Only using 90% of the allowed time budget

  return (list(c = cclassifiers, r = ratio, p = params))
}
