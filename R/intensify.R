#' @title Intensify
#' @description Compare performance of Candidates compared to selected best Parameters
#' @param R list of tested configurations
#' @param bestparams set of best parameters found till now
#' @param bestPerf performance of best parameters on forest trees' folds
#' @param candidateConfs list of candidate configurations
#' @param foldedSet numbers of instances in each fold of trees of random forest
#' @param trainingSet the path to the dataset
#' @param validationSet maximum time in minutes to run the automation process
#' @param classifierAlgorithm type of classifier algorithm to use
#' @keywords AutoML, SMAC
#' @seealso \code{\link[utils]{head}}
#' @return performance of classifier Algorithm on validation set
#' @examples \dontrun{ runClassifier(trainingSet, validationSet, params, classifierAlgorithm)
#' }
intensify <- function(R, bestParams, bestPerf, candidateConfs, foldedSet, trainingSet, validationSet, classifierAlgorithm, maxTime, timeTillNow , B = 10) {
  cat('TIME TILL NOW: ', timeTillNow, '\n')
  for(j in 1:nrow(candidateConfs)){
    cntParams <- candidateConfs[j,]
    cntPerf <- c()
    folds <- sample(1:B)
    pointer <- 1
    timeFlag <- FALSE
    N <- 1
    #number of folds with higher performance for candidate configuration
    forMe <- 0
    #number of folds with lower performance for candidate configuration
    againstMe <- 0
    while(pointer < B){
      for(i in pointer:min(pointer+N-1, B)){
        cntPerf <- c(cntPerf, runClassifier(trainingSet[foldedSet[[i]], ], validationSet, cntParams, classifierAlgorithm))
        if(i > length(bestPerf))
          bestPerf <- c(bestPerf, runClassifier(trainingSet[foldedSet[[i]], ], validationSet, bestParams, classifierAlgorithm))

        if(cntPerf[i] >= bestPerf[i])forMe <- forMe + 1
        else againstMe <- againstMe + 1

        t <- toc(quiet = TRUE)
        timeTillNow <- timeTillNow + t$toc - t$tic
        tic(quiet = TRUE)
        if(timeTillNow > maxTime){
          timeFlag <- TRUE
          break
        }
      }
      if(forMe < againstMe || timeFlag == TRUE) break
      pointer <- pointer + N
      N <- N * 2
    }
    #make the current candidate as the best candidate
    if(timeFlag == FALSE && forMe > againstMe){
      bestParams <- cntParams
      bestPerf <- cntPerf
    }
    cntParams$performance <- mean(cntPerf)
    bestParams$performance <- mean(bestPerf)
    R <- rbind(R, cntParams)
  }
  return(list(params = bestParams, perf = bestPerf, r = R, timeTillNow = timeTillNow))
}
