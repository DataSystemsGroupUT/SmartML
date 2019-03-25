#' @title Select Candidate Parameter Configuration
#'
#' @description Generate neighbor parameter configurations, sort them according to the expected improvement, and select the top promising ones as candidate configurations.
#'
#' @param R Dataframe of tried out parameter configurations.
#' @param classifierAlgorithm String value of the classifier Name.
#' @param tree List of data frames, representing the data structure for the forest of trees of the SMAC model.
#' @param bestParams String of best parameter configuration found till now.
#' @param B number of trees in the forest of trees of SMAC optimization algorithm (default = 10).
#'
#' @return Vector of strings of candidate parameter configurations.
#'
#' @examples selectConfiguration(c('1'), 'knn', data.frame(fold = c(), parent = c(), params = c(), leftChild = c(), rightChild = c(), performance = c(), rowN = c()), '1', 10)
#'
#' @import rjson
#' @importFrom stats rnorm
#'
#' @noRd
#'
#' @keywords internal

selectConfiguration <- function(R, classifierAlgorithm, tree, bestParams, B = 10) {
  #Read Classifier Algorithm Configuration Parameters
  #Open the Classifier Parameters Configuration File
  classifierConfDir <- paste('./man/classifiersData/', classifierAlgorithm,'.json',sep="")
  result <- fromJSON(file = classifierConfDir)
  #get list of Classifier Parameters
  params <- result$params

  #minimum error rate found till now
  cmin <- (1 - bestParams$performance)

  #calculate Expected Improvement for all saved configurations
  for(i in 1:nrow(R)){
    cntParams <- R[i,]
    cntParamStr <- paste( unlist(cntParams), collapse='#')
    cntPerf <- c()
    #calculate Expected improvment from SMAC random forest model
    for(j in 1:B){
      cntNode <- tree[tree$fold==j & is.na(tree$parent), ]
      while(!is.na(cntNode[1])){
        cParent <- cntNode$rowN
        cntNode$params
        if(cntParamStr > as.character(cntNode$params) && !is.na(cntNode$rightChild)){
          cntNode <- tree[cntNode$rightChild, ]
        }
        else if(cntParamStr < as.character(cntNode$params) && !is.na(cntNode$leftChild)){
          cntNode <- tree[cntNode$leftChild, ]
        }
        else{
          cntPerf <- c(cntPerf, cntNode$performance)
          cntNode <- NA
        }
      }
    }
    cntParams$EI <- computeEI(cmin, cntPerf)
    R[i, ] <- cntParams
  }
  #sort according to Expected Improvement
  sortedR <- R[order(-R$EI),]
  #choose best promising configurations to suggest candidate configurations
  candidates <- R[0,]
  for(i in 1:min(10, nrow(R))){
    cntParams <- R[i,]
    for(parI in params){
      tmpParams <- cntParams
      cntParam <- cntParams[[parI]]
      if(is.na(cntParam))
        next
      #for continuous Integer parameters
      if(result[[parI]]$type == 'continuous' && result[[parI]]$scale == 'int'){
        minVal <- as.double(result[[parI]]$minVal)
        maxVal <- as.double(result[[parI]]$maxVal)
        cntParam <- as.double(cntParam)

        #generate a candidate
        parValues <- c(result[[parI]]$values)

        while(cntParam == cntParams[[parI]]){
          cntParam <- sample(minVal:maxVal, 1, TRUE)
          if(result[[parI]]$constraint == 'odd' && (cntParam %% 2) == 0)
            cntParam = cntParams[[parI]]
        }
        tmpParams[[parI]] <- cntParam
        gparams <- c(parI)
        i <- 1
        while(i <= length(gparams)){
          parTmp <- gparams[i]
          if(parTmp != parI){
            if(is.na(cntParams[[parTmp]]))tmpParams[[parTmp]] <- result[[parTmp]]$default
            else tmpParams[[parTmp]] <- cntParams[[parTmp]]
          }
          i <- i + 1
        }
        tmpParams$EI <- NA
        tmpParams$performance <- NA
        candidates <- rbind(candidates, tmpParams)
      }
      #for continuous Non-Integer parameters
      else if(result[[parI]]$type == 'continuous'){
        minVal <- as.double(result[[parI]]$minVal)
        maxVal <- as.double(result[[parI]]$maxVal)
        cntParam <- as.double(cntParam)
        meanU <- (cntParam - minVal)/(maxVal - minVal)
        #generate four candidates
        num <- 1
        while(num < 5){
          cntParam <- rnorm(1, mean = meanU, sd = 0.2)
          if(cntParam <= 1 && cntParam >= 0){
            num <- num + 1
            tmpParams[[parI]] <- as.character(cntParam * (maxVal - minVal) + minVal)
            tmpParams$EI <- NA
            tmpParams$performance <- NA
            candidates <- rbind(candidates, tmpParams)
          }
        }
      }
      #for Categorical (discrete parameters)
      else if(result[[parI]]$type == 'discrete'){
        parValues <- c(result[[parI]]$values)
        while(cntParam == cntParams[[parI]])
          cntParam <- sample(parValues, 1)
        tmpParams[[parI]] <- cntParam
        gparams <- c(parI)
        i <- 1
        while(i <= length(gparams)){
          parTmp <- gparams[i]
          if(parTmp != parI){
            if(is.na(cntParams[[parTmp]]))tmpParams[[parTmp]] <- result[[parTmp]]$default
            else tmpParams[[parTmp]] <- cntParams[[parTmp]]
          }
          require <- result[[parTmp]]$'requires'[[cntParam]]$require
          gparams <- c(gparams, require)
          i <- i + 1
        }
        tmpParams$EI <- NA
        tmpParams$performance <- NA
        candidates <- rbind(candidates, tmpParams)
      }
    }
  }
  candidates <- unique(candidates)

  #Remove Duplicate Candidate Configurations
  duplicates <- c()
  for(i in 1:nrow(candidates)){
    for(j in 1:nrow(R)){
      flager <- FALSE
      for(k in 1:(ncol(candidates)-2)){
        if((!is.na(candidates[i,k]) && !is.na(candidates[i,k])) || candidates[i,k] != R[j,k]){
          flager <- TRUE
          break
        }
      }
      if(flager == FALSE)
        duplicates <- c(duplicates, i)
    }
  }
  if(length(duplicates) > 0)
    candidates <- candidates[-duplicates,]
  #End Remove Candidate Configurations
  return(candidates)
}
